# Phase 1: CUSA All-Conference Random Forest Model
# OUTCOME (what we predict):  all_conference = 1 (selected) or 0 (not selected)
# PREDICTORS (inputs):         avg_PIE, PER, net_rating (BPM from Sports Reference), ts_percent, ws
#
# Validation:
#   PRIMARY   — leave-one-season-out (each year held out once)
#   SECONDARY — time-based forward test (train <= 2020, test 2021-2026)



# PART 1: Libraries

# tidyverse  - data wrangling (filter, mutate, read/write CSV, plotting)
# ranger     - fast Random Forest implementation
# vip        - variable importance plots
library(tidyverse)
library(ranger)
library(vip)

# Load cleaned data from cusa_modeling_data.R:
#   build_modeling_dataset(), season_cv_folds(), compute_binary_metrics(), recall_at_k()
source("cusa_modeling_data.R")

# Create the output folder if it does not exist 
dir.create("outputs/rf", recursive = TRUE, showWarnings = FALSE)


# PART 2: Build the modeling dataset

# Join All-Conference labels to player stats and writes modeling_dataset.csv
modeling <- build_modeling_dataset()

# Keep only rows where all five predictors are present 
# Convert all_conference to a factor so ranger treats this as classification
model_df <- modeling |>
  filter(
    !is.na(avg_PIE), !is.na(PER), !is.na(net_rating),
    !is.na(ts_percent), !is.na(ws)
  ) |>
  mutate(all_conference = factor(all_conference, levels = c(0, 1)))

# Print sample size info to the console while script running
cat("Complete-case rows:", nrow(model_df), "\n")
cat("Positive class:", sum(model_df$all_conference == 1), "\n")


# PART 3: Model formula and split weights

# List of column names used as inputs to the model
predictors <- c("avg_PIE", "PER", "net_rating", "ts_percent", "ws")

# Build formula: all_conference ~ avg_PIE + PER + net_rating + ts_percent + ws
formula <- as.formula(paste("all_conference ~", paste(predictors, collapse = " + ")))

# split.select.weights tells ranger how often each predictor is considered for splits.
# Higher = more influence. Tuned to DOWN-weight win shares and UP-weight the other four
# (avg_PIE, PER, BPM/net_rating, ts_percent). ranger requires weights in [0, 1],
# so we normalize by the maximum.
#
# Note: aggressively zeroing-out ws HURT recall@15 in tuning. A moderate down-weight
# plus an eligible-team ranking pool works better for hitting ~10 correct top-15 picks.
split_weight_raw <- c(
  avg_PIE = 1.8,
  PER = 1.6,
  net_rating = 1.7,
  ts_percent = 1.7,
  ws = 0.7
)
split_weights <- split_weight_raw / max(split_weight_raw)

# Conference-eligible teams for ranking: teams that had All-Conference representation
# that season (proxy for CUSA membership). Ranking only within this pool stops
# former/non-CUSA high-stat players from crowding the top-15 shortlist.
eligible_teams_by_season <- model_df |>
  filter(all_conference == 1) |>
  distinct(season, team_display_name)

filter_eligible <- function(df, season) {
  elig <- eligible_teams_by_season |> filter(.data$season == .env$season)
  df |> semi_join(elig, by = "team_display_name")
}


# PART 4: Set up leave-one-season-out cross-validation (CV)
# Instead of a random train/test split, we hold out ONE season at a time.
# Example: train on 2011-2024, test on 2025. Repeat for every season.
folds <- season_cv_folds(model_df)

# Empty containers to store results from each CV fold
cv_results <- vector("list", length(folds))           # metrics per season
season_predictions <- vector("list", length(folds))   # player-level predictions
all_truth <- c()   # actual labels pooled across all CV folds
all_prob <- c()    # predicted probabilities pooled across all CV folds


# PART 5: Cross-validation loop: train and test one season at a time

for (i in seq_along(folds)) {
  fold <- folds[[i]]
  train <- fold$train   # all seasons except the held-out year
  test <- fold$test     # the single held-out season

  # Skip this fold if training data has only one class (model cant learn)
  if (length(unique(train$all_conference)) < 2) next

  # Class weights address imbalance: ~15 All-Conference vs ~180+ non-selections per season. 
  # up-weight the minority class (all_conference = 1).
  n0 <- sum(train$all_conference == 0)  # count of negatives in training set
  n1 <- sum(train$all_conference == 1)  # count of positives in training set
  class_weights <- c(`0` = 1, `1` = n0 / max(n1, 1))

  # Train the Random Forest on the training seasons
  fit <- ranger(
    formula,
    data = train,
    probability = TRUE,              # return probabilities, not just 0/1 votes
    importance = "permutation",        # measure how much each predictor matters
    num.trees = 500,                   # number of trees in the forest
    class.weights = class_weights,     # handle class imbalance
    split.select.weights = as.numeric(split_weights[predictors]),
    seed = 42                          # fixed seed for reproducible results
  )

  # Predict probability of All-Conference (= 1) for each player in test season
  prob <- predict(fit, data = test)$predictions[, "1"]

  # Convert factor labels to integer 0/1 for metric functions
  truth <- as.integer(test$all_conference) - 1L

  # Rank top-15 only among conference-eligible teams for this season
  eligible_test <- test |>
    mutate(prob = prob, truth = truth) |>
    filter_eligible(fold$season)

  eligible_truth <- eligible_test$truth
  eligible_prob <- eligible_test$prob

  # Standard metrics on the full test fold; recall@15 on the eligible pool
  metrics <- compute_binary_metrics(truth, prob) |>
    mutate(
      season = fold$season,
      recall_at_15 = recall_at_k(eligible_truth, eligible_prob, k = 15),
      hits_at_15 = {
        ord <- order(eligible_prob, decreasing = TRUE)
        sum(eligible_truth[ord[seq_len(min(15, length(ord)))]] == 1)
      },
      eligible_players = nrow(eligible_test)
    )

  cv_results[[i]] <- metrics

  # Save test-set predictions with player names for export 
  season_predictions[[length(season_predictions) + 1]] <- test |>
    mutate(
      prob = prob,
      truth = truth
    )

  # Pool all CV predictions for an overall ROC curve at the end
  all_truth <- c(all_truth, truth)
  all_prob <- c(all_prob, prob)
}

# PART 6: Save cross-validation metrics

cv_metrics <- bind_rows(cv_results)
write_csv(cv_metrics, "outputs/rf/rf_metrics.csv")


# PART 6b: Secondary validation — time-based (train past, test future)
# Primary validation (Part 5) rotates each season out. This secondary check is stricter:
# train ONLY on older seasons, then predict newer ones (closer to "deploy once, use forward").
# Split: train 2011–2020, test each of 2021–2026. Same top-15 + eligible-team rules.

time_train_max <- 2020L
time_test_seasons <- 2021:2026

time_train <- model_df |> filter(season <= time_train_max)
time_n0 <- sum(time_train$all_conference == 0)
time_n1 <- sum(time_train$all_conference == 1)

time_fit <- ranger(
  formula,
  data = time_train,
  probability = TRUE,
  num.trees = 500,
  class.weights = c(`0` = 1, `1` = time_n0 / max(time_n1, 1)),
  split.select.weights = as.numeric(split_weights[predictors]),
  seed = 42
)

time_results <- map_dfr(time_test_seasons, function(s) {
  test <- model_df |> filter(season == s)
  if (nrow(test) == 0 || length(unique(time_train$all_conference)) < 2) {
    return(tibble())
  }

  prob <- predict(time_fit, data = test)$predictions[, "1"]
  scored <- test |>
    mutate(
      prob = prob,
      truth = as.integer(all_conference) - 1L
    ) |>
    filter_eligible(s)

  ord <- order(scored$prob, decreasing = TRUE)
  top_idx <- ord[seq_len(min(15, length(ord)))]
  hits <- sum(scored$truth[top_idx] == 1)
  n_pos <- sum(scored$truth == 1)

  tibble(
    validation = "time_based",
    train_seasons = paste0("<=", time_train_max),
    season = s,
    hits_at_15 = hits,
    recall_at_15 = if (n_pos == 0) NA_real_ else hits / n_pos,
    eligible_players = nrow(scored),
    n_actual = n_pos
  )
})

write_csv(time_results, "outputs/rf/rf_time_based_metrics.csv")

# Side-by-side summary for client slides: primary vs secondary validation
validation_compare <- bind_rows(
  cv_metrics |>
    transmute(
      validation = "leave_one_season_out",
      season,
      hits_at_15,
      recall_at_15
    ),
  time_results |>
    select(validation, season, hits_at_15, recall_at_15)
)
write_csv(validation_compare, "outputs/rf/rf_validation_comparison.csv")


# PART 7: Train a final model on ALL data (for variable importance)

# CV models are used for evaluation. 
# This final model uses every season to produce stable importance rankings and plots. 
# Not using it to claim CV performance — that comes from part 5.
final_fit <- ranger(
  formula,
  data = model_df,
  probability = TRUE,
  importance = "permutation",
  num.trees = 500,
  class.weights = c(
    `0` = 1,
    `1` = sum(model_df$all_conference == 0) / max(sum(model_df$all_conference == 1), 1)
  ),
  split.select.weights = as.numeric(split_weights[predictors]),
  seed = 42
)


# PART 8: Export top-K predicted vs actual players for one season

export_top15_vs_actual <- function(pred_df, target_season, k = 15) {
  # Subset to the season we want to report on (ex. 2025), then keep only
  # conference-eligible teams so the shortlist matches the real ballot pool
  fold <- pred_df |>
    filter(.data$season == target_season) |>
    filter_eligible(target_season)
  if (nrow(fold) == 0) {
    return(tibble())
  }

  # Model's top k players by predicted probability
  top_k <- fold |>
    arrange(desc(prob)) |>
    slice_head(n = min(k, nrow(fold))) |>
    mutate(
      season = target_season,
      list_type = "top_predicted",
      rank = row_number(),
      in_top_k = TRUE
    )

  # All actual All-Conference players that season, ranked by model probability
  actual <- fold |>
    filter(truth == 1L) |>
    arrange(desc(prob)) |>
    mutate(
      season = target_season,
      list_type = "actual",
      rank = row_number(),
      in_top_k = athlete_display_name %in% top_k$athlete_display_name
    )

  bind_rows(top_k, actual) |>
    select(
      season, list_type, rank, athlete_display_name, team_display_name,
      prob, truth, in_top_k, all_conference, avg_PIE, PER, net_rating, ts_percent, ws
    )
}

# Combine predictions from every CV fold and export 2025 comparison table
pred_by_season <- bind_rows(season_predictions)
top15_2025 <- export_top15_vs_actual(pred_by_season, target_season = 2025, k = 15)
write_csv(top15_2025, "outputs/rf/rf_top15_vs_actual_2025.csv")


# PART 9: Export variable importance

# Permutation importance: how much worse predictions get when the predictor is shuffled randomly 
# Higher value = more important to the model
importance_tbl <- tibble(
  variable = names(final_fit$variable.importance),
  importance = as.numeric(final_fit$variable.importance)
) |>
  arrange(desc(importance))

write_csv(importance_tbl, "outputs/rf/rf_variable_importance.csv")


# PART 10: Save plots

# Bar chart of permutation importance
p_imp <- vip(final_fit) +
  labs(title = "Random Forest Permutation Importance", x = NULL, y = "Importance")
ggsave("outputs/rf/rf_importance_plot.png", p_imp, width = 8, height = 5, dpi = 150)

# ROC curve from pooled CV predictions (requires pROC package)
if (requireNamespace("pROC", quietly = TRUE) && length(unique(all_truth)) > 1) {
  roc_obj <- pROC::roc(all_truth, all_prob, quiet = TRUE)
  roc_df <- tibble(
    fpr = 1 - roc_obj$specificities,   # false positive rate
    tpr = roc_obj$sensitivities         # true positive rate
  )
  p_roc <- ggplot(roc_df, aes(fpr, tpr)) +
    geom_line(linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      title = "Random Forest ROC (pooled CV predictions)",
      x = "False positive rate",
      y = "True positive rate"
    )
  ggsave("outputs/rf/rf_roc_plot.png", p_roc, width = 7, height = 5, dpi = 150)
}


# part 11: Print summary results to console

cat("\n--- Random Forest CV summary (PRIMARY: leave-one-season-out) ---\n")
cat("Split weights (raw):", paste(names(split_weight_raw), "=", split_weight_raw, collapse = ", "), "\n")
cat("Ranking pool: conference-eligible teams only\n")
print(cv_metrics |> summarize(
  mean_roc_auc = mean(roc_auc, na.rm = TRUE),
  mean_pr_auc = mean(pr_auc, na.rm = TRUE),
  mean_recall_at_15 = mean(recall_at_15, na.rm = TRUE),
  mean_hits_at_15 = mean(hits_at_15, na.rm = TRUE),
  median_hits_at_15 = median(hits_at_15, na.rm = TRUE),
  seasons_with_10plus_hits = sum(hits_at_15 >= 10, na.rm = TRUE),
  mean_f1 = mean(f1, na.rm = TRUE)
))
cat("\nHits in top-15 by season (leave-one-season-out):\n")
print(cv_metrics |> select(season, hits_at_15, recall_at_15, eligible_players))

cat("\n--- Random Forest time-based validation (SECONDARY: train <=", time_train_max, ") ---\n", sep = "")
cat("Test seasons:", paste(time_test_seasons, collapse = ", "), "\n")
print(time_results)
print(time_results |> summarize(
  mean_hits_at_15 = mean(hits_at_15, na.rm = TRUE),
  median_hits_at_15 = median(hits_at_15, na.rm = TRUE),
  mean_recall_at_15 = mean(recall_at_15, na.rm = TRUE),
  seasons_with_10plus_hits = sum(hits_at_15 >= 10, na.rm = TRUE)
))

cat("\nPermutation importance (final model):\n")
print(importance_tbl)
