# Purpose: justify WHICH variables enter the final 5-predictor models
#   1) Univariate logistic odds ratios for every numeric column in Graham's sheet
#   2) Ridge + LASSO logistic on a broader complete-case predictor set


library(tidyverse)
library(broom)

source("cusa_modeling_data.R")

dir.create("outputs/feature_screen", recursive = TRUE, showWarnings = FALSE)


# Data: Graham features + already-matched All-Conference labels


graham <- read_csv(file.path(DATA_DIR, "final_ncaa_dataset.csv"), show_col_types = FALSE) |>
  filter(season >= 2011, season <= 2026) |>
  mutate(
    # Align with model: use Sports Ref TS when present
    ts_percent = coalesce(ts_percent, TS_pct / 100),
    net_rating = bpm
  )

labels <- read_csv("modeling_dataset.csv", show_col_types = FALSE) |>
  select(athlete_display_name, team_display_name, season, all_conference)

screen_df <- graham |>
  inner_join(labels, by = c("athlete_display_name", "team_display_name", "season"))

# Every numeric column on Graham's sheet (plus net_rating for BPM)
id_cols <- c("athlete_display_name", "team_display_name", "season", "all_conference")
candidate_vars <- screen_df |>
  select(where(is.numeric), -any_of(c("season", "all_conference"))) |>
  names() |>
  setdiff(c("season"))

# Drop exact duplicate of coalesced ts_percent if both present; keep both TS_pct and ts_percent in univariate so the sheet is fully represented, but for
# regularized models drop TS_pct (redundant with ts_percent) and net_rating to avoid duplicates in the matrix.
regularized_drop <- c("TS_pct", "net_rating")
regularized_vars <- setdiff(candidate_vars, regularized_drop)

cat("Screening rows:", nrow(screen_df), "\n")
cat("Positive class:", sum(screen_df$all_conference == 1), "\n")
cat("Univariate candidates:", length(candidate_vars), "\n")
cat("Regularized candidates:", length(regularized_vars), "\n")

case_weights <- function(y) {
  n0 <- sum(y == 0)
  n1 <- sum(y == 1)
  ifelse(y == 1, n0 / max(n1, 1), 1)
}



# PART 1: Univariate logistic ORs (z-scored predictor, one at a time)



univariate_or <- function(data, var) {
  d <- data |>
    select(all_conference, value = all_of(var)) |>
    filter(!is.na(value), is.finite(value))

  n <- nrow(d)
  n_pos <- sum(d$all_conference == 1)
  if (n < 50 || n_pos < 5 || sd(d$value) == 0) {
    return(tibble(
      term = var,
      n = n,
      n_positive = n_pos,
      missing_rate = 1 - n / nrow(data),
      odds_ratio = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      note = "skipped (too few cases or zero variance)"
    ))
  }

  d <- d |>
    mutate(z = as.numeric(scale(value)))

  fit <- glm(
    all_conference ~ z,
    data = d,
    family = binomial(),
    weights = case_weights(d$all_conference)
  )

  tidy(fit, conf.int = TRUE, exponentiate = TRUE) |>
    filter(term == "z") |>
    transmute(
      term = var,
      n = n,
      n_positive = n_pos,
      missing_rate = 1 - n / nrow(data),
      odds_ratio = estimate,
      conf.low,
      conf.high,
      p.value,
      note = NA_character_
    )
}

univ_tbl <- map_dfr(candidate_vars, \(v) univariate_or(screen_df, v)) |>
  arrange(desc(odds_ratio))

# Flag the five variables used in the production RF / logistic models
final_five <- c("avg_PIE", "PER", "net_rating", "ts_percent", "ws")
# bpm is the source of net_rating on Graham's sheet
univ_tbl <- univ_tbl |>
  mutate(
    in_final_five = term %in% final_five | term == "bpm",
    final_five_note = case_when(
      term == "bpm" ~ "used as net_rating in RF/logistic",
      term %in% final_five ~ "in final RF/logistic",
      TRUE ~ NA_character_
    )
  )

write_csv(univ_tbl, "outputs/feature_screen/univariate_odds_ratios.csv")

p_univ <- univ_tbl |>
  filter(!is.na(odds_ratio)) |>
  mutate(term = fct_reorder(term, odds_ratio)) |>
  ggplot(aes(odds_ratio, term, color = in_final_five)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), orientation = "y", width = 0.2) +
  scale_x_log10() +
  scale_color_manual(
    values = c(`TRUE` = "firebrick", `FALSE` = "gray40"),
    labels = c(`TRUE` = "In final 5 (or BPM)", `FALSE` = "Screened only")
  ) +
  labs(
    title = "Univariate Logistic Odds Ratios (per 1 SD)",
    subtitle = "All numeric columns from final_ncaa_dataset.csv",
    x = "Odds ratio (log scale)",
    y = NULL,
    color = NULL
  )
ggsave(
  "outputs/feature_screen/univariate_odds_ratios.png",
  p_univ,
  width = 9,
  height = 10,
  dpi = 150
)




# PART 2: Ridge + LASSO on broader complete-case set




if (!requireNamespace("glmnet", quietly = TRUE)) {
  stop("glmnet is required for ridge/LASSO screening. install.packages('glmnet')")
}

reg_df <- screen_df |>
  select(all_conference, all_of(regularized_vars)) |>
  drop_na()

cat("Regularized complete-case rows:", nrow(reg_df), "\n")
cat("Regularized positives:", sum(reg_df$all_conference == 1), "\n")

x <- scale(as.matrix(reg_df[, regularized_vars]))
# protect against any zero variance columns after complete case filter
keep <- apply(x, 2, \(col) !anyNA(col) && sd(col) > 0)
x <- x[, keep, drop = FALSE]
y <- reg_df$all_conference
w <- case_weights(y)

set.seed(42)
ridge_cv <- glmnet::cv.glmnet(
  x = x,
  y = y,
  family = "binomial",
  alpha = 0,
  weights = w,
  standardize = FALSE
)

lasso_cv <- glmnet::cv.glmnet(
  x = x,
  y = y,
  family = "binomial",
  alpha = 1,
  weights = w,
  standardize = FALSE
)

coef_to_tbl <- function(cv_fit, model_name, s = "lambda.1se") {
  cm <- as.matrix(coef(cv_fit, s = s))
  tibble(
    term = rownames(cm),
    coefficient = cm[, 1],
    model = model_name,
    lambda_rule = s
  ) |>
    filter(term != "(Intercept)") |>
    mutate(
      odds_ratio = exp(coefficient),
      selected = coefficient != 0,
      in_final_five = term %in% c("avg_PIE", "PER", "ts_percent", "ws") |
        term == "bpm"
    ) |>
    arrange(desc(abs(coefficient)))
}

ridge_tbl <- coef_to_tbl(ridge_cv, "ridge")
lasso_1se <- coef_to_tbl(lasso_cv, "lasso", "lambda.1se")
lasso_min <- coef_to_tbl(lasso_cv, "lasso", "lambda.min")

reg_tbl <- bind_rows(ridge_tbl, lasso_1se, lasso_min)
write_csv(reg_tbl, "outputs/feature_screen/regularized_coefficients.csv")

# Compact LASSO selected comparison for the narrative
lasso_selected <- lasso_1se |>
  filter(selected) |>
  select(term, coefficient, odds_ratio, in_final_five)
write_csv(lasso_selected, "outputs/feature_screen/lasso_selected_lambda1se.csv")

p_lasso <- lasso_1se |>
  filter(selected) |>
  mutate(term = fct_reorder(term, abs(coefficient))) |>
  ggplot(aes(odds_ratio, term, color = in_final_five)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(size = 2.5) +
  scale_x_log10() +
  scale_color_manual(
    values = c(`TRUE` = "firebrick", `FALSE` = "gray40"),
    labels = c(`TRUE` = "In final 5 (or BPM)", `FALSE` = "Other survivor")
  ) +
  labs(
    title = "LASSO Logistic Survivors (lambda.1se)",
    subtitle = "Broader full predictor set; complete cases only",
    x = "Odds ratio (log scale)",
    y = NULL,
    color = NULL
  )
ggsave(
  "outputs/feature_screen/lasso_selected.png",
  p_lasso,
  width = 8,
  height = 6,
  dpi = 150
)

p_ridge <- ridge_tbl |>
  mutate(term = fct_reorder(term, abs(coefficient))) |>
  ggplot(aes(odds_ratio, term, color = in_final_five)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point(size = 2) +
  scale_x_log10() +
  scale_color_manual(
    values = c(`TRUE` = "firebrick", `FALSE` = "gray40"),
    labels = c(`TRUE` = "In final 5 (or BPM)", `FALSE` = "Screened only")
  ) +
  labs(
    title = "Ridge Logistic Coefficients (lambda.1se)",
    subtitle = "Broader full predictor set; complete cases only",
    x = "Odds ratio (log scale)",
    y = NULL,
    color = NULL
  )
ggsave(
  "outputs/feature_screen/ridge_coefficients.png",
  p_ridge,
  width = 8,
  height = 9,
  dpi = 150
)





# Console summary 





cat("\n--- Univariate OR leaders (top 10) ---\n")
print(
  univ_tbl |>
    filter(!is.na(odds_ratio)) |>
    select(term, odds_ratio, conf.low, conf.high, p.value, in_final_five, n) |>
    slice_head(n = 10)
)

cat("\n--- Final-five / BPM univariate ORs ---\n")
print(
  univ_tbl |>
    filter(in_final_five) |>
    select(term, odds_ratio, conf.low, conf.high, p.value, n, final_five_note)
)

cat("\n--- LASSO selected at lambda.1se ---\n")
print(lasso_selected)

cat("\n--- Ridge OR for final-five / BPM ---\n")
print(
  ridge_tbl |>
    filter(in_final_five) |>
    select(term, coefficient, odds_ratio)
)

cat("\nOutputs written to outputs/feature_screen/\n")

# helper table: where the final five stand in each screen
univ_ranked <- univ_tbl |>
  filter(!is.na(odds_ratio)) |>
  mutate(univariate_rank = row_number())

narrative <- univ_ranked |>
  filter(term %in% c(final_five, "bpm")) |>
  transmute(
    term,
    univariate_or = odds_ratio,
    univariate_rank,
    n_univariate = n
  ) |>
  left_join(
    ridge_tbl |> select(term, ridge_or = odds_ratio),
    by = "term"
  ) |>
  left_join(
    lasso_1se |> transmute(term, lasso_1se_selected = selected, lasso_1se_or = odds_ratio),
    by = "term"
  ) |>
  left_join(
    lasso_min |> transmute(term, lasso_min_selected = selected, lasso_min_or = odds_ratio),
    by = "term"
  )

write_csv(narrative, "outputs/feature_screen/final_five_vs_screen.csv")
cat("\n--- Final five vs screen summary ---\n")
print(narrative)
