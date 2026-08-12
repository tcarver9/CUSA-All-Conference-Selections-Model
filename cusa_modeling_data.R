# Shared data prep for CUSA All-Conference modeling.

# --- Libraries ---
library(tidyverse)   
library(fuzzyjoin)  
library(stringdist)  

# Folder with Graham's source files 
DATA_DIR <- "graham"

# Standardize player names so labels and features can be joined reliably
# "Chris Cokley Jr." and "chris cokley" both become "chris cokley"
normalize_name <- function(x) {
  x |>
    str_to_lower() |>
    str_remove_all("[\\.\\'\\-]") |>
    str_remove_all("\\b(jr|sr|iii|ii|iv|v)\\b") |>
    str_squish()
}

# Maps short school names in allconference.csv to full hoopR team names in final_ncaa_dataset.csv
# "Louisiana Tech" -> "Louisiana Tech Bulldogs"
school_crosswalk <- tibble(
  school_label = c(
    "Rice", "Louisiana Tech", "Florida Atlantic", "UAB", "Western Kentucky",
    "UCF", "Central Florida", "Missouri State", "Florida International", "Marshall",
    "New Mexico State", "Middle Tennessee", "Middle Tennessee State", "UTEP",
    "TCU", "Texas Christian", "Memphis", "Marquette", "Sam Houston", "Sam Houston State",
    "Liberty", "Tulsa", "Cincinnati", "Houston", "Jacksonville State",
    "Charlotte", "DePaul", "SMU", "Southern Methodist", "Louisville",
    "East Carolina", "Tulane", "Southern Miss", "Southern Mississippi",
    "Saint Louis", "Delaware", "Kennesaw State", "North Texas", "UTSA",
    "Old Dominion", "South Florida"
  ),
  team_display_name = c(
    "Rice Owls", "Louisiana Tech Bulldogs", "Florida Atlantic Owls", "UAB Blazers",
    "Western Kentucky Hilltoppers", "UCF Knights", "UCF Knights", "Missouri State Bears",
    "Florida International Panthers", "Marshall Thundering Herd", "New Mexico State Aggies",
    "Middle Tennessee Blue Raiders", "Middle Tennessee Blue Raiders", "UTEP Miners",
    "TCU Horned Frogs", "TCU Horned Frogs", "Memphis Tigers", "Marquette Golden Eagles",
    "Sam Houston Bearkats", "Sam Houston Bearkats", "Liberty Flames", "Tulsa Golden Hurricane",
    "Cincinnati Bearcats", "Houston Cougars", "Jacksonville State Gamecocks",
    "Charlotte 49ers", "DePaul Blue Demons", "SMU Mustangs", "SMU Mustangs",
    "Louisville Cardinals", "East Carolina Pirates", "Tulane Green Wave",
    "Southern Miss Golden Eagles", "Southern Miss Golden Eagles", "Saint Louis Billikens",
    "Delaware Blue Hens", "Kennesaw State Owls", "North Texas Mean Green", "UTSA Roadrunners",
    "Old Dominion Monarchs", "South Florida Bulls"
  )
) |>
  distinct(school_label, .keep_all = TRUE)

# Historical + current CUSA-related teams. # this is not being used
all_cusa_teams <- c(
  "Delaware Blue Hens", "Florida International Panthers", "Jacksonville State Gamecocks",
  "Kennesaw State Owls", "Liberty Flames", "Louisiana Tech Bulldogs",
  "Middle Tennessee Blue Raiders", "Missouri State Bears", "New Mexico State Aggies",
  "Sam Houston Bearkats", "UTEP Miners", "Western Kentucky Hilltoppers",
  "Charlotte 49ers", "Cincinnati Bearcats", "DePaul Blue Demons",
  "East Carolina Pirates", "Florida Atlantic Owls", "Houston Cougars",
  "Louisville Cardinals", "Marquette Golden Eagles", "Marshall Thundering Herd",
  "Memphis Tigers", "Rice Owls", "Saint Louis Billikens", "SMU Mustangs",
  "Southern Miss Golden Eagles", "TCU Horned Frogs", "Tulane Green Wave",
  "Tulsa Golden Hurricane", "UAB Blazers", "UCF Knights",
  "North Texas Mean Green", "UTSA Roadrunners", "Old Dominion Monarchs",
  "South Florida Bulls"
)

# Read allconference.csv and return one row per All-Conference selection (top 15 per season).
parse_allconference_labels <- function(path = file.path(DATA_DIR, "allconference.csv")) {
  read_csv(path, skip = 1, show_col_types = FALSE) |>
    filter(!is.na(Season), !is.na(Player), Player != "Player") |>
    mutate(
      # "2024-25" -> 2025
      season = as.integer(paste0("20", str_sub(Season, -2, -1))),
      team_display_name = school_crosswalk$team_display_name[
        match(School, school_crosswalk$school_label)
      ],
      join_name = normalize_name(Player)
    ) |>
    filter(season >= 2011, season <= 2026, !is.na(team_display_name)) |>
    group_by(season) |>
    mutate(
      pick_order = row_number(),
      # Tier 1 = top 5, tier 2 = 6-10, tier 3 = 11-15
      team_tier = case_when(
        pick_order <= 5 ~ 1L,
        pick_order <= 10 ~ 2L,
        pick_order <= 15 ~ 3L,
        TRUE ~ NA_integer_
      ),
      all_conference = if_else(pick_order <= 15, 1L, 0L)
    ) |>
    ungroup() |>
    filter(all_conference == 1L) |>
    select(season, Player, School, team_display_name, join_name, team_tier, all_conference)
}

# Merge Graham's feature table with All-Conference labels; write modeling_dataset.csv
build_modeling_dataset <- function(
    features_path = file.path(DATA_DIR, "final_ncaa_dataset.csv"),
    labels_path = file.path(DATA_DIR, "allconference.csv"),
    output_path = "modeling_dataset.csv") {

  # Load player-season stats from CUSA_v2.R
  features <- read_csv(features_path, show_col_types = FALSE) |>
    filter(season >= 2011, season <= 2026) |>
    mutate(
      join_name = normalize_name(athlete_display_name),
      # Sports Ref ts_percent is 0-1; hoopR TS_pct is 0-100
      ts_percent = coalesce(ts_percent, TS_pct / 100),
      # BPM from Graham's dataset; kept as net_rating so model scripts stay unchanged
      net_rating = bpm
    )

  labels <- parse_allconference_labels(labels_path)
  features_prep <- features

  # Step 1: join labels on normalized name + team + season
  modeling <- features_prep |>
    left_join(
      labels |> select(season, team_display_name, join_name, team_tier, all_conference),
      by = c("season", "team_display_name", "join_name")
    ) |>
    mutate(
      all_conference = coalesce(all_conference, 0L),  # non-selections = 0
      match_type = if_else(all_conference == 1L, "exact", NA_character_)
    )

  # Step 2: fuzzy-match labels that failed exact join 
  unmatched <- labels |>
    anti_join(
      modeling |> filter(all_conference == 1L) |> select(season, team_display_name, join_name),
      by = c("season", "team_display_name", "join_name")
    )

  if (nrow(unmatched) > 0) {
    candidates <- features_prep |>
      select(season, team_display_name, join_name) |>
      distinct()

    # Find closest feature name within edit distance 4, same team and season
    fuzzy_map <- unmatched |>
      stringdist_inner_join(
        candidates,
        by = "join_name",
        max_dist = 4,
        method = "lv",
        distance_col = "dist"
      ) |>
      filter(season.x == season.y, team_display_name.x == team_display_name.y) |>
      group_by(season.x, team_display_name.x, join_name.x) |>
      slice_min(dist, n = 1, with_ties = FALSE) |>
      ungroup() |>
      transmute(
        season = season.x,
        team_display_name = team_display_name.x,
        label_join_name = join_name.x,
        feature_join_name = join_name.y,
        team_tier = team_tier,
        all_conference = all_conference
      )

    # Apply each fuzzy match back onto the modeling table
    for (i in seq_len(nrow(fuzzy_map))) {
      fm <- fuzzy_map[i, ]
      modeling <- modeling |>
        mutate(
          all_conference = if_else(
            season == fm$season &
              team_display_name == fm$team_display_name &
              join_name == fm$feature_join_name,
            fm$all_conference,
            all_conference
          ),
          team_tier = if_else(
            season == fm$season &
              team_display_name == fm$team_display_name &
              join_name == fm$feature_join_name,
            fm$team_tier,
            team_tier
          ),
          match_type = if_else(
            season == fm$season &
              team_display_name == fm$team_display_name &
              join_name == fm$feature_join_name,
            "fuzzy",
            match_type
          )
        )
    }
  }

  # Keep columns needed for modeling
  modeling <- modeling |>
    select(
      athlete_display_name, team_display_name, season, join_name,
      all_conference, team_tier, match_type, GP, MP,
      avg_PIE, PER, net_rating, ts_percent, ws
    )

  write_csv(modeling, output_path)

  # Print join diagnostics to console
  n_labels <- nrow(labels)
  n_matched <- sum(modeling$all_conference == 1L)
  cat("All-Conference labels (2011-2026):", n_labels, "\n")
  cat("Matched in feature set:", n_matched, "\n")
  cat("Match rate:", round(100 * n_matched / n_labels, 1), "%\n")

  modeling
}

# Leave-one-season-out CV: each fold trains on all seasons except one held-out year
season_cv_folds <- function(data, season_col = "season") {
  seasons <- sort(unique(data[[season_col]]))
  lapply(seasons, function(s) {
    list(
      season = s,
      train = data |> filter(.data[[season_col]] != s),
      test = data |> filter(.data[[season_col]] == s)
    )
  })
}

# Of all actual All-Conference picks in a season, what fraction landed in the model's top 15?
recall_at_k <- function(truth, prob, k = 15) {
  ord <- order(prob, decreasing = TRUE)
  top_k <- truth[ord][seq_len(min(k, length(truth)))]
  sum(top_k) / sum(truth)
}

# Standard classification metrics for one train/test fold
compute_binary_metrics <- function(truth, prob, threshold = 0.5) {
  pred <- as.integer(prob >= threshold)
  tp <- sum(pred == 1 & truth == 1)
  fp <- sum(pred == 1 & truth == 0)
  fn <- sum(pred == 0 & truth == 1)
  tn <- sum(pred == 0 & truth == 0)

  tibble(
    accuracy = (tp + tn) / length(truth),
    precision = if (tp + fp > 0) tp / (tp + fp) else NA_real_,
    recall = if (tp + fn > 0) tp / (tp + fn) else NA_real_,
    f1 = if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
      2 * precision * recall / (precision + recall)
    } else {
      NA_real_
    },
    # returns NA if not installed
    roc_auc = {
      if (requireNamespace("pROC", quietly = TRUE) && length(unique(truth)) > 1) {
        as.numeric(pROC::auc(pROC::roc(truth, prob, quiet = TRUE)))
      } else {
        NA_real_
      }
    },
    pr_auc = {
      if (requireNamespace("PRROC", quietly = TRUE) && sum(truth) > 0) {
        PRROC::pr.curve(
          scores.class0 = prob[truth == 1],
          scores.class1 = prob[truth == 0]
        )$auc.integral
      } else {
        NA_real_
      }
    }
  )
}
