# Visuals 

library(tidyverse)

dir.create("outputs/presentation", recursive = TRUE, showWarnings = FALSE)

# Color palette
col_yes <- "#1F4E79"
col_no <- "#A8B5C2"
col_bar <- "#2E75B6"
col_bar_hi <- "#C45911"  # call out best/worst if needed
col_target <- "#C00000"
col_match <- "#2E7D32"
col_miss <- "#B71C1C"

# Data

model_df <- read_csv("modeling_dataset.csv", show_col_types = FALSE) |>
  filter(
    !is.na(avg_PIE), !is.na(PER), !is.na(net_rating),
    !is.na(ts_percent), !is.na(ws)
  ) |>
  mutate(
    selection = if_else(all_conference == 1, "All-Conference", "Not selected")
  )

cv_metrics <- read_csv("outputs/rf/rf_metrics.csv", show_col_types = FALSE)
time_metrics <- read_csv("outputs/rf/rf_time_based_metrics.csv", show_col_types = FALSE)

# Visual 1: Predictors vs All-Conference (boxplots)

pred_long <- model_df |>
  select(selection, avg_PIE, PER, net_rating, ts_percent, ws) |>
  pivot_longer(
    cols = -selection,
    names_to = "predictor",
    values_to = "value"
  ) |>
  mutate(
    predictor = recode(
      predictor,
      avg_PIE = "avg PIE",
      PER = "PER",
      net_rating = "BPM",
      ts_percent = "True shooting %",
      ws = "Win shares"
    ),
    predictor = factor(
      predictor,
      levels = c("Win shares", "PER", "BPM", "True shooting %", "avg PIE")
    ),
    selection = factor(selection, levels = c("Not selected", "All-Conference"))
  )

p_box <- ggplot(pred_long, aes(selection, value, fill = selection)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.65) +
  facet_wrap(~predictor, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Not selected" = col_no, "All-Conference" = col_yes)) +
  labs(
    title = "Selected Players Look Different on Our Five Stats",
    subtitle = "All-Conference honorees vs everyone else (complete cases, 2011–2026)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(
  "outputs/presentation/predictors_vs_selection_boxplots.png",
  p_box,
  width = 14,
  height = 5.5,
  dpi = 200
)

# Visual 2: Hits by season (leave-one-season-out)

hits_cv <- cv_metrics |>
  mutate(
    season = as.integer(season),
    highlight = case_when(
      hits_at_15 >= 12 ~ "Best",
      hits_at_15 <= 6 ~ "Worst",
      TRUE ~ "Typical"
    )
  )

p_hits <- ggplot(hits_cv, aes(factor(season), hits_at_15, fill = highlight)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 10, color = col_target, linewidth = 0.9, linetype = "dashed") +
  annotate(
    "text",
    x = 16.2,
    y = 10.35,
    label = "Target: 10",
    color = col_target,
    hjust = 1,
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(aes(label = hits_at_15), vjust = -0.35, size = 3.3) +
  scale_fill_manual(
    values = c(Best = "#1B7A3D", Worst = "#B71C1C", Typical = col_bar),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 3)) +
  labs(
    title = "Correct Picks in the Top 15, by Season",
    subtitle = "Leave-one-season-out validation · typical year ≈ 10 of 15",
    x = "Season",
    y = "Correct names in top 15"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank()
  )

ggsave(
  "outputs/presentation/hits_by_season_loso.png",
  p_hits,
  width = 11,
  height = 5.5,
  dpi = 200
)

# Visual 3: Forward time-based validation

p_time <- ggplot(time_metrics, aes(factor(season), hits_at_15)) +
  geom_col(fill = col_bar, width = 0.7) +
  geom_hline(yintercept = 10, color = col_target, linewidth = 0.9, linetype = "dashed") +
  annotate(
    "text",
    x = 6.35,
    y = 10.35,
    label = "Target: 10",
    color = col_target,
    hjust = 1,
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(aes(label = hits_at_15), vjust = -0.35, size = 4) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 3)) +
  labs(
    title = "Forward Test: Train Through 2020, Predict Later Seasons",
    subtitle = "Secondary validation · average ≈ 9.8 of 15 · median 10.5",
    x = "Season",
    y = "Correct names in top 15"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank()
  )

ggsave(
  "outputs/presentation/hits_by_season_forward.png",
  p_time,
  width = 9,
  height = 5.5,
  dpi = 200
)

# Visual 4: 2024 / 2025 model vs actual 

make_actual_strip <- function(path_csv, season_label, hits_n, out_png) {
  actual <- read_csv(path_csv, show_col_types = FALSE) |>
    filter(list_type == "actual") |>
    arrange(rank) |>
    mutate(
      result = if_else(in_top_k, "In model top 15", "Missed by model"),
      player_lab = paste0(rank, ". ", athlete_display_name),
      player_lab = fct_rev(factor(player_lab, levels = player_lab))
    )

  p <- ggplot(actual, aes(x = 1, y = player_lab, fill = result)) +
    geom_tile(width = 1.2, height = 0.88, color = "white") +
    geom_text(aes(label = result), color = "white", size = 3.1, fontface = "bold") +
    scale_fill_manual(
      values = c(
        "In model top 15" = col_match,
        "Missed by model" = col_miss
      )
    ) +
    labs(
      title = paste0(season_label, ": Actual Ballot vs Model Top 15"),
      subtitle = paste0(hits_n, " of 15 actual honorees appeared in the model shortlist"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 15),
      panel.grid = element_blank(),
      axis.text.x = element_blank()
    )

  ggsave(out_png, p, width = 9, height = 7, dpi = 200)
}

make_actual_strip(
  "outputs/rf/rf_top15_vs_actual_2024.csv",
  "2024",
  11,
  "outputs/presentation/match_actual_vs_model_2024.png"
)

make_actual_strip(
  "outputs/rf/rf_top15_vs_actual_2025.csv",
  "2025",
  10,
  "outputs/presentation/match_actual_vs_model_2025.png"
)


# Visual 5: Headline KPI strip


kpi <- tibble(
  metric = c("Typical year", "Average", "Seasons at 10+", "Best", "Worst"),
  value = c("10 / 15", "9.5 / 15", "9 of 16", "12 / 15", "6 / 15"),
  detail = c("median", "mean hits", "history", "2012 & 2013", "2022")
) |>
  mutate(metric = factor(metric, levels = metric))

p_kpi <- ggplot(kpi, aes(metric, 1, fill = metric)) +
  geom_tile(color = "white", height = 0.9) +
  geom_text(aes(label = value), color = "white", fontface = "bold", size = 5, vjust = 0.2) +
  geom_text(aes(label = detail), color = "white", size = 3.2, vjust = 2.2) +
  scale_fill_manual(
    values = c(
      "Typical year" = col_bar,
      "Average" = col_bar,
      "Seasons at 10+" = col_yes,
      "Best" = "#1B7A3D",
      "Worst" = "#B71C1C"
    )
  ) +
  labs(
    title = "Historical Shortlist Performance at a Glance",
    subtitle = "Leave-one-season-out validation (2011–2026)",
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    plot.margin = margin(15, 15, 15, 15)
  )

ggsave(
  "outputs/presentation/performance_kpi_strip.png",
  p_kpi,
  width = 11,
  height = 3.2,
  dpi = 200
)

cat("Wrote presentation visuals to outputs/presentation/\n")
list.files("outputs/presentation", pattern = "\\.png$")
