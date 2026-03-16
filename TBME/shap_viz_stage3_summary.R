library(tidyverse)
library(ggh4x)

# ------------------------------------------------------------
# STANDALONE: Unified 2×2 Discriminator Plot
# Updated to use pct_bilateral (proportion) for SD outcomes
# ------------------------------------------------------------

combined_path <- "python/shap_stage3_both_actor_delta3_combined_90.csv"

df <- 
  read_csv(combined_path, show_col_types = FALSE) %>%
  transmute(
    feature,
    model,
    out,
    mean_abs_impact = mean_abs_impact_both,
    on_delta = coalesce(on_delta, FALSE),
    on_actor = coalesce(on_actor, FALSE),
    spear_delta = spear_delta
  ) |>
  mutate(
    outcome_type = case_when(
      str_detect(out, "7avg") ~ "mean",
      str_detect(out, "7sd")  ~ "sd",
      TRUE ~ "other"
    ),
    model_out = paste(model, out, sep = "_") |> str_remove("_[^_]+$")
  ) |>
  filter(outcome_type %in% c("mean", "sd"))

# 2. Cell state classification
df <- df |>
  mutate(
    shap_cat = if_else(on_delta, cut(spear_delta, breaks = c(-1, 0, 1)), NA),
    cell_state = case_when(
      on_delta & !is.na(shap_cat) & shap_cat == "(-1,0]" ~ "delta_neg",
      on_delta & !is.na(shap_cat) & shap_cat == "(0,1]"  ~ "delta_pos",
      on_actor & !on_delta                                ~ "actor_only",
      TRUE                                                ~ "none"
    )
  )

# 3. Extract PT/FM and build db/phase bins (CORRECTED PREPROCESSING)
exp_by_phase <- df |>
  mutate(
    ptfm = str_extract(feature, "(fm1)|(pt1)"),
    db = case_when(
      str_detect(feature, "^cort.*acutesaliva$") ~ "CortSaliva",
      str_detect(feature, "^aa.*acutesaliva$")   ~ "AASaliva",
      str_detect(feature, "^dhea.*acutesaliva$") ~ "DHEASaliva",
      str_detect(feature, "coreg") ~ "coreg",
      str_detect(feature, "coag")  ~ "coag",
      str_detect(feature, "cardio") ~ "cardio",
      str_detect(feature, "clo")  ~ "clo",
      TRUE ~ NA_character_
    )
  ) |>
  # Filter to physiological features only
  filter(db %in% c("clo", "CortSaliva", "AASaliva", "DHEASaliva", "coreg", "coag", "cardio")) |>
  separate(feature, into = c("feat", "phase", "rest"), sep = "_", extra = "merge") |>
  mutate(
    db = case_when(
      db == "clo"        ~ "CLO",
      db == "CortSaliva" ~ "CortSaliva",
      db == "AASaliva"   ~ "AASaliva",
      db == "DHEASaliva" ~ "DHEASaliva",
      db == "cardio"     ~ "HFP",
      db == "coreg"      ~ "Coregulation",
      db == "coag"       ~ "Coagitation",
      TRUE ~ "Other"
    ),
    ptfm = case_when(
      ptfm == "pt1" ~ "PT",
      ptfm == "fm1" ~ "FM",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(ptfm))

# 4. Compute Δ proportion
model_impact <- exp_by_phase |>
  group_by(model_out, outcome_type) |>
  summarise(
    delta_impact = sum(mean_abs_impact[cell_state %in% c("delta_neg", "delta_pos")], na.rm = TRUE),
    actor_impact = sum(mean_abs_impact[cell_state == "actor_only"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    total_impact = delta_impact + actor_impact,
    delta_proportion = if_else(total_impact > 0, delta_impact / total_impact, 0)
  )

# 5. Compute bilateral bins AND pct_bilateral
bilateral_bins <- exp_by_phase |>
  filter(cell_state %in% c("delta_neg", "delta_pos")) |>
  group_by(model_out, outcome_type, db, phase) |>
  summarise(
    has_pt = any(ptfm == "PT"),
    has_fm = any(ptfm == "FM"),
    is_bilateral = has_pt & has_fm,
    .groups = "drop"
  )

model_bilateral <- bilateral_bins |>
  group_by(model_out, outcome_type) |>
  summarise(
    n_bilateral_bins = sum(is_bilateral, na.rm = TRUE),
    n_delta_bins = n(),
    .groups = "drop"
  ) |>
  mutate(
    # pct_bilateral: proportion of delta bins that are bilateral
    pct_bilateral = if_else(n_delta_bins > 0, n_bilateral_bins / n_delta_bins, 0)
  )

# 6. Combine metrics
model_metrics <- model_impact |>
  left_join(model_bilateral, by = c("model_out", "outcome_type")) |>
  mutate(
    n_bilateral_bins = coalesce(n_bilateral_bins, 0L),
    n_delta_bins = coalesce(n_delta_bins, 0L),
    pct_bilateral = coalesce(pct_bilateral, 0)
  )

# 7. Dyadic status - FROM STAGE 2 SCRIPT (supervized_viz_stage2.R)
dyadic_status <- 
  read_csv("python/loocv_bstrap_results021026_combined_stage2_status_90.csv") %>% 
  mutate(
    model_out = paste(model, out, sep = "_") |> str_remove("_[^_]+$"),
    dyadic_label = if_else(dyadic_sig, "Dyadic-positive", "Dyadic-null")
  )

# 8. Join dyadic status to model_metrics
model_metrics <- model_metrics |>
  left_join(dyadic_status |> select(model_out, dyadic_label, dyadic_sig), by = "model_out") |>
  mutate(
    dyadic_label = coalesce(dyadic_label, "Dyadic-null"),
    dyadic_sig = coalesce(dyadic_sig, FALSE)
  )

# 9. Compute optimal thresholds from data
compute_optimal_threshold <- function(values, labels) {
  unique_vals <- sort(unique(values))
  if (length(unique_vals) < 2) {
    return(tibble(threshold = NA_real_, accuracy = NA_real_))
  }
  candidates <- (unique_vals[-length(unique_vals)] + unique_vals[-1]) / 2
  candidates <- c(min(values) - 0.1, candidates, max(values) + 0.1)
  
  best_acc <- 0
  best_thresh <- NA_real_
  
  for (thresh in candidates) {
    predicted <- values >= thresh
    acc <- mean(predicted == labels, na.rm = TRUE)
    if (acc > best_acc) {
      best_acc <- acc
      best_thresh <- thresh
    }
  }
  tibble(threshold = best_thresh, accuracy = best_acc * 100)
}

# Compute thresholds for all 4 combinations
# Mean: delta_proportion (%) and pct_bilateral (%)
# SD: delta_proportion (%) and pct_bilateral (%)
threshold_results <- bind_rows(
  # Delta proportion for Mean outcomes
  model_metrics |> filter(outcome_type == "mean") |>
    summarise(compute_optimal_threshold(delta_proportion * 100, dyadic_sig)) |>
    mutate(metric = "delta_proportion", outcome_type = "mean"),
  # Delta proportion for SD outcomes
  model_metrics |> filter(outcome_type == "sd") |>
    summarise(compute_optimal_threshold(delta_proportion * 100, dyadic_sig)) |>
    mutate(metric = "delta_proportion", outcome_type = "sd"),
  # pct_bilateral for Mean outcomes
  model_metrics |> filter(outcome_type == "mean") |>
    summarise(compute_optimal_threshold(pct_bilateral * 100, dyadic_sig)) |>
    mutate(metric = "pct_bilateral", outcome_type = "mean"),
  # pct_bilateral for SD outcomes
  model_metrics |> filter(outcome_type == "sd") |>
    summarise(compute_optimal_threshold(pct_bilateral * 100, dyadic_sig)) |>
    mutate(metric = "pct_bilateral", outcome_type = "sd")
) |>
  group_by(outcome_type) |>
  mutate(is_best = accuracy == max(accuracy, na.rm = TRUE)) |>
  ungroup()

threshold_data <- threshold_results |>
  mutate(
    metric_label = factor(
      if_else(metric == "delta_proportion", "Δ Proportion (%)", "% Bilateral"),
      levels = c("% Bilateral", "Δ Proportion (%)")
    ),
    outcome_label = factor(
      if_else(outcome_type == "mean", "Mean Slope", "Slope SD"),
      levels = c("Mean Slope", "Slope SD")
    ),
    acc_label = sprintf("%.0f%%", accuracy)
  )

# Check thresholds
cat("\n=== Threshold Results ===\n")
print(threshold_data |> select(metric_label, outcome_label, threshold, accuracy, is_best))

# Assuming exp_by_phase is already loaded with cell_state and dyadic_sig columns
# and filtered to mean outcomes only

# 10. Build plot data with outcome-appropriate y-ordering
# Mean outcomes ordered by delta_proportion; SD outcomes ordered by pct_bilateral
plot_data_unified <- model_metrics |>
  select(model_short = model_out, outcome_type, dyadic_label,
         delta_proportion, pct_bilateral) |>
  # Create ordering variable BEFORE pivoting based on outcome type
  mutate(
    order_value = if_else(outcome_type == "mean", delta_proportion, pct_bilateral)
  ) |>
  # Create ordered factor for each outcome type separately
  group_by(outcome_type) |>
  mutate(
    model_ordered = factor(model_short, levels = model_short[order(order_value)])
  ) |>
  # mutate(
  #   model_ordered = str_replace_all(model_ordered, c("slp7" = "", "avg" = "AVG", "sd" = "SD", "pt1" = "PT", "fm1" = "FM"))
  # ) %>% 
  ungroup() |>
  mutate(model_ordered = forcats::fct_relabel(
    model_ordered,
    ~ str_replace_all(.x, c("slp7"="", "_aa"="_AA", "_cort"="_CORT", "_dheas"="_DHEAS", "avg"="avg", "sd"="sd", "pt1"="pt", "fm1"="fm"))
  )) %>% 
  pivot_longer(
    cols = c(delta_proportion, pct_bilateral),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    # Both metrics are now proportions, convert to percentages
    value = value * 100,
    metric_label = factor(
      if_else(metric == "delta_proportion", "Δ Proportion (%)", "% Bilateral"),
      levels = c("Δ Proportion (%)", "% Bilateral")
    ),
    outcome_label = factor(
      if_else(outcome_type == "mean", "Mean Slope", "Slope SD"),
      levels = c("Mean Slope", "Slope SD")
    ),
    # Both are now percentages
    val_label = sprintf("%s%%", format(value, digits = 2)),
    display_value = pmax(value, 0.5)  # minimum visible width for 0-value bars
  )

plot_data_unified |>
  filter(outcome_type == "sd") |>
  select(model_short, outcome_type, dyadic_label, order_value, model_ordered, metric, value) |>
  pivot_wider(names_from = metric, values_from = value) |>
  group_by(dyadic_label) |>
  summarise(
    mean_delta_prop = mean(delta_proportion),
    mean_pct_bilateral = mean(pct_bilateral),
    n = n()
  )

# 11. Build the unified plot
g_unified <- ggplot(plot_data_unified, aes(x = display_value, y = model_ordered, fill = dyadic_label)) +
  # Threshold lines
  # geom_vline(
  #   data = threshold_data,
  #   aes(xintercept = threshold, linetype = is_best),
  #   inherit.aes = FALSE,
  #   color = "black", linewidth = 0.6, alpha = 0.8
  # ) +
  # Bars
  geom_col(width = 0.7, alpha = 1, color = NA) +
  # Value labels - rounded to 0 decimal places, larger size
  geom_text(aes(x = ifelse(value < 1, 0.5, value),  # Position label at x=1 for very small values
                label = sprintf("%.0f%%", value),
                hjust = -0.1),
            size = 4.5, 
            fontface = "bold", show.legend = FALSE) +
  # Faceting - reordered with % Bilateral first, with proportional row heights
  facet_grid2(
    outcome_label ~ metric_label,
    scales = "free",
    independent = "none",
    space = "free_y"  # This makes row heights proportional to number of items
  ) +
  # Scales - new colors with appropriate legend title
  scale_fill_manual(
    values = c("Dyadic-positive" = "#0072B2", "Dyadic-null" = "#FF6F61"),
    name = "Incremental Dyadic Effect"
  ) +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +  # Remove left expansion, add right space for labels
  # Labels
  labs(
    x = NULL, y = NULL
  ) +
  # Theme
  theme_Publication() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 16),
    strip.background = element_rect(fill = "gray90", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines"),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(hjust = 0, size = 8, face = "italic"),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_blank()
  )

print(g_unified)

bilateral_bins |> 
  filter(model_out == "RF_dheasslp7avg_pt1", is_bilateral) |>
  select(db, phase, has_pt, has_fm)

# # Optionally save
# ggsave("shap_heatmap2_90.png", g_unified, width = 10, height = 10, dpi = 150)
