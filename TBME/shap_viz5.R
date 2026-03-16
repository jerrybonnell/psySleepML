library(biscale)
library(cowplot)
library(grid)
library(gtable)
library(tidyverse)
library(ggh4x)

#------------------------------------------------------------------------------
# Load and prepare data
#------------------------------------------------------------------------------
# df <- read_csv("python/shap_stage3_both_actor_delta3_avg.csv") |>
# df <- read_csv("python/shap_stage3_both_actor_delta3_sd.csv") |>
# df <- read_csv("python/shap_stage3_both_actor_delta3_sd_jerry.csv") |>
df <- read_csv("python/shap_stage3_both_actor_delta3_combined_90.csv") |>
  transmute(
    feature,
    model,
    out,
    mean_abs_impact = mean_abs_impact_both,
    rank = rank_both,
    on_delta = on_delta,
    on_actor = on_actor,   
    on_both  = on_both, 
    spear_delta = spear_delta,
    gate_both = gate_both,
    gate_actor = gate_actor,
    spear_both = spear_both,
    spear_actor = spear_actor,
    direction_flip = direction_flip
  ) |>
  mutate(model_out = paste(model, out, sep = "_"),
         model_out = str_remove(model_out, "_[^_]+$")) |>
  group_by(model_out) |>
  mutate(new_model_name = paste0(model, cur_group_id()),
         rev_rank = ntile(rank, 2)) |>
  ungroup() |>
  # Incremental SHAP: show tiles where BOTH is ON and ACTOR is OFF
  mutate(shap_cat = case_when(
    on_delta ~ cut(spear_delta, breaks = c(-1, 0, 1)),
    TRUE ~ NA_character_
  )) |>
  mutate(rev_rank = factor(rev_rank, levels = c("1", "2")),
         shap_cat = factor(shap_cat, levels = c("(-1,0]", "(0,1]"))) |>
  arrange(out)

df <- df |>
  arrange(out) |>
  mutate(model_out = factor(model_out, levels = df$model_out |> unique())) 

#------------------------------------------------------------------------------
# Detect outcome type: mean vs SD
#------------------------------------------------------------------------------
df <- df |>
  mutate(
    outcome_type = case_when(
      str_detect(out, "7avg") ~ "mean",
      str_detect(out, "7sd")  ~ "sd",
      TRUE ~ "other"
    )
  ) %>% 
  filter(outcome_type == "mean")

#------------------------------------------------------------------------------
# Filter to physiological features and create phase structure
#------------------------------------------------------------------------------
exp_by_phase <- df |>
  mutate(ptfm = str_extract(feature, "(fm1)|(pt1)|(PC)"),
         is_q_feat = str_detect(feature, ".*q.*"),
         db = case_when(
           str_detect(feature, "^cort.*acutesaliva$") ~ "CortSaliva",
           str_detect(feature, "^aa.*acutesaliva$")   ~ "AASaliva",
           str_detect(feature, "^dhea.*acutesaliva$") ~ "DHEASaliva",
           str_detect(feature, "PC") ~ "q",
           str_detect(feature, "coreg") ~ "coreg",
           str_detect(feature, "coag") ~ "coag",
           TRUE ~ str_extract(feature, "[^_]+$"))) |>
  filter(db %in% c("clo", "CortSaliva", "AASaliva", "DHEASaliva", "coreg", "coag", "cardio")) |>
  separate(feature, into = c("feat", "phase", "rest"), sep = "_", extra = "merge") |>
  mutate(db = case_when(
    db == "clo" ~ "CLO",
    db == "CortSaliva" ~ "CortSaliva",
    db == "AASaliva" ~ "AASaliva",
    db == "DHEASaliva" ~ "DHEASaliva",
    db == "cardio" ~ "HFP",
    db == "coreg" ~ "Coregulation",
    db == "coag" ~ "Coagitation",
    TRUE ~ "Other"),
    db = factor(db, levels = c("CortSaliva", "AASaliva", "DHEASaliva", "Coregulation", "Coagitation", "HFP", "CLO"))
  ) %>%
  mutate(ptfm = case_when(
    ptfm == "pt1" ~ "PT feats",
    ptfm == "fm1" ~ "FM feats"),
    ptfm = factor(ptfm, levels = c("PT feats", "FM feats"))
  ) %>%
  mutate(
    phase = case_when(
      str_detect(db, "Saliva") ~ factor(phase, 
                                        levels = c("pb", "b", "s", "sp1", "sp2", "r1", "r2")),
      TRUE ~ factor(phase, 
                    levels = c("b", "s", "p", "sp1", "sp2", "r"))
    )
  ) %>% 
  mutate(model_out = factor(model_out))

#------------------------------------------------------------------------------
# Fill in missing logical flags
#------------------------------------------------------------------------------
exp_by_phase <- exp_by_phase |>
  mutate(
    on_actor = coalesce(on_actor, FALSE),
    on_delta = coalesce(on_delta, FALSE)
  )

#------------------------------------------------------------------------------
# Dyadic status: which model×outcome combos showed significant dyadic signal
#------------------------------------------------------------------------------
# GET THIS FROM STAGE 2 SCRIPT
dyadic_status <- 
#   bind_rows(
#   read_csv("python/loocv_bstrap_results020126_sd_stage2_status.csv"),
#   read_csv("python/loocv_bstrap_results020126_stage2_status_95.csv")
# )
  read_csv("python/loocv_bstrap_results021026_combined_stage2_status_90.csv")

exp_by_phase <- exp_by_phase |>
  left_join(dyadic_status, by = c("out", "model")) |>
  mutate(
    dyadic_sig = coalesce(dyadic_sig, FALSE),
    model_out_label = paste0(
      model_out, 
      if_else(dyadic_sig, "\n[+]", "\n[∅]")
    )
  ) %>% 
  mutate(model_out_label = forcats::fct_relabel(
    model_out_label,
    ~ str_replace_all(.x, c("slp7"="", "_aa"="_AA", "_cort"="_CORT", "_dheas"="_DHEAS", "avg"="avg", "sd"="sd", "pt1"="pt", "fm1"="fm"))
  ))

#------------------------------------------------------------------------------
# MODEL-LEVEL cell states
#------------------------------------------------------------------------------
exp_by_phase <- exp_by_phase |>
  mutate(
    cell_state = case_when(
      on_delta & !is.na(shap_cat) & shap_cat == "(-1,0]" ~ "delta_neg",
      on_delta & !is.na(shap_cat) & shap_cat == "(0,1]"  ~ "delta_pos",
      on_actor & !on_delta                               ~ "actor_only",
      TRUE                                               ~ "none"
    ),
    cell_state = factor(
      cell_state,
      levels = c("delta_neg", "delta_pos", "actor_only", "none")
    )
  )

#------------------------------------------------------------------------------
# Compute BOTH discriminators per model
#------------------------------------------------------------------------------

# 1. Delta proportion (for mean outcomes)
model_impact <- exp_by_phase |>
  group_by(model_out, model_out_label, dyadic_sig, outcome_type) |>
  summarise(
    delta_impact = sum(mean_abs_impact[cell_state %in% c("delta_neg", "delta_pos")]),
    actor_impact = sum(mean_abs_impact[cell_state == "actor_only"]),
    .groups = "drop"
  ) |>
  mutate(
    total_impact = delta_impact + actor_impact,
    delta_proportion = if_else(total_impact > 0, delta_impact / total_impact, 0)
  )

# 2. Bilateral bins (for SD outcomes) - bins where BOTH PT and FM show delta
bilateral_bins <- exp_by_phase |>
  filter(cell_state %in% c("delta_neg", "delta_pos")) |>
  group_by(model_out, db, phase) |>
  summarise(
    has_pt = any(ptfm == "PT feats"),
    has_fm = any(ptfm == "FM feats"),
    is_bilateral = has_pt & has_fm,
    .groups = "drop"
  )

model_bilateral <- bilateral_bins |>
  group_by(model_out) |>
  summarise(
    n_bilateral_bins = sum(is_bilateral),
    n_delta_bins = n(),
    .groups = "drop"
  ) |>
  mutate(
    # pct_bilateral: proportion of delta bins that are bilateral
    pct_bilateral = if_else(n_delta_bins > 0, n_bilateral_bins / n_delta_bins, 0)
  )

# 3. Delta density (keeping for reference)
model_density <- exp_by_phase |>
  filter(cell_state %in% c("delta_neg", "delta_pos")) |>
  group_by(model_out) |>
  summarise(
    n_delta = n(),
    .groups = "drop"
  ) |>
  left_join(model_bilateral, by = "model_out") |>
  mutate(
    delta_density = if_else(n_delta_bins > 0, n_delta / n_delta_bins, NA_real_)
  )

# Join metrics
model_metrics <- model_impact |>
  left_join(model_density |> select(model_out, n_delta, delta_density, n_bilateral_bins, n_delta_bins, pct_bilateral), by = "model_out") |>
  mutate(
    n_bilateral_bins = coalesce(n_bilateral_bins, 0L),
    n_delta_bins = coalesce(n_delta_bins, 0L),
    pct_bilateral = coalesce(pct_bilateral, 0),
    # Choose discriminator based on outcome type
    # For MEAN: delta_proportion (magnitude)
    # For SD: pct_bilateral (proportion of delta bins that are bilateral)
    discriminator_value = case_when(
      outcome_type == "mean" ~ delta_proportion,
      outcome_type == "sd"   ~ pct_bilateral,
      TRUE ~ NA_real_
    ),
    discriminator_label = case_when(
      outcome_type == "mean" ~ sprintf("%.0f%%", delta_proportion * 100),
      outcome_type == "sd"   ~ sprintf("%.0f%%", pct_bilateral * 100),
      TRUE ~ NA_character_
    )
  )

# Join metrics to main data
exp_by_phase <- exp_by_phase |>
  left_join(
    model_metrics |> select(model_out, delta_proportion, delta_density, n_bilateral_bins, pct_bilateral, discriminator_value),
    by = "model_out"
  )

# Mark bilateral bins for SD outcomes (for ring overlay)
exp_by_phase <- exp_by_phase |>
  left_join(
    bilateral_bins |> filter(is_bilateral) |> select(model_out, db, phase, is_bilateral),
    by = c("model_out", "db", "phase")
  ) |>
  mutate(
    is_bilateral = coalesce(is_bilateral, FALSE),
    # Show ring on delta cells in bilateral bins (SD outcomes only)
    show_bilateral_ring = is_bilateral & 
      cell_state %in% c("delta_neg", "delta_pos") &
      outcome_type == "sd"
  )

#------------------------------------------------------------------------------
# Create ordered factor based on discriminator (descending)
#------------------------------------------------------------------------------
model_order <- model_metrics |>
  arrange(desc(discriminator_value)) |>
  pull(model_out_label)

exp_by_phase <- exp_by_phase |>
  mutate(model_out_label = factor(model_out_label, levels = model_order))

model_metrics <- model_metrics |>
  mutate(model_out_label = factor(model_out_label, levels = model_order))

#------------------------------------------------------------------------------
# Color palette
#------------------------------------------------------------------------------
state_pal_model <- c(
  delta_neg  = "#ad5b9c",
  delta_pos  = "#434e87",
  actor_only = "#64acbe",
  none       = "#f5f5f5"
)

#------------------------------------------------------------------------------
# Determine outcome type for titles/labels
#------------------------------------------------------------------------------
current_outcome_type <- unique(exp_by_phase$outcome_type)

if (length(current_outcome_type) > 1) {
  outcome_label <- "Mixed"
  discriminator_name <- "Discriminator"
  threshold_note <- ""
} else if (current_outcome_type == "mean") {
  outcome_label <- "Mean Slope"
  discriminator_name <- "Δ Proportion"
  # threshold_note <- "Δ proportion ≥ 40% discriminates with 93% accuracy"
  threshold_note <- "Δ proportion ROC AUC = 0.91, 95% CI [0.78, 1.00]"
} else if (current_outcome_type == "sd") {
  outcome_label <- "Slope SD"
  discriminator_name <- "% Bilateral"
  # threshold_note <- "% bilateral ≥ 8% discriminates with 92% accuracy | ○ = bilateral bin (PT+FM both Δ)"
  threshold_note <- "% bilateral ROC AUC = 0.90, 95% CI [0.73, 1.00] | ○ = bilateral bin (PT+FM both Δ)"
} else {
  outcome_label <- "Other"
  discriminator_name <- "Discriminator"
  threshold_note <- ""
}

#------------------------------------------------------------------------------
# Main heatmap
#------------------------------------------------------------------------------
g_heatmap_model <- ggplot(exp_by_phase) +
  
  # Background tiles (actor_only and none)
  geom_tile(
    data = exp_by_phase |> filter(cell_state %in% c("actor_only", "none")),
    aes(x = phase, y = ptfm, fill = cell_state),
    color = "#d0d0d0", linewidth = 0.35
  ) +
  
  # Delta tiles (incremental signal)
  geom_tile(
    data = exp_by_phase |> filter(cell_state %in% c("delta_neg", "delta_pos")),
    aes(x = phase, y = ptfm, fill = cell_state),
    color = "#2d2d2d", linewidth = 0.5
  ) +
  
  # Bilateral rings (for SD outcomes): bins where both PT and FM show delta
  geom_point(
    data = exp_by_phase |> filter(show_bilateral_ring),
    aes(x = phase, y = ptfm),
    shape = 21,
    fill = NA,
    color = "white",
    size = 2.5,
    stroke = 1.2
  ) +
  
  scale_fill_manual(
    values = state_pal_model,
    name = "Cell State",
    labels = c(
      delta_neg  = "Δ− (negative)",
      delta_pos  = "Δ+ (positive)",
      actor_only = "Actor-only",
      none       = "Not selected"
    ),
    guide = guide_legend(nrow = 1)
  ) +
  
  scale_y_discrete(position = "right") +
  
  facet_grid2(
    model_out_label ~ db,
    drop = TRUE,
    scales = "free_x",
    space = "free_x",
    switch = "y",
    labeller = labeller(db = label_wrap_gen(width = 8))
  ) +
  
  labs(
    title = element_blank(),
    caption = paste0("[+] = dyadic-positive | [∅] = dyadic-null | ", threshold_note)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 17, angle = 50, vjust = 0.6),
    axis.text.y.right = element_text(size = 15),
    axis.ticks.y.right = element_line(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 0.5, size = 13, face = "bold",
                                     margin = margin(r = 4.5)),
    strip.background = element_rect(color = "black", fill = "gray92", linewidth = 0.6),
    strip.text.x = element_text(face = "bold", size = 15),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.3, "lines"),
    legend.title = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = 17),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "gray40"),
    plot.caption = element_text(hjust = 0.5, size = 17, face = "italic")
  )

#------------------------------------------------------------------------------
# Sidebar showing discriminator value
#------------------------------------------------------------------------------

# Set midpoint based on outcome type
# For MEAN: threshold is 0.4 (40%)
# For SD: threshold is 0.08 (8%)
if (length(current_outcome_type) == 1 && current_outcome_type == "mean") {
  sidebar_midpoint <- 0.4
  sidebar_limits <- c(0, 1)
} else if (length(current_outcome_type) == 1 && current_outcome_type == "sd") {
  sidebar_midpoint <- 0.08  # 8% threshold for pct_bilateral
  
  sidebar_limits <- c(0, 0.25)  # 0-25% range
} else {
  sidebar_midpoint <- 0.5
  sidebar_limits <- c(0, 1)
}

g_sidebar <- ggplot(model_metrics, aes(x = 1, y = model_out_label)) +
  geom_tile(aes(fill = discriminator_value), color = "white", linewidth = 0.5) +
  geom_text(aes(label = discriminator_label), 
            size = 5, fontface = "bold") +
  scale_fill_gradient2(
    low = "#f5f5f5", mid = "#b8d4e3", high = "#1a4e66",
    midpoint = sidebar_midpoint,
    limits = sidebar_limits,
    oob = scales::squish,
    name = discriminator_name
  ) +
  # Reverse y-axis to match facet_grid ordering (first level at top)
  scale_y_discrete(limits = rev(levels(model_metrics$model_out_label))) +
  labs(x = NULL, y = NULL, title = "Δ\nProportion") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold")
  )

#------------------------------------------------------------------------------
# Combine with cowplot
#------------------------------------------------------------------------------
library(grid)
gt = ggplot_gtable(ggplot_build(g_heatmap_model))
gt$widths[33] = 1.5*gt$widths[33]

g_combined <- plot_grid(
  g_sidebar, gt,
  nrow = 1,
  rel_widths = c(0.08, 0.92),
  align = "h",
  axis = "tb"
)

print(g_combined)

#------------------------------------------------------------------------------
# Print summary statistics
#------------------------------------------------------------------------------
cat("\n=== Model Metrics Summary ===\n")
model_metrics |>
  select(model_out_label, outcome_type, dyadic_sig, delta_proportion, pct_bilateral, n_bilateral_bins, n_delta_bins, discriminator_value) |>
  arrange(outcome_type, desc(discriminator_value)) |>
  print(n = 40)

#------------------------------------------------------------------------------
# Print classification accuracy
#------------------------------------------------------------------------------
cat("\n=== Classification Accuracy ===\n")

library(pROC)

mean_models <- model_metrics |>
  filter(outcome_type == "mean")

sd_models <- model_metrics |>
  filter(outcome_type == "sd")


# MEAN MODELS DELTA PROPORTION
# AUC
roc_delta <- roc(
  response = mean_models$dyadic_sig,
  predictor = mean_models$delta_proportion
)

auc_delta <- auc(roc_delta)
ci_delta = ci.auc(roc_delta)

print(auc_delta)
print(ci_delta)

# coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

if (nrow(mean_models) > 0) {
  mean_models <- mean_models |>
    mutate(pred_pos = delta_proportion >= 0.4)
  mean_acc <- mean(mean_models$dyadic_sig == mean_models$pred_pos)
  cat(sprintf("Mean outcomes (Δ proportion ≥ 40%%): %.1f%% accuracy (%d/%d correct)\n",
              mean_acc * 100, sum(mean_models$dyadic_sig == mean_models$pred_pos), nrow(mean_models)))
}

# MEAN MODELS BILATERAL CONVERGENCE
roc_bilateral <- roc(
  response = mean_models$dyadic_sig,
  predictor = mean_models$pct_bilateral
)

auc_bilateral <- auc(roc_bilateral)
ci_bilateral <- ci.auc(roc_bilateral)

print(auc_bilateral)
print(ci_bilateral)

if (nrow(mean_models) > 0) {
  mean_models <- mean_models |>
    mutate(pred_pos = pct_bilateral >= 0.0706)
  mean_acc <- mean(mean_models$dyadic_sig == mean_models$pred_pos)
  cat(sprintf("Mean outcomes (%% bilateral ≥ 7.06%%): %.1f%% accuracy (%d/%d correct)\n",
              mean_acc * 100, sum(mean_models$dyadic_sig == mean_models$pred_pos), nrow(mean_models)))

  # Show misclassifications
  misclass <- mean_models |> filter(dyadic_sig != pred_pos)
  if (nrow(misclass) > 0) {
    cat("\nMisclassified AVG models:\n")
    for (i in 1:nrow(misclass)) {
      row <- misclass[i, ]
      actual <- if (row$dyadic_sig) "positive" else "null"
      predicted <- if (row$pred_pos) "positive" else "null"
      cat(sprintf("  %s: actual=%s, predicted=%s (pct_bilateral=%.1f%%)\n",
                  row$model_out_label, actual, predicted, row$pct_bilateral * 100))
    }
  }
}

model_metrics %>% group_by(dyadic_sig) %>% summarize(mean_delta_prop = mean(delta_proportion))
model_metrics %>% group_by(dyadic_sig) %>% summarize(mean_pct_bilateral = mean(pct_bilateral))


# SD MODELS DELTA PROPORTION
roc_delta <- roc(
  response = sd_models$dyadic_sig,
  predictor = sd_models$delta_proportion
)

auc_delta <- auc(roc_delta)
ci_delta = ci.auc(roc_delta)

print(auc_delta)
print(ci_delta)

sd_models <- model_metrics |> filter(outcome_type == "sd")
if (nrow(sd_models) > 0) {
  sd_models <- sd_models |>
    mutate(pred_pos = delta_proportion >= 0.536)
  sd_acc <- mean(sd_models$dyadic_sig == sd_models$pred_pos)
  cat(sprintf("sd outcomes (Δ proportion ≥ 53.6%%): %.1f%% accuracy (%d/%d correct)\n",
              sd_acc * 100, sum(sd_models$dyadic_sig == sd_models$pred_pos), nrow(sd_models)))
}

# SD MODELS BILATERAL CONVERGENCE
roc_bilateral <- roc(
  response = sd_models$dyadic_sig,
  predictor = sd_models$pct_bilateral
)

auc_bilateral <- auc(roc_bilateral)
ci_bilateral <- ci.auc(roc_bilateral)

print(auc_bilateral)
print(ci_bilateral)

sd_models <- model_metrics |> filter(outcome_type == "sd")
if (nrow(sd_models) > 0) {
  sd_models <- sd_models |>
    mutate(pred_pos = pct_bilateral >= 0.08)
  sd_acc <- mean(sd_models$dyadic_sig == sd_models$pred_pos)
  cat(sprintf("sd outcomes (%% bilateral ≥ 8%%): %.1f%% accuracy (%d/%d correct)\n",
              sd_acc * 100, sum(sd_models$dyadic_sig == sd_models$pred_pos), nrow(sd_models)))

  # Show misclassifications
  misclass <- sd_models |> filter(dyadic_sig != pred_pos)
  if (nrow(misclass) > 0) {
    cat("\nMisclassified SD models:\n")
    for (i in 1:nrow(misclass)) {
      row <- misclass[i, ]
      actual <- if (row$dyadic_sig) "positive" else "null"
      predicted <- if (row$pred_pos) "positive" else "null"
      cat(sprintf("  %s: actual=%s, predicted=%s (pct_bilateral=%.1f%%)\n",
                  row$model_out_label, actual, predicted, row$pct_bilateral * 100))
    }
  }
}

model_metrics %>% group_by(dyadic_sig) %>% summarize(mean_delta_prop = mean(delta_proportion))
model_metrics %>% group_by(dyadic_sig) %>% summarize(mean_pct_bilateral = mean(pct_bilateral))

# # Optionally save
# ggsave("shap_heatmap1_mean_full_90.png", g_combined, width = 16, height = 14, dpi = 150, bg = "white")

# ggsave("shap_heatmap1_sd_full_90.png", g_combined, width = 16, height = 14, dpi = 150, bg = "white")

#------------------------------------------------------------------------------
# SUMMARY DOT PLOT (Main Figure candidate)
#------------------------------------------------------------------------------
# 
# # Use model_metrics directly (n_bilateral_bins already computed earlier)
# summary_for_plot <- model_metrics |>
#   mutate(
#     n_bilateral_bins = coalesce(n_bilateral_bins, 0L),
#     # Choose visualization metric based on outcome type
#     viz_value = case_when(
#       outcome_type == "mean" ~ delta_proportion * 100,  # as percentage
#       outcome_type == "sd"   ~ as.numeric(n_bilateral_bins),
#       TRUE ~ NA_real_
#     ),
#     viz_label = case_when(
#       outcome_type == "mean" ~ sprintf("%.0f%%", delta_proportion * 100),
#       outcome_type == "sd"   ~ sprintf("%d", n_bilateral_bins),
#       TRUE ~ NA_character_
#     ),
#     # Clean model name for display
#     model_short = str_remove(model_out_label, " \\[.\\]$"),
#     dyadic_label = if_else(dyadic_sig, "Dyadic-positive", "Dyadic-null")
#   )
# 
# # Extract outcome info for labeling
# outcome_info <- summary_for_plot |>
#   mutate(
#     biomarker = case_when(
#       str_detect(model_out, "cort") ~ "Cortisol",
#       str_detect(model_out, "aa") ~ "sAA",
#       str_detect(model_out, "dhea") ~ "DHEA-S"
#     ),
#     target = case_when(
#       str_detect(model_out, "_pt1") ~ "Patient",
#       str_detect(model_out, "_fm1") ~ "Caregiver"
#     )
#   )
# 
# # Create the summary plot
# # Facet by outcome type with different x-axis scales
# 
# # For MEAN outcomes
# mean_summary <- outcome_info |> filter(outcome_type == "mean")
# 
# if (nrow(mean_summary) > 0) {
#   g_summary_mean <- ggplot(mean_summary, 
#                            aes(x = viz_value, 
#                                y = reorder(model_short, viz_value),
#                                color = dyadic_label)) +
#     geom_vline(xintercept = 40, linetype = "dashed", color = "gray50", linewidth = 0.5) +
#     geom_segment(aes(xend = 0, yend = reorder(model_short, viz_value)), 
#                  linewidth = 0.8, alpha = 0.7) +
#     geom_point(size = 4) +
#     geom_text(aes(label = viz_label), hjust = -0.3, size = 3, fontface = "bold") +
#     scale_color_manual(
#       values = c("Dyadic-positive" = "#2c7fb8", "Dyadic-null" = "#c0c0c0"),
#       name = NULL
#     ) +
#     scale_x_continuous(
#       limits = c(0, 100),
#       breaks = seq(0, 100, 20),
#       labels = function(x) paste0(x, "%")
#     ) +
#     labs(
#       title = "Mean Slope Outcomes",
#       subtitle = "Discriminator: Δ Proportion (share of SHAP impact)",
#       x = "Δ Proportion",
#       y = NULL,
#       caption = "Dashed line: 40% threshold (93% classification accuracy)"
#     ) +
#     theme_minimal() +
#     theme(
#       legend.position = "top",
#       panel.grid.major.y = element_blank(),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(face = "bold", size = 14),
#       plot.subtitle = element_text(size = 10, color = "gray40"),
#       plot.caption = element_text(hjust = 0, size = 9, face = "italic"),
#       axis.text.y = element_text(size = 10)
#     )
#   
#   print(g_summary_mean)
# }
# 
# # For SD outcomes
# sd_summary <- outcome_info |> filter(outcome_type == "sd")
# 
# if (nrow(sd_summary) > 0) {
#   g_summary_sd <- ggplot(sd_summary, 
#                          aes(x = viz_value, 
#                              y = reorder(model_short, viz_value),
#                              color = dyadic_label)) +
#     geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray50", linewidth = 0.5) +
#     geom_segment(aes(xend = 0, yend = reorder(model_short, viz_value)), 
#                  linewidth = 0.8, alpha = 0.7) +
#     geom_point(size = 4) +
#     geom_text(aes(label = viz_label), hjust = -0.5, size = 3, fontface = "bold") +
#     scale_color_manual(
#       values = c("Dyadic-positive" = "#2c7fb8", "Dyadic-null" = "#c0c0c0"),
#       name = NULL
#     ) +
#     scale_x_continuous(
#       limits = c(-0.5, max(sd_summary$viz_value, na.rm = TRUE) + 1),
#       breaks = seq(0, 10, 2)
#     ) +
#     labs(
#       title = "Slope SD Outcomes",
#       subtitle = "Discriminator: Bilateral Bins (db×phase with both PT & FM showing Δ)",
#       x = "Number of Bilateral Bins",
#       y = NULL,
#       caption = "Dashed line: ≥2 bilateral bins threshold (94% classification accuracy)"
#     ) +
#     theme_minimal() +
#     theme(
#       legend.position = "top",
#       panel.grid.major.y = element_blank(),
#       panel.grid.minor = element_blank(),
#       plot.title = element_text(face = "bold", size = 14),
#       plot.subtitle = element_text(size = 10, color = "gray40"),
#       plot.caption = element_text(hjust = 0, size = 9, face = "italic"),
#       axis.text.y = element_text(size = 10)
#     )
#   
#   print(g_summary_sd)
# }
# 
# # Combined panel if both outcome types present
# if (nrow(mean_summary) > 0 && nrow(sd_summary) > 0) {
#   g_summary_combined <- plot_grid(
#     g_summary_mean, g_summary_sd,
#     nrow = 1,
#     labels = c("A", "B"),
#     label_size = 16
#   )
#   print(g_summary_combined)
#   
#   # ggsave("shap_summary_dotplot.png", g_summary_combined, width = 14, height = 8, dpi = 150)
# }
# 
# #------------------------------------------------------------------------------
# # Print discrimination accuracy for the chosen metrics
# #------------------------------------------------------------------------------
# cat("\n=== Classification Accuracy Check ===\n")
# 
# if (nrow(mean_summary) > 0) {
#   mean_summary <- mean_summary |>
#     mutate(pred_pos = delta_proportion >= 0.4)
#   mean_acc <- mean(mean_summary$dyadic_sig == mean_summary$pred_pos)
#   cat(sprintf("Mean outcomes (Δ proportion ≥ 0.4): %.1f%% accuracy\n", mean_acc * 100))
# }
# 
# if (nrow(sd_summary) > 0) {
#   sd_summary <- sd_summary |>
#     mutate(pred_pos = n_bilateral_bins >= 2)
#   sd_acc <- mean(sd_summary$dyadic_sig == sd_summary$pred_pos)
#   cat(sprintf("SD outcomes (bilateral bins ≥ 2): %.1f%% accuracy\n", sd_acc * 100))
# }
# 
# 
# 
# #------------------------------------------------------------------------------
# # Print summary statistics
# #------------------------------------------------------------------------------
# cat("\n=== Model Metrics Summary ===\n")
# model_metrics |>
#   select(model_out_label, outcome_type, dyadic_sig, delta_proportion, n_bilateral_bins, discriminator_value) |>
#   arrange(outcome_type, desc(discriminator_value)) |>
#   print(n = 40)
# 
# # Optionally save
# # ggsave("shap_heatmap_model_level_v2.png", g_combined, width = 16, height = 14, dpi = 150)
# 
# #------------------------------------------------------------------------------
# # UNIFIED SUMMARY PLOT: Both metrics × Both outcome types (2×2 faceted)
# #------------------------------------------------------------------------------
# 
# # Use model_metrics directly (n_bilateral_bins already computed earlier)
# summary_for_plot <- model_metrics |>
#   mutate(
#     n_bilateral_bins = coalesce(n_bilateral_bins, 0L),
#     # Clean model name for display
#     model_short = str_remove(model_out_label, " \\[.\\]$"),
#     dyadic_label = if_else(dyadic_sig, "Dyadic-positive", "Dyadic-null")
#   )
# 
# # Create long-format data with both metrics
# plot_data_long <- summary_for_plot |>
#   select(model_short, outcome_type, dyadic_sig, dyadic_label, 
#          delta_proportion, n_bilateral_bins) |>
#   pivot_longer(
#     cols = c(delta_proportion, n_bilateral_bins),
#     names_to = "metric",
#     values_to = "value"
#   ) |>
#   mutate(
#     # Scale delta_proportion to percentage
#     value = if_else(metric == "delta_proportion", value * 100, value),
#     # Nice labels
#     metric_label = case_when(
#       metric == "delta_proportion" ~ "Δ Proportion (%)",
#       metric == "n_bilateral_bins" ~ "Bilateral Bins (n)"
#     ),
#     metric_label = factor(metric_label, levels = c("Δ Proportion (%)", "Bilateral Bins (n)")),
#     outcome_label = case_when(
#       outcome_type == "mean" ~ "Mean Slope",
#       outcome_type == "sd" ~ "Slope SD"
#     ),
#     outcome_label = factor(outcome_label, levels = c("Mean Slope", "Slope SD"))
#   )
# 
# # Define thresholds and accuracy for each combination
# threshold_data <- tribble(
#   ~metric_label, ~outcome_label, ~threshold, ~accuracy, ~is_best,
#   "Δ Proportion (%)", "Mean Slope", 40, 92.9, TRUE,
#   "Δ Proportion (%)", "Slope SD", 40, 61.1, FALSE,
#   "Bilateral Bins (n)", "Mean Slope", 2, 71.4, FALSE,
#   "Bilateral Bins (n)", "Slope SD", 2, 100.0, TRUE
# ) |>
#   mutate(
#     metric_label = factor(metric_label, levels = c("Δ Proportion (%)", "Bilateral Bins (n)")),
#     outcome_label = factor(outcome_label, levels = c("Mean Slope", "Slope SD")),
#     acc_label = sprintf("%.0f%%", accuracy)
#   )
# 
# # Create the unified 2×2 plot
# g_unified <- ggplot(plot_data_long, 
#                     aes(x = value, 
#                         y = reorder(model_short, value),
#                         color = dyadic_label)) +
#   
#   # Threshold lines
#   geom_vline(
#     data = threshold_data,
#     aes(xintercept = threshold, linetype = is_best),
#     color = "gray40",
#     linewidth = 0.6
#   ) +
#   
#   # Accuracy annotation
#   geom_text(
#     data = threshold_data,
#     aes(x = threshold, y = Inf, label = acc_label),
#     inherit.aes = FALSE,
#     vjust = 2, hjust = -0.2,
#     size = 3.5, fontface = "bold",
#     color = "gray30"
#   ) +
#   
#   # Lollipop segments
#   geom_segment(
#     aes(xend = 0, yend = reorder(model_short, value)),
#     linewidth = 0.6, alpha = 0.6
#   ) +
#   
#   # Points
#   geom_point(size = 2.5) +
#   
#   # Facet: metrics as rows, outcomes as columns
#   facet_grid(
#     metric_label ~ outcome_label,
#     scales = "free",
#     switch = "y"
#   ) +
#   
#   scale_color_manual(
#     values = c("Dyadic-positive" = "#2c7fb8", "Dyadic-null" = "#c0c0c0"),
#     name = NULL
#   ) +
#   
#   scale_linetype_manual(
#     values = c("TRUE" = "solid", "FALSE" = "dashed"),
#     guide = "none"
#   ) +
#   
#   labs(
#     title = "Orthogonal Discriminators for Mean vs. SD Outcomes",
#     subtitle = "Solid line = best discriminator for that outcome type; percentages show classification accuracy",
#     x = NULL,
#     y = NULL,
#     caption = "Δ Proportion: share of SHAP impact from incremental features | Bilateral Bins: db×phase bins with both PT & FM showing Δ"
#   ) +
#   
#   theme_minimal() +
#   theme(
#     legend.position = "top",
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     strip.placement = "outside",
#     strip.text = element_text(face = "bold", size = 11),
#     strip.background = element_rect(fill = "gray95", color = NA),
#     panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.5),
#     panel.spacing = unit(0.8, "lines"),
#     plot.title = element_text(face = "bold", size = 14),
#     plot.subtitle = element_text(size = 10, color = "gray40"),
#     plot.caption = element_text(hjust = 0, size = 8, face = "italic"),
#     axis.text.y = element_text(size = 8),
#     axis.text.x = element_text(size = 9)
#   )
# 
# print(g_unified)
# 
# 
# 
# 
# 
# 