library(biscale)
library(cowplot)
library(grid)
library(gtable)
library(tidyverse)
library(ggh4x)

#------------------------------------------------------------------------------
# Load and prepare BOTH outcome types
#------------------------------------------------------------------------------
# df_mean <- read_csv("python/shap_stage3_both_actor_delta3_avg.csv") |>
#   transmute(
#     feature,
#     model,
#     out,
#     mean_abs_impact = mean_abs_impact_both,
#     rank = rank_both,
#     on_delta = on_delta,
#     on_actor = on_actor,   
#     on_both  = on_both, 
#     spear_delta = spear_delta,
#     gate_both = gate_both,
#     gate_actor = gate_actor,
#     spear_both = spear_both,
#     spear_actor = spear_actor,
#     direction_flip = direction_flip
#   ) |>
#   mutate(outcome_type = "Mean Slope")
# 
# df_sd <- read_csv("python/shap_stage3_both_actor_delta3_sd.csv") |>
#   transmute(
#     feature,
#     model,
#     out,
#     mean_abs_impact = mean_abs_impact_both,
#     rank = rank_both,
#     on_delta = on_delta,
#     on_actor = on_actor,   
#     on_both  = on_both, 
#     spear_delta = spear_delta,
#     gate_both = gate_both,
#     gate_actor = gate_actor,
#     spear_both = spear_both,
#     spear_actor = spear_actor,
#     direction_flip = direction_flip
#   ) |>
#   mutate(outcome_type = "Slope SD")
# 
# # Combine both datasets
# df <- bind_rows(df_mean, df_sd) |>
#   mutate(model_out = paste(model, out, sep = "_"),
#          model_out = str_remove(model_out, "_[^_]+$")) |>
#   group_by(model_out) |>
#   mutate(new_model_name = paste0(model, cur_group_id()),
#          rev_rank = ntile(rank, 2)) |>
#   ungroup() |>
#   # Incremental SHAP: show tiles where BOTH is ON and ACTOR is OFF
#   mutate(shap_cat = case_when(
#     on_delta ~ cut(spear_delta, breaks = c(-1, 0, 1)),
#     TRUE ~ NA_character_
#   )) |>
#   mutate(rev_rank = factor(rev_rank, levels = c("1", "2")),
#          shap_cat = factor(shap_cat, levels = c("(-1,0]", "(0,1]"))) |>
#   arrange(outcome_type, out)
#------------------------------------------------------------------------------
# Load and prepare BOTH outcome types (combined file)
#------------------------------------------------------------------------------

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
  # Derive outcome type from outcome name
  mutate(
    outcome_type = case_when(
      str_detect(out, "avg") ~ "Mean Slope",
      str_detect(out, "sd")  ~ "Slope SD",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    model_out = paste(model, out, sep = "_"),
    model_out = str_remove(model_out, "_[^_]+$")
  ) |>
  group_by(model_out) |>
  mutate(
    new_model_name = paste0(model, cur_group_id()),
    rev_rank = ntile(rank, 2)
  ) |>
  ungroup() |>
  # Incremental SHAP: show tiles where DELTA is ON
  mutate(
    shap_cat = case_when(
      on_delta ~ cut(spear_delta, breaks = c(-1, 0, 1)),
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    rev_rank = factor(rev_rank, levels = c("1", "2")),
    shap_cat = factor(shap_cat, levels = c("(-1,0]", "(0,1]"))
  ) |>
  arrange(outcome_type, out)


df <- df |>
  mutate(model_out = factor(model_out, levels = df$model_out |> unique()))

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
  ) |>
  mutate(ptfm = case_when(
    ptfm == "pt1" ~ "PT feats",
    ptfm == "fm1" ~ "FM feats"),
    ptfm = factor(ptfm, levels = c("PT feats", "FM feats"))
  ) |>
  mutate(
    phase = case_when(
      str_detect(db, "Saliva") ~ factor(phase, 
                                        levels = c("pb", "b", "s", "sp1", "sp2", "r1", "r2")),
      TRUE ~ factor(phase, 
                    levels = c("b", "s", "p", "sp1", "sp2", "r"))
    )
  ) |>
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
# dyadic_status <- bind_rows(
#   read_csv("python/loocv_bstrap_results020126_sd_stage2_status.csv"),
#   read_csv("python/loocv_bstrap_results020126_stage2_status.csv")
# )
dyadic_status <- read_csv("python/loocv_bstrap_results021026_combined_stage2_status_90.csv")

exp_by_phase <- exp_by_phase |>
  left_join(dyadic_status, by = c("out", "model")) |>
  mutate(dyadic_sig = coalesce(dyadic_sig, FALSE))

#------------------------------------------------------------------------------
# FILTER TO DYADIC-POSITIVE ONLY
#------------------------------------------------------------------------------
exp_by_phase <- exp_by_phase |>
  filter(dyadic_sig == TRUE) |>
  mutate(
    # Create combined label with outcome type
    model_out_label = paste0(model_out, "\n(", outcome_type, ")")
  )

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
# Compute discriminators per model
#------------------------------------------------------------------------------

# Delta proportion (for mean outcomes)
model_impact <- exp_by_phase |>
  group_by(model_out, model_out_label, outcome_type) |>
  summarise(
    delta_impact = sum(mean_abs_impact[cell_state %in% c("delta_neg", "delta_pos")]),
    actor_impact = sum(mean_abs_impact[cell_state == "actor_only"]),
    .groups = "drop"
  ) |>
  mutate(
    total_impact = delta_impact + actor_impact,
    delta_proportion = if_else(total_impact > 0, delta_impact / total_impact, 0)
  )

# Bilateral bins (for SD outcomes) - compute both count and proportion
bilateral_bins <- exp_by_phase |>
  filter(cell_state %in% c("delta_neg", "delta_pos")) |>
  group_by(model_out, db, phase, outcome_type) |>
  summarise(
    has_pt = any(ptfm == "PT feats"),
    has_fm = any(ptfm == "FM feats"),
    is_bilateral = has_pt & has_fm,
    .groups = "drop"
  )

model_bilateral <- bilateral_bins |>
  group_by(model_out, outcome_type) |>
  summarise(
    n_bilateral_bins = sum(is_bilateral),
    n_delta_bins = n(),
    .groups = "drop"
  ) |>
  mutate(
    # pct_bilateral: proportion of delta bins that are bilateral
    pct_bilateral = if_else(n_delta_bins > 0, n_bilateral_bins / n_delta_bins, 0)
  )

# Join metrics
model_metrics <- model_impact |>
  left_join(model_bilateral, by = c("model_out", "outcome_type")) |>
  mutate(
    n_bilateral_bins = coalesce(n_bilateral_bins, 0L),
    n_delta_bins = coalesce(n_delta_bins, 0L),
    pct_bilateral = coalesce(pct_bilateral, 0)
  )

# Mark bilateral bins for ring overlay on SD outcomes
exp_by_phase <- exp_by_phase |>
  left_join(
    bilateral_bins |> filter(is_bilateral) |> select(model_out, db, phase, outcome_type, is_bilateral),
    by = c("model_out", "db", "phase", "outcome_type")
  ) |>
  mutate(
    is_bilateral = coalesce(is_bilateral, FALSE),
    show_bilateral_ring = is_bilateral & 
      cell_state %in% c("delta_neg", "delta_pos")
  )

#------------------------------------------------------------------------------
# Create ordering: group by outcome type, then order by appropriate discriminator
# Mean outcomes: ordered by delta_proportion
# SD outcomes: ordered by pct_bilateral
#------------------------------------------------------------------------------
mean_order <- model_metrics |>
  filter(outcome_type == "Mean Slope") |>
  arrange(desc(delta_proportion)) |>
  pull(model_out_label)

sd_order <- model_metrics |>
  filter(outcome_type == "Slope SD") |>
  arrange(desc(pct_bilateral)) |>
  pull(model_out_label)

# Combine: mean outcomes first, then SD outcomes
model_order <- c(mean_order, sd_order)

exp_by_phase <- exp_by_phase |>
  mutate(model_out_label = factor(model_out_label, levels = model_order)) %>% 
  mutate(model_out_label = forcats::fct_relabel(
    model_out_label,
    ~ str_replace_all(.x, c("slp7"="", "_aa"="_AA", "_cort"="_CORT", "_dheas"="_DHEAS", "avg"="avg", "sd"="sd", "pt1"="pt", "fm1"="fm"))
  ))

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
# Main heatmap
#------------------------------------------------------------------------------
g_heatmap <- ggplot(exp_by_phase) +
  
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
  
  # Bilateral rings (for SD outcomes only)
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
    # title = "Incremental SHAP Patterns in Dyadic-Positive Models",
    # subtitle = "Mean slope outcomes (ordered by Δ proportion) | Slope SD outcomes (ordered by % bilateral)",
    # caption = "Sidebar: Δ% = delta proportion (mean outcomes) | %Bilat = % bilateral (SD outcomes) | ○ = bilateral bin (PT+FM both show Δ)"
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 50, vjust = 0.6),
    axis.text.y.right = element_text(size = 12),
    axis.ticks.y.right = element_line(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 0.5, size = 11, face = "bold",
                                     margin = margin(r = 4)),
    strip.background = element_rect(color = "black", fill = "gray92", linewidth = 0.6),
    strip.text.x = element_text(face = "bold", size = 11),
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.3, "lines"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    plot.caption = element_text(hjust = 0, size = 9, face = "italic")
  )

#------------------------------------------------------------------------------
# Create unified sidebar with two columns
# Now using pct_bilateral (%) instead of n_bilateral_bins
#------------------------------------------------------------------------------
# Prepare sidebar data with both metrics
sidebar_data <- model_metrics |>
  mutate(
    delta_label = sprintf("%.0f%%", delta_proportion * 100),
    pct_bilateral_label = sprintf("%.0f%%", pct_bilateral * 100),
    # Both are now on 0-1 scale for consistent gradient
    delta_fill = delta_proportion,
    pct_bilateral_fill = pct_bilateral
  )

# Single unified sidebar
g_sidebar <- ggplot(sidebar_data, aes(y = model_out_label)) +
  # Delta proportion column (all rows)
  geom_tile(aes(x = 1, fill = delta_fill), color = "white", linewidth = 0.5) +
  geom_text(aes(x = 1, label = delta_label), size = 4.5, fontface = "bold") +
  
  # pct_bilateral column (all rows)
  geom_tile(aes(x = 2, fill = pct_bilateral_fill), color = "white", linewidth = 0.5) +
  geom_text(aes(x = 2, label = pct_bilateral_label), size = 4.5, fontface = "bold") +
  
  scale_fill_gradient2(
    low = "#f5f5f5", mid = "#b8d4e3", high = "#1a4e66",
    # midpoint = 0.2,  # midpoint for both metrics (adjust as needed)
    limits = c(0, 1),
    oob = scales::squish,
    na.value = NA
  ) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Δ%", "%Bilat"),
    position = "top"
  ) +
  scale_y_discrete(limits = rev(levels(sidebar_data$model_out_label))) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x.top = element_text(size = 14, face = "bold", lineheight = 0.9),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 5.5, r = 2, b = 5.5, l = 5),
    plot.background = element_blank(),
    panel.spacing = unit(0, "lines")
  )

#------------------------------------------------------------------------------
# Combine sidebar with main heatmap
#------------------------------------------------------------------------------
g_final <- plot_grid(
  g_sidebar, g_heatmap,
  nrow = 1,
  rel_widths = c(0.10, 0.90),
  align = "h",
  axis = "tb"
)

print(g_final)

#------------------------------------------------------------------------------
# Print summary of dyadic-positive models
#------------------------------------------------------------------------------
cat("\n=== Dyadic-Positive Model Metrics ===\n")
model_metrics |>
  select(model_out_label, outcome_type, delta_proportion, pct_bilateral, n_bilateral_bins, n_delta_bins) |>
  mutate(
    delta_proportion = sprintf("%.1f%%", delta_proportion * 100),
    pct_bilateral = sprintf("%.1f%%", pct_bilateral * 100)
  ) |>
  arrange(outcome_type, desc(model_out_label)) |>
  print(n = 20)

# # # Optional: save high-res version
# ggsave("shap_heatmap1_90.png", g_final,
#        width = 16, height = 10, dpi = 300, bg = "white")