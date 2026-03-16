## Script to create 2 panel dyadic add-on results plot and create csv with add-on results for stage 3

library(tidyverse)
library(ggh4x)
library(legendry) 

theme_Publication <- function(base_size=14) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(fill = "white", colour = NA),
            plot.background  = element_rect(fill = "white", colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#fdb462","#4477AA","#984ea3","#EE6677","#386cb0","#7fc97f","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# ---- helper: one-sided significance flags with BH adjustment ----
add_sig_flags_pos <- function(df, value_col = "mean", lower_col = "lower", p_col = "p_val") {
  df |>
    mutate(
      adjusted_p_val = p.adjust(.data[[p_col]], method = "BH"),
      sig95 = adjusted_p_val <= 0.05 & .data[[lower_col]] > 0,
      sig90 = adjusted_p_val <= 0.10 & .data[[lower_col]] > 0
    )
}

BOOT_PATH   <- "python/loocv_stage2_boot_021226_095647.csv"
RESID_PATH  <- "python/loocv_stage2_resid_delta_boot_021226_095647.csv"
PARTIAL_PATH <- "python/loocv_stage2_partial_boot_021226_095647.csv"
MANIFEST_CSV <- "python/loocv_bstrap_results021026_combined_stage2.csv"

boot_raw <- read_csv(BOOT_PATH, show_col_types = FALSE)
resid_raw <- read_csv(RESID_PATH, show_col_types = FALSE)
partial_raw <- read_csv(PARTIAL_PATH, show_col_types = FALSE)

manifest <- read_csv(MANIFEST_CSV, show_col_types = FALSE) |>
  # filter(significant.95 & significant_ci.95) %>% 
  mutate(
    feat = str_replace_all(feat, c("fm1" = "fm", "pt1" = "pt")),
    out = paste0(feat, "_", db)
  ) |>
  group_by(out, model) |>
  arrange(desc(significant.95), desc(significant_ci.95), desc(mean), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(out, model, preproc_org)

stage2_delta_roles <- function(raw_boot, metric = "spearman") {
  raw_boot |>
    select(boot_id, out, model, preproc, view, all_of(metric)) |>
    rename(est = all_of(metric)) |>
    separate(out, sep = "_(?=[^_]+$)", into = c("feat", "db")) |>
    pivot_wider(names_from = view, values_from = est) |>
    mutate(
      delta_add_pt = both - fm,  # add PT to FM-only
      delta_add_fm = both - pt   # add FM to PT-only
    ) |>
    pivot_longer(c(delta_add_pt, delta_add_fm),
                 names_to = "contrast",
                 values_to = "delta") |>
    mutate(
      contrast = recode(contrast,
                        delta_add_pt = "Add PT: both vs FM-only",
                        delta_add_fm = "Add FM: both vs PT-only ")
    ) |>
    group_by(model, feat, db, preproc, contrast) |>
    summarize(
      mean  = mean(delta, na.rm = TRUE),
      lower = quantile(delta, 0.025, na.rm = TRUE),
      upper = quantile(delta, 0.975, na.rm = TRUE),
      lower2 = quantile(delta, 0.05, na.rm = TRUE),
      upper2 = quantile(delta, 0.95, na.rm = TRUE),
      p_val = (sum(delta <= 0, na.rm = TRUE) + 1) / (sum(!is.na(delta)) + 1),
      .groups = "drop"
    ) |>
    add_sig_flags_pos()
}

delta_roles_all <- stage2_delta_roles(boot_raw, metric = "spearman") |>
  mutate(
    feat = str_replace_all(feat, c("fm1"="fm", "pt1"="pt")),
    out = paste0(feat, "_", db)) |>
  mutate(
    contrast = factor(contrast, levels = c(
      "Add FM: both vs PT-only ",
      "Add PT: both vs FM-only"
    ))
  )

delta_roles_manifest <- delta_roles_all |>
  inner_join(manifest, by = c("out","model","preproc"="preproc_org"))

partial_sum <- partial_raw |>
  group_by(model, out, preproc) |>
  summarize(
    mean  = mean(partial_spearman_add, na.rm = TRUE),
    lower = quantile(partial_spearman_add, 0.025, na.rm = TRUE),
    upper = quantile(partial_spearman_add, 0.975, na.rm = TRUE),
    lower2 = quantile(partial_spearman_add, 0.05, na.rm = TRUE),
    upper2 = quantile(partial_spearman_add, 0.95, na.rm = TRUE),
    p_val = (sum(partial_spearman_add <= 0, na.rm = TRUE) + 1) /
      (sum(!is.na(partial_spearman_add)) + 1),
    .groups = "drop"
  ) |>
  add_sig_flags_pos() |>
  separate(out, sep = "_(?=[^_]+$)", into = c("feat", "db")) |>
  mutate(
    feat = str_replace_all(feat, c("fm1"="fm", "pt1"="pt")),
    out2 = paste0(feat, "_", db),
    contrast = "Partial add-value: ρ( y , ŷ_BOTH | ŷ_actor )"
  ) |>
  inner_join(manifest, by = c("out2" = "out", "model", "preproc" = "preproc_org"))

best_supp_keys <- partial_sum |>
  distinct(out = out2, model, preproc)

abs_rho <- boot_raw |>
  select(boot_id, out, model, preproc, view, spearman) |>
  group_by(model, out, preproc, view) |>
  summarize(
    mean  = mean(spearman, na.rm = TRUE),
    lower = quantile(spearman, 0.025, na.rm = TRUE),
    upper = quantile(spearman, 0.975, na.rm = TRUE),
    lower2 = quantile(spearman, 0.05, na.rm = TRUE),
    upper2 = quantile(spearman, 0.95, na.rm = TRUE),
    p_val = (sum(spearman <= 0, na.rm = TRUE) + 1) / (sum(!is.na(spearman)) + 1),
    .groups = "drop"
  ) |>
  add_sig_flags_pos() |>
  separate(out, sep = "_(?=[^_]+$)", into = c("feat", "db")) |>
  mutate(
    feat = str_replace_all(feat, c("fm1"="fm", "pt1"="pt")),
    out2 = paste0(feat, "_", db)
  ) |>
  inner_join(best_supp_keys, by = c("out2"="out", "model", "preproc"))

# Panel A data (two bars per x)
plot_abs <- abs_rho |>
  mutate(
    panel = "ρ(y, ŷ)",
    stat = recode(view, pt   = "PT-only",
                  fm   = "FM-only",
                  both = "BOTH"),
    label = case_when(sig95 ~ "**", sig90 ~ "*", TRUE ~ "")
  ) |>
  transmute(
    panel, model, feat, db, preproc, stat, mean, lower, upper, sig95, 
    sig90, label
  )

# Panel B data (one bar per x)
plot_suppress <- partial_sum |>
  mutate(
    panel = "ρ(y, ŷ_BOTH | ŷ_actor)",
    stat = "Partial add-value",
    label = case_when(sig95 ~ "**", sig90 ~ "*", TRUE ~ "")
  ) |>
  transmute(
    panel, model, feat, db, preproc,
    stat, mean, lower, upper, sig95, sig90, label
  )

plot_all <- bind_rows(plot_abs, plot_suppress) |>
  mutate(
    panel = factor(panel, levels = c(
      "ρ(y, ŷ)",
      "ρ(y, ŷ_BOTH | ŷ_actor)"
    ))
  )


# 2) choose an explicit order of outcomes and models
feat_levels  <- plot_all |> distinct(feat)  |> pull(feat)
model_levels <- plot_all |> distinct(model) |> pull(model)

plot_all <- plot_all |>
  mutate(
    feat_f  = factor(feat,  levels = feat_levels),
    model_f = factor(model, levels = model_levels),
    x = interaction(model_f, feat_f, lex.order = TRUE)
  )

# 4) set x levels in the correct order (feat block, then model within)
x_levels <- plot_all |>
  distinct(feat_f, model_f, x) |>
  arrange(
    !grepl("avg", feat_f),  # FALSE (avg) comes first
    feat_f,
    model_f
  ) |>
  pull(x)

plot_all <- plot_all |>
  mutate(x = factor(x, levels = x_levels))


pd <- position_dodge(width = 0.85)

plot_all |> select(-c(x,model_f,feat_f,lower,upper,db)) |> 
  mutate(mean=100*mean) |> print(n=60) |> 
  arrange(model, feat, stat) |> print(n=100)

## FOR STAGE 3
STATUS_CSV <- sub("\\.csv$", "_status.csv", MANIFEST_CSV)
plot_all |> 
  filter(panel=="ρ(y, ŷ_BOTH | ŷ_actor)") |> 
  unite(out, c(feat,db)) |>
  distinct(out,model,sig90) |> print(n=30) |>
  rename(dyadic_sig=sig90) |>
  mutate(out=str_replace_all(out, c("fm" = "fm1", "pt" = "pt1"))) |>
  write_csv(STATUS_CSV)

g_both <- plot_all |>
  ggplot(aes(x = x, y = mean, fill = stat, group = stat)) +
  geom_col(
    position = pd, color = "gray", linewidth = 0.3, width = 0.78
  ) +
  # geom_errorbar(
  #   aes(ymin = lower, ymax = upper),
  #   position = pd, width = 0.18, linewidth = 0.4
  # ) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_text(
    aes(label = label),
    position = pd, vjust = -0.2, size = 5
  ) +
  facet_grid(panel ~ db, scales = "free", space = "free_x") +
  scale_x_discrete(guide = guide_axis_nested(
    key = key_range_auto(sep = "\\."),
    angle = 90,
    drop_zero = FALSE,
    levels_text = list(
      NULL,
      element_text(angle = 45)
    )
  )) +
  theme_Publication() +
  labs(
    x = "Outcome (model × role)",
    y = "Correlation (Spearman)",
    fill = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  theme(
    legend.position = "top",
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    strip.background = element_rect(fill = "gray90", color = "black", linewidth = 0.5)
  ) +
  scale_fill_Publication()

g_both
