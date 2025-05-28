## Script to generate R^2 plot and feature importance plot

library(tidyverse)

theme_Publication <- function(base_size=14) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
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
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

##--- R2 PLOT W NEW SIG--------------------------------------------------------------
library(tidyverse)
library(ggh4x)
my_read_csv <- function(path) {
  read_csv(path) |>
    pivot_longer(c(rsq, rmse), names_to = ".metric", values_to = "est") |>
    # filter(!is.na(est)) %>% 
    group_by(preproc, model, out, .metric) |>
    summarize(
      mean = mean(est),
      lower = quantile(est, 0.025),
      upper = quantile(est, 0.975),
      # use False Discovery Rate (FDR) to correct multiple testing errors
      lower2 = quantile(est, 0.05),#quantile(est, 3.9e-05),
      upper2 = quantile(est, 0.95),#quantile(est, 0.999961), 
      p_val = (sum(est <= 0.01))/n()
    ) |>
    ungroup() |>
    separate(out, sep = "_(?=[^_]+$)", into = c("feat", "db")) |>
    mutate(preproc_org = preproc) |>
    mutate(preproc = case_when(preproc == "000" ~ "none",
                               preproc == "001" ~ "A",
                               preproc == "010" ~ "C",
                               preproc == "011" ~ "C+A",
                               preproc == "100" ~ "P",
                               preproc == "101" ~ "P+A",
                               preproc == "110" ~ "P+C",
                               preproc == "111" ~ "P+C+A"),
           new_preproc = case_when(preproc == "none" ~ "none",
                                   preproc == "A" ~ "1",
                                   preproc == "C" ~ "1",
                                   preproc == "P" ~ "1",
                                   preproc == "C+A" ~ "2",
                                   preproc == "P+A" ~ "2",
                                   preproc == "P+C" ~ "2",
                                   preproc == "P+C+A" ~ "3")) #|>
  # mutate(preproc = factor(preproc,
  #                         levels = c("none", "preproc", "P", "C", "A",
  #                                    "C+A", "P+A", "P+C", "P+C+A")))
}


df <- my_read_csv("python/loocv_bstrap_results051425.csv") %>% 
  filter(.metric=="rsq")

df <- df |>
  mutate(adjusted_p_val = p.adjust(p_val, method = "BH"),
         significant.88 = adjusted_p_val <= 0.12,
         significant_ci.88 = lower2 > 0.01,
         significant.90 = adjusted_p_val <= 0.1,
         significant_ci.90 = lower2 > 0.01,
         significant.95 = adjusted_p_val <= 0.05,
         significant_ci.95 = lower > 0.01)

check_groups<-df |>
  filter(.metric == "rsq") |>
  group_by(model, feat, db) |>
  arrange(desc(significant.90), desc(significant_ci.90), desc(mean),
          .by_group = TRUE) |>
  # arrange(desc(significant.90), desc(significant_ci.90), desc(mean), 
  #          .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  mutate(preproc = factor(preproc,
                          levels = c("none", "preproc", "P", "C", "A",
                                     "C+A", "P+A", "P+C", "P+C+A"))) |>
  mutate(preproc = case_when(
    significant.90 & significant_ci.90 ~ preproc, 
    TRUE ~ NA)) %>% 
  mutate(feat = str_replace(feat, "fm1", "fm")) %>% 
  mutate(feat = str_replace(feat, "pt1", "pt"))

dev.off()

g = check_groups |>
  ggplot(aes(x = interaction(model, feat),
             y = mean, fill = preproc,
             group = as_factor(model))) +
  geom_bar(
    color = "gray",
    linewidth=0.3,
    stat = "identity", position = "dodge") +
  geom_text(aes(label = case_when(
    significant.95 & significant_ci.95 ~ "**", 
    # significant.90 & significant_ci.90 ~ "**", 
    significant.90 & significant_ci.90 ~ "*", 
    TRUE ~ NA)),
    vjust = 0,
    size=6) +
  facet_wrap(~db, scales = "free_x") +
  scale_x_discrete(guide =
                     guide_axis_nested(angle=90)) +
  scale_fill_viridis_d(option="F", direction = -1) +
  theme_Publication() +
  theme(
    axis.text.x = element_text(
      size = 10
    ),
    ggh4x.axis.nesttext.x = element_text(
      angle = 45, hjust = 1, size = 13
    ),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),  # Adjust x-axis title size
    axis.title.y = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 18, color = "black"),
    legend.title = element_text(size = 18),  # Make legend title smaller and not italicized
    legend.text = element_text(size = 16),
    strip.background = element_rect(color = "black", fill = "gray90", size = 1),  
    panel.border = element_rect(color = "black", size = 1),
    panel.spacing = unit(0.5, "lines")
  ) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.35)) +
  labs(
    x = "Sleep Health Outcome",
    y = "Resampled R² score\n",
    fill = "Preprocessing Combination:"
  ) +
  guides(fill = guide_legend(nrow = 1))

library(grid)
gt = ggplot_gtable(ggplot_build(g))
grid.draw(gt)

##--- VAR IMP PLOT --------------------------------------------------------------
df <- read_csv("python/shap_ranked_dist_all4_na.csv") |>
  select(feature, model, out, feat_shap_corr, mean_abs_impact, rank) |>
  mutate(model_out = paste(model, out, sep = "_"),
         model_out = str_remove(model_out, "_[^_]+$")) |>
  group_by(model_out) |>
  mutate(new_model_name = paste0(model, cur_group_id()),
         # rev_rank = max(rank) + 1 - rank) |>
         rev_rank = ntile(rank, 2), 
         rev_rank = case_when(is.na(feat_shap_corr) ~ NA,
                              TRUE ~ rev_rank)) |>
  ungroup() |>
  #select(-model_out)|>
  mutate(shap_cat = cut(feat_shap_corr, breaks = c(-1, 0, 1))) |>
  mutate(rev_rank = factor(rev_rank, levels = c("1", "2")), 
         shap_cat = factor(shap_cat, levels = c("(-1,0]", "(0,1]"))) |>
  mutate(rev_rank = case_when(rev_rank == "2" ~ NA,
                              TRUE ~ rev_rank)) |>
  mutate(shap_cat = case_when(is.na(rev_rank) ~ NA, 
                              TRUE ~ shap_cat)) |>
  mutate(new_model_name = factor(new_model_name,
                                 levels = c("RF4", "RIDGE5", "LR1",
                                            "SVR7", "LR2", "SVR8",
                                            "RF3", "RIDGE6"))) |>
  arrange(out)

df <- df |>
  arrange(out) |>
  mutate(model_out = factor(model_out, levels = df$model_out |> unique()))

library(biscale)
library(cowplot)
df |> 
  distinct(new_model_name, model_out)

my_pal <- list(`(-1,0]` = "#ad5b9c", `(0,1]` = "#434e87")

# biscale::bi_pal("DkBlue2", dim = 2, preview=FALSE) |> 
# as.list()
exp_by_phase <- df |>
  mutate(ptfm = str_extract(feature, "(fm1)|(pt1)|(PC)"),
         is_q_feat = str_detect(feature, ".*q.*"),
         db = case_when(
           str_detect(feature, "PC") ~ "q",
           str_detect(feature, "coreg") ~ "coreg",
           str_detect(feature, "coag") ~ "coag",
           TRUE ~ str_extract(feature, "[^_]+$"))) |>
  filter(db %in% c("clo", "coreg", "coag", "cardio")) |>
  separate(feature, into = c("feat", "phase", "rest"), sep = "_", extra = "merge") |>
  mutate(db = case_when(
    db == "clo" ~ "CLO\nGamma",
    db == "cardio" ~ "HFP",
    db == "coreg" ~ "Coregulation",
    db == "coag" ~ "Coagitation",
    TRUE ~ "Other"),
    db = factor(db, levels = c("Coregulation", "Coagitation", "HFP", "CLO\nGamma"))
  ) %>%
  mutate(ptfm = case_when(
    ptfm == "pt1" ~ "Patient Features",
    ptfm == "fm1" ~ "Caregiver Features"),
    ptfm = factor(ptfm, levels = c("Patient Features", "Caregiver Features"))
  ) %>%
  #unite("phase_ptfm", c(phase, ptfm), sep = "_", remove = F) |>
  mutate(phase = factor(phase, 
                        levels = c("b", "s",
                                   "p", "sp1",
                                   "sp2", "r"))) %>%
  mutate(
    model_out = factor(model_out, levels = c("RF_avgWASO_min_fm", "RF_avgSD_hr_fm", "RF_SRI_pt1",
                                             "RF_tb110_mean_pt1", "RF_IV_fm1", "LR_IV_fm1",
                                             "LR_se36_mean_pt1", "SVR_se36_mean_pt1", "RIDGE_se36_mean_pt1",
                                             "LR_IS_fm1", "SVR_IS_fm1", "RIDGE_IS_fm1"))
  ) %>%
  mutate(
    model_out = case_when(
      model_out == "RF_avgWASO_min_fm" ~ "avgWASO_min_fm (RF)",
      model_out == "RF_avgSD_hr_fm" ~ "avgSD_hr_fm (RF)",
      model_out == "RF_SRI_pt1" ~ "SRI_pt (RF)",
      model_out == "RF_tb110_mean_pt1" ~ "tb110_mean_pt (RF)",
      model_out == "RF_IV_fm1" ~ "IV_fm (RF)",
      model_out == "LR_IV_fm1" ~ "IV_fm (LR)",
      model_out == "LR_se36_mean_pt1" ~ "se36_mean_pt (LR)",
      model_out == "SVR_se36_mean_pt1" ~ "se36_mean_pt (SVR)",
      model_out == "RIDGE_se36_mean_pt1" ~ "se36_mean_pt (RIDGE)",
      model_out == "LR_IS_fm1" ~ "IS_fm (LR)",
      model_out == "SVR_IS_fm1" ~ "IS_fm (SVR)",
      model_out == "RIDGE_IS_fm1" ~ "IS_fm (RIDGE)",
    )
  )

dev.off()
gplot = ggplot(exp_by_phase) +
  geom_tile(aes(x = phase, y = model_out, fill = shap_cat), 
            color = "black", size=0.2, show.legend = T) +
  scale_fill_manual(values = my_pal, na.value = "gray92") + 
  # bi_scale_fill(pal = "DkBlue2", dim = 2, rotate_pal = T, flip_axes = F,
  #               na.value = "white") +
  # scale_fill_brewer(palette = "Reds", direction = -1) +
  # scale_fill_gradient2(low="blue", mid = 'white', high="red") +
  facet_grid(ptfm~db, drop=TRUE, scales = "free_x", space = "free_x") + 
  theme_Publication() +
  theme(
    axis.text.x = element_text(size = 17, angle = 45, vjust=0.8),
    axis.text.y = element_text(size = 14),
    strip.background = element_rect(color = "black", fill = "gray90", size = 1),  
    axis.title.x = element_text(size = 18),  # Adjust x-axis title size
    axis.title.y = element_text(size = 18),
    strip.text = element_text(face = "bold", size = 15, color = "black"),
    panel.background = element_rect(fill = "white", color = NA),  # Set grid background to gray
    # panel.grid.major = element_blank(),  # Remove major grid lines if needed
    # panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", size = 1),
    panel.spacing = unit(0.5, "lines"),
    legend.title = element_text(size = 18),  # Make legend title smaller and not italicized
    legend.text = element_text(size = 18)
  ) +
  labs(
    x = "Phase",
    y = "Sleep Outcome by Model",
    fill = "SHAP value directional impact  "
  ) 

gt = ggplot_gtable(ggplot_build(gplot))
grid.draw(gt)
