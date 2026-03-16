# Script to generate resampled Spearman correlation bar plot (predictive performance)
# Also writes csv with significant configs for stage 2

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggh4x)
library(viridis)
library(grid)
library(ggpubr)

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
  read_csv(path) %>%
    pivot_longer(c(rsq, rmse,spearman), names_to = ".metric", values_to = "est") |>
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
                                   preproc == "P+C+A" ~ "3"))
}

df <- my_read_csv("python/loocv_bstrap_results021026_combined.csv") %>%
  filter(.metric=="spearman")

df.rmse <- my_read_csv("python/loocv_bstrap_results021026_combined.csv") |>
  filter(.metric == "rmse")

df <- df |>
  mutate(adjusted_p_val = p.adjust(p_val, method = "BH"),
         significant.88 = adjusted_p_val <= 0.12,
         significant_ci.88 = lower2 > 0.01,
         significant.90 = adjusted_p_val <= 0.1,
         significant_ci.90 = lower2 > 0.01,
         significant.95 = adjusted_p_val <= 0.05,
         significant_ci.95 = lower > 0.01)

df_baseline <- df |> filter(preproc == "none") |>
  select(model, feat, db, mean) |>
  rename(mean_none = mean)

df2 <- df |>
  left_join(df_baseline, by = c("model","feat","db")) |>
  filter(.metric == "spearman", significant.95 & significant_ci.95)  |>
  mutate(delta_vs_none = mean - mean_none) |>
  select(preproc, model, feat, db, .metric, mean, delta_vs_none)
df2

df |>
  filter(.metric == "spearman", significant.90 & significant_ci.90) |>
  arrange(feat) |>
  write_csv("python/loocv_bstrap_results021026_combined_stage2.csv") # significant configs for stage 2

check_groups <- df |>
  filter(.metric == "spearman") |>
  group_by(model, feat, db) |>
  arrange(desc(significant.90), desc(significant_ci.90), desc(mean), .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  mutate(model = factor(model, levels = c("ET", "RF", "GBR", "LR", "RIDGE"))) |>
  mutate(
    root = case_when(
      str_detect(feat, "aa") ~ "sAA",
      str_detect(feat, "cort") ~ "Cortisol",
      str_detect(feat, "dheas") ~ "DHEAS"
    ),
    stat_group = if_else(str_detect(feat, "avg"), "Mean", "SD"),
    feat = str_replace_all(feat, c("slp7" = "", "avg" = "AVG", "sd" = "SD", "pt1" = "PT", "fm1" = "FM")),
    # Only keep preproc if significant
    preproc = if_else(significant.90 & significant_ci.90, preproc, NA_character_)
  ) |>
  mutate(
    root = factor(root, levels = c("sAA", "Cortisol", "DHEAS")),
    stat_group = factor(stat_group, levels = c("Mean", "SD")),
    preproc = factor(preproc,
                     levels = c("none", "preproc", "P", "C", "A",
                                "C+A", "P+A", "P+C", "P+C+A"))
  ) |>
  arrange(root, stat_group, feat) |>
  mutate(feat = factor(feat, levels = unique(feat)))

# ggh4x deprecated several functions, use legendry instead
library(legendry)

# Visualization
g <- check_groups |>
  ggplot(aes(x = interaction(model, feat),
             y = mean,
             fill = preproc)) +
  geom_bar(
    color = "gray40",
    linewidth = 0.3,
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(aes(label = case_when(
    significant.95 & significant_ci.95 ~ "**",
    significant.90 & significant_ci.90 ~ "*",
    TRUE ~ NA
  )),
  vjust = -0.2,
  size = 6) +
  facet_nested(
    ~ root + stat_group, 
    scales = "free_x", 
    space = "free_x",
    strip = strip_nested(
      background_x = list(
        element_rect(fill = "#b8d4e3", color = "black"),  # root level
        element_rect(fill = "gray90", color = "black")      # stat level
      ),
      text_x = list(
        element_text(size = 14, face = "bold", color = "black"),
        element_text(size = 12, face = "bold", color = "black") 
      ),
      by_layer_x = TRUE
    )
  ) +
  scale_x_discrete(
    guide = legendry::guide_axis_nested(
      key = key_range_auto(sep = "\\."),
      angle = 90,  # controls inner nest
      levels_text = list(
        NULL,                                             
        element_text(angle = 45, vjust = 0.7, size = 12) # outer nest
      )
    )
  ) +
  scale_fill_viridis_d(option = "F", direction = -1, na.value = "gray95") +
  theme_Publication() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16, angle = 90, vjust = 1),
    strip.text = element_text(face = "bold", size = 14, color = "black"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.spacing = unit(0.5, "lines"),
    legend.position = "top"
    # ggh4x.facet.nestline = element_line(colour = "black")
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.55)) +
  labs(
    x = "Daily Saliva Outcome",
    y = "Resampled Spearman corr.\n",
    fill = "Preprocessing Combination:"
  ) +
  guides(fill = guide_legend(nrow = 1))

print(g)

# # save high-res version
# ggsave("loocv_saliva_stage1_v2.png", g,
#        width = 16, height = 6.5, dpi = 300, bg = "white")
