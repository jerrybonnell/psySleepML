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


# df <- my_read_csv("python/loocv_bstrap_results020325_fullwrf.csv") |>
df <- my_read_csv("python/loocv_bstrap_results051425_test.csv") %>% 
  filter(.metric=="rsq")

df <- df |>
  mutate(adjusted_p_val = p.adjust(p_val, method = "BH"),
         significant.88 = adjusted_p_val <= 0.12,
         significant_ci.88 = lower2 > 0.01,
         significant.90 = adjusted_p_val <= 0.1,
         significant_ci.90 = lower2 > 0.01,
         significant.95 = adjusted_p_val <= 0.05,
         significant_ci.95 = lower > 0.01)
View(df)

# Type I error rate to account for multiple testing
# # 4*8*20
# df |>
#   filter(.metric == "rsq") 
# df |>
#   filter(.metric == "rsq", significant.90 & significant_ci.90) 
# df |>
#   filter(.metric == "rsq", significant.95 & significant_ci.95) 

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


check_groups
#check_groups
#arrange(feat,model)
#filter(model == "RIDGE", feat == "IV_pt1") |> arrange(desc(mean))
#filter(model == "RIDGE", feat == "se36_mean_pt1") |> arrange(desc(mean))

# df |>
#   filter(.metric == "rsq") |>
#   group_by(model, feat, db) |>
#   slice_max(mean) |>
#   ungroup() |>
# Clear the current plot
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
gt$widths[7] = 1.5*gt$widths[7]
gt$widths[11] = 0.9*gt$widths[11]
gt$widths[15] = 0.6*gt$widths[15]
grid.draw(gt)

##--- VAR IMP PLOT --------------------------------------------------------------
# varimp_res <- read_csv("python/loocv_var_imp030125_fullwrf.csv")
# 
# varimp_df <- varimp_res |>
#   #filter(val > 0.00) |>
#   # NOTE: change Factor to PC if not using EFA
#   mutate(ptfm = str_extract(var, "(fm1)|(pt1)|(PC)"),
#          is_q_feat = str_detect(var, ".*q.*"),
#          db = case_when(
#            str_detect(var, "PC") ~ "q",
#            str_detect(var, "coreg") ~ "coreg",
#            str_detect(var, "coag") ~ "coag",
#            TRUE ~ str_extract(var, "[^_]+$"))) |>
#   mutate(ptfm = case_when(is.na(ptfm) ~ "fm1",
#                           TRUE ~ ptfm)) |>
#   mutate(db = case_when(
#     is_q_feat ~ "q",
#     TRUE ~ db)) |>
#   unite("db2", c(ptfm, db), sep = "_") |>
#   group_by(out, model, db2, preproc) |>
#   summarize(avg_db = mean(val)) |>
#   ungroup() |>
#   # mutate(db2 = factor(db2,
#   #                     levels = unique(db2[order(avg_db, decreasing = TRUE)]),
#   #                     ordered = TRUE)) |>
#   group_by(out, model, preproc) |>
#   # max absolute value scaling
#   mutate(avg_db_minmax = avg_db / max(abs(avg_db))) |>
#   ungroup() #|>
# varimp_df

# best_with_importance <- check_groups |>
#   arrange(desc(mean)) |>
#   unite("out", c(feat, db)) |>
#   left_join(varimp_df,
#             by = c("out", "model", "preproc_org" = "preproc")) |>
#   select(-c(lower, upper, new_preproc)) |>
#   separate(out, sep = "_(?=[^_]+$)", into = c("feat", "db")) |>
#   mutate(db2 = factor(db2,
#                       levels = c("pt1_q", "fm1_q", "PC_q",
#                                  "pt1_cardio", "fm1_cardio",
#                                  "pt1_coag", "fm1_coag",
#                                  "pt1_coreg", "fm1_coreg",
#                                  "pt1_co", "fm1_co",
#                                  "pt1_clo", "fm1_clo")),
#          model = factor(model,
#                         levels = c("LR", "RIDGE", "SVR", "RF", "LASSO")))
# 
# best_with_importance |>
#   #mutate(high = mean >= 0.1) |>
#   #drop_na(preproc) |>
#   # filter(lower2 > 0.01) |>
#   filter(significant.95 & significant_ci.95) |>
#   ggplot(aes(y = db2, x = model, fill = avg_db_minmax)) +
#   geom_tile(aes(color = significant.95 & significant_ci.95), size=0.7) +
#   facet_wrap(db~feat) +
#   scale_fill_gradient2(low="blue", mid = 'white', high="red") +
#   scale_color_manual(values = c('FALSE'="black", 'TRUE'="blue")) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#--------- PCA loadings analysis (02/07/25)
# ##--- PCA PLOT --------------------------------------------------------------
# df <- read_csv("python/pca_avg_components.csv") |>
#   mutate(across(everything(), abs))
# df |>
#   slice(3) |>
#   pivot_longer(everything(), names_to = "var", values_to = "val") |>
#   arrange(desc(val)) |>
#   slice_head(n=20) |>
#   ggplot() +
#   geom_bar(aes(y = reorder(var, val), x = val), stat = 'identity')

# # TODO maybe heatmap? but something needs aggregation... 
# df |>
#   mutate(PC = row_number()) |> 
#   pivot_longer(cols=ADL_1_q43_X1:last_col(offset=1), 
#                names_to = "var", values_to = "val") |>
#   ggplot() +
#   geom_tile(aes(x = PC, y = var, color = val))

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
df
#group_by(new_model_name) |>
# mutate(rank = (rank - min(rank)) / (max(rank) - min(rank)))
#mutate(rank_tiled = as_factor(ntile(rank, 3))) |>
#ungroup()

library(biscale)
library(cowplot)
# df <- df |>
#   bi_class(x = shap_cat, y = rev_rank, dim = 2) |>
#   mutate(bi_class = case_when(bi_class == "NA-NA" ~ NA, 
#                               bi_class == "1-NA" ~ NA, 
#                               bi_class == "2-NA" ~ NA, 
#                               TRUE ~ bi_class))

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
# gt$widths[13] = 1.7*gt$widths[13]
grid.draw(gt)
# df |>
#   mutate(ptfm = str_extract(feature, "(fm1)|(pt1)|(PC)"),
#          is_q_feat = str_detect(feature, ".*q.*"),
#          db = case_when(http://127.0.0.1:25597/graphics/10897da4-d640-44fd-bfb8-f719bffcea7d.png
#            str_detect(feature, "PC") ~ "q",
#            str_detect(feature, "coreg") ~ "co",
#            TRUE ~ str_extract(feature, "[^_]+$"))) |>
#   mutate(feature = factor(feature,
#                           levels = c("PC2", "PC3", "PC9", "PC19",
#                                      "PC6", "PC15", "PC16",
#                                      "PC12", "PC20", "PC14",
#                                      "PC5", "PC11", "PC17",
#                                      "PC13", "PC18",
#                                      "PC1", "PC4", "PC7",
#                                      "PC8", "PC10", "PC21",
#                                      "PC22", "PC23", "PC24"))) |>
#   filter(db %in% c("q")) |> # kills RF features 
#   ggplot() +
#   geom_tile(aes(x = feature, y = new_model_name, fill = shap_cat), 
#             color = "gray", show.legend = T) +
#   scale_fill_manual(values = my_pal, na.value = "white") + 
#   # bi_scale_fill(pal = "DkBlue2", dim = 2, rotate_pal = T, flip_axes = F,
#   #               na.value = "white") +
#   # scale_fill_brewer(palette = "Reds", direction = -1) +
#   facet_wrap(~db, ncol = 1) +
#   theme_Publication() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 



exp_q_features <- df |>
  filter(!feat_shap_corr == "NaN") %>% 
  filter(str_detect(feature, ".*q.*") | str_detect(feature, "PC")) |>  # Select questionnaire features
  mutate(feature = case_when(
    str_detect(feature, "pt?_q") ~ "pt1_q",
    # str_detect(feature, "fm1_q") ~ "fm1_q",
    str_detect(feature, "PC") ~ feature,  # Keep individual PCs as is
    TRUE ~ "fm1_q"
  )) %>% 
  mutate(
    feature = factor(feature, levels = c("pt1_q", "fm1_q", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", 
                   "PC10", "PC11", "PC12", "PC13", "PC14", "PC15", "PC16", "PC17", 
                   "PC18", "PC19", "PC20", "PC21", "PC22", "PC23", "PC24"))
  ) %>% 
  filter(!(feature %in% c("pt1_q", "fm1_q") & is.na(rev_rank))) %>% 
  group_by(feature, model, out) %>% 
  mutate(
    feat_shap_corr = mean(feat_shap_corr, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(shap_cat = cut(feat_shap_corr, breaks = c(-1, 0, 1))) %>% 
  mutate(rev_rank = factor(rev_rank, levels = c("1", "2")), 
         shap_cat = factor(shap_cat, levels = c("(-1,0]", "(0,1]"))) %>% 
  mutate(rev_rank = case_when(rev_rank == "2" ~ NA,
                              TRUE ~ rev_rank)) |>
  mutate(shap_cat = case_when(is.na(rev_rank) ~ NA, 
                              TRUE ~ shap_cat))

# Create the heatmap
ggplot(exp_q_features) +
  geom_tile(aes(x = feature, y = new_model_name, fill = shap_cat), 
            color = "black", size=0.2, show.legend = TRUE) +
  scale_fill_manual(values = my_pal, na.value = "white") +
  # scale_fill_gradient2(low="blue", mid = 'white', high="red") +
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


##--- PCA PLOT --------------------------------------------------------------

important_models = check_groups %>%  
  filter(str_detect(preproc, "P")) %>% 
  filter(significant.88 & significant_ci.88) %>% 
  mutate(preproc = case_when(preproc == "P" ~ "100",
                             preproc == "P+A" ~ "101",
                             preproc == "P+C" ~ "110",
                             preproc == "P+C+A" ~ "111")) %>%
  mutate(out = paste(feat, db, sep = "_")) %>%
  mutate(combined = paste(out, model, preproc_org, sep = "_"))

filtered_varimp_res <- varimp_res %>%
  mutate(combined = paste(out, model, preproc, sep = "_")) %>%
  filter(combined %in% important_models$combined) %>%
  select(-combined) %>%
  separate(out, sep = "_(?=[^_]+$)", into = c("feat", "db")) %>%
  # group_by(feat, db, model) %>%
  # slice_max(order_by = abs(shap_val), n = 5) %>%
  group_by(feat, db, model) %>%
  slice_max(order_by = abs(shap_val), n = 10) %>%
  ungroup() %>%
  filter(str_detect(var, "PC")) %>%
  mutate(component_num = as.numeric(str_extract(var, "\\d+")))
  # ungroup()

pca_imp_long <- read_csv("python/pca_avg_components.csv") |>
  mutate(across(everything(), abs)) %>%
  mutate(component = rownames(.)) %>%  # Keep PCA components (assuming rownames are component names)
  pivot_longer(cols = -component, names_to = "variable", values_to = "loading") %>% 
  mutate(group = sapply(str_split(variable, "_"), function(x) paste(x[(length(x)-1):length(x)-1], collapse = "_"))) %>% 
  mutate(group = case_when(
    str_detect(group, "^pt") ~ str_replace(group, "^[^_]+", "pt1"),
    TRUE ~ str_replace(group, "^[^_]+", "fm1")
  ))


library(tibble)

group_mapping <- tribble(
  ~group, ~group_name, ~section,
  # Section 1: Demographic Info
  "pt1_q65", "age_pt1", "demographic_pt1",
  "fm1_q72", "age_fm1", "demographic_fm1",
  "pt1_q1", "ethnicity_pt1", "demographic_pt1",
  "fm1_q1", "ethnicity_fm1", "demographic_fm1",
  "pt1_q2", "relationship_dur_pt1", "demographic_pt1",
  "fm1_q2", "relationship_dur_fm1", "demographic_fm1",
  "pt1_q3", "education_pt1", "demographic_pt1",
  "fm1_q3", "education_fm1", "demographic_fm1",
  "pt1_q4", "household_size_pt1", "demographic_pt1",
  "fm1_q4", "household_size_fm1", "demographic_fm1",
  "pt1_q5", "income_pt1", "demographic_pt1",
  "fm1_q5", "income_fm1", "demographic_fm1",
  "pt1_q6", "employment_pt1", "demographic_pt1",
  "fm1_q6", "employment_fm1", "demographic_fm1",
  "pt1_q7", "citizenship_pt1", "demographic_pt1",
  "fm1_q7", "citizenship_fm1", "demographic_fm1",
  
  # Section 2: Medical History
  "pt1_q8", "tobacco_exercise_pt1", "medical_history_pt1",
  "fm1_q8", "tobacco_exercise_fm1", "medical_history_fm1",
  "pt1_q9-12", "food_drink_intake_pt1", "medical_history_pt1",
  "fm1_q9-12", "food_drink_intake_fm1", "medical_history_fm1",
  "pt1_q13-14", "drugs_alcohol_pt1", "medical_history_pt1",
  "fm1_q13-14", "drugs_alcohol_fm1", "medical_history_fm1",
  "pt1_q15", "medical_morbidity_pt1", "medical_history_pt1",
  "fm1_q15", "medical_morbidity_fm1", "medical_history_fm1",
  "pt1_q16a-m", "healthcare_utilization_pt1", "medical_history_pt1",
  "fm1_q16a-m", "healthcare_utilization_fm1", "medical_history_fm1",
  "pt1_q16np", "personal_family_crc_pt1", "medical_history_pt1",
  "fm1_q16np", "personal_family_crc_fm1", "medical_history_fm1",
  "pt1_q17-20", "smoking_drinking_pt1", "medical_history_pt1",
  "fm1_q17-20", "smoking_drinking_fm1", "medical_history_fm1",
  "pt1_q21", "drug_screening_pt1", "medical_history_pt1",
  "fm1_q21", "drug_screening_fm1", "medical_history_fm1",
  
  # Section 4: Thoughts & Feelings (PT)
  "pt1_q36", "optimism_pt1", "thoughts_feelings_pt1",
  "pt1_q37", "big5_personality_pt1", "thoughts_feelings_pt1",
  "pt1_q38", "urgency_self_control_pt1", "thoughts_feelings_pt1",
  "pt1_q39", "uncertainty_illness_pt1", "thoughts_feelings_pt1",
  "pt1_q40", "attachment_qualities_pt1", "thoughts_feelings_pt1",
  "pt1_q41", "social_support_scale_pt1", "thoughts_feelings_pt1",
  "pt1_q42-43", "duke_religiosity_pt1", "thoughts_feelings_pt1",
  "pt1_q44", "social_constraints_pt1", "thoughts_feelings_pt1",
  "pt1_q45-48", "dyadic_adjustment_pt1", "thoughts_feelings_pt1",
  "pt1_q49", "relationship_quality_pt1", "thoughts_feelings_pt1",
  
  # Section 5: Daily Life (PT)
  "pt1_q50", "perceived_stress_pt1", "daily_life_pt1",
  "pt1_q51", "b_cope_pt1", "daily_life_pt1",
  "pt1_q52", "loneliness_pt1", "daily_life_pt1",
  "pt1_q53-54", "isel_support_pt1", "daily_life_pt1",
  "pt1_q55", "cancer_related_stress_pt1", "daily_life_pt1",
  "pt1_q56", "bas_bis_pt1", "daily_life_pt1",
  "pt1_q57", "low_bicultural_involvement_pt1", "daily_life_pt1",
  "pt1_q58-63", "social_support_network_pt1", "daily_life_pt1",
  "pt1_q64", "familism_pt1", "daily_life_pt1",
  
  # Section 4: Thoughts & Feelings (FM)
  "fm1_q35", "optimism_fm1", "thoughts_feelings_fm1",
  "fm1_q36", "big5_personality_fm1", "thoughts_feelings_fm1",
  "fm1_q37", "urgency_self_control_fm1", "thoughts_feelings_fm1",
  "fm1_q38", "uncertainty_illness_fm1", "thoughts_feelings_fm1",
  "fm1_q39-40", "duke_religiosity_fm1", "thoughts_feelings_fm1",
  "fm1_q41", "social_constraints_fm1", "thoughts_feelings_fm1",
  "fm1_q42", "dimensions_care_task_fm1", "thoughts_feelings_fm1",
  "fm1_q43", "adl_iadl_fm1", "thoughts_feelings_fm1",
  "fm1_q44", "caregiving_duration_fm1", "thoughts_feelings_fm1",
  "fm1_q45", "caregiving_stress_fm1", "thoughts_feelings_fm1",
  "fm1_q46", "caregiving_motivation_fm1", "thoughts_feelings_fm1",
  "fm1_q47-49", "caregiving_experienc_fm1", "thoughts_feelings_fm1",
  "fm1_q50", "attachment_qualities_fm1", "thoughts_feelings_fm1",
  "fm1_q51", "social_support_scale_fm1", "thoughts_feelings_fm1",
  "fm1_q52-55", "dyadic_adjustment_fm1", "thoughts_feelings_fm1",
  "fm1_q56", "relationship_quality_fm1", "thoughts_feelings_fm1",
  
  # Section 5: Daily Life (FM)
  "fm1_q57", "perceived_stress_fm1", "daily_life_fm1",
  "fm1_q58", "b_cope_fm1", "daily_life_fm1",
  "fm1_q59", "loneliness_fm1", "daily_life_fm1",
  "fm1_q60-61", "isel_support_fm1", "daily_life_fm1",
  "fm1_q62", "cancer_related_stress_fm1", "daily_life_fm1",
  "fm1_q63", "bas_bis_fm1", "daily_life_fm1",
  "fm1_q64", "low_bicultural_involvement_fm1", "daily_life_fm1",
  "fm1_q65-70", "social_support_network_fm1", "daily_life_fm1",
  "fm1_q71", "familism_fm1", "daily_life_fm1"
  )

pca_imp_long <- pca_imp_long %>%
  left_join(group_mapping, by = "group") %>% 
  mutate(
    group_name = case_when(
      str_detect(variable, "lang") & str_detect(group, "pt1") ~ "survey_lang_pt1",
      str_detect(variable, "lang") & str_detect(group, "fm1") ~ "survey_lang_fm1",
      str_detect(variable, "female") & str_detect(group, "pt1") ~ "gender_pt1",
      str_detect(variable, "female") & str_detect(group, "fm1") ~ "gender_fm1",
      TRUE ~ group_name
    ),
    section = case_when(
      str_detect(variable, "lang") & str_detect(group, "pt1") ~ "demographic_pt1",
      str_detect(variable, "lang") & str_detect(group, "fm1") ~ "demographic_fm1",
      str_detect(variable, "female") & str_detect(group, "pt1") ~ "demographic_pt1",
      str_detect(variable, "female") & str_detect(group, "fm1") ~ "demographic_fm1",
      TRUE ~ section
    )
  )

# Step 2: Get the top 5 PCA components per feat/db/model based on absolute SHAP value
top_pcs <- filtered_varimp_res %>%
  group_by(feat, db, model) %>%
  slice_max(order_by = abs(shap_val), n = 4) %>%
  ungroup()


pca_imp_long_group = pca_imp_long %>% 
  group_by(section, group_name, component) %>%
  summarize (max_loading = max(loading)) %>%
  mutate(component_num = as.numeric(component)) %>% 
  select(-component)
View(pca_imp_long_group)

top_pcs_groups <- top_pcs %>%
  left_join(pca_imp_long_group, by = "component_num", relationship="many-to-many") %>%
  group_by(feat, db, model, var, component_num) %>%
  slice_max(order_by = max_loading, n = 5) %>%  # Keep top 5 contributing variable groups
  filter(max_loading >= 0.04) %>%
  ungroup() %>%
  mutate(y_label = paste0(group_name, "_PC", component_num, "_", feat, "_", db, "_", model))  # Unique y-axis labels


# Filter for the selected feat/db combo
selected_data <- top_pcs_groups %>%
  # filter(feat == "se36_mean_pt1", db == "sleep") %>% 
  filter(feat == "SRI_pt1", db == "actrhythm") %>% 
  mutate(
    component_num = paste("Comp", component_num)
  )

ggplot(selected_data, aes(x = model, y = group_name, fill = shap_val)) +
  geom_tile(aes(alpha = max_loading), color = "black") +  # Black border for each tile
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # Red/Blue for SHAP values
  scale_alpha(range = c(0, 5)) +  # Intensity based on average loading
  facet_grid(rows = vars(str_extract(group_name, "(fm1|pt1)$")),  # Facet by pt/fm vertically
             cols = vars(component_num),  # Facet by PCA component horizontally
             scales = "free", space = "free") +
  theme_light() +  # Lighter background for better readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
    strip.background = element_rect(color = "black", fill = "gray90", size = 1),  # Borders for facet labels
    strip.text = element_text(face = "bold", size = 10, color = "black"),  # Set facet label text color to black
    panel.border = element_rect(color = "black", size = 1),  # Border around each facet
    panel.spacing = unit(0.5, "lines")  # Space between facets
  ) +
  labs(
    title = paste("Top PCA Components & Variable group_names", unique(selected_data$feat), unique(selected_data$db)),
    x = "Model",
    y = "Variable group_name",
    fill = "SHAP Value",
    alpha = "Max Loading"
  )


filtered_varimp_res <- varimp_res %>%
  select(-n_components) %>% 
  mutate(combined = paste(out, model, preproc, sep = "_")) %>% 
  filter(combined %in% important_models$combined) %>% 
  select(-combined) %>% 
  separate(out, sep = "_(?=[^_]+$)", into = c("feat", "db")) %>%
  filter(str_detect(var, "PC")) %>%
  mutate(component_num = as.numeric(str_extract(var, "\\d+"))) 

# Step 2: Get the top 5 PCA components per feat/db/model based on absolute SHAP value
top_pcs <- filtered_varimp_res %>%
  group_by(feat, db, model) %>%
  slice_max(order_by = abs(shap_val), n = 5) %>%
  ungroup()


pca_imp_long_section = pca_imp_long %>% 
  group_by(section, component) %>%
  summarize(avg_loading = mean(loading)) %>%
  mutate(component_num = as.numeric(component)) %>% 
  select(-component)
# View(pca_imp_long_section)

top_pcs_sections <- top_pcs %>%
  left_join(pca_imp_long_section, by = "component_num", relationship="many-to-many") %>%
  group_by(feat, db, model, var, component_num) %>%
  ungroup() %>%
  mutate(y_label = paste0(section, "_PC", component_num, "_", feat, "_", db, "_", model))  # Unique y-axis labels

# Filter for the selected feat/db combo
selected_data_sections <- top_pcs_sections %>%
  mutate(
    component_num = paste("Comp", component_num)
  )

View(selected_data_sections)


ggplot(selected_data_sections, aes(x = model, y = section, fill = avg_loading)) +
  geom_tile(aes(alpha = avg_loading), color = "black") +  # Black border for each tile
  # scale_fill_gradient2(low = "white", high = "red", midpoint = 0.0001) +  # Red/Blue for SHAP values
  # scale_alpha(range = c(0, 0.5)) +  # Intensity based on average loading
  scale_fill_gradient2(low = "white", high = "red", midpoint = (min(selected_data_sections$avg_loading) + max(selected_data_sections$avg_loading)) / 3, 
                       limits = c(min(selected_data_sections$avg_loading), max(selected_data_sections$avg_loading))) +  # Adjust fill scale
  scale_alpha_continuous(range = c(0, 10)) +
  facet_grid(rows = vars(str_extract(section, "(fm1|pt1)$")),  # Facet by pt/fm vertically
             cols = vars(feat), 
             scales = "free", space = "free") +
  theme_light() +  # Lighter background for better readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
    strip.background = element_rect(color = "black", fill = "gray90", size = 1),  # Borders for facet labels
    strip.text = element_text(face = "bold", size = 10, color = "black"),  # Set facet label text color to black
    panel.border = element_rect(color = "black", size = 1),  # Border around each facet
    panel.spacing = unit(0.5, "lines")  # Space between facets
  ) +
  labs(
    title = "Top PCA Components & Variable group_names",
    x = "Model",
    y = "Variable group_name",
    fill = "SHAP Value",
    alpha = "Max Loading"
  )
# ggplot(selected_data, aes(x = model, y = group, fill = shap_val)) +
#   geom_tile(aes(alpha = average_loading), color = "white") +
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # Red/Blue for SHAP values
#   scale_alpha(range = c(0.3, 1)) +  # Intensity based on average loading
#   facet_grid(rows = vars(str_extract(group, "^(fm1|pt1)")),  # Facet by pt/fm vertically
#              cols = vars(component_num),  # Facet by PCA component horizontally
#              scales = "free", space = "free") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Top PCA Components & Variable Groups (IV_fm1, actrhythm)",
#        x = "Model",
#        y = "Variable Group",
#        fill = "SHAP Value",
#        alpha = "Avg Loading")


library(ggplot2)
library(dplyr)
library(tidyr)

# Manually creating the dataset from your table
data <- tribble(
  ~outcome, ~model, ~preproc, ~preproc_added, ~R2,
  
  "FM Interdaily Stability", "SVR", "None", "None", 0.497,
  "FM Interdaily Stability", "SVR", "P+C+A", "P+C+A", 27.2,
  
  "FM Interdaily Stability", "RIDGE", "None", "None", 0.436,
  "FM Interdaily Stability", "RIDGE", "P+C", "P+C", 25.0,
  "FM Interdaily Stability", "RIDGE", "P+C+A", "A", 31.4,
  
  "FM Interdaily Stability", "LR", "None", "None", 1.01,
  "FM Interdaily Stability", "LR", "P+C", "P+C", 25.2,
  "FM Interdaily Stability", "LR", "P+C+A", "A", 31.5,
  
  "PT Sleep Efficiency", "SVR", "None", "None", 6.05,
  "PT Sleep Efficiency", "SVR", "P+A", "P+A", 12.9,
  
  "PT Sleep Efficiency", "RIDGE", "None", "None", 6.44,
  "PT Sleep Efficiency", "RIDGE", "P", "P", 13.4,
  "PT Sleep Efficiency", "RIDGE", "P+A", "A", 17.4,
  
  "PT Sleep Efficiency", "LR", "None", "None", 0.696,
  "PT Sleep Efficiency", "LR", "P", "P", 14.2,
  "PT Sleep Efficiency", "LR", "P+A", "A", 18.0,
  
  "PT Sleep Onset Latency", "SVR", "None", "None", 1.09,
  "PT Sleep Onset Latency", "SVR", "C", "C", 9.86,
  
  "PT Time In Bed", "RF", "None", "None", 2.58,
  "PT Time In Bed", "RF", "A", "A", 13.0
)


# Convert preproc and preproc_added to factors to control the order
data <- data %>%
  mutate(
    preproc = factor(preproc, levels = c("None", "P", "C", "A", "P+C", "P+A", "P+C+A")),
    preproc_added = factor(preproc_added, levels = c("None", "P", "C", "A", "P+C", "P+A", "P+C+A")),
    model = factor(model, levels = c("SVR", "RIDGE", "LR", "RF")),
    
  ) 
# 
# "#440154FF" "#46337EFF" "#365C8DFF" "#277F8EFF" "#1FA187FF" "#4AC16DFF" "#9FDA3AFF" "#FDE725FF"
# 
# "#440154FF" "#3B528BFF" "#21908CFF" "#5DC863FF" "#FDE725FF"
 

data <- data %>%
  arrange(outcome, model, preproc) %>%
  group_by(outcome, model) %>%
  mutate(R2_incremental = R2 - lag(R2, default = 0)) %>%
  mutate(preproc_added = fct_rev(preproc_added)) %>%
  mutate(preproc = fct_rev(preproc))

custom_colors <- c(
  "None" = "gray",
  "P" = "#9FDA3AFF",
  "C" = "#FDE725FF",
  "A" = "#277F8EFF",
  "P+C" = "#1FA187FF",
  "P+A" = "#46337EFF",
  "P+C+A" = "#03051AFF"
)

# Plot
ggplot(data, aes(x = model, y = R2_incremental, fill = preproc_added, group = preproc)) +
  geom_bar(stat = "identity", position = "stack") +
  # facet_wrap(~outcome, scales = "free_y") +
  facet_grid(cols = vars(outcome), 
             scales = "free", space = "free") +
  geom_text(aes(label = paste0(R2, "%")), 
            color = ifelse(data$preproc_added %in% c("None", "P", "C"), "black", "white"),
            position = position_stack(vjust = 0.5), 
            size = 3) +
  # scale_fill_viridis_d(option="F", direction = 1) +
  scale_fill_manual(values = custom_colors) +
  theme_light() +  # Lighter background for better readability
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
    strip.background = element_rect(color = "black", fill = "gray90", size = 1),  # Borders for facet labels
    strip.text = element_text(face = "bold", size = 10, color = "black"),  # Set facet label text color to black
    panel.border = element_rect(color = "black", size = 1),  # Border around each facet
    panel.spacing = unit(0.5, "lines"),  # Space between facets
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    x = "Model",
    y = "R-squared (%)",
    fill = "preprocessing step added"
  ) +
  theme(legend.position = "bottom")

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


# df <- my_read_csv("python/loocv_bstrap_results020325_fullwrf.csv") |>
df <- my_read_csv("python/loocv_bstrap_results021825.csv") |>
  filter(.metric == "rsq") %>% 
  filter(model != "LASSO")
  
df <- df |>
  mutate(adjusted_p_val = p.adjust(p_val, method = "BH"),
         significant.88 = adjusted_p_val <= 0.12,
         significant_ci.88 = lower2 > 0.01,
         significant.90 = adjusted_p_val <= 0.1,
         significant_ci.90 = lower2 > 0.01,
         significant.95 = adjusted_p_val <= 0.05,
         significant_ci.95 = lower > 0.01)

# Type I error rate to account for multiple testing
# # 4*8*20
# df |>
#   filter(.metric == "rsq") 
# df |>
#   filter(.metric == "rsq", significant.90 & significant_ci.90) 
# df |>
#   filter(.metric == "rsq", significant.95 & significant_ci.95) 

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
    TRUE ~ NA))


check_groups = check_groups %>% 
  mutate(
    model = factor(model, levels = c("RF", "LR", "RIDGE", "SVR"))
  ) %>%
  mutate(feat = case_when(
    feat == "IS_pt1" ~ "Interdaily Stability (PT)", 
    feat == "IV_pt1" ~ "Intradaily Variability (PT)",
    feat == "SRI_pt1" ~ "Sleep Regularity Index (PT)", 
    feat == "avgSD_hr_pt" ~ "Sleep Duration (PT)",
    feat == "avgSE_pt" ~ "Sleep Efficiency (PT)",
    feat == "avgSOL_min_pt" ~ "Sleep Onset Latency (PT)",
    feat == "avgTIB_hr_pt" ~ "Avg. Time In Bed (PT)",
    feat == "avgWASO_min_pt" ~ "Wake Onset Latency (PT)",
    feat == "se36_mean_pt1" ~ "Sleep Efficiency (PT)",
    feat == "tb110_mean_pt1" ~ "Time In Bed (PT)",
    
    feat == "IS_fm1" ~ "Interdaily Stability (FM)", 
    feat == "IV_fm1" ~ "Intradaily Variability (FM)",
    feat == "SRI_fm1" ~ "Sleep Regularity Index (FM)", 
    feat == "avgSD_hr_fm" ~ "Sleep Duration (FM)",
    feat == "avgSE_fm" ~ "Sleep Efficiency (FM)",
    feat == "avgSOL_min_fm" ~ "Sleep Onset Latency (FM)",
    feat == "avgTIB_hr_fm" ~ "Avg. Time In Bed (FM)",
    feat == "avgWASO_min_fm" ~ "Wake Onset Latency (FM)",
    feat == "se36_mean_fm1" ~ "Sleep Efficiency (FM)",
    feat == "tb110_mean_fm1" ~ "Time In Bed (FM)",
    TRUE ~ "Other"              
  )) %>%
  mutate(db = case_when(
    db == "actcomp" ~ "Actigraph Composition", 
    db == "actrhythm" ~ "Actigraph Rhythm",
    db == "sleep" ~ "Sleep Log",
    TRUE ~ "Other"              
  )) %>%
  mutate(mean = mean * 100)
  
#check_groups
#arrange(feat,model)
#filter(model == "RIDGE", feat == "IV_pt1") |> arrange(desc(mean))
#filter(model == "RIDGE", feat == "se36_mean_pt1") |> arrange(desc(mean))

# df |>
#   filter(.metric == "rsq") |>
#   group_by(model, feat, db) |>
#   slice_max(mean) |>
#   ungroup() |>

check_groups |>
  group_by(feat, db) |> 
  filter(any(!is.na(preproc))) |>  # Keep groups where at least one model is significant
  ungroup() |>
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
    vjust = 0) +
  facet_wrap(~db, scales = "free_x") +
  scale_x_discrete(guide =
                     guide_axis_nested(angle=90)) +
  scale_fill_viridis_d(option="D", direction = -1) +
  theme_Publication() +
  theme(
    axis.text.x = element_text(
      size = 5
    ),
    ggh4x.axis.nesttext.x = element_text(
      angle = 45, hjust = 1, size = 8
    )
  ) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,35)) +
  theme(
    strip.background = element_rect(color = "black", fill = "gray90", size = 1),  # Borders for facet labels
    strip.text = element_text(face = "bold", size = 10, color = "black"),  # Set facet label text color to black
    panel.border = element_rect(color = "gray", size = 1),  # Border around each facet
    panel.spacing = unit(0.5, "lines")  # Space between facets
  ) +
  labs(
    x = "Sleep Health Outcome",
    y = "R-squared (%)",
    fill = "preprocessing combination"
  ) 



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
df
#group_by(new_model_name) |>
# mutate(rank = (rank - min(rank)) / (max(rank) - min(rank)))
#mutate(rank_tiled = as_factor(ntile(rank, 3))) |>
#ungroup()

library(biscale)
library(cowplot)
# df <- df |>
#   bi_class(x = shap_cat, y = rev_rank, dim = 2) |>
#   mutate(bi_class = case_when(bi_class == "NA-NA" ~ NA, 
#                               bi_class == "1-NA" ~ NA, 
#                               bi_class == "2-NA" ~ NA, 
#                               TRUE ~ bi_class))

df |> 
  distinct(new_model_name, model_out)

my_pal <- list(`(-1,0]` = "#46337EFF", `(0,1]` = "#5DC863FF")

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
  #unite("phase_ptfm", c(phase, ptfm), sep = "_", remove = F) |>
  mutate(db = case_when(
    db == "clo" ~ "CLO", 
    db == "cardio" ~ "HRV (ECG)",
    db == "coreg" ~ "Coregulation",
    db == "coag" ~ "Coagitation",
    TRUE ~ "Other"),
    db = factor(db, levels = c("Coregulation", "Coagitation", "HRV (ECG)", "CLO"))
  ) %>% 
  mutate(ptfm = case_when(
    ptfm == "pt1" ~ "Patient", 
    ptfm == "fm1" ~ "Caregiver"),
    ptfm = factor(ptfm, levels = c("Patient", "Caregiver"))
  ) %>% 
  mutate(model_out = case_when(
     model_out == "LR_IS_fm1" ~ "FM Interdaily Stability (LR)", 
     model_out == "RIDGE_IS_fm1" ~ "FM Interdaily Stability (Ridge)", 
     model_out == "SVR_IS_fm1" ~ "FM Interdaily Stability (SVR)", 
     model_out == "LR_se36_mean_pt1" ~ "PT Sleep Efficiency (LR)",
     model_out == "RIDGE_se36_mean_pt1" ~ "PT Sleep Efficiency (Ridge)",
     model_out == "SVR_se36_mean_pt1" ~ "PT Sleep Efficiency (SVR)",
     model_out == "RF_tb110_mean_pt1" ~ "PT Time In Bed (RF)",
     model_out == "RF_SRI_pt1" ~ "PT Sleep Regularity Index (RF)")
  ) %>% 
  mutate(phase = toupper(phase)) %>% 
  mutate(phase = factor(phase, 
                          levels = c("B", "S",
                                     "P", "SP1",
                                     "SP2", "R")))

ggplot(exp_by_phase) +
  geom_tile(aes(x = phase, y = model_out, fill = shap_cat), 
            color = "black", size=0.2, show.legend = T) +
  scale_fill_manual(values = my_pal, na.value = "white") + 
  # bi_scale_fill(pal = "DkBlue2", dim = 2, rotate_pal = T, flip_axes = F,
  #               na.value = "white") +
  # scale_fill_brewer(palette = "Reds", direction = -1) +
  # scale_fill_gradient2(low="blue", mid = 'white', high="red") +
  facet_grid(ptfm~db, drop=TRUE, scales = "free_x", space = "free_x") + 
  theme_Publication() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(
    x = "Phase",
    y = "Sleep Outcome by Model",
    fill = "SHAP value directional impact"
  ) 

library(scales)

# Create a viridis color palette (e.g., 5 colors)
color_palette <- viridis_pal(option = "D")(8)

# Print the color codes
print(color_palette)

"#440154FF" "#46337EFF" "#365C8DFF" "#277F8EFF" "#1FA187FF" "#4AC16DFF" "#9FDA3AFF" "#FDE725FF"

"#440154FF" "#3B528BFF" "#21908CFF" "#5DC863FF" "#FDE725FF"


"#03051AFF" "#3F1B44FF" "#841E5AFF" "#CB1B4FFF" "#F06043FF" "#F6AA82FF" "#FAEBDDFF"

"#03051AFF" "#36193EFF" "#701F57FF" "#AE1759FF" "#E13342FF" "#F37651FF" "#F6B48EFF" "#FAEBDDFF"