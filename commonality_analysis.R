# library(tidyverse)
# library(tidymodels)
# library(bestNormalize)
# library(dplyr)
# library(readr)
# library(progress)
# library(yhat)
# 
# # df <- read_csv("integrated/BiPs_PTFM_T1_integrated_sleep_binned_012325.csv") %>% 
# df <- read_csv("python/after_pca.csv") #%>% 
#   # mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) #%>% 
#   #select(matches("coreg.*co")|se36_mean_pt1_sleep)
#   # mutate(across(matches("se36_mean_pt1_sleep"), ~ predict(bestNormalize(.x))))
# 
# # # Step 2: Log transform variables ending in 'cardio' or 'clo'
# # df <- df %>%
# #   mutate(across(matches("cardio$|clo$"), ~ log1p(.)))
# # 
# # # Step 3: Normalize all numeric columns
# # numeric_cols <- df %>% select(where(is.numeric)) %>% names()
# # df[numeric_cols] <- scale(df[numeric_cols])
# # 
# # # Step 4: Remove zero variance predictors
# # df <- df[, sapply(df, function(col) var(col, na.rm = TRUE) != 0)]
# 
# # Step 5: Randomly select 10 predictors (from all columns except outcome)
# outcome_var <- "se36_mean_pt1_sleep"
# predictor_vars <- setdiff(names(df), outcome_var)
# predictor_vars <- predictor_vars[!grepl("PC", predictor_vars)]
# predictor_vars <- sample(predictor_vars, 12)
# 
# # Step 6: Fit linear model
# formula <- as.formula(paste(outcome_var, "~", paste(predictor_vars, collapse = " + ")))
# lm_model <- lm(formula, data = df)
# 
# # Step 5: Run commonality analysis
# result <- regr(lm_model)
# 
# # Step 6: View results
# print(result)
# 
# commonality_df <- as.data.frame(result$Commonality_Data$CCTotalbyVar) %>%
#   rownames_to_column("Variable") %>%
#   mutate(Structure_Coefficients = result$Structure_Coefficients[1,]) %>%
#   arrange(abs(Common))
# 
# commonality_df
# 
# df_long <- commonality_df %>%
#   pivot_longer(cols = c("Unique", "Common", "Total"),
#                names_to = "Component",
#                values_to = "Value") %>%
#   mutate(Sign = ifelse(Structure_Coefficients >= 0, "Positive", "Negative"))
# 
# library(forcats)
# 
# # First, reorder Variable based on Structure Coefficients
# df_long <- df_long %>%
#   mutate(Variable = fct_reorder(Variable, Structure_Coefficients))
# 
# # Then create a new label with structure coefficient for display
# df_long <- df_long %>%
#   mutate(VarLabel = paste0(Variable, "\n(", round(Structure_Coefficients, 2), ")"))
# 
# # Use the reordered Variable for the x-axis and display the label text manually
# ggplot(df_long, aes(x = Variable, y = Value, color = Component, shape = Sign)) +
#   geom_point(size = 10) +
#   scale_shape_manual(values = c("Positive" = 16, "Negative" = 17)) +
#   scale_x_discrete(labels = setNames(df_long$VarLabel, df_long$Variable)) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(size=15, angle = 45, hjust = 1),
#     axis.text.y = element_text(size=15)) +
#   labs(
#     title = "Commonality Components by Variable",
#     y = "Value",
#     x = "Variable (Structure Coefficient)",
#     shape = "Structure Coefficient Sign",
#     color = "Component"
#   )
# 
# df <- read_csv("integrated/BiPs_PTFM_T1_integrated_sleep_binned_012325.csv") |>
#   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) |>
#   #matches(".*_q.*")|
#   # select(matches(".*_q.*")|matches("coag.*co")|matches("coreg.*co")|matches("*cardio")|se36_mean_pt1_sleep) |>
#   select(matches("coag.*co")|matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   # select(matches("coag.*co")|matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   #select(matches("coag.*co")|matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   #select(matches("coag.*co")|se36_mean_pt1_sleep) |>
#   # select(matches("coag.*co")|matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   # mutate(across(matches("*cardio"), ~ scale(.x)))
#   # select(matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   # select(matches("coag.*co")|se36_mean_pt1_sleep) |>
#   # mutate(across(matches("coag.*co")|matches("coreg.*co"), ~ log1p(.x))) |>
#   mutate(across(matches("cardio"), ~ log1p(.x))) |>
#   mutate(across(matches("se36_mean_pt1_sleep"), ~ predict(bestNormalize(.x)))) |>
#   mutate(across(matches("coag.*co")|matches("coreg.*co")|matches("*cardio")|se36_mean_pt1_sleep, scale))
# 
# # qdata <- df |>
# #   select(matches(".*_q.*")) |>
# #   select(where(~ sd(.x, na.rm = TRUE) > 0)) |>
# #   prcomp(scale = TRUE, rank. = 21)
# #
# # df <- qdata |>
# #   augment(df |> select(-matches(".*_q.*"))) |>
# #   select(-.rownames)
# # df
# 
# colnames <- df |> colnames()
# dv_name <- "se36_mean_pt1_sleep"
# # iv_names <- colnames[-25]#colnames[! colnames %in% "se36_mean_pt1_sleep"]
# iv_names <- colnames[! colnames %in% "se36_mean_pt1_sleep"]
# iv_names
# 
# 
# library(yhat)
# n_samples <- 10
# set.seed(123)  # for reproducibility
# library(pbapply)
# 
# 
# run_all_subsets_lr <- function() {
#   # Randomly select a subset size (e.g., between 1 and 25)
#   # subset_size <- sample(3:9, 1)
#   use_df <- df#|>
#   # slice_sample(n = nrow(df), replace=TRUE)
#   subset_predictors <- sample(iv_names,6)
#   # Fit the model with this subset and calculate commonality contribution
#   sub_formula <- as.formula(paste(dv_name, "~",
#                                   paste(subset_predictors, collapse = " + ")))
#   model <- lm(sub_formula, data = use_df)
#   my_regr <- regr(model)
# 
#   p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
# 
#   significance_tibble <- tibble(
#     variable = names(p_values),
#     p_value = p_values,
#     is_significant = p_values < 0.05
#   ) |>
#     filter(variable != "(Intercept)")
# 
#   out <- list()
#   # out[['Structure_Coefficients']] <- my_regr$Structure_Coefficients |>
#   #   as_tibble()
# 
#   # out[['CCTotalbyVar']] <- my_regr$Commonality_Data$CC |>
#   #   as.data.frame() |>
#   #   rownames_to_column() |>
#   #   as_tibble()
#   #   #filter(`    % Total` < 0)
#   out[['CCTotalbyVar']] <- my_regr$Commonality_Data$CCTotalbyVar |>
#     as.data.frame() |>
#     rownames_to_column() |>
#     as_tibble() |>
#     mutate(ratio = abs(Common)/(abs(Common) + abs(Unique)))
# 
#   beta_weights_tibble <- my_regr$Beta_Weights |>
#     as.data.frame() |>
#     rownames_to_column(var = "variable") |>
#     rename(beta_weight = `my_regr$Beta_Weights`) |>
#     as_tibble()
# 
#   out[['Structure_Coefficients']] <- as_tibble(
#     as.data.frame(my_regr$Structure_Coefficients), rownames = "row_id") |>
#     pivot_longer(cols = -row_id, names_to = "variable", values_to="value") |>
#     left_join(beta_weights_tibble, by = "variable") |>
#     left_join(significance_tibble, by = "variable")
# 
#   out[['CC']] <- my_regr$Commonality_Data$CC |>
#     as.data.frame() |>
#     rownames_to_column() |>
#     as_tibble() |>
#     filter(!str_detect(rowname,"Total")) |>
#     mutate(variables = str_remove(rowname, "^Unique to ")) |>
#     mutate(variables = str_remove(variables, "^Common to ")) |>
#     mutate(variables = str_squish(variables))|>
#     mutate(variables = str_remove(variables, "and "))|>
#     # mutate(variables = str_sort(variables)) |>
#     select(-rowname)
# 
#   # print(out[['CC']]  |> filter(Coefficient < 0, !str_detect(rowname,"Total")) |> summarize(sum(`    % Total`)))
#   # print(out[['CC']]  |> filter(Coefficient >= 0, !str_detect(rowname,"Total")) |> summarize(sum(`    % Total`)))
#   # print(out[['CC']]  |> summarize(sum(`    % Total`)))
#   # filter(str_detect(rowname, "^Common to"), Coefficient < 0) |>
#   # mutate(variables = str_remove(rowname, "^Common to ")) |>
#   # mutate(variables = str_squish(variables))|>
#   # mutate(variables = str_remove(variables, "and "))|>
#   # mutate(variables = str_sort(variables)) |>
#   # select(-rowname) |>
#   # print(n=30)
# 
#   # mutate(variables = str_remove(variables,",\\s*(?:and\\s*)?|\\s+and\\s+"))|>
#   #mutate(variables =
#   #  str_sort(str_split_1(variables, ",\\s*(?:and\\s*)?|\\s+and\\s+")))
#   out
# }
# 
# 
# 
# res<-pbreplicate(n=1000, run_all_subsets_lr(), simplify = F)
# 
# 
# #----
# cc_tibble <- map(res, pluck, "CC") |>
#   bind_rows(.id = "id")
# 
# cc_tibble |>
#   group_by(variables) |>
#   summarize(tot=mean(`    % Total`)) |>
#   arrange((tot))
# 
# df_edges <- cc_tibble |>
#   # filter(`    % Total` < 0, str_count(variables, ",") == 1) |>
#   filter(`    % Total` < 0) |>
#   # separate_rows(variables, sep = ",\\s*") #|>
# 
#   # mutate(
#   #   variables = variables |>
#   #     str_split(",\\s*") |>
#   #     map(~ sort(.x)) |>
#   #     map_chr(~ paste(.x, collapse = ", "))
#   # ) |>
# 
#   mutate(var_list = str_split(variables, ", ")) |>
#   mutate(variables = map(var_list, function(vars) {
#     if(length(vars) < 2) return(list())
#     combn(vars, 2, simplify = FALSE) |>
#       map_chr(~ paste(.x, collapse = ", "))
#   })) |>
#   unnest(variables) |>
#   select(-var_list) |>
#   # group_by(variables) |>
#   # mutate(var_id = cur_group_id()) |>
#   # ungroup() |>
#   group_by(variables) |>
#   summarize(mean_tot = mean(`    % Total`)) |>
#   ungroup() |>
#   separate(variables, into = c("from", "to"), sep = ",\\s*") |>
#   mutate(weight = abs(mean_tot)) |>
#   group_by(from) |>
#   # slice_max(weight, n = 3) |>
#   # filter(weight > median(weight)) |>
#   ungroup()
# 
# 
# df_edges
# library(igraph)
# graph <- graph_from_data_frame(df_edges, directed = FALSE)
# graph
# 
# library(ggraph)
# library(tidygraph)
# tidy_g <- as_tbl_graph(graph) |>
#   activate(edges) |>
#   mutate(abs_weight = abs(weight)) |>
#   activate(nodes)  |>
#   mutate(degree = centrality_degree(),
#          weighted_degree = centrality_degree(weights = abs_weight)) %>%
#   mutate(community = as.factor(cluster_walktrap(., steps=3)$membership))
# 
# tidy_g
# 
# ggraph(tidy_g, layout = 'fr') +
#   geom_edge_link(aes(width = weight, color = weight, alpha=weight)) +
#   # geom_node_point(aes(size = weighted_degree, color = community), color = "lightgrey") +
#   geom_node_point(aes(size = weighted_degree, color = community)) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 3) +
#   scale_edge_width(range = c(0.5, 3)) +
#   scale_edge_colour_gradient(low = "lightblue", high = "darkblue") +
#   theme_graph()
library(tidyverse)
library(tidymodels)
library(bestNormalize)

theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_blank(), 
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
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

# df <- read_csv("integrated/BiPs_PTFM_T1_integrated_sleep_binned_012325.csv") |>
#   mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) |>
#   #matches(".*_q.*")|
#   # select(matches(".*_q.*")|matches("coag.*co")|matches("coreg.*co")|matches("*cardio")|se36_mean_pt1_sleep) |>
#   select(matches("coag.*co")|matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   # select(matches("coag.*co")|matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   #select(matches("coag.*co")|matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   #select(matches("coag.*co")|se36_mean_pt1_sleep) |>
#   # select(matches("coag.*co")|matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   # mutate(across(matches("*cardio"), ~ scale(.x)))
#   # select(matches("coreg.*co")|se36_mean_pt1_sleep) |>
#   # select(matches("coag.*co")|se36_mean_pt1_sleep) |>
#   # mutate(across(matches("coag.*co")|matches("coreg.*co"), ~ log1p(.x))) |>
#   mutate(across(matches("cardio"), ~ log1p(.x))) |>
#   mutate(across(matches("se36_mean_pt1_sleep"), ~ predict(bestNormalize(.x)))) |>
#   mutate(across(matches("coag.*co")|matches("coreg.*co")|matches("*cardio")|se36_mean_pt1_sleep, scale))

df <- read_csv("python/after_pca.csv") %>%
  select(matches("HFP")|matches("coag.*co")|se36_mean_pt1_sleep)


# include_vars = read_csv("python/loocv_var_imp051225.csv") %>% distinct(var) %>% 
#   filter(var %in% c("coreg_sp2_pt1_co") | grepl("^PC", var)) %>% 
#   pull(var)

# include_vars = c("HFP_b_pt1_cardio", "HFP_p_pt1_cardio", "HFP_r_fm1_mean_cardio", "HFP_r_pt1_mean_cardio", "HFP_sp1_pt1_cardio", "HFP_sp2_pt1_cardio",
#                  "gamma_sp1_pt1_clo", "coreg_sp2_pt1_co", "IS_fm1_actrhythm")

# df <- read_csv("python/after_pca.csv") %>%
#   select(all_of(c(include_vars, "IS_fm1_actrhythm")))


# qdata <- df |>
#   select(matches(".*_q.*")) |>
#   select(where(~ sd(.x, na.rm = TRUE) > 0)) |>
#   prcomp(scale = TRUE, rank. = 21)

# df <- qdata |>
#   augment(df |> select(-matches(".*_q.*"))) |>
#   select(-.rownames)
# df

colnames <- df |> colnames()
dv_name <- "se36_mean_pt1_sleep"
# iv_names <- colnames[-25]#colnames[! colnames %in% "se36_mean_pt1_sleep"]
iv_names <- colnames[! colnames %in% "se36_mean_pt1_sleep"]
iv_names


library(yhat)
n_samples <- 10 
set.seed(123)  # for reproducibility
library(pbapply)

n_permutations <- 1000

run_all_subsets_lr <- function(data, iv_names, dv_name) {
  subset_predictors <- sample(iv_names, 6)  
  sub_formula <- as.formula(paste(dv_name, "~", paste(subset_predictors, collapse = " + ")))
  model <- lm(sub_formula, data = data)
  my_regr <- regr(model)
  
  cc <- my_regr$Commonality_Data$CC |>
    as.data.frame() |>
    rownames_to_column() |>
    as_tibble() |>
    filter(!str_detect(rowname,"Total")) |>
    mutate(variables = str_remove(rowname, "^Unique to |^Common to ")) |>
    mutate(variables = str_remove(variables, "and "))|>
    mutate(variables = str_squish(variables))|>
    select(-rowname) |>
    filter(`    % Total` < 0) |>
    mutate(var_list = str_split(variables, ",\\s*")) |> 
    rowwise() |> 
    mutate(pair = list(
      if(length(var_list) >= 2)
        combn(sort(var_list), 2, simplify=FALSE) |> map_chr(~paste(.x, collapse=", "))
      else NA
    )) |> 
    unnest(pair) |>
    filter(!is.na(pair)) |>
    select(pair, suppression = `    % Total`)
  
  return(cc)
}

# Observed suppression results
obs_res <- pbreplicate(1000, run_all_subsets_lr(df, iv_names, dv_name), simplify = FALSE) |>
  bind_rows() |>
  group_by(pair) |>
  summarize(mean_suppression_obs = mean(suppression, na.rm=TRUE))

obs_res

permute_and_run <- function(data, iv_names, dv_name) {
  permuted_names <- sample(iv_names)
  permuted_data <- data
  colnames(permuted_data)[match(iv_names, colnames(data))] <- permuted_names
  run_all_subsets_lr(permuted_data, iv_names, dv_name)
}

# Generate null distribution
perm_res <- pbreplicate(n_permutations, permute_and_run(df, iv_names, dv_name), 
                        simplify = FALSE) |>
  bind_rows(.id = "perm_id") |>
  group_by(pair, perm_id) |>
  summarize(mean_suppression_perm = mean(suppression, na.rm=TRUE)) |>
  ungroup()

p_values <- obs_res |>
  left_join(perm_res, by = "pair") |>
  group_by(pair, mean_suppression_obs) |>
  summarize(p_value = mean(mean_suppression_perm <= mean_suppression_obs, na.rm=TRUE)) |>
  ungroup()

significant_edges <- p_values |>
  mutate(sig = p_value < 0.2) |>
  # obs_res |> 
  separate(pair, into = c("from", "to"), sep = ",\\s*") |>
  mutate(weight = abs(mean_suppression_obs))
significant_edges


library(igraph)
graph <- graph_from_data_frame(significant_edges, directed = FALSE)
graph

library(ggraph)
library(tidygraph)
tidy_g <- as_tbl_graph(graph) |>
  activate(edges) |>
  mutate(abs_weight = abs(weight)) |>
  activate(nodes)  |>
  mutate(degree = centrality_degree(), 
         weighted_degree = centrality_degree(weights = abs_weight)) %>%
  mutate(community = as.factor(cluster_walktrap(., steps=3)$membership))

tidy_g


ggraph(tidy_g, layout = 'fr') +
  geom_edge_link(aes(width = weight, color = sig, alpha = weight)) +
  geom_node_point(aes(color = community), size = 4) +
  geom_node_text(aes(label = name), 
                 repel = TRUE,
                 box.padding = 0.6,
                 point.padding = 0.6,
                 size = 4.7, 
                 color = "black", 
                 stroke = 0.3,  # thin stroke for better separation
                 bg.color = "white", 
                 bg.r = 0.15) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_edge_color_manual(
    values = c("FALSE" = "lightgray", "TRUE" = "darkblue"),
    guide = guide_legend(override.aes = list(edge_width = 2))  # Thicker lines in legend
  ) +
  theme_Publication() + 
  theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.spacing.x = unit(1, "cm"),
    legend.margin = margin(t = 2, b = 2),
    legend.key.width = unit(0.5, "cm"),  # Increase the width of the legend keys
    legend.text = element_text(size = 18),       # Size of legend labels
    legend.title = element_text(size = 18)  # Optional: size and bold titles
  )


# ggraph(tidy_g, layout = 'fr') +
#   geom_edge_link(aes(width = weight, color = sig, alpha=weight)) +
#   # geom_node_point(aes(size = weighted_degree, color = community), color = "lightgrey") +
#   # geom_node_point(aes(size = weighted_degree, color = community)) +
#   geom_node_point(aes(color = community), size=4) +
#   geom_node_text(aes(label = name), repel = TRUE, size = 4) +
#   scale_edge_width(range = c(0.5, 3)) +
#   # scale_edge_color_gradient(low = "lightblue", high = "darkblue") +
#   scale_edge_color_manual(values = c("FALSE" = "lightgray", "TRUE" = "darkblue"))+#_gradient(low = "lightblue", high = "darkblue") +
#   theme_Publication() + 
#   theme(
#     legend.position = "top",  # keep if you're using a top legend
#     legend.box = "horizontal",  # keeps guides side by side
#     legend.spacing.x = unit(1.2, "cm"),  # horizontal space between guides
#     legend.margin = margin(t = 5, b = 5)  # padding around the whole legend
#   )