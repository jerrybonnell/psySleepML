## Script to perform commonality analysis and generate feature suppression cluster plot

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

df <- read_csv("python/after_pca.csv") %>%
  select(matches("coreg.*co")|matches("coag.*co")|se36_mean_pt1_sleep)


colnames <- df |> colnames()
dv_name <- "se36_mean_pt1_sleep"
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
