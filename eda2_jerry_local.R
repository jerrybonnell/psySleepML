library(tidyverse)
# library(haven)
# library(labelled)
# library(surveytoolbox)
library(dplyr)
library(ggcorrplot)
# library(ggstatsplot)
# library(correlation)
# install.packages("fastDummies")
# library(fastDummies)
library(tidymodels)
# library(recipes)
# library(embed)
# library(ggplot2)

# with_sleep <- read_csv("integrated/BiPs_PTFM_T1_integrated_sleep_082124.csv")
# with_sleep <- read_csv("integrated/BiPs_PTFM_T1_integrated_sleep_101424.csv")
with_sleep <- read_csv("integrated/BiPs_PTFM_T1_integrated_sleep_012325.csv")
# TODO toggle between these two source() lines
source("my_step_custom.R")
source("step_correlated.R")
attach(recipes_extended)
attach(recipes_extended2)

### NOTE run this script in isolation first so that the function specifications 
### are collected into the R environment, before use in eda2_jerry.R

##############################################################################
# Creating factors given from syntax: test_factor
test_factor = with_sleep |>
  mutate(
    `pt_female_pt1_q` = factor(`pt_female_pt1_q`, levels = 0:2, labels = c("Male", "Female", "Other"), exclude = NULL),
    `lang_pt1_q` = factor(`lang_pt1_q`, levels = 1:2, labels = c("English", "Spanish"), exclude = NULL),
    `RaceEth_pt1_q1` = factor(`RaceEth_pt1_q1`, levels = 1:12, labels = c("non-Hispanic African American", "non-Hispanic American Indian", "non-Hispanic Asian", "non-Hispanic Pacific Islander", "non-Hispanic Caucasian", "Hispanic", "non-Hispanic Middle Eastern", "Other", "Multi-Ethnicity", "White Hispanic", "Black Hispanic", "Asian Hispanic"), exclude = NULL),
    `p_hisp_pt1_q1` = factor(`p_hisp_pt1_q1`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `RaceGrp_pt1_q1` = factor(`RaceGrp_pt1_q1`, levels = 1:4, labels = c("White", "Black", "Asian", "Other"), exclude = NULL),
    `edulow_pt_q3` = factor(`edulow_pt_q3`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `minor_pt1_q4` = factor(`minor_pt1_q4`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `inclow_pt_q5` = factor(`inclow_pt_q5`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `employed_pt_q6` = factor(`employed_pt_q6`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `smoked_pt1_q8` = factor(`smoked_pt1_q8`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `exced_pt1_q8` = factor(`exced_pt1_q8`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `atedrnk_pt1_q9-12` = factor(`atedrnk_pt1_q9-12`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `cof_pt1_q9-12` = factor(`cof_pt1_q9-12`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `tea_pt1_q9-12` = factor(`tea_pt1_q9-12`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `caff_pt1_q9-12` = factor(`caff_pt1_q9-12`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `drgmed_pt1_q13-14` = factor(`drgmed_pt1_q13-14`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `alc_pt1_q13-14` = factor(`alc_pt1_q13-14`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `pcrchist_pt1_q16np` = factor(`pcrchist_pt1_q16np`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `fcrchist_pt1_q16np` = factor(`fcrchist_pt1_q16np`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `smoking_pt1_q17-20` = factor(`smoking_pt1_q17-20`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `othtob_pt1_q17-20` = factor(`othtob_pt1_q17-20`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `hvdrink_pt1_q17-20` = factor(`hvdrink_pt1_q17-20`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `hvdrinkpday_pt1_q17-20` = factor(`hvdrinkpday_pt1_q17-20`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `dastzone_pt1_q21` = factor(`dastzone_pt1_q21`, levels = 1:4, labels = c("healthy", "risky", "harmful", "dependent"), exclude = NULL),
    `religaffil_pt1_q42-43` = factor(`religaffil_pt1_q42-43`, levels = 0:1, labels = c("No", "Yes"), exclude = NULL),
    `Ptnr_pt_screen` = factor(`Ptnr_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `Sec1_pos_pt_screen` = factor(`Sec1_pos_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `Sec2_pos_pt_screen` = factor(`Sec2_pos_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `Sec3_pos_pt_screen` = factor(`Sec3_pos_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `CPAP_pt_screen` = factor(`CPAP_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `N5_pt_screen` = factor(`N5_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `N7a_pt_screen` = factor(`N7a_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `N7b_pt_screen` = factor(`N7b_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `psych_pt_screen` = factor(`psych_pt_screen`, levels = 1:3, labels = c("yes no med", "yes with med", "no"), exclude = NULL),
    `subs_pt_screen` = factor(`subs_pt_screen`, levels = 1:3, labels = c("yes no med/no treatment", "yes with med/treatment", "no"), exclude = NULL),
    `demen_pt_screen` = factor(`demen_pt_screen`, levels = 1:3, labels = c("yes no med/no treatment", "yes with med/treatment", "no"), exclude = NULL),
    `harm_pt_screen` = factor(`harm_pt_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    
    `fm_female_fm1_q` = factor(`fm_female_fm1_q`, levels = 1:3, labels = c("Male", "Female", "Other"), exclude = NULL),
    `lang_fm1_q` = factor(`lang_fm1_q`, levels = 1:2, labels = c("English", "Spanish"), exclude = NULL),
    `RaceEth_fm1_q1` = factor(`RaceEth_fm1_q1`, levels = 1:12, labels = c("non-Hispanic African American", "non-Hispanic American Indian", "non-Hispanic Asian", "non-Hispanic Pacific Islander", "non-Hispanic Caucasian", "Hispanic", "non-Hispanic Middle Eastern", "Other", "Multi-Ethnicity", "White Hispanic", "Black Hispanic", "Asian Hispanic"), exclude = NULL),
    `p_hisp_fm1_q1` = factor(`p_hisp_fm1_q1`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `RaceGrp_fm1_q1` = factor(`RaceGrp_fm1_q1`, levels = 1:4, labels = c("White", "Black", "Asian", "Other"), exclude = NULL),
    `edulow_fm_q3` = factor(`edulow_fm_q3`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `minor_fm1_q4` = factor(`minor_fm1_q4`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `inclow_fm_q5` = factor(`inclow_fm_q5`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `employed_fm_q6` = factor(`employed_fm_q6`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `smoked_fm1_q8` = factor(`smoked_fm1_q8`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `exced_fm1_q8` = factor(`exced_fm1_q8`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `atedrnk_fm1_q9-12` = factor(`atedrnk_fm1_q9-12`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `cof_fm1_q9-12` = factor(`cof_fm1_q9-12`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `tea_fm1_q9-12` = factor(`tea_fm1_q9-12`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `caff_fm1_q9-12` = factor(`caff_fm1_q9-12`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `drgmed_fm1_q13-14` = factor(`drgmed_fm1_q13-14`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `alc_fm1_q13-14` = factor(`alc_fm1_q13-14`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `pcrchist_fm1_q16np` = factor(`pcrchist_fm1_q16np`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `fcrchist_fm1_q16np` = factor(`fcrchist_fm1_q16np`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `smoking_fm1_q17-20` = factor(`smoking_fm1_q17-20`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `othtob_fm1_q17-20` = factor(`othtob_fm1_q17-20`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `hvdrink_fm1_q17-20` = factor(`hvdrink_fm1_q17-20`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `hvdrinkpday_fm1_q17-20` = factor(`hvdrinkpday_fm1_q17-20`, levels = 0:1, labels = c("no", "yes"), exclude = NULL),
    `dastzone_fm1_q21` = factor(`dastzone_fm1_q21`, levels = 1:4, labels = c("healthy", "risky", "harmful", "dependent"), exclude = NULL),
    `religaffil_fm1_q39-40` = factor(`religaffil_fm1_q39-40`, levels = 0:1, labels = c("No", "Yes"), exclude = NULL),
    `hpothill_fm1_q47-49` = factor(`hpothill_fm1_q47-49`, levels = 0:1, labels = c("helped PT with other illness", "not helped"), exclude = NULL),
    `Ptnr_fm_screen` = factor(`Ptnr_fm_screen`, levels = 1:4, labels = c("Spouse", "Partner", "Companion", "Other"), exclude = NULL),
    `Sec1_pos_fm_screen` = factor(`Sec1_pos_fm_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `Sec2_pos_fm_screen` = factor(`Sec2_pos_fm_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `Sec3_pos_fm_screen` = factor(`Sec3_pos_fm_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `CPAP_fm_screen` = factor(`CPAP_fm_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `N5_fm_screen` = factor(`N5_fm_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `N7a_fm_screen` = factor(`N7a_fm_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `N7b_fm_screen` = factor(`N7b_fm_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    `psych_fm_screen` = factor(`psych_fm_screen`, levels = 1:3, labels = c("yes no med", "yes with med", "no"), exclude = NULL),
    `subs_fm_screen` = factor(`subs_fm_screen`, levels = 1:3, labels = c("yes no med/no treatment", "yes with med/treatment", "no"), exclude = NULL),
    `demen_fm_screen` = factor(`demen_fm_screen`, levels = 1:3, labels = c("yes no med/no treatment", "yes with med/treatment", "no"), exclude = NULL),
    `harm_fm_screen` = factor(`harm_fm_screen`, levels = 1:2, labels = c("yes", "no"), exclude = NULL),
    # TODO jerry: additional factors to consider?
    `MultiEth_pt1_q1` = factor(`MultiEth_pt1_q1`, levels = 1:2, labels = c("1", "2"), exclude = NULL),
    `MultiEth_fm1_q1` = factor(`MultiEth_fm1_q1`, levels = 1:2, labels = c("1", "2"), exclude = NULL)
  ) |>
  select(starts_with(colnames(with_sleep)))

bin_data <- function(data, thrsh_count) {
  # Idea for this algorithm: 
  # 
  # for each feature, figure out what the bin width should be such that i 
  # get at least X examples in the bin 
  # example frequency table (unique value/occurrence): 
  # 0: 50 
  # 1: 30
  # 2: 15
  # 3: 7
  # 4: 5
  # 5: 6
  # 6: 3
  # 7: 6
  # sort the values from smallest to largest, look at how many examples in 
  # data equal to that value 
  # 
  # start off with how many 0’s are there 
  # at elast 20 examples in each bin 
  # 50 > 20 , so 0 becomes its own bin 
  # 30 > 20, 1 becomes its own bin 
  # 15 !> 20, so let’s go onto 3 
  # 15 + 7 > 20, so let’s make [2,3] a bin 
  # final bins: [0], [1], [2,3], [4,5,6,7]
  #
  #
  #
  val <- tibble(col = data) |> count(col) |> filter(!is.na(col))
  
  bins <- list()
  current_bin <- c()
  current_sum <- 0
  
  # arrggh ... for loops
  for (i in seq_along(val$n)) {
    current_sum <- current_sum + val$n[i]
    current_bin <- c(current_bin, val$col[i])
    
    # print(current_bin)
    if (current_sum >= thrsh_count || i == length(val$n)) {
      bins <- append(bins, list(current_bin))
      current_bin <- c()
      current_sum <- 0
    }
  }
  
  bins <- append(bins, list(NA))
  
  val <- val |>
    mutate(binned = map_chr(col, function(x) {
      bin <- bins[map_lgl(bins, ~ x %in% .x)]
      revised <- round(unlist(bin)[c(1, length(unlist(bin)))], 2)
      paste0("[", paste(revised, collapse = ","), "]")
    }))
  
  extract_bin <- function(bin) {
    as.numeric(str_extract_all(bin, "\\d+")[[1]])
  }
  
  # val$binned <- factor(val$binned, 
  #                     levels = val |>
  #                       pull(binned) |>
  #                       unique() |>
  #                       sort(), 
  #                     ordered = TRUE)
  # NOTE binned map is to integer encode the bins, for easier visualization
  # with the data wrangler; skip this step and use val directly to recover
  # the bin label 
  binned_map <- val |> 
    distinct(binned) |>
    mutate(recoded = row_number() - 1) 
  # print(binned_map)
  
  val <- val |>
    left_join(binned_map, by = "binned")
  
  tibble(col = data) |>
    left_join(val, by="col") |>
    pull(recoded)
  # pull(binned)
  
}

### testing...
myt <- tibble(test = c(rep(0, 50), rep(1, 30), rep(2, 15),
                       rep(3, 7), rep(4, 5), rep(5, 6),
                       rep(6, 3), rep(7, 6), NA))
bin_data(myt$test, 20)
bin_data(test_factor$`hcareut_fm1_q16a-m`, 20)
# bin_data(runif(100), 20)



apply_binning <- function(data, max_bins = 4) {
  # Check if the data is numeric
  if (!is.numeric(data)) {
    stop("Data must be numeric.")
  }
  
  # Calculate the interquartile range (IQR) and sample size
  IQR <- IQR(data, na.rm = TRUE)
  range_data <- max(data, na.rm = TRUE) - min(data, na.rm = TRUE)
  n <- length(data)
  
  # Use Freedman-Diaconis rule to determine the initial number of bins
  bin_width <- if (IQR == 0 || range_data == 0) {
    1  # Avoiding / by zero
  } else {
    2 * IQR / (n^(1/3))
  }
  num_bins <- if (bin_width > 0) {
    ceiling(range_data / bin_width)
  } else {
    1  # Making sure bin width isn't 0 or negative
  }
  
  num_bins <- min(max(num_bins, 1), max_bins)
  
  # Function to get unique breakpoints and adjust number of bins if needed
  get_unique_bins <- function(data, num_bins) {
    
    num_bins <- max(num_bins, 1)
    # Calculate quantile edges
    quantile_edges <- quantile(data, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE)
    # Remove duplicates
    quantile_edges <- unique(quantile_edges)
    # Check if we have enough unique breakpoints
    if (length(quantile_edges) < num_bins + 1) {
      return(NULL)  # Indicate that the breakpoints are not unique
    }
    return(quantile_edges)
  }
  
  # Reduce the number of bins until unique breakpoints are found
  while (TRUE) {
    quantile_edges <- get_unique_bins(data, num_bins)
    if (!is.null(quantile_edges)) {
      break
    }
    num_bins <- max(num_bins - 1, 1)  # Reduce the number of bins, ensuring at least 1 bin
  }
  
  # Create bins using the calculated quantiles
  binned_data <- cut(data, breaks = quantile_edges, include.lowest = TRUE)
  
  return(binned_data)
}


# # Testing distribution from binning algorithm
# hist(test_factor$morb_pt1_q15)
# 
# barplot(table(test_binned$morb_pt1_q15), 
#         main = "Bar Plot of Factor Variable by Count", 
#         xlab = "Factor Levels", 
#         ylab = "Frequency", 
#         col = "skyblue", 
#         border = "black")
# 
# hist(as.numeric(test_binned$morb_pt1_q15))

recode_fct_strings <- function(col_data) {
  #col_name <- quo_name(enquo(col))
  tib <- tibble(col = col_data)
  
  binned_map <- tib |> 
    distinct(col) |>
    mutate(recoded = row_number() - 1) |>
    # keep missing as missing
    mutate(recoded = ifelse(is.na(col), NA, recoded))
  
  tib |>
    left_join(binned_map, by = "col") |>
    mutate(recoded = as.factor(recoded))|>
    pull(recoded)
}

################################################################################
# recode factors to be integer-valued for better data wrangler visualization 
test_factor2 <- test_factor |>
  mutate(across(
    .cols = which(sapply(names(test_factor), function(col) 
      is.factor(test_factor[[col]]) && grepl("_q|screen|_sleep|_actcomp|_actrhythm", col))),
    .fns = ~recode_fct_strings(.x) 
  ))

# Applies binning algorithm to test_factor (bins numeric variables from specified datasets)
# TODO screener is not included in this, but why not? 
test_binned = test_factor2 |>
  mutate(across(
    .cols = which(sapply(names(test_factor2), function(col) 
      # is.numeric(test_factor[[col]]) && grepl("_q", col))),
      is.numeric(test_factor[[col]]) && grepl("_q|screen", col))),
    # TODO JB: toggle next lines between the binning algorithm approaches
    .fns = ~bin_data(.x, thrsh_count=30)
    # .fns = apply_binning
  ))

## TODO the binning should use a different thrsh_count for sleep columns
## to produce a lower number of bins 
# test_binned = test_binned |>
#   mutate(across(
#     .cols = which(sapply(names(test_binned), function(col) 
#       is.numeric(test_factor[[col]]) && grepl("_sleep|_actcomp|_actrhythm", col))),
#     # TODO JB: toggle between the binning algorithm approaches
#     .fns = ~bin_data(.x, thrsh_count=40)
#     # .fns = apply_binning
#   ))

## all questionnaire and screener items should be factor
test_binned<- test_binned |>
  mutate(across(matches(".*_q.*"), as.factor)) |>
  mutate(across(matches(".*_screen"), as.factor)) 

test_binned |> write_csv("integrated/BiPs_PTFM_T1_integrated_sleep_binned_012325.csv")
#### STOP HERE

only_q <- test_binned |>
  select(matches(".*_q.*")|contains(c("_sleep", "_actcomp", "_actrhythm")))
only_screen <- test_binned |>
  select(matches(".*_screen")|contains(c("_sleep", "_actcomp", "_actrhythm")))
only_ecg <- test_binned |>
  select(matches(".*_cardio")|contains(c("_sleep", "_actcomp", "_actrhythm")))
only_co <- test_binned |>
  select(matches(".*_co$")|contains(c("_sleep", "_actcomp", "_actrhythm")))
################################################################################
library(rlang)

include_list <- c(".*_q.*", ".*_screen", ".*_cardio", ".*_co$")
pred_names <- test_binned |>
  select(matches(include_list)) |>
  colnames()
out_names <- test_binned |>
  select(contains(c("_sleep", "_actcomp", "_actrhythm"))|BiPs_DID) |>
  colnames()


folds <- bootstraps(test_binned, times = 10)

my_recipe <- function(outcome) {
  other_outs <- out_names[out_names != outcome]
  # form <- new_formula(ensym(outcome), expr(.))
  form <- new_formula(ensym(outcome), expr(.))
  # NOTE do not worry that the recipe is defined using the full data in
  # test_binned; see the following two links: 
  # https://recipes.tidymodels.org/articles/recipes.html#an-initial-recipe
  # The data contained in the data argument need not be the training set; 
  # this data is only used to catalog the names of the 
  # variables and their types (e.g. numeric, etc.).
  # https://rsample.tidymodels.org/articles/Applications/Recipes_and_rsample.html
  # While the original data object ames is used in the call, it is only used 
  # to define the variables and their characteristics so a single recipe is 
  # valid across all resampled versions of the data. The recipe can be 
  # estimated on the analysis component of the resample.
  recipe(form, data = test_binned) |>
    step_rm(any_of(other_outs)) |> 
    step_impute_mean(all_outcomes()) |>
    step_impute_mean(all_numeric_predictors()) |>
    step_unknown(all_factor_predictors()) |>
    step_log(ends_with(c("cardio")), offset = 1) |>
    step_log(all_outcomes(), offset = 1) |>
    step_normalize(all_numeric_predictors()) |>
    step_dummy(all_factor_predictors()) |>
    step_zv(all_predictors()) |>
    step_custom(all_predictors(),
                outcome = outcome, mtry = 3, ntree = 1000,
                minbucket = 5, minsplit = 10)
  #step_nzv(all_predictors(), freq_cut = 95/5)
}

my_recipe2 <- function(outcome) {
  other_outs <- out_names[out_names != outcome]
  form <- new_formula(ensym(outcome), expr(.))
  
  recipe(form, data = test_binned) |>
    step_rm(any_of(other_outs)) |> 
    step_impute_mean(all_outcomes()) |>
    step_impute_mean(all_numeric_predictors()) |>
    step_unknown(all_factor_predictors()) |>
    step_log(ends_with("cardio")|ends_with("co"), offset = 1) |>
    step_log(all_outcomes(), offset = 1) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors()) |>
    step_dummy(all_factor_predictors()) |>  # Generate dummy variables
    step_correlated(all_predictors(),  # Use the dummy predictors
                    outcome = outcome, r_val = 0.2)  # Call your custom step here
} # extraversion_TIPI_pt1_q37_X3 

juice(prep(my_recipe("tb110_mean_fm1_sleep")))
# juice(prep(my_recipe2("tb110_mean_fm1_sleep")))
juice(prep(my_recipe2("tb110_mean_fm1_sleep"), folds |> pluck(1,3,1)))

# feat_num_check <- function (index, my_col_to_check) {
#   tib<-juice(prep(my_recipe(my_col_to_check), folds |> pluck(1,index,1))) |>
#     list(x = _) |>
#     with(summarize(x,
#                    screen=sum(str_detect(colnames(x), ".*_screen_.*"))/71,
#                    q=sum(str_detect(colnames(x), ".*_q.*"))/850,
#                    cardio=sum(str_detect(colnames(x), ".*_cardio$"))/12,
#                    co=sum(str_detect(colnames(x), ".*_co$"))/24))
#   list(tib |> mutate(checked = my_col_to_check))
# }
# feat_checked <- map(out_names[out_names != "BiPs_DID"], 
#         \(x) map(1:5, \(y) feat_num_check(y, x)) |> bind_rows(),
#         .progress = T) |>
#   bind_rows() |>
#   summarize(across(c(screen, q, cardio, co), mean))


# TODO note that LASSO was replaced by SVM here. this is because I found that
# the LASSO model zero-d out all predictors, effectively a useless model. I
# suspect that after the variable selection work the LASSO model may perform
# better. we should reintroduce the LASSO model to evaluate this better
lin_mod1 <- linear_reg(penalty = 0.1, mixture = 0) |>
  set_engine("glmnet")
lin_mod2 <- svm_rbf(mode="regression") |>
  set_engine("kernlab")
# TODO mtry and trees needs tuning, via tune(), 
# workflow_map below probably needs to be changed to "tune_gird"
# https://tune.tidymodels.org/reference/tune_grid.html
forest <- rand_forest(mode = "regression", mtry = 20, trees = 500) |>
  set_engine("ranger", importance = "impurity")

# step through and test each of the individual outcomes 
# TODO toggle between these 
my_recipes <- map(out_names[out_names != "BiPs_DID"], my_recipe)
# my_recipes <- map(out_names[out_names != "BiPs_DID"], my_recipe2)

names(my_recipes) <- out_names[out_names != "BiPs_DID"]
# wf_set <- workflow_set(my_recipes, list(ridge=lin_mod1,
#                                         svm=lin_mod2,
#                                         forest=forest))
# wf_set <- workflow_set(my_recipes, list(ridge=lin_mod1, svm=lin_mod2))
wf_set <- workflow_set(my_recipes, list(ridge=lin_mod1))

library(future)
library(doFuture)
library(parallel)

setup_worker <- function() {
  library(recipes)
  library(tidymodels)
  library(dplyr)
  library(correlation)
  # TODO toggle between these; change these paths! 
  source("/Users/jerrybonnell/local/postdoc/bips/Psycho-Oncology-IDSC/my_step_custom.R")
  source("/Users/jerrybonnell/local/postdoc/bips/Psycho-Oncology-IDSC/step_correlated.R")
  attach(recipes_extended)
  attach(recipes_extended2)
}

cl_spec <- rep("localhost", each = 6)
plan(cluster, workers = cl_spec)

future.apply::future_lapply(seq_along(cl_spec), function(i) setup_worker())

registerDoFuture()

options(tidymodels.dark = T)
wf_res <- workflow_map(wf_set, "fit_resamples", resamples = folds, 
                       metrics = metric_set(rmse, rsq),
                       verbose = TRUE, 
                       control = control_resamples(save_workflow = TRUE,
                                                   save_pred = FALSE, 
                                                   parallel_over = "resamples"))
wf_res

rank_results(wf_res, rank_metric="rsq") |>
  filter(.metric %in% c("rsq", "rmse")) |>
  separate(wflow_id, sep = "_(?=[^_]+$)", into = c("out", "mname")) |>
  separate(out, sep = "_(?=[^_]+$)", into = c("feat", "db")) |>
  ggplot() +
  geom_point(aes(x = rank, y = mean, color = mname, shape = db)) + 
  geom_errorbar(aes(x = rank, color = mname,
                    ymin = mean - std_err * qnorm(0.95),
                    ymax = mean + std_err * qnorm(0.95)),
                width=0.3) +
  facet_wrap(~.metric, scales = "free") +
  theme_bw() + 
  labs(color = "model")

