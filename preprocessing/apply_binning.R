### Script to apply adaptive binning to questionnaire predictors

library(tidyverse)
library(dplyr)
library(ggcorrplot)
library(tidymodels)

with_sleep <- read_csv("integrated/BiPs_PTFM_T1_integrated_sleep_012325.csv")

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
