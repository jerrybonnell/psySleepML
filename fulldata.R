# Script to generate integrated sleep csv

library(tidyverse)
library(haven)
library(labelled)
library(surveytoolbox)
library(dplyr)
library(readxl)

##### RUN QUESTIONNAIRE.R FIRST #####
########################################################################

dyad_exclude_list <- c("B085", "B181", "B372", "B691", "B692", "B761", "B781", "B824", "B929", "B814")

### SCREENER
t1_screen <- read_sav("data/T1 Screening Database PTFM_12.5.23_TT_No Identifier.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list))
t1_screen

t1_screen_varl <- t1_screen |> 
  varl_tb()
t1_screen_varl

t1_screen

#### CARDIO DB
# PT
t1_cardio_pt_orig <- read_sav("PT T1 HRV 3.1&3.2 MERGED_4.1.23 TT_YK.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list))
t1_cardio_pt_orig 

t1_cardio_pt_varl <- t1_cardio_pt_orig |> 
  varl_tb()
t1_cardio_pt_varl

# FM
dyad_exclude_list_cardio_fm <- c("B085", "B181", "B372", "B691", "B692", "B761", "B781", "B824", "B940", "B929", "B814")
t1_cardio_fm_orig <- read_sav("FM T1 HRV 3.1&3.2 MERGED_5.21.23 TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_cardio_fm))
t1_cardio_fm_orig 

Reduce(setdiff, list(t1_cardio_pt_orig |> distinct(BiPs_DID), 
                     t1_cardio_fm_orig |> distinct(BiPs_DID)))
Reduce(setdiff, list(t1_cardio_fm_orig |> distinct(BiPs_DID), 
                     t1_cardio_pt_orig |> distinct(BiPs_DID)))

t1_cardio_fm_varl <- t1_cardio_fm_orig |> 
  varl_tb()
t1_cardio_fm_varl

#### MOOD DB
# PT
t1_mood_pt_orig <- read_sav("data/PT_T1 Daily Mood Log_long_09.25.23_NA.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list))
t1_mood_pt_orig 

# t1_mood_pt_varl <- t1_mood_pt_orig[,1:46] |>
#   varl_tb()
# t1_mood_pt_varl

# FM
dyad_exclude_list_mood_fm <- c("B085", "B181", "B372", "B691", "B692", "B725", "B761", "B781", "B824", "B929", "B814")
t1_mood_fm_orig <- read_sav("data/FM_T1 Daily Mood Log_long_09.25.23_NA.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_mood_fm))
t1_mood_fm_orig 

Reduce(setdiff, list(t1_mood_pt_orig |> distinct(BiPs_DID), 
                     t1_mood_fm_orig |> distinct(BiPs_DID)))
Reduce(setdiff, list(t1_mood_fm_orig |> distinct(BiPs_DID), 
                     t1_mood_pt_orig |> distinct(BiPs_DID)))

# t1_mood_fm_varl <- t1_mood_fm_orig |> 
#   varl_tb()
# t1_mood_fm_varl

#### SLEEP DB
# PT
dyad_exclude_list_sleep_pt <- c("B085", "B147", "B181", "B372", "B691", "B692", "B761", "B781", "B824", "B929", "B814")
t1_sleep_pt_orig <- read_sav("data/PT T1 Daily Sleep Log_clean WIDE w WINSORIZED composites w updated SD&SE_08.03.24.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_sleep_pt))
t1_sleep_pt_orig 

t1_sleep_pt_varl <- t1_sleep_pt_orig |> 
  varl_tb()
t1_sleep_pt_varl

# FM
t1_sleep_fm_orig <- read_sav("data/FM T1 Daily Sleep Log_clean_WIDE w WINSORIZED composites w updated SD&SE_08.03.24.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list))
t1_sleep_fm_orig 

Reduce(setdiff, list(t1_sleep_pt_orig |> distinct(BiPs_DID), 
                     t1_sleep_fm_orig |> distinct(BiPs_DID)))
Reduce(setdiff, list(t1_sleep_fm_orig |> distinct(BiPs_DID), 
                     t1_sleep_pt_orig |> distinct(BiPs_DID)))

t1_sleep_fm_varl <- t1_sleep_fm_orig |> 
  varl_tb()
t1_sleep_fm_varl


#### ACTIGRAPH COMPOSITION DB
# PT
dyad_exclude_list_actcomp_pt <- c("B085", "B147", "B181", "B372", "B600", "B691", "B692", "B761", "B781", "B824", "B929", "B814")
t1_actcomp_pt_orig <- read_sav("data/PT T1 actigraph-derived sleep composition indices_wide_WINSORIZED_5.1.24.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_actcomp_pt))
t1_actcomp_pt_orig 

t1_actcomp_pt_varl <- t1_actcomp_pt_orig |> 
  varl_tb()
t1_actcomp_pt_varl

# FM
dyad_exclude_list_actcomp_fm <- c("B085", "B181", "B372", "B392", "B413", "B691", "B692", 
                                  "B712", "B763", "B767", "B815", "B824", "B929", "B814")
t1_actcomp_fm_orig <- read_sav("data/FM actigraph-derived sleep composition indices_wide_WINSORIZED_5.1.24_TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_actcomp_fm))
t1_actcomp_fm_orig 

Reduce(setdiff, list(t1_actcomp_pt_orig |> distinct(BiPs_DID), 
                     t1_actcomp_fm_orig |> distinct(BiPs_DID)))
Reduce(setdiff, list(t1_actcomp_fm_orig |> distinct(BiPs_DID), 
                     t1_actcomp_pt_orig |> distinct(BiPs_DID)))

t1_actcomp_fm_varl <- t1_actcomp_fm_orig |> 
  varl_tb()
t1_actcomp_fm_varl

#### ACTIGRAPH SLEEP RHYTHM DB
# PT
dyad_exclude_list_actrhythm_pt <- c("B085", "B181", "B307", "B372", "B400", "B691", "B692", 
                                    "B761", "B781", "B824", "B909", "B929", "B814")
t1_actrhythm_pt_orig <- read_sav("data/T1 PT Sleep Rhythm_5.3.24_TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_actrhythm_pt)) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_actrhythm")))
t1_actrhythm_pt_orig 

t1_actrhythm_pt_varl <- t1_actrhythm_pt_orig |> 
  varl_tb()
t1_actrhythm_pt_varl

# FM
dyad_exclude_list_actrhythm_fm <- c("B085", "B178", "B181", "B307", "B372", "B392", 
                                    "B413", "B691", "B692", "B725", "B761", "B781", "B815", "B824", "B929", "B814")
t1_actrhythm_fm_orig <- read_sav("data/T1 FM Sleep Rhythm_5.5.24_TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_actrhythm_fm)) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_actrhythm")))
t1_actrhythm_fm_orig 

Reduce(setdiff, list(t1_actrhythm_pt_orig |> distinct(BiPs_DID), 
                     t1_actrhythm_fm_orig |> distinct(BiPs_DID)))
Reduce(setdiff, list(t1_actrhythm_fm_orig |> distinct(BiPs_DID), 
                     t1_actrhythm_pt_orig |> distinct(BiPs_DID)))

t1_actrhythm_fm_varl <- t1_actrhythm_fm_orig |> 
  varl_tb()
t1_actrhythm_fm_varl


#### AFFECT DB
dyad_exclude_list_affect <- c("B068", "B085", "B178", "B181", "B372", "B606", "B691", "B692", 
                              "B761", "B781", "B824", "B909", "B929", "B814")
# PT
t1_affect_pt_orig <- read_sav("data/PT T1 Acute Affect_3.27.24_TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_affect))
t1_affect_pt_orig 

# t1_affect_pt_varl <- t1_affect_pt_orig[,1:4] |>
#   varl_tb()
# t1_affect_pt_varl

# FM
t1_affect_fm_orig <- read_sav("data/FM T1 Acute Affect_3.27.24_TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_affect))
t1_affect_fm_orig 

Reduce(setdiff, list(t1_affect_pt_orig |> distinct(BiPs_DID), 
                     t1_affect_fm_orig |> distinct(BiPs_DID)))
Reduce(setdiff, list(t1_affect_fm_orig |> distinct(BiPs_DID), 
                     t1_affect_pt_orig |> distinct(BiPs_DID)))

# t1_affect_fm_varl <- t1_affect_fm_orig |> 
#   varl_tb()
# t1_affect_fm_varl


#### BLOOD PRESSURE DB
# PT
t1_bp_pt_orig <- read_sav("data/PT T1 BiPs_Blood Pressure_ExpPhase_cleaned_2.20.24_TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list))
t1_bp_pt_orig 

t1_bp_pt_varl <- t1_bp_pt_orig |> 
  varl_tb()
t1_bp_pt_varl

# FM
t1_bp_fm_orig <- read_sav("data/FM T1 BiPs_Blood Pressure_ExpPhase 6.17.23_TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list))
t1_bp_fm_orig 

Reduce(setdiff, list(t1_bp_pt_orig |> distinct(BiPs_DID), 
                     t1_bp_fm_orig |> distinct(BiPs_DID)))
Reduce(setdiff, list(t1_bp_fm_orig |> distinct(BiPs_DID), 
                     t1_bp_pt_orig |> distinct(BiPs_DID)))

t1_bp_fm_varl <- t1_bp_fm_orig |> 
  varl_tb()
t1_bp_fm_varl

#### NEW COREGULATION COAGITATION DB
# PT
t1_coregag_full_orig <- read_excel("data/T1 cardio coregulation_coagitation by phases.xlsx") |>
  filter(DID != "B763")
View(t1_coregag_full_orig)

t1_coregag_pt_orig <- t1_coregag_full_orig |> select(DID, phase, contains("_PT")) |>
  rename(BiPs_DID = DID, coreg = Coregulation_PT, coag = Coagitation_PT) |>
  mutate(phase = case_when(
    phase == "BASELINE" ~ "b",
    phase == "SCENARIO" ~ "s",
    phase == "PREP" ~ "p",
    phase == "SPEECH I" ~ "sp1",
    phase == "SPEECH II" ~ "sp2",
    phase == "RECOVERY" ~ "r"
  )) |>
  pivot_wider(names_from = phase, 
              values_from = c(coreg,coag), 
              names_glue = "{.value}_{phase}_pt1") |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_co")))
View(t1_coregag_pt_orig)


t1_coregag_fm_orig <- t1_coregag_full_orig |> select(DID, phase, contains("_FM")) |>
  rename(BiPs_DID = DID, coreg = Coregulation_FM, coag = Coagitation_FM) |>
  mutate(phase = case_when(
    phase == "BASELINE" ~ "b",
    phase == "SCENARIO" ~ "s",
    phase == "PREP" ~ "p",
    phase == "SPEECH I" ~ "sp1",
    phase == "SPEECH II" ~ "sp2",
    phase == "RECOVERY" ~ "r"
  )) |>
  pivot_wider(names_from = phase, 
              values_from = coreg:coag, 
              names_glue = "{.value}_{phase}_fm1") |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_co")))
# View(t1_coregag_fm_orig)


#### NEW CLO DB
t1_clo_full_orig <- read_csv("data/T1 CLO output_102924_wide.csv")
# View(t1_clo_full_orig)

t1_clo_pt_orig <- t1_clo_full_orig |> select(Dyad_ID, contains("_pt.")) |>
  rename_with(~ .x |> 
                str_replace_all("pt\\.(BL|SC|PR|S1|S2|RE)", 
                                "\\1_pt1_clo") |> 
                str_replace_all(c("BL" = "b", "SC" = "s", "PR" = "p", "S1" = "sp1", "S2" = "sp2", "RE" = "r")) |> 
                str_to_lower() |>
                str_replace("dyad_id", "BiPs_DID"))

t1_clo_pt_orig <- t1_clo_pt_orig %>% select(-contains("zeta"))

View(t1_clo_pt_orig)

t1_clo_fm_orig <- t1_clo_full_orig |> select(Dyad_ID, contains("_fm.")) |>
  rename_with(~ .x |> 
                str_replace_all("fm\\.(BL|SC|PR|S1|S2|RE)", 
                                "\\1_fm1_clo") |> 
                str_replace_all(c("BL" = "b", "SC" = "s", "PR" = "p", "S1" = "sp1", "S2" = "sp2", "RE" = "r")) |> 
                str_to_lower() |>
                str_replace("dyad_id", "BiPs_DID"))

t1_clo_fm_orig <- t1_clo_fm_orig %>% select(-contains("zeta"))

View(t1_clo_fm_orig)
########################################################################
## PREDICTER EXTRACTIONS

## CARDIO - heart rate variability HF-HRV(?), which is labeled HFP_[phase]_[pt1 or fm1] 
# (e.g., HFP_b_pt1 is HF-HRV at baseline for patient)

# PT: 
t1_cardio_HRV_pt = t1_cardio_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("HFP_")) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_cardio")))
# FM
t1_cardio_HRV_fm = t1_cardio_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("HFP_")) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_cardio")))

# -------------------------
## SCREENER PREDICTOR EXTRACTIONS (ALL)
# Marital Status, Sleep Apnea, Narcolepsy, Restless Leg Syndrome, Shift Work, 
# Diagnosis of Psychosis, Substance Dependence, or Dementia, Having Suicidal ideation

# PT
t1_screen_pred_pt = t1_screen |> select(BiPs_IID, BiPs_DID, 
          Ptnr_pt,                              
          names(t1_screen)[grepl("^Sec.*pt$", names(t1_screen))], CPAP_pt, Num_pos_pt, 
          Narcol_sum_pt,
          N5_pt,
          N7a_pt, N7b_pt,
          psych_pt,
          subs_pt,
          demen_pt,
          harm_pt) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_screen")))
# View(t1_screen_pred_pt)
#FM
t1_screen_pred_fm = t1_screen |> select(BiPs_IID, BiPs_DID, 
          Ptnr_fm,
          names(t1_screen)[grepl("^Sec.*fm$", names(t1_screen))], CPAP_fm, Num_pos_fm, 
          Narcol_sum_fm,
          N5_fm,
          N7a_fm, N7b_fm,
          psych_fm,
          subs_fm,
          demen_fm,
          harm_fm) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_screen")))
# View(t1_screen_pred_fm)

########################################################################
## OUTCOME EXTRACTIONS

## MOOD
# # PT
# t1_mood_out_pt = t1_mood_pt_orig |> select(BiPs_IID, BiPs_DID, daynum_pt1, happy_pt1:intimacy_pt1) |>
#   pivot_wider(
#     id_cols = c(BiPs_IID, BiPs_DID),
#     names_from = daynum_pt1,
#     values_from = c(happy_pt1:intimacy_pt1),
#     names_glue = "{.value}_day{daynum_pt1}_pt1"
#   ) |>
#   rename_with(~gsub("_pt1_", "_", .x), contains("_pt1_")) |>
#   filter(BiPs_IID != "B0050_old") |>
#   rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_mood")))
# # FM
# t1_mood_out_fm = t1_mood_fm_orig |> select(BiPs_IID, BiPs_DID, daynum_fm1, happy_fm1:week_fm1, wekd_fm1:intimacy_fm1) |>
#   pivot_wider(
#     id_cols = c(BiPs_IID, BiPs_DID),
#     names_from = daynum_fm1,
#     values_from = c(happy_fm1:intimacy_fm1),
#     names_glue = "{.value}_day{daynum_fm1}_fm1"
#   ) |>
#   rename_with(~gsub("_fm1_", "_", .x), contains("_fm1_")) |>
#   filter(BiPs_IID != "B0051_old") |>
#   rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_mood")))

# PT
t1_mood_out_pt = t1_mood_pt_orig |> select(BiPs_DID, happy_pt1:stressful_pt1, pa_pt1:intimacy_pt1) |>
  group_by(BiPs_DID) |>
  summarize(across(
    everything(), 
    list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
    .names = "{.fn}_{.col}")
  ) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  # rename_with(~ gsub("^mean_", "", .x), starts_with("mean_")) |>
  rename_with(~if_else(.x %in% c("BiPs_DID"), .x, paste0(.x, "_mood")))

# FM
t1_mood_out_fm = t1_mood_fm_orig |> select(BiPs_DID, happy_fm1:stressful_fm1, pa_fm1:intimacy_fm1) |>
  group_by(BiPs_DID) |>
  summarize(across(
    everything(), 
    list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
    .names = "{.fn}_{.col}")
  ) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_DID"), .x, paste0(.x, "_mood")))
    
  
# -------------------------

## SLEEP
# PT
t1_sleep_out_pt = t1_sleep_pt_orig |> select(BiPs_IID, BiPs_DID, tb110_mean_pt1:se36_mean_pt1) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_sleep")))
# FM
t1_sleep_out_fm = t1_sleep_fm_orig |> select(BiPs_IID, BiPs_DID, tb110_mean_fm1:se36_mean_fm1) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_sleep")))
# -------------------------

## ACTIGRAPH

# Composition
# PT
t1_actcomp_out_pt = t1_actcomp_pt_orig |> select(BiPs_IID_pt, BiPs_DID, avgSD_hr_pt, avgSE_pt,
                                             avgSOL_min_pt, avgTIB_hr_pt, avgWASO_min_pt) |>
  rename(BiPs_IID = BiPs_IID_pt) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_actcomp")))
# FM
t1_actcomp_out_fm = t1_actcomp_fm_orig |> select(BiPs_IID_fm, BiPs_DID, avgSD_hr_fm, avgSE_fm,
                                             avgSOL_min_fm, avgTIB_hr_fm, avgWASO_min_fm) |>
  rename(BiPs_IID = BiPs_IID_fm) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_actcomp")))

# Sleep rhythm = t1_actrhythm_pt/fm orig
# -------------------------

## AFFECT
# PT
t1_affect_out_pt = t1_affect_pt_orig |> select(BiPs_IID, BiPs_DID, acute_pa1_pt1:stressful7_pt1) |>
  select(-starts_with("stressful"), everything()) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_affect")))
# FM
t1_affect_out_fm = t1_affect_fm_orig |> select(BiPs_IID, BiPs_DID, acute_pa1_fm1:stressful7_fm1) |>
  select(-starts_with("stressful"), everything()) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_affect")))

## BLOOD PRESSURE
# PT
t1_bp_out_pt = t1_bp_pt_orig |> select(BiPs_IID, BiPs_DID, SBP_1_t1:Pulse_7_t1) |>
  rename_with(~ gsub("t1$", "pt1", .), ends_with("t1")) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_bp")))
# FM
t1_bp_out_fm = t1_bp_fm_orig |> select(BiPs_IID, BiPs_DID, SBP_1_t1:Pulse_7_t1) |>
  rename_with(~ gsub("t1$", "fm1", .), ends_with("t1")) |> 
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_bp")))


##############################################################################
# Full dataset (predictors and outcomes)

library(purrr)

dfs_list = list(
  # PT PREDICTORS
  t1_q_age_pt, t1_q_gender_pt, t1_q_lang_pt, t1_q_ethnicity_pt, t1_q_rlpdur_pt, 
  t1_q_edu_pt, t1_q_minor_pt, t1_q_inc_pt, t1_q_employ_pt, t1_q_liveUS_pt, t1_q_smokeexc_pt, 
  t1_q_intake_pt, t1_q_drgmedalc_pt, t1_q_MICCI_pt, t1_q_hcareut_pt, t1_q_crchist_pt, 
  t1_q_drink_pt, t1_q_DAST_pt, t1_q_LOTR_pt, t1_q_TIPI_pt, t1_q_impulse_pt, t1_q_uncertainty_pt, 
  t1_q_maq_pt, t1_q_ssss_pt, t1_q_DUREL_pt, t1_q_SCC_pt, t1_q_DAS4_pt, t1_q_RQS_pt, t1_q_PSS_pt, 
  t1_q_bcope_pt, t1_q_loneliness_pt, t1_q_ISEL_pt, t1_q_castress_pt, t1_q_BAS_pt, t1_q_biculturalism_pt, 
  t1_q_SSN_pt, t1_q_familism_pt, t1_screen_pred_pt, t1_cardio_HRV_pt,
  t1_coregag_pt_orig, t1_clo_pt_orig,
  # FM PREDICTORS
  t1_q_age_fm, t1_q_gender_fm, t1_q_lang_fm, t1_q_ethnicity_fm, t1_q_rlpdur_fm,
  t1_q_edu_fm, t1_q_minor_fm, t1_q_inc_fm, t1_q_employ_fm, t1_q_liveUS_fm, t1_q_smokeexc_fm,
  t1_q_intake_fm, t1_q_drgmedalc_fm, t1_q_MICCI_fm, t1_q_hcareut_fm, t1_q_crchist_fm,
  t1_q_drink_fm, t1_q_DAST_fm, t1_q_LOTR_fm, t1_q_TIPI_fm, t1_q_impulse_fm, t1_q_uncertainty_fm,
  t1_q_maq_fm, t1_q_ssss_fm, t1_q_DUREL_fm, t1_q_SCC_fm, t1_q_DAS4_fm, t1_q_RQS_fm,
  t1_q_dimensions_fm, t1_q_ADL_fm, t1_q_caredur_fm, t1_q_carestress_fm, t1_q_caremot_fm,
  t1_q_cra_fm, t1_q_PSS_fm, t1_q_bcope_fm, t1_q_loneliness_fm, t1_q_ISEL_fm, t1_q_castress_fm,
  t1_q_BAS_fm, t1_q_biculturalism_fm, t1_q_SSN_fm, t1_q_familism_fm,
  t1_screen_pred_fm, t1_cardio_HRV_fm,
  t1_coregag_fm_orig, t1_clo_fm_orig,
  # PT OUTCOMES
  t1_q_promis_pt, t1_q_sympt_pt, t1_q_lsat_pt, t1_q_PSQI_pt, t1_q_distr_pt, t1_q_cesd_pt,
  t1_q_factc_pt, t1_q_factsp_pt, t1_q_ISI_pt, t1_q_ESS_pt, t1_q_BF_pt, t1_q_POMS_pt,
  t1_q_MASQ_pt, t1_q_IES_pt, t1_mood_out_pt, t1_sleep_out_pt, t1_actcomp_out_pt,
  t1_actrhythm_pt_orig, t1_affect_out_pt, t1_bp_out_pt, 
  # FM OUTCOMES
  t1_q_promis_fm, t1_q_sympt_fm, t1_q_lsat_fm, t1_q_PSQI_fm, t1_q_distr_fm, t1_q_cesd_fm,
  t1_q_factsp_fm, t1_q_ISI_fm, t1_q_ESS_fm, t1_q_BF_fm, t1_q_POMS_fm, t1_q_MASQ_fm,
  t1_q_IES_fm, t1_mood_out_fm, t1_sleep_out_fm, t1_actcomp_out_fm, t1_actrhythm_fm_orig, 
  t1_affect_out_fm, t1_bp_out_fm
  )

# rename_columns <- function(df, df_name) {
#   suffix <- str_extract(df_name, "_[a-z]+_[a-z]+")
#   if (!is.na(suffix)) {
#     colnames(df)[colnames(df) != "BiPs_DID"] <- paste0(colnames(df)[colnames(df) != "BiPs_DID"], suffix)
#   }
#   return(df)
# }
# 
# for(i in 1:123) {
#   df = dfs_list[[i]]
#   df_name = dfs_nameslist[[i]]
#   rename_columns(df, df_name)
# }

df_full = reduce(dfs_list, full_join, by = "BiPs_DID") |> select(-starts_with("BiPs_IID"))
# View(df_full)
# names(df_full)
# View(data.frame(names(df_full)))

###########################################################################
# labelling

variable_labels = c(
  age_ = "Age",
  MultiEth_ = "q1. Multiple Racial Background",
  RaceEth_ = "Ethnicity-12 groups",
  RaceGrp_ = "Race-4 groups",
  edulow_pt = "Education LT high school",
  minor_ = "have minor at home",
  inclow_pt = "income <$40K",
  employed_pt = "employed ",
  liveUSmn_ = "Total time spent in the US in Months",
  liveUSyr_ = "Total time spent in the US in Years",
  smoked_ = "had cigarettes/tobacco last 2 hours",
  exced_ = "exercised last 2 hours",
  atedrnk_ = "ate/drank besides water last 4 hours",
  cof_ = "had coffee last 4 hours",
  tea_ = "had tea last 4 hours",
  caff_ = "had caffeine last 4 hours",
  drgmed_ = "had drugs/medications last 24 hours",
  alc_ = "had alcohol last 24 hours",
  morb_ = "total # of morbidity",
  psychmorb_pt1 = "total # of psychiatric morbidity",
  psychmorb_fm1 = "total # of psychiatric morbidity",
  physmorb_pt1 = "total # of physical morbidity",
  physmorb_fm1 = "total # of physical morbidity",
  med_ = "total # of medications taken",
  hypertension_ = "dx of hypertension",
  cholesterol_ = "dx of cholesterol",
  heart_ = "dx of heart disease",
  kidney_ = "dx of kidney disease",
  gallbaldder_ = "dx of gallbladder disease",
  liver_ = "dx of liver disease",
  mentalhealth_ = "dx of mental health disorder",
  diabetes_ = "dx of diabetes",
  pulmonary_ = "dx of pulmonary disease",
  thyroid_ = "dx of thyroid disease",
  GI_ = "dx of GI disease",
  hcareut_ = "count of 13 healthcare utilization",
  hcareut_ov_ = "count of 7 healthcare over-utilization",
  hcareut_ud_ = "count of 6 healthcare under-utilization",
  hcareut_sum_ = "total frequency of 13 kinds of healthcare utilization",
  hcareut_ovsum_ = "total frequency of 7 kinds of healthcare over-utilization",
  hcareut_udsum_ = "total frequency of 6 kinds of healthcare under-utilization",
  pcrchist_ = "personal history of CRC prior to current one",
  fcrchist_ = "family history of CRC",
  smoking_ = "currently smoke cigarettes",
  othtob_ = "currently use other tobacco products",
  hvdrink_ = "used alcohol 6+ times/week",
  drinkpday_ = "number of alcohol drinks per day",
  hvdrinkpday_ = "drink >=2 drinks per day",
  totdrug_ = "total number of drugs used in past year",
  dast_ = "total DAST score",
  dastzone_ = "zone of drug use",
  pro_phy_ = "PROMIS physical function",
  Tpro_phy_ = "Physical Function T-score",
  Tpro_anx_ = "Anxiety T-score",
  pro_dep_ = "PROMIS depression",
  Tpro_dep_ = "Depression T-score",
  pro_fat_ = "PROMIS fatigue",
  Tpro_fat_ = "Fatigue T-score",
  pro_slp_ = "PROMIS sleep disturbance",
  pro_sor_ = "PROMIS satisfaction with social role",
  pro_paininfr_ = "PROMIS pain interference",
  pro_painit_ = "PROMIS pain intensity",
  totprosm_ = "PROMIS overall sum score",
  totpromn_ = "PROMIS T-overall health_mean",
  sympt_ = "Total physical symptoms",
  lsat_ = "life satisfaction",
  psqi_sbqual_ = "PSQI component 1: Subjective sleep quality score",
  fallslphr_ = "PSQI: How long to fall asleep, Hours",
  psol_ = "PSQI sleep onset latency minutes",
  psqi_latncgr_ = "PSQI component 2: Sleep latency score",
  slphr_ = "PSQI: Actual sleep, Hours",
  slpmn_ = "PSQI: Actual sleep, Minutes",
  psqi_sdmn_ = "PSQI sleep duration, minutes",
  psqi_sdhr_ = "PSQI-sleep duration, hours",
  goodsdhr_ = "Good Sleep Duration (7-9 hours) cut-off",
  psqi_sdgr_ = "PSQI component 3: Sleep duration score",
  psqi_tb_hr_ = "PSQI-total hours spent in bed",
  psqi_se_ = "PSQI habitual sleep efficiency",
  goodse_ = "Good SE 85% cut-off",
  psqi_segr_ = "PSQI component 4: Habitual sleep efficiency score",
  psqi_distbgr_ = "PSQI component 5: Sleep disturbances",
  sleepmed_ = "PSQI compoent 6: Use of sleep medication",
  psqi_dysfgr_ = "PSQI component 7: Daytime dysfunction",
  psqi_gl_ = "PSQI global score",
  distr_therm_ = "distress thermometer",
  CESD_ = "Depressive Symptoms CESD",
  FACT_PWB_pt1 = "physical well-being - PT1 ONLY",
  FACT_SFWB_pt1 = "social/family well-being - PT1 ONLY",
  FACT_EWB_pt1 = "emotional well-being - PT1 ONLY",
  FACT_FWB_pt1 = "functional well-being - PT1 ONLY",
  FACT_CCS_pt1 = "additional cancer concerns - PT1 ONLY",
  FACT_G_pt1 = "Functional Assessment of Cancer Therapy - General - PT1 ONLY",
  FACT_C_pt1 = "Functional Assessment of Cancer Therapy - composite score - PT1 ONLY",
  isi_ = "total insomnia severity ",
  ess_ = "total daytime sleepiness",
  bf_ac_ = "benefit finding in acceptance",
  bf_em_ = "benefit finding in empathy",
  bf_ap_ = "benefit finding in appreciation",
  bf_fa_ = "benefit finding in family",
  bf_sv_ = "benefit finding in positive self-view",
  bf_re_ = "benefit finding in reprioritization",
  bf_ = "total benefit finding",
  POMS_TA_ = "Tension-Anxiety",
  POMS_DP_ = "Depression-Dejection",
  POMS_AH_ = "Anger-Hostility",
  POMS_VA_ = "Vigor-Activity",
  POMS_FI_ = "Fatigue-Inertia",
  POMS_CB_ = "Confusion-Bewilderment",
  POMS_TMD_ = "Total Mood Disturbance",
  masq_anx_ = "MASQ anxiety symptoms",
  urgency_ = "Urgency",
  pos_urgency_ = "Positive Urgency",
  control_ = "Self-control",
  Uncertain_ = "Uncertainty",
  maq_sec_ = "MAQ- Security",
  maq_avd_ = "MAQ- Avoidance",
  maq_ambw_ = "MAQ- Ambivalence-Worry",
  maq_ambm_ = "MAQ- Ambivalence-Merger",
  religaffil_ = "Religious Affiliation",
  constraint_ = "Social constraints",
  das_ = "Total dyadic adjustment",
  rqi_ = "relationship quality",
  bcope_SD_ = "self-distraction",
  bcope_AC_ = "active coping",
  bcope_DN_ = "denial",
  bcope_SU_ = "substance use",
  bcope_ES_ = "emotional support",
  bcope_IS_ = "instrumental support",
  bcope_BD_ = "behavioral disengagement",
  bcope_VT_ = "venting",
  bcope_PR_ = "positive reframing",
  bcope_PL_ = "planning",
  bcope_HR_ = "humor",
  bcope_ACC_ = "acceptance",
  bcope_RL_ = "religious coping",
  bcope_SB_ = "self-blame",
  cope_ee_ = "emotional expression",
  cope_ep_ = "emotional processing",
  ISEL_pt1 = "Perceived social support",
  ISEL_fm1 = "Perceived social support",
  ISELST_ = "Satisfaction with perceived social support",
  castress_SF_ = "cancer-related stress to oneself",
  castress_FA_ = "cancer-related stress to family",
  castress_tot_ = "Total Cancer-Related Stress",
  drive_ = "BAS drive",
  funseek_ = "BAS fun seeking",
  reward_ = "BAS reward responsiveness",
  inhibition_ = "BAS behavioral inhibition",
  languse_ = "Language use - ethnic loyalty",
  media_ = "Media",
  relations_ = "Ethnic social relations",
  accultur_ = "Acculturation",
  SSNTQ_ = "social support network-quality",
  famob_ = "Familism- Family Obligation",
  famsupport_ = "Familism- Support from the Family",
  famref_ = "Familism- Family as Referents",
  CTask_1 = "Dimension of Care Task Count - Overall - FM1 ONLY",
  CTaskEMO_1 = "Dimension of Care Task Count - Emotional - FM1 ONLY",
  CTaskINS_1 = "Dimension of Care Task Count - Instrumental - FM1 ONLY",
  CTaskTAN_1 = "Dimension of Care Task Count  - Tangible - FM1 ONLY",
  CTaskMED_1 = "Dimension of Care Task Count - Medical - FM1 ONLY",
  CTaskfreq_EMO_1 = "Dimension of Care Task Frequency - Emotional - FM1 ONLY",
  CTaskfreq_INS_1 = "Dimension of Care Task Frequency - Instrumental - FM1 ONLY",
  CTaskfreq_TAN_1 = "Dimension of Care Task Frequency - Tangible - FM1 ONLY",
  CTaskfreq_MED_1 = "Dimension of Care Task Frequency - Medical - FM1 ONLY",
  CTaskdiff_EMO_1 = "Dimension of Care Task Difficulty - Emotional - FM1 ONLY",
  CTaskdiff_INS_1 = "Dimension of Care Task Difficulty - Instrumental - FM1 ONLY",
  CTaskdiff_TAN_1 = "Dimension of Care Task Difficulty - Tangible - FM1 ONLY",
  CTaskdff_MED_1 = "Dimension of Care Task Difficulty - Medical - FM1 ONLY",
  ADL_1 = "ADL/IADL - Count of activities fm needed help with - FM1 ONLY",
  ADL_fm1 = "ADL/IADL - Count of activities fm helped with - FM1 ONLY",
  ADL_upset1 = "ADL/IALDL - Providing help was upsetting - FM1 ONLY",
  care_dur_fm1 = "caregiving duration per day in hours - FM1 ONLY",
  c_overload1 = "Pearlin Stress Scale - Overload - FM1 ONLY",
  mot_auto1 = "Autonomous caregiving motives - FM1 ONLY",
  mot_ex1 = "External caregiving motives - FM1 ONLY",
  mot_ij1 = "Introjected caregiving motives - FM1 ONLY",
  mot_cont1 = "Controlled caregiving motives - FM1 ONLY",
  cra_EST_1 = "Caregiver Reaction Assessment - Esteem - FM1 ONLY",
  cra_LFS_1 = "Caregiver Reaction Assessment - Lack of Family Support - FM1 ONLY",
  cra_IOS_1 = "Caregiver Reaction Assessment - Impact on Schedule - FM1 ONLY",
  cra_IOF_1 = "Caregiver Reaction Assessment - Impact on Finances - FM1 ONLY",
  hpothill_fm1 = "helping the PT with other illness/disability - FM1 ONLY",
  help_others_fm1 = " helping any other family or friends who are ill - FM1 ONLY"
)

value_labels = list(
  RaceEth_ = c("non-Hispanic African American" = 1, "non-Hispanic American Indian" = 2, 
               "non-Hispanic Asian" = 3, "non-Hispanic Pacific Islander" = 4, 
               "non-Hispanic Caucasian" = 5, "Hispanic" = 6, "non-Hispanic Middle Eastern" = 7, 
               "Other" = 8, "Multi-Ethnicity" = 9, "White Hispanic" = 10, "Black Hispanic" = 11, 
               "Asian Hispanic" = 12),
  RaceGrp_ = c("White" = 1, "Black" = 2, "Asian" = 3, "Other" = 4),
  dastzone_ = c("healthy" = 1, "risky" = 2, "harmful" = 3, "dependent" = 4),
  lsat_ = c("extremely satisfied" = 31:35, "satisfied" = 26:30, "slightly satisfied" = 21:25,
            "neutral" = 20, "slightly dissatisfied" = 15:19, "dissatisfied" = 10:14,
            "extremely dissatisfied" = 5:9),
  psqi_sbqual_ = c("Very good" = 0, "Fairly good" = 1, "Fairly bad" = 2, "Very bad" = 3),
  psolgr_ = c("<=15 minutes" = 0, "16-30 minutes" = 1, "31-60 minutes" = 2, ">60 minutes" = 3),
  psqi_latncgr_ = c("score 0" = 0, "score 1-2" = 1, "score 3-4" = 2, "score 5-6" = 3),
  goodsdhr_ = c("Short: <7 hours" = 0, "Ideal: 7-9 hours" = 1, "Long: >9 hours" = 2),
  psqi_sdgr_ = c(">7 hours" = 0, "6-7 hours" = 1, "5-6 hours" = 2, "<5 hours" = 3),
  goodse_ = c("poor SE <85%" = 0, "good SE >=85%" = 1),
  psqi_segr_ = c(">=85%" = 0, "75-84%" = 1, "65-74%" = 2, "<65%" = 3),
  psqi_distbgr_ = c("score 0" = 0, "score 1-9" = 1, "score 10-18" = 2, "score 19-27" = 3),
  sleepmed_ = c("Not during the past month" = 0, "Less than once a week" = 1, 
                "Once or twice a week" = 2, "Three or more times a week" = 3),
  psqi_dysfgr_ = c("score 0" = 0, "score 1-2" = 1, "score 3-4" = 2, "score 5-6" = 3),
  psqi_gdslp = c("good sleep quality by PSQI global" = 1, "poor sleep quality by PSQI global" = 0),
  distr_therm_ = c("no distress" = 0, "distress" = 1),
  hpothill_fm1 = c("helped PT with other illness" = 1, "not helped" = 0),
  isi_gr_ = c("absence of insomnia" = 0, "sub-threshold insomnia" = 1, 
              "moderate insomnia" = 2, "severe insomnia" = 3),
  ess_gr_ = c("lower normal daytime sleepiness" = 0, "higher normal daytime sleepiness" = 1, 
              "mild excessive daytime sleepiness" = 2, "moderate excessive daytime sleepiness" = 3, 
              "severe excessive daytime sleepiness" = 4),
  religaffil_ = c("No" = 0, "Yes" = 1)
)

add_labels = function(df, varlabels, vallabels) {
  for (base_name in names(varlabels)) {
    matched_vars = grep(paste0("^", base_name), colnames(df), value = TRUE)
    for (var in matched_vars) {
      var_label(df[[var]]) = varlabels[[base_name]]
    }
  }
  for (base_name in names(vallabels)) {
    matched_vars = grep(paste0("^", base_name), colnames(df), value = TRUE)
    for (var in matched_vars) {
      val_labels(df[[var]]) = vallabels[[base_name]]
    }
  }
  return(df)
}

df_full = add_labels(df_full, variable_labels, value_labels)

# df_full_varl = df_full[,1:561] |> 
#   varl_tb()
# 
# View(df_full_varl)

get_labels = function(df) {
  
  variable_labels = var_label(df)
  
  value_labels_list = lapply(df, val_labels)
  
  labels_df = do.call(rbind, lapply(names(df), function(var) {
    var_lbl = ifelse(!is.null(variable_labels[[var]]), variable_labels[[var]], NA)
    value_lbls = value_labels_list[[var]]
    if (!is.null(value_lbls)) {
      value_lbls_str = paste(names(value_lbls), value_lbls, sep = " = ", collapse = ", ")
    } else {
      value_lbls_str = NA
    }
    data.frame(
      Variable = var,
      VariableLabel = var_lbl,
      ValueLabels = value_lbls_str,
      stringsAsFactors = FALSE
    )
  }))
  
  return(labels_df)
}

# all labels BEFORE removing variables
df_full_labels = get_labels(df_full)
View(df_full_labels)

var_removelist = c("rlpdurmn_",       # have relationship in years, don't need in months
                   "sd_rlpdurmn_",    # no sd needed for relationship duration
                   "l/iveUSmn_",       # have in years, don't need in months
                   "sd_liveUSmn_", 
                   "psychmorb_dich_", # based on psychmorb_pt1/fm1 (0 if psychmorb = 0, 1 otherwise)
                   "physmorb_dich_",  # based on physmorb_pt1/fm1 (0 if psychmorb = 0, 1 otherwise)
                   "druguse_",        # based on totdrug_ (0 if 0, 1 if otherwise, mostly repeat)
                   # "pro_", "Tpro_", "sd_pro_", # PROMIS not "overall" composite? keeping for now
                   "ssss_negative_pt1r", # same as ssss_neg_pt1 (repeat)
                   "ssss_negative_fm1r",
                   "q42_", "q39_",    # religious affiliation individual not composites
                   "q43.", "q40.",    # religious affiliation individual not composites
                   "sd_CTask_", "sd_CTaskEMO_", "sd_CTaskINS_", "sd_CTaskTAN_", "sd_CTaskMED_" #sd all 0/NA (see questions doc)
)

df_full = df_full |> select(-starts_with(var_removelist)) |> arrange(BiPs_DID)
View(df_full)

df_full |>
  write_csv("integrated/BiPs_PTFM_T1_integrated_full_012325.csv")


#---------------------------------------------------------------------------
pred_list = c("_q", "_screen", "_cardio", "_co", "_clo")

with_sleep = df_full |> select(BiPs_DID, contains(c(pred_list, "_sleep", "_actcomp", "_actrhythm")), -starts_with("sd_"))

with_sleep |>
  write_csv("integrated/BiPs_PTFM_T1_integrated_sleep_012325.csv")
