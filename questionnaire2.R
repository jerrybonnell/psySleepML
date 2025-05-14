### Script to assemble tables for each questionnaire predictor group from given syntax

library(tidyverse)
library(haven)
library(surveytoolbox)
library(labelled)
library(dplyr)
library(lubridate)

####### 
# QUESTION DB

# The following dyads enrolled in the study (provided signed informed consent) 
# but withdrew from the study before providing any T1 data. They (6 dyads)
# must be removed from your database as their missing is not random. 
# dyad_exclude_list <- c("B085", "B181", "B372", "B691", "B692", "B824")

dyad_exclude_list_q <- c("B085", "B068", "B178", "B181", "B372", "B606", "B691", 
                         "B692",  "B761", "B781", "B824","B929", "B814")

# T1 Questionnaire - Patient- Data: "T1 Questionnaire PT_1.31.24_NA"
t1_q_pt_orig <- read_sav("T1 Questionnaire PT_1.31.24_NA.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_q))
t1_q_pt_orig # 152

t1_q_pt_varl <- t1_q_pt_orig |> 
  varl_tb()
t1_q_pt_varl

# T1 Questionnaire - Family Caregiver/Member - Data:"T1 Questionnaire FM_06.28.23_YK_TT"
t1_q_fm_orig <- read_sav("T1 Questionnaire FM_06.28.23_YK_TT.sav") |>
  filter(!BiPs_DID %in% c(dyad_exclude_list_q))
t1_q_fm_orig # 152

Reduce(setdiff, list(t1_q_pt_orig |> distinct(BiPs_DID), 
                     t1_q_fm_orig |> distinct(BiPs_DID)))
Reduce(setdiff, list(t1_q_fm_orig |> distinct(BiPs_DID), 
                     t1_q_pt_orig |> distinct(BiPs_DID)))

t1_q_fm_varl <- t1_q_fm_orig |> 
  varl_tb()
t1_q_fm_varl

################################################################################
# PREDICTORS

########## PT PREDICTOR EXTRACTIONS
# SEE: BiPs PT T1 Syntax_11.1.23_TT.sps


##########
# AGE PT
t1_q_age_pt =  t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, DOB_pt, starts_with("q65")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate(
    q65_pt1 = make_date(q65_yr_pt1, q65_mn_pt1, q65_da_pt1)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    age_pt1 = interval(DOB_pt, q65_pt1) / years(1)
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q65")))
# # View(t1_q_age_pt)

# GENDER PT
t1_q_gender_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, Gender_pt) |>
 transmute(
   BiPs_IID, 
   BiPs_DID,
   pt_female_pt1 = recode(Gender_pt, `2` = 1, `1` = 0, .default = NA_real_)
 ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q")))
# View(t1_q_gender_pt)

# PRIMARY LANGUAGE PT (1 = English, 2 = Spanish)
t1_q_lang_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, lang_pt1) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q")))
# View(t1_q_lang_pt)

# ETHNICITY PT
t1_q_ethnicity_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, DOB_pt, starts_with("q1_")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>

  mutate(
    MultiEth_pt1 = sum(c_across(q1_aa_pt1:q1_ot_pt1), na.rm=T)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    MultiEth_pt1,
    RaceEth_pt1 = case_when(
      MultiEth_pt1  ==  0 ~ 0,
      MultiEth_pt1  ==  1 && q1_aa_pt1 == 1 ~ 1,
      MultiEth_pt1  ==  1 && q1_ai_pt1 == 1 ~ 2,
      MultiEth_pt1  ==  1 && q1_as_pt1 == 1 ~ 3,
      MultiEth_pt1  ==  1 && q1_pi_pt1 == 1 ~ 4,
      MultiEth_pt1  ==  1 && q1_wh_pt1 == 1 ~ 5,
      MultiEth_pt1  ==  1 && q1_hi_pt1 == 1 ~ 6,
      MultiEth_pt1  ==  1 && q1_me_pt1 == 1 ~ 7,
      MultiEth_pt1  ==  1 && q1_ot_pt1 == 1 ~ 8,
      MultiEth_pt1  >=  2 ~ 9,
      q1_wh_pt1 == 1 && q1_hi_pt1 == 1 ~ 10,
      q1_aa_pt1 == 1 && q1_hi_pt1 == 1 ~ 11,
      q1_as_pt1 == 1 && q1_hi_pt1 == 1 ~ 12,
      
      TRUE ~ NA_real_ ),
    p_hisp_pt1 = ifelse(q1_hi_pt1 == 1 && !is.na(q1_hi_pt1), 1, 0)
  ) |>
  mutate(
    RaceGrp_pt1 = case_when(
      RaceEth_pt1 %in% c(5, 10) ~ 1,
      RaceEth_pt1 %in% c(1, 11) ~ 2,
      RaceEth_pt1 %in% c(3, 12) ~ 3,
      TRUE ~ 4
    )
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q1")))
# View(t1_q_ethnicity_pt)

# TODO Questionnaire Section 1: all variables in socio-demographic variables and lifestyle & medical history sections

####
# SOCIO DEMOGRAPHIC

# RELATIONSHIP DURATION PT
t1_q_rlpdur_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q2")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    rlpdurmn_pt1 = sum(c(q2_yr_pt1*12, q2_mn_pt1), na.rm=T),
    sd_rlpdurmn_pt1 = sd(c(q2_yr_pt1*12, q2_mn_pt1), na.rm=T)
    
  ) |>
  mutate(rlpduryr_pt1 = rlpdurmn_pt1/12) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q2")))
# # # View(t1_q_rlpdur_pt)

# EDUCATION PT
t1_q_edu_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q3")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    edulow_pt = ifelse(q3_pt1 %in% 1:3, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q3")))
# # # View(t1_q_edu_pt)

# HOUSEHOLD SIZE PT
t1_q_minor_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q4")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    minor_pt1 = ifelse(q4.1_pt1 %in% c(0, NA_real_), 0, 1)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q4")))
# # # View(t1_q_minor_pt)

# INCOME PT
t1_q_inc_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q5")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    inclow_pt = ifelse(q5_pt1 %in% 1:4, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q5")))
# # # View(t1_q_inc_pt)

# EMPLOYMENT PT
t1_q_employ_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q6")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    employed_pt = ifelse(q6_pt1 %in% 1:3, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q6")))
# # # View(t1_q_employ_pt)

# CITIZENSHIP & COUNTRY OF BIRTH/ORIGIN PT
t1_q_liveUS_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q7")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    liveUSmn_pt1 = sum(c(q7.1_yr_pt1 * 12, q7.1_mn_pt1), na.rm=T),
    
    sd_liveUSmn_pt1 = sd(c(q7.1_yr_pt1 * 12, q7.1_mn_pt1), na.rm=T)
  ) |>
  mutate(liveUSyr_pt1 = liveUSmn_pt1 / 12) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q7")))
# # View(t1_q_liveUS_pt)

####
# MEDICAL HISTORY

# Cig/Tobacco Use (Q8a) and Exercise (Q8b) in Last 2 hours PT
t1_q_smokeexc_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q8")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    smoked_pt1 = ifelse(q8a_pt1 == 1, 1, 0),
    exced_pt1 = ifelse(q8b_pt1 == 1, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q8")))
# # # # View(t1_q_smokeexc_pt)

#  Food/Drink, Coffee, Tea, and Caffeine Intake in Last 4 hours
t1_q_intake_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, q9_pt1:q12_pt1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    atedrnk_pt1 = ifelse(q9_pt1 == 1, 1, 0),
    cof_pt1 = ifelse(q10_pt1 == 1, 1, 0),
    tea_pt1 = ifelse(q11_pt1 == 1, 1, 0),
    caff_pt1 = ifelse(q12_pt1 == 1, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q9-12")))
# # # # View(t1_q_intake_pt)

#  Drugs/Meds and Alcohol Intake in Last 24 hours
t1_q_drgmedalc_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, q13_pt1:q14_pt1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    drgmed_pt1 = ifelse(q13_pt1 == 1, 1, 0),
    alc_pt1 = ifelse(q14_pt1 == 1, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q13-14")))
# # # View(t1_q_drgmedalc_pt)

# Medical Morbidity = Modified MICCI: Morbidities Index for Caregivers of Chronic Illnesses
MICCIvars = c("q15a2_pt1", "q15j2_pt1", "q15k2_pt1", "q15q2_pt1", "q15t2_pt1", "q15v2_pt1", "q15y2_pt1", 
              "q15aa2_pt1", "q15cc2_pt1", "q15ff2_pt1", "q15hh2_pt1")
MICCInames = c("hypertension_pt1", "cholesterol_pt1", "heart_pt1", "kidney_pt1", "gallbaldder_pt1", 
               "liver_pt1", "mentalhealth_pt1", "diabetes_pt1", "pulmonary_pt1", "thyroid_pt1", "GI_pt1")

t1_q_MICCI_pt <- t1_q_pt_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q15"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate(
    morb_pt1 = rowSums(across(c(q15a_pt1, q15b_pt1:q15j_pt1, q15k.a_pt1:q15k.c_pt1,
                                q15l_pt1:q15q_pt1, q15r_pt1:q15t_pt1, q15u_pt1:q15v_pt1,
                                q15w_pt1:q15y_pt1, q15z_pt1:q15aa_pt1, q15bb_pt1:q15cc_pt1,
                                q15dd_pt1:q15ff_pt1, q15gg_pt1:q15hh_pt1, q15ii_pt1:q15ll_pt1
    )) == 1, na.rm=T)
  ) |>
  mutate_at(
    vars(q15a_pt1, q15b_pt1:q15j_pt1, q15k.a_pt1:q15k.c_pt1, q15l_pt1, q15m_pt1: q15r_pt1, q15s_pt1: q15v_pt1, q15w_pt1, q15x_pt1:q15z_pt1,
         q15aa_pt1, q15bb_pt1, q15cc_pt1, q15dd_pt1, q15ee_pt1, q15ff_pt1, q15gg_pt1:q15hh_pt1, q15ii_pt1:q15ll_pt1 
    ), ~ ifelse(is.na(.), 0, .)
  ) |>
  mutate(
    psychmorb_pt1 = rowSums(across(c(q15o_pt1, q15y_pt1)) == 1, na.rm = T)
  ) |>
  mutate(
    psychmorb_dich_pt1 = ifelse(psychmorb_pt1 >= 1, 1, 0),
    physmorb_pt1 = morb_pt1 - psychmorb_pt1
  ) |>
  mutate(
    physmorb_dich_pt1 = ifelse(physmorb_pt1 >= 1, 1, physmorb_pt1)
  ) |>
  mutate_at(
    vars(MICCIvars), ~ifelse(is.na(.) | . != 1, 0, 1)
  ) |>
  mutate(
    setNames(across(MICCIvars), MICCInames)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    morb_pt1,
    psychmorb_pt1,
    psychmorb_dich_pt1,
    physmorb_pt1,
    physmorb_dich_pt1,
    med_pt1 = rowSums(across(MICCInames) == 1, na.rm=T)
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q15")))
# # # View(t1_q_MICCI_pt)

# Healthcare utilization: 13 Over or Under utilizations during the past 12 months PT
hcareut_df = select(t1_q_pt_orig, q16a_pt1:q16m_times_pt1)
hcareut_vars = names(hcareut_df)[grepl("^([^_]*_){1}[^_]*$", names(hcareut_df))]
hcareut_times_vars = grep("times", names(hcareut_df), value=TRUE)
t1_q_hcareut_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, q16a_pt1:q16m_times_pt1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    hcareut_pt1 = rowSums(across(all_of(hcareut_vars)) == 1, na.rm=T),
    hcareut_ov_pt1 = rowSums(across(all_of(hcareut_vars[1:7])) == 1, na.rm=T),
    hcareut_ud_pt1 = rowSums(across(all_of(hcareut_vars[8:13])) == 1, na.rm=T),
    
    hcareut_sum_pt1 = sum(c_across(all_of(hcareut_times_vars)), na.rm=T),
    hcareut_ovsum_pt1 = sum(c_across(all_of(hcareut_times_vars[1:7])), na.rm=T),
    hcareut_udsum_pt1 = sum(c_across(all_of(hcareut_times_vars[8:13])), na.rm=T),
    
    sd_hcareut_pt1 = sd(across(all_of(hcareut_vars)), na.rm=T),
    sd_hcareut_ov_pt1 = sd(across(all_of(hcareut_vars[1:7])), na.rm=T),
    sd_hcareut_ud_pt1 = sd(across(all_of(hcareut_vars[8:13])), na.rm=T),
    
    sd_hcareut_sum_pt1 = sd(c_across(all_of(hcareut_times_vars)), na.rm=T),
    sd_hcareut_ovsum_pt1 = sd(c_across(all_of(hcareut_times_vars[1:7])), na.rm=T),
    sd_hcareut_udsum_pt1 = sd(c_across(all_of(hcareut_times_vars[8:13])), na.rm=T)
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q16a-m")))
# # # View(t1_q_hcareut_pt)

# Personal/Family History CRC and Related Diseases
t1_q_crchist_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q16")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    pcrchist_pt1 = ifelse(q16n_pt1 == 1, 1, 0),
    fcrchist_pt1 = ifelse(q16p_pt1 == 1, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q16np")))
# # # # View(t1_q_crchist_pt)

#  Smoking & Drinking 
t1_q_drink_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, q17a_pt1:q20_pt1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    smoking_pt1 = recode(q17a_pt1, `1` = 0, `2` = 1, .missing = 0),
    othtob_pt1 = recode(q18_pt1, `1` = 1, `2` = 0, .missing = 0),
    hvdrink_pt1 = recode(q20_pt1, `1` = 0, `2` = 0, `3` = 0, `4` = 1, .missing = 0),
    drinkpday_pt1 = recode(q20_pt1, `1` = 0, `2` = 1, `3` = 2, `4` = 3, .missing = 0)
  ) |>
  mutate(
    hvdrinkpday_pt1 = recode(drinkpday_pt1, `0` = 0, `1` = 0, `2` = 1, `3` = 1)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q17-20")))
# # # # View(t1_q_drink_pt)

# DAST drug screening questionnaire PT
t1_q_DAST_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q21")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q21a_mth_pt1:q21a_no_pt1), ~ case_when(
      . == 1 ~ 1,
      TRUE ~ 0 )
  ) |>
  mutate_at(
    vars(q21b_pt1:q21l_pt1), ~ case_when(
      . == 1 ~ 1,
      . == 2 ~ 0,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q21m_pt1:q21n_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 1,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    totdrug_pt1 = sum(c_across(q21a_mth_pt1:q21a_ot_pt1), na.rm=T),
    dast_pt1 = sum(c_across(q21b_pt1:q21n_pt1), na.rm=T),
    
    sd_totdrug_pt1 = sd(c_across(q21a_mth_pt1:q21a_no_pt1), na.rm=T),
    sd_dast_pt1 = sd(c_across(q21b_pt1:q21n_pt1), na.rm=T)
  ) |>
  mutate(
    druguse_pt1 = recode(totdrug_pt1, `0` = 0, .default = 1),
    dastzone_pt1 = recode(dast_pt1, `0` = 1, `1` = 2, `2` = 2, `3` = 3, 
                          `4` = 3, `5` = 3, `6` = 4, `7` = 4)
  ) |> 
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q21")))
# # # # View(t1_q_DAST_pt)


##########
# Questionnaire Section 2: Optimism, Big-5, Urgency, Positive Urgency, 
# Self-control, Mishel Uncertainty in Illness, Measurement of Attachment Quality, 
# Sources of Social Support, Duke Religiosity, Social Constraint, 
# Dyadic Adjustment, Relationship Quality
# TODO Dimensions of Care Task (caregivers only), 
# Activities of Daily Living (ADL) & IADL(caregivers only), 
# Caregiving Duration (caregivers only), Caregiving Stress (caregivers only), 
# Caregiving Motivation(caregivers only), Caregiving Experiences (caregivers only)

# OPTIMISM PT
t1_q_LOTR_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q36")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q36a_pt1:q36f_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q36b_pt1, q36d_pt1:q36e_pt1), ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    LOTR_pt1 = mean(c_across(q36a_pt1:last_col()), na.rm=T),
    sd_LOTR_pt1 = sd(c_across(q36a_pt1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q36")))
# # # # # View(t1_q_LOTR_pt)

# BIG 5 PT
t1_q_TIPI_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q37")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q37b_pt1, q37f_pt1, q37h_pt1:q37j_pt1), ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      TRUE ~ . )
  )  |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    extraversion_TIPI_pt1 = mean(c_across(c(q37a_pt1, q37f_pt1 )), na.rm=T),
    agreeableness_TIPI_pt1 = mean(c_across(c(q37b_pt1, q37g_pt1 )), na.rm=T),
    conscientiousness_TIPI_pt1 = mean(c_across(c(q37c_pt1, q37h_pt1 )), na.rm=T),
    neuroticism_TIPI_pt1 = mean(c_across(c(q37d_pt1, q37i_pt1 )), na.rm=T),
    openness_TIPI_pt1 = mean(c_across(c(q37e_pt1, q37j_pt1 )), na.rm=T),
    
    sd_extraversion_TIPI_pt1 = sd(c_across(c(q37a_pt1, q37f_pt1 )), na.rm=T),
    sd_agreeableness_TIPI_pt1 = sd(c_across(c(q37b_pt1, q37g_pt1 )), na.rm=T),
    sd_conscientiousness_TIPI_pt1 = sd(c_across(c(q37c_pt1, q37h_pt1 )), na.rm=T),
    sd_neuroticism_TIPI_pt1 = sd(c_across(c(q37d_pt1, q37i_pt1 )), na.rm=T),
    sd_openness_TIPI_pt1 = sd(c_across(c(q37e_pt1, q37j_pt1 )), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q37")))
# # # # # View(t1_q_TIPI_pt)

# URGENCY, POSITIVE URGENCY, SELF CONTROL PT
t1_q_impulse_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q38")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q38k_pt1, q38u_pt1:q38x_pt1, q38z_pt1, q38bb_pt1:q38cc_pt1, 
         q38ee_pt1:q38ff_pt1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    urgency_pt1 = mean(c_across(q38a_pt1:q38l_pt1), na.rm=T),
    pos_urgency_pt1 = mean(c_across(q38m_pt1:q38s_pt1), na.rm=T),
    control_pt1 = sum(c_across(q38t_pt1:last_col()), na.rm=T),
    
    sd_urgency_pt1 = sd(c_across(q38a_pt1:q38l_pt1), na.rm=T),
    sd_pos_urgency_pt1 = sd(c_across(q38m_pt1:q38s_pt1), na.rm=T),
    sd_control_pt1 = sd(c_across(q38t_pt1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q38")))
# # # # # View(t1_q_impulse_pt)

# UNCERTAINTY PT
t1_q_uncertainty_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, 
                                             starts_with("q39")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q39a_pt1:q39e_pt1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ . )
  )  |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    uncertain_pt1 = mean(c_across(q39a_pt1:q39e_pt1), na.rm=T),
    sd_uncertain_pt1 = sd(c_across(q39a_pt1:q39e_pt1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q39")))
# # # # # View(t1_q_uncertainty_pt)

# Measurement of Attachment Quality PT
t1_q_maq_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q40")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q40d_pt1, q40h_pt1:q40i_pt1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      . == 5 ~ 0,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q40a_pt1:last_col()), ~ case_when(
      . == 1 ~ 1,
      . == 2 ~ 2,
      . == 3 ~ 3,
      . == 4 ~ 4,
      . == 5 ~ NA_real_,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    maq_sec_pt1 = mean(c_across(c(q40a_pt1, q40g_pt1, q40n_pt1)), na.rm=T),
    maq_avd_pt1 = mean(c_across(c(q40d_pt1, q40f_pt1, q40h_pt1, q40k_pt1, q40m_pt1)), na.rm=T),
    maq_ambw_pt1 = mean(c_across(c(q40b_pt1, q40e_pt1, q40i_pt1)), na.rm=T),
    maq_ambm_pt1 = mean(c_across(c(q40c_pt1, q40j_pt1, q40l_pt1)), na.rm=T),
    
    sd_maq_sec_pt1 = sd(c_across(c(q40a_pt1, q40g_pt1, q40n_pt1)), na.rm=T),
    sd_maq_avd_pt1 = sd(c_across(c(q40d_pt1, q40f_pt1, q40h_pt1, q40k_pt1, q40m_pt1)), na.rm=T),
    sd_maq_ambw_pt1 = sd(c_across(c(q40b_pt1, q40e_pt1, q40i_pt1)), na.rm=T),
    sd_maq_ambm_pt1 = sd(c_across(c(q40c_pt1, q40j_pt1, q40l_pt1)), na.rm=T)
  ) |>
  mutate(
    across(maq_avd_pt1, ~ ifelse(is.na(.), NA_real_, 5 - .), .names = "{col}")
  ) |>
  mutate(
    att_dep_pt1 = mean(c_across(c(maq_sec_pt1, maq_avd_pt1)), na.rm=T),
    att_anx_pt1 = mean(c_across(c(maq_ambw_pt1, maq_ambm_pt1)), na.rm=T),
    
    sd_att_dep_pt1 = sd(c_across(c(maq_sec_pt1, maq_avd_pt1)), na.rm=T),
    sd_att_anx_pt1 = sd(c_across(c(maq_ambw_pt1, maq_ambm_pt1)), na.rm=T)
  ) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q40")))
# # # # # View(t1_q_maq_pt)


# SOURCES OF SOCIAL SUPPORT PT
t1_q_ssss_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, 
                                             starts_with("q41")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q41i_pt1:q41j_pt1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ . )
  )  |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    ssss_neg_pt1 = mean(c_across(q41g_pt1:q41h_pt1), na.rm=T),
    ssss_info_pt1 = q41a_pt1,
    ssss_inst_pt1 = q41b_pt1,
    ssss_positive_pt1 = mean(c_across(q41a_pt1:q41f_pt1), na.rm=T),
    ssss_negative_pt1 = mean(c_across(q41g_pt1:q41j_pt1), na.rm=T),
    ssss_negative_pt1r = mean(c_across(q41g_pt1:q41h_pt1), na.rm=T),
    
    sd_ssss_neg_pt1 = sd(c_across(q41g_pt1:q41h_pt1), na.rm=T),
    sd_ssss_info_pt1 = q41a_pt1,
    sd_ssss_inst_pt1 = q41b_pt1,
    sd_ssss_positive_pt1 = sd(c_across(q41a_pt1:q41f_pt1), na.rm=T),
    sd_ssss_negative_pt1 = sd(c_across(q41g_pt1:q41j_pt1), na.rm=T),
    sd_ssss_negative_pt1r = sd(c_across(q41g_pt1:q41h_pt1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q41")))
# # # # # View(t1_q_ssss_pt)
    
# DUKE RELIGIOSITY PT
t1_q_DUREL_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, 
                                      starts_with(c("q42","q43"))) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate(religaffil_pt1 = case_when(
    q42_pt1 == 7 | is.na(q42_pt1) ~ 0,
    TRUE ~ 1
    )
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q42-43")))
# # # # # View(t1_q_DUREL_pt)

# SOCIAL CONSTRAINT PT
t1_q_SCC_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, 
                                       starts_with("q44")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    constraint_pt1 = sum(c_across(q44a_pt1:last_col()), na.rm=T),
    sd_constraint_pt1 = sd(c_across(q44a_pt1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q44")))
# # # # # View(t1_q_SCC_pt)

# DYADIC ADJUSTMENT PT
t1_q_DAS4_pt = t1_q_pt_orig |> 
  select(BiPs_IID, BiPs_DID, q45_pt1:q48_pt1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q45_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      . == 6 ~ 5,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q46_pt1:q47_pt1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      . == 6 ~ 0,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q48_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      . == 6 ~ 5,
      . == 7 ~ 6,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    das_pt1 = sum(c_across(q45_pt1:last_col()), na.rm=T),
    sd_das_pt1 = sd(c_across(q45_pt1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q45-48")))
# # # # # View(t1_q_DAS4_pt)

# RELATIONSHIP QUALITY PT
t1_q_RQS_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, 
                                             starts_with("q49")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    rqi_pt1 = mean(c_across(q49a_pt1:q49f_pt1), na.rm=T),
    sd_rqi_pt1 = sd(c_across(q49a_pt1:q49f_pt1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q49")))
# # # # # View(t1_q_RQS_pt)


##########
# Questionnaire Section 3 (done): Perceived Stress, Coping with Cancer Stress, 
# Loneliness, Perceived Social support, Cancer-related Stress Appraisal, 
# BAS/BIS scale, Biculturalism, Social Support Network Size, Familism

# PERCEIVED STRESS PT
t1_q_PSS_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q50")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q50a_pt1:q50c_pt1, q50f_pt1, q50i_pt1:q50j_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q50d_pt1:q50e_pt1, q50g_pt1:q50h_pt1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      . == 5 ~ 0,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    pss_pt1 = sum(c_across(q50a_pt1:last_col()), na.rm=T),
    sd_pss_pt1 = sd(c_across(q50a_pt1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q50")))
# # # # # View(t1_q_PSS_pt)

# B-COPE: Brief COPE + Emotional Approach and Emotional Processing PT
t1_q_bcope_pt <- t1_q_pt_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q51"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    # syntax has labels for composites
    bcope_SD_pt1 = mean(c_across(c(q51a_pt1, q51o_pt1)), na.rm=T),
    bcope_AC_pt1 = mean(c_across(c(q51b_pt1, q51f_pt1)), na.rm=T), 
    bcope_DN_pt1 = mean(c_across(c(q51c_pt1, q51h_pt1)), na.rm=T),  
    bcope_SU_pt1 = mean(c_across(c(q51d_pt1, q51i_pt1)), na.rm=T), 
    bcope_ES_pt1 = mean(c_across(c(q51w_pt1, q51aa_pt1)), na.rm=T), 
    bcope_IS_pt1 = mean(c_across(c(q51y_pt1, q51ee_pt1)), na.rm=T), 
    bcope_BD_pt1 = mean(c_across(c(q51e_pt1, q51m_pt1)), na.rm=T), 
    bcope_VT_pt1 = mean(c_across(c(q51x_pt1, q51dd_pt1)), na.rm=T),
    bcope_PR_pt1 = mean(c_across(c(q51z_pt1, q51bb_pt1)), na.rm=T), 
    bcope_PL_pt1 = mean(c_across(c(q51l_pt1, q51r_pt1)), na.rm=T), 
    bcope_HR_pt1 = mean(c_across(c(q51n_pt1, q51u_pt1)), na.rm=T),
    bcope_ACC_pt1 = mean(c_across(c(q51cc_pt1, q51ff_pt1)), na.rm=T),
    bcope_RL_pt1 = mean(c_across(c(q51q_pt1, q51t_pt1)), na.rm=T), 
    bcope_SB_pt1 = mean(c_across(c(q51k_pt1, q51s_pt1)), na.rm=T), 
    cope_ee_pt1 = mean(c_across(c(q51j_pt1, q51v_pt1)), na.rm=T),
    cope_ep_pt1 = mean(c_across(c(q51g_pt1, q51p_pt1)), na.rm=T),
    
    
    sd_bcope_SD_pt1 = sd(c_across(c(q51a_pt1, q51o_pt1)), na.rm=T),
    sd_bcope_AC_pt1 = sd(c_across(c(q51b_pt1, q51f_pt1)), na.rm=T), 
    sd_bcope_DN_pt1 = sd(c_across(c(q51c_pt1, q51h_pt1)), na.rm=T),  
    sd_bcope_SU_pt1 = sd(c_across(c(q51d_pt1, q51i_pt1)), na.rm=T), 
    sd_bcope_ES_pt1 = sd(c_across(c(q51w_pt1, q51aa_pt1)), na.rm=T), 
    sd_bcope_IS_pt1 = sd(c_across(c(q51y_pt1, q51ee_pt1)), na.rm=T), 
    sd_bcope_BD_pt1 = sd(c_across(c(q51e_pt1, q51m_pt1)), na.rm=T), 
    sd_bcope_VT_pt1 = sd(c_across(c(q51x_pt1, q51dd_pt1)), na.rm=T),
    sd_bcope_PR_pt1 = sd(c_across(c(q51z_pt1, q51bb_pt1)), na.rm=T), 
    sd_bcope_PL_pt1 = sd(c_across(c(q51l_pt1, q51r_pt1)), na.rm=T), 
    sd_bcope_HR_pt1 = sd(c_across(c(q51n_pt1, q51u_pt1)), na.rm=T),
    sd_bcope_ACC_pt1 = sd(c_across(c(q51cc_pt1, q51ff_pt1)), na.rm=T),
    sd_bcope_RL_pt1 = sd(c_across(c(q51q_pt1, q51t_pt1)), na.rm=T), 
    sd_bcope_SB_pt1 = sd(c_across(c(q51k_pt1, q51s_pt1)), na.rm=T), 
    sd_cope_ee_pt1 = sd(c_across(c(q51j_pt1, q51v_pt1)), na.rm=T),
    sd_cope_ep_pt1 = sd(c_across(c(q51g_pt1, q51p_pt1)), na.rm=T)) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q51")))
# # # # # View(t1_q_bcope_pt)

# LONELINESS PT
t1_q_loneliness_pt = t1_q_pt_orig |> 
  select(BiPs_IID, BiPs_DID, starts_with("q52")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q52c_pt1:q52d_pt1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    loneliness_pt1 = mean(c_across(q52a_pt1:last_col()), na.rm=T),
    sd_loneliness_pt1 = sd(c_across(q52a_pt1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q52")))
# # # # # View(t1_q_loneliness_pt)



#  ISEL - Interpersonal support evaluation list (perceived social support) PT
t1_q_ISEL_pt = t1_q_pt_orig |> 
  select(BiPs_IID, BiPs_DID, starts_with(c("q53","q54"))) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q53c_pt1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q54a_pt1:q54c_pt1), ~ case_when(
      . == 1 ~ NA_real_,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    ISEL_pt1 = mean(c_across(q53a_pt1:q53f_pt1), na.rm=T),
    ISEL_SSAA_pt1 = mean(c_across(c(q53b_pt1,q53f_pt1)), na.rm=T),
    ISEL_SSAB_pt1 = mean(c_across(c(q53a_pt1, q53e_pt1)), na.rm=T),
    ISEL_SSAT_pt1  = mean(c_across(q53c_pt1:q53d_pt1), na.rm=T),
    ISELST_pt1 = mean(c_across(q54a_pt1:q54c_pt1), na.rm=T),
    
    
    sd_ISEL_pt1 = sd(c_across(q53a_pt1:q53f_pt1), na.rm=T),
    sd_ISEL_SSAA_pt1 = sd(c_across(c(q53b_pt1,q53f_pt1)), na.rm=T),
    sd_ISEL_SSAB_pt1 = sd(c_across(c(q53a_pt1, q53e_pt1)), na.rm=T),
    sd_ISEL_SSAT_pt1  = sd(c_across(q53c_pt1:q53d_pt1), na.rm=T),
    sd_ISELST_pt1 = sd(c_across(q54a_pt1:q54c_pt1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q53-54")))
# # # # # View(t1_q_ISEL_pt)

# CANCER RELATED STRESS APPRAISAL PT
t1_q_castress_pt = t1_q_pt_orig |> 
  select(BiPs_IID, BiPs_DID, starts_with("q55")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q55.1b_pt1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    castress_SF_pt1 = mean(c_across(q55.1a_pt1:q55.1b_pt1), na.rm=T),
    castress_FA_pt1 = mean(c_across(q55.2a_pt1:q55.2e_pt1), na.rm=T),
    castress_tot_pt1 = mean(c_across(q55.1a_pt1:q55.2e_pt1), na.rm=T),
    
    sd_castress_SF_pt1 = sd(c_across(q55.1a_pt1:q55.1b_pt1), na.rm=T),
    sd_castress_FA_pt1 = sd(c_across(q55.2a_pt1:q55.2e_pt1), na.rm=T),
    sd_castress_tot_pt1 = sd(c_across(q55.1a_pt1:q55.2e_pt1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q55")))
# # # # # View(t1_q_castress_pt)

# BAS/BIS Scale PT
t1_q_BAS_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q56")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q56a_pt1, q56r_pt1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    drive_pt1 = sum(c_across(c(q56b_pt1, q56g_pt1, q56i_pt1, q56q_pt1)), 
                    na.rm=T),
    funseek_pt1 = sum(c_across(c(q56d_pt1, q56h_pt1, q56l_pt1, q56p_pt1)), 
                      na.rm=T),
    reward_pt1 = sum(c_across(c(q56c_pt1, q56e_pt1, q56k_pt1, q56n_pt1, 
                                q56s_pt1)), na.rm=T),
    inhibition_pt1 = sum(c_across(c(q56a_pt1, q56f_pt1, q56j_pt1, q56m_pt1, 
                                    q56o_pt1, q56r_pt1, q56t_pt1)), na.rm=T),
    
    
    sd_drive_pt1 = sd(c_across(c(q56b_pt1, q56g_pt1, q56i_pt1, q56q_pt1)), 
                      na.rm=T),
    sd_funseek_pt1 = sd(c_across(c(q56d_pt1, q56h_pt1, q56l_pt1, q56p_pt1)), 
                        na.rm=T),
    sd_reward_pt1 = sd(c_across(c(q56c_pt1, q56e_pt1, q56k_pt1, q56n_pt1, 
                                  q56s_pt1)), na.rm=T),
    sd_inhibition_pt1 = sd(c_across(c(q56a_pt1, q56f_pt1, q56j_pt1, q56m_pt1, 
                                      q56o_pt1, q56r_pt1, q56t_pt1)), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q56")))
# # # # # View(t1_q_BAS_pt)

# Biculturalism PT 
# this derives predictors for *both* composite and all sub-scales, as per SPSS
t1_q_biculturalism_pt <- t1_q_pt_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q57"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    # this should follow exactly from SPSS syntax given
    languse_pt1 = mean(c_across(q57a_pt1:q57e_pt1), na.rm=T),
    media_pt1 = mean(c_across(q57f_pt1:q57h_pt1), na.rm=T),
    relations_pt1 = mean(c_across(q57i_pt1:q57l_pt1), na.rm=T),
    accultur_pt1 = mean(c_across(q57a_pt1:q57l_pt1), na.rm=T),
    # NOTE SD versions for diagnostic only , to be removed later in modeling
    sd_languse_pt1 = sd(c_across(q57a_pt1:q57e_pt1), na.rm=T),
    sd_media_pt1 = sd(c_across(q57f_pt1:q57h_pt1), na.rm=T),
    sd_relations_pt1 = sd(c_across(q57i_pt1:q57l_pt1), na.rm=T),
    sd_accultur_pt1 = sd(c_across(q57a_pt1:q57l_pt1), na.rm=T)) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q57")))
# # View(t1_q_biculturalism_pt)

# TODO the above code chunk can be repeated for each of the PT items in the above
# todo list. BiPs_IID and BiPs_DID should be included in the derived PT tables 
# (as shown above) so that the individual tables can be joined to form 
# a table of predictors for the questionnaire DB (containing both PT and FM)
# NOTE 1: let us derive predictors for *both* composite and all sub-scales 
# NOTE 2: some predictors will be available only for PT *or* FM, not both, e.g.,
# Caregiving Experiences is available only for FM. 


# SOCIAL SUPPORT NETWORK SIZE PT
# Recoding
t1_q_SSN_r_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, q58_pt1:q61_pt1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(across(q58_pt1:q61_pt1, ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    . == 3 ~ 2,
    . == 4 ~ 3.5,
    . == 5 ~ 6.5,
    . == 6 ~ 9,
    TRUE ~ . )),
    BiPs_IID, 
    BiPs_DID) |> 
  transmute(
    BiPs_IID, 
    SSNS_quant_pt1 = sum(c_across(c(q58_pt1, q60_pt1)), na.rm=T),
    SSNS_rlose_pt1 = sum(c_across(c(q59_pt1, q61_pt1)), na.rm=T),
    sd_SSNS_quant_pt1 = sd(c_across(c(q58_pt1, q60_pt1)), na.rm=T),
    sd_SSNS_rlose_pt1 = sd(c_across(c(q59_pt1, q61_pt1)), na.rm=T),
    
  )

# NOTE: certain composites are calculated BEFORE reordering
t1_q_SSN_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, q58_pt1:q63_pt1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    SSNS_pt1 = mean(c_across(q58_pt1:q61_pt1), na.rm=T),
    SSNS_fam_pt1 = mean(c_across(q58_pt1:q59_pt1), na.rm=T),
    SSNS_friend_pt1 = mean(c_across(q60_pt1:q61_pt1), na.rm=T),
    SSNTQ_pt1 = mean(c_across(q62_pt1:q63_pt1), na.rm=T),
    
    sd_SSNS_pt1 = sd(c_across(q58_pt1:q61_pt1), na.rm=T),
    sd_SSNS_fam_pt1 = sd(c_across(q58_pt1:q59_pt1), na.rm=T),
    sd_SSNS_friend_pt1 = sd(c_across(q60_pt1:q61_pt1), na.rm=T),
    sd_SSNTQ_pt1 = sd(c_across(q62_pt1:q63_pt1), na.rm=T)) |>
  
  merge(t1_q_SSN_r_pt, by = "BiPs_IID", all=TRUE) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q58-63")))
# # # # # View(t1_q_SSN_pt)

# FAMILISM PT
t1_q_familism_pt <- t1_q_pt_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q64"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    # this should follow exactly from SPSS syntax given
    familism_tot_pt1 = sum(c_across(q64a_pt1:q64n_pt1), na.rm=T),
    famob_pt1 = sum(c_across(q64a_pt1:q64f_pt1), na.rm=T),
    famsupport_pt1 = sum(c_across(c(q64g_pt1,q64h_pt1,q64n_pt1)), na.rm=T),
    famref_pt1 = sum(c_across(q64i_pt1:q64m_pt1), na.rm=T),
    # NOTE SD versions for diagnostic only , to be removed later in modeling
    sd_familism_tot_pt1 = sd(c_across(q64a_pt1:q64n_pt1), na.rm=T),
    sd_famob_pt1 = sd(c_across(q64a_pt1:q64f_pt1), na.rm=T),
    sd_famsupport_pt1 = sd(c_across(c(q64g_pt1,q64h_pt1,q64n_pt1)), na.rm=T),
    sd_famref_pt1 = sd(c_across(q64i_pt1:q64m_pt1), na.rm=T)) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q64")))
# # # # # View(t1_q_familism_pt)




# -------------------------------------------------------------------
########## FM PREDICTOR EXTRACTIONS
# SEE: BiPs FM T1 Syntax_5.9.23_YK.sps

##########
# AGE fm
t1_q_age_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, DOB_fm, starts_with("q72")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate(
    q72_fm1 = make_date(q72_yr_fm1, q72_mn_fm1, q72_da_fm1)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    age_fm1 = interval(DOB_fm, q72_fm1) / years(1)
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q72")))
# # View(t1_q_age_fm)

# GENDER fm
t1_q_gender_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, Gender_fm) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    fm_female_fm1 = recode(Gender_fm, `2` = 1, `1` = 0, .default = NA_real_)
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q")))
# # View(t1_q_gender_fm)

# PRIMARY LANGUAGE fm (1 = English, 2 = Spanish)
t1_q_lang_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, lang_fm1) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q")))
# View(t1_q_lang_fm)

# ETHNICITY fm
t1_q_ethnicity_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, DOB_fm, starts_with("q1_")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  
  mutate(
    MultiEth_fm1 = sum(c_across(q1_aa_fm1:q1_ot_fm1), na.rm=T)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    MultiEth_fm1,
    RaceEth_fm1 = case_when(
      MultiEth_fm1  ==  0 ~ 0,
      MultiEth_fm1  ==  1 && q1_aa_fm1 == 1 ~ 1,
      MultiEth_fm1  ==  1 && q1_ai_fm1 == 1 ~ 2,
      MultiEth_fm1  ==  1 && q1_as_fm1 == 1 ~ 3,
      MultiEth_fm1  ==  1 && q1_pi_fm1 == 1 ~ 4,
      MultiEth_fm1  ==  1 && q1_wh_fm1 == 1 ~ 5,
      MultiEth_fm1  ==  1 && q1_hi_fm1 == 1 ~ 6,
      MultiEth_fm1  ==  1 && q1_me_fm1 == 1 ~ 7,
      MultiEth_fm1  ==  1 && q1_ot_fm1 == 1 ~ 8,
      MultiEth_fm1  >=  2 ~ 9,
      q1_wh_fm1 == 1 && q1_hi_fm1 == 1 ~ 10,
      q1_aa_fm1 == 1 && q1_hi_fm1 == 1 ~ 11,
      q1_as_fm1 == 1 && q1_hi_fm1 == 1 ~ 12,
      
      TRUE ~ NA_real_ ),
    p_hisp_fm1 = ifelse(q1_hi_fm1 == 1 && !is.na(q1_hi_fm1), 1, 0)
  ) |>
  mutate(
    RaceGrp_fm1 = case_when(
      RaceEth_fm1 %in% c(5, 10) ~ 1,
      RaceEth_fm1 %in% c(1, 11) ~ 2,
      RaceEth_fm1 %in% c(3, 12) ~ 3,
      TRUE ~ 4
    )
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q1")))
# View(t1_q_ethnicity_fm)


##########
# Questionnaire Section 1

####
# SOCIO DEMOGRAPHIC

# RELATIONSHIP DURATION fm
t1_q_rlpdur_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q2")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    rlpdurmn_fm1 = sum(c(q2_yr_fm1*12, q2_mn_fm1), na.rm=T),
    sd_rlpdurmn_fm1 = sd(c(q2_yr_fm1*12, q2_mn_fm1), na.rm=T)
    
  ) |>
  mutate(rlpduryr_fm1 = rlpdurmn_fm1/12) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q2")))
# # # View(t1_q_rlpdur_fm)

# EDUCATION fm
t1_q_edu_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q3")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    edulow_fm = ifelse(q3_fm1 %in% 1:3, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q3")))
# # # View(t1_q_edu_fm)

# HOUSEHOLD SIZE fm
t1_q_minor_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q4")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    minor_fm1 = ifelse(q4.1_fm1 %in% c(0, NA_real_), 0, 1)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q4")))
# # # View(t1_q_minor_fm)

# INCOME fm
t1_q_inc_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q5")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    inclow_fm = ifelse(q5_fm1 %in% 1:4, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q5")))
# # # View(t1_q_inc_fm)

# EMPLOYMENT fm
t1_q_employ_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q6")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    employed_fm = ifelse(q6_fm1 %in% 1:3, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q6")))
# # # View(t1_q_employ_fm)

# CITIZENSHIP & COUNTRY OF BIRTH/ORIGIN fm
t1_q_liveUS_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q7")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    liveUSmn_fm1 = sum(c(q7.1_yr_fm1 * 12, q7.1_mn_fm1), na.rm=T),
    
    sd_liveUSmn_fm1 = sd(c(q7.1_yr_fm1 * 12, q7.1_mn_fm1), na.rm=T)
  ) |>
  mutate(liveUSyr_fm1 = liveUSmn_fm1 / 12) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q7")))
# # View(t1_q_liveUS_fm)


### MEDICAL HISTORY

# Cig/Tobacco Use (Q8a) and Exercise (Q8b) in Last 2 hours PT
t1_q_smokeexc_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q8")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    smoked_fm1 = ifelse(q8a_fm1 == 1, 1, 0),
    exced_fm1 = ifelse(q8b_fm1 == 1, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q8")))
# # # # View(t1_q_smokeexc_fm)

#  Food/Drink, Coffee, Tea, and Caffeine Intake in Last 4 hours
t1_q_intake_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, q9_fm1:q12_fm1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    atedrnk_fm1 = ifelse(q9_fm1 == 1, 1, 0),
    cof_fm1 = ifelse(q10_fm1 == 1, 1, 0),
    tea_fm1 = ifelse(q11_fm1 == 1, 1, 0),
    caff_fm1 = ifelse(q12_fm1 == 1, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q9-12")))
# # # # View(t1_q_intake_fm)

#  Drugs/Meds and Alcohol Intake in Last 24 hours
t1_q_drgmedalc_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, q13_fm1:q14_fm1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    drgmed_fm1 = ifelse(q13_fm1 == 1, 1, 0),
    alc_fm1 = ifelse(q14_fm1 == 1, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q13-14")))
# # # # View(t1_q_drgmedalc_fm)

# Medical Morbidity = Modified MICCI: Morbidities Index for Caregivers of Chronic Illnesses
MICCIdf_fm = select(t1_q_fm_orig, starts_with("q15"))
MICCIvars_fm = names(MICCIdf_fm)[grepl("^[^_]*[^0-9]_[^_]*$", names(MICCIdf_fm))][1:40]
MICCIvars2_fm = c("q15a2_fm1", "q15d2_fm1", "q15k2_fm1", "q15l2_fm1", "q15r2_fm1", "q15u2_fm1", 
                  "q15w2_fm1", "q15z2_fm1", "q15bb2_fm1", "q15dd2_fm1", "q15gg2_fm1", "q15ii2_fm1")
MICCInames_fm = c("hypertension_medtx_fm1", "cancer_medtx_fm1", "cholesterol_medtx_fm1", 
                  "heart_medtx_fm1", "kidney_medtx_fm1", "gallbaldder_medtx_fm1", "liver_medtx_fm1", 
                  "mentalhealth_medtx_fm1", "diabetes_medtx_fm1", "pulmonary_medtx_fm1", 
                  "thyroid_medtx_fm1", "GI_medtx_fm1")

t1_q_MICCI_fm <- t1_q_fm_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q15"))|>
  # mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate(
    morb_fm1 = rowSums(across(all_of(MICCIvars_fm)) == 1, na.rm=T)
  ) |>
  mutate(
    psychmorb_fm1 = rowSums(across(c(q15p_fm1, q15z_fm1)) == 1, na.rm = T)
  ) |>
  mutate(
    psychmorb_dich_fm1 = ifelse(psychmorb_fm1 >= 1, 1, 0),
    physmorb_fm1 = morb_fm1 - psychmorb_fm1
  ) |>
  mutate(
    physmorb_dich_fm1 = ifelse(physmorb_fm1 >= 1, 1, physmorb_fm1)
  ) |>
  mutate_at(
    vars(MICCIvars2_fm), ~ifelse(is.na(.) | . != 1, 0, 1)
  ) |>
  mutate(
    setNames(across(MICCIvars2_fm), MICCInames_fm)
  ) |>
  transmute(
    BiPs_IID,
    BiPs_DID,
    
    morb_fm1,
    psychmorb_fm1,
    psychmorb_dich_fm1,
    physmorb_fm1,
    physmorb_dich_fm1,
    med_fm1 = rowSums(across(MICCInames_fm) == 1, na.rm=T)
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q15")))
# # View(t1_q_MICCI_fm)

# Healthcare utilization: 13 Over or Under utilizations during the past 12 months FM
hcareut_df = select(t1_q_fm_orig, q16a_fm1:q16m_times_fm1)
hcareut_vars = names(hcareut_df)[grepl("^([^_]*_){1}[^_]*$", names(hcareut_df))]
hcareut_times_vars = grep("times", names(hcareut_df), value=TRUE)
t1_q_hcareut_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, q16a_fm1:q16m_times_fm1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    hcareut_fm1 = rowSums(across(all_of(hcareut_vars)) == 1, na.rm=T),
    hcareut_ov_fm1 = rowSums(across(all_of(hcareut_vars[1:7])) == 1, na.rm=T),
    hcareut_ud_fm1 = rowSums(across(all_of(hcareut_vars[8:13])) == 1, na.rm=T),
    
    hcareut_sum_fm1 = sum(c_across(all_of(hcareut_times_vars)), na.rm=T),
    hcareut_ovsum_fm1 = sum(c_across(all_of(hcareut_times_vars[1:7])), na.rm=T),
    hcareut_udsum_fm1 = sum(c_across(all_of(hcareut_times_vars[8:13])), na.rm=T),
    
    sd_hcareut_fm1 = sd(across(all_of(hcareut_vars)), na.rm=T),
    sd_hcareut_ov_fm1 = sd(across(all_of(hcareut_vars[1:7])), na.rm=T),
    sd_hcareut_ud_fm1 = sd(across(all_of(hcareut_vars[8:13])), na.rm=T),
    
    sd_hcareut_sum_fm1 = sd(c_across(all_of(hcareut_times_vars)), na.rm=T),
    sd_hcareut_ovsum_fm1 = sd(c_across(all_of(hcareut_times_vars[1:7])), na.rm=T),
    sd_hcareut_udsum_fm1 = sd(c_across(all_of(hcareut_times_vars[8:13])), na.rm=T)
  ) |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q16a-m")))
# # # View(t1_q_hcareut_fm)

# Personal/Family History CRC and Related Diseases FM
t1_q_crchist_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q16")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    pcrchist_fm1 = ifelse(q16n_fm1 == 1, 1, 0),
    fcrchist_fm1 = ifelse(q16p_fm1 == 1, 1, 0)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q16np")))
# # # View(t1_q_crchist_fm)


#  Smoking & Drinking FM
t1_q_drink_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, q17a_fm1:q20_fm1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    smoking_fm1 = recode(q17a_fm1, `1` = 0, `2` = 1, .missing = 0),
    othtob_fm1 = recode(q18_fm1, `1` = 1, `2` = 0, .missing = 0),
    hvdrink_fm1 = recode(q20_fm1, `1` = 0, `2` = 0, `3` = 0, `4` = 1, .missing = 0),
    drinkpday_fm1 = recode(q20_fm1, `1` = 0, `2` = 1, `3` = 2, `4` = 3, .missing = 0)
  ) |>
  mutate(
    hvdrinkpday_fm1 = recode(drinkpday_fm1, `0` = 0, `1` = 0, `2` = 1, `3` = 1)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q17-20")))
# # # View(t1_q_drink_fm)

# DAST drug screening questionnaire FM
t1_q_DAST_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q21")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q21a_mth_fm1:q21a_no_fm1), ~ case_when(
      . == 1 ~ 1,
      TRUE ~ 0 )
  ) |>
  mutate_at(
    vars(q21b_fm1:q21l_fm1), ~ case_when(
      . == 1 ~ 1,
      . == 2 ~ 0,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q21m_fm1:q21n_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 1,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    totdrug_fm1 = sum(c_across(q21a_mth_fm1:q21a_ot_fm1), na.rm=T),
    dast_fm1 = sum(c_across(q21b_fm1:q21n_fm1), na.rm=T),
    
    sd_totdrug_fm1 = sd(c_across(q21a_mth_fm1:q21a_no_fm1), na.rm=T),
    sd_dast_fm1 = sd(c_across(q21b_fm1:q21n_fm1), na.rm=T)
  ) |>
  mutate(
    druguse_fm1 = recode(totdrug_fm1, `0` = 0, .default = 1),
    dastzone_fm1 = recode(dast_fm1, `0` = 1, `1` = 2, `2` = 2, `3` = 3, 
                          `4` = 3, `5` = 3, `6` = 4, `7` = 4)
  ) |> 
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q21")))
# # View(t1_q_DAST_fm)



##########
# Questionnaire Section 2
# OPTIMISM FM
t1_q_LOTR_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q35")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q35a_fm1:q35f_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q35b_fm1, q35d_fm1:q35e_fm1), ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    LOTR_fm1 = mean(c_across(q35a_fm1:last_col()), na.rm=T),
    sd_LOTR_fm1 = sd(c_across(q35a_fm1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q35")))
# # # # # View(t1_q_LOTR_fm)

# BIG 5 FM
t1_q_TIPI_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q36")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q36b_fm1, q36f_fm1, q36h_fm1:q36j_fm1), ~ case_when(
      . == 1 ~ 7,
      . == 2 ~ 6,
      . == 3 ~ 5,
      . == 4 ~ 4,
      . == 5 ~ 3,
      . == 6 ~ 2,
      . == 7 ~ 1,
      TRUE ~ . )
  )  |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    extraversion_TIPI_fm1 = mean(c_across(c(q36a_fm1, q36f_fm1 )), na.rm=T),
    agreeableness_TIPI_fm1 = mean(c_across(c(q36b_fm1, q36g_fm1 )), na.rm=T),
    conscientiousness_TIPI_fm1 = mean(c_across(c(q36c_fm1, q36h_fm1 )), na.rm=T),
    neuroticism_TIPI_fm1 = mean(c_across(c(q36d_fm1, q36i_fm1 )), na.rm=T),
    openness_TIPI_fm1 = mean(c_across(c(q36e_fm1, q36j_fm1 )), na.rm=T),
    
    sd_extraversion_TIPI_fm1 = sd(c_across(c(q36a_fm1, q36f_fm1 )), na.rm=T),
    sd_agreeableness_TIPI_fm1 = sd(c_across(c(q36b_fm1, q36g_fm1 )), na.rm=T),
    sd_conscientiousness_TIPI_fm1 = sd(c_across(c(q36c_fm1, q36h_fm1 )), na.rm=T),
    sd_neuroticism_TIPI_fm1 = sd(c_across(c(q36d_fm1, q36i_fm1 )), na.rm=T),
    sd_openness_TIPI_fm1 = sd(c_across(c(q36e_fm1, q36j_fm1 )), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q36")))
# # # # # View(t1_q_TIPI_fm)

# URGENCY, POSITIVE URGENCY, SELF CONTROL FM
t1_q_impulse_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q37")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q37k_fm1, q37u_fm1:q37x_fm1, q37z_fm1, q37bb_fm1:q37cc_fm1, 
         q37ee_fm1:q37ff_fm1), ~ case_when(
           . == 1 ~ 5,
           . == 2 ~ 4,
           . == 3 ~ 3,
           . == 4 ~ 2,
           . == 5 ~ 1,
           TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    urgency_fm1 = mean(c_across(q37a_fm1:q37l_fm1), na.rm=T),
    pos_urgency_fm1 = mean(c_across(q37m_fm1:q37s_fm1), na.rm=T),
    control_fm1 = sum(c_across(q37t_fm1:last_col()), na.rm=T),
    
    sd_urgency_fm1 = sd(c_across(q37a_fm1:q37l_fm1), na.rm=T),
    sd_pos_urgency_fm1 = sd(c_across(q37m_fm1:q37s_fm1), na.rm=T),
    sd_control_fm1 = sd(c_across(q37t_fm1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q37")))
# # # # # View(t1_q_impulse_fm)

# UNCERTAINTY FM
t1_q_uncertainty_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, 
                                             starts_with("q38")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q38a_fm1:q38e_fm1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ . )
  )  |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    uncertain_fm1 = mean(c_across(q38a_fm1:q38e_fm1), na.rm=T),
    sd_uncertain_fm1 = sd(c_across(q38a_fm1:q38e_fm1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q38")))
# # # # # View(t1_q_uncertainty_fm)

# Measurement of Attachment Quality FM
t1_q_maq_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q50")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q50d_fm1, q50h_fm1:q50i_fm1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      . == 5 ~ 0,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q50a_fm1:last_col()), ~ case_when(
      . == 1 ~ 1,
      . == 2 ~ 2,
      . == 3 ~ 3,
      . == 4 ~ 4,
      . == 5 ~ NA_real_,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    maq_sec_fm1 = mean(c_across(c(q50a_fm1, q50g_fm1, q50n_fm1)), na.rm=T),
    maq_avd_fm1 = mean(c_across(c(q50d_fm1, q50f_fm1, q50h_fm1, q50k_fm1, q50m_fm1)), na.rm=T),
    maq_ambw_fm1 = mean(c_across(c(q50b_fm1, q50e_fm1, q50i_fm1)), na.rm=T),
    maq_ambm_fm1 = mean(c_across(c(q50c_fm1, q50j_fm1, q50l_fm1)), na.rm=T),
    
    sd_maq_sec_fm1 = sd(c_across(c(q50a_fm1, q50g_fm1, q50n_fm1)), na.rm=T),
    sd_maq_avd_fm1 = sd(c_across(c(q50d_fm1, q50f_fm1, q50h_fm1, q50k_fm1, q50m_fm1)), na.rm=T),
    sd_maq_ambw_fm1 = sd(c_across(c(q50b_fm1, q50e_fm1, q50i_fm1)), na.rm=T),
    sd_maq_ambm_fm1 = sd(c_across(c(q50c_fm1, q50j_fm1, q50l_fm1)), na.rm=T)
  ) |>
  mutate(
    across(maq_avd_fm1, ~ ifelse(is.na(.), NA_real_, 5 - .), .names = "{col}")
  ) |>
  mutate(
    att_dep_fm1 = mean(c_across(c(maq_sec_fm1, maq_avd_fm1)), na.rm=T),
    att_anx_fm1 = mean(c_across(c(maq_ambw_fm1, maq_ambm_fm1)), na.rm=T),
    
    sd_att_dep_fm1 = sd(c_across(c(maq_sec_fm1, maq_avd_fm1)), na.rm=T),
    sd_att_anx_fm1 = sd(c_across(c(maq_ambw_fm1, maq_ambm_fm1)), na.rm=T)
  ) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q50")))
# # # # # View(t1_q_maq_fm)

# SOURCES OF SOCIAL SUPPORT FM
t1_q_ssss_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, 
                                      starts_with("q51")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q51i_fm1:q51j_fm1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ . )
  )  |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    ssss_neg_fm1 = mean(c_across(q51g_fm1:q51h_fm1), na.rm=T),
    ssss_info_fm1 = q51a_fm1,
    ssss_inst_fm1 = q51b_fm1,
    ssss_positive_fm1 = mean(c_across(q51a_fm1:q51f_fm1), na.rm=T),
    ssss_negative_fm1 = mean(c_across(q51g_fm1:q51j_fm1), na.rm=T),
    ssss_negative_fm1r = mean(c_across(q51g_fm1:q51h_fm1), na.rm=T),
    
    sd_ssss_neg_fm1 = sd(c_across(q51g_fm1:q51h_fm1), na.rm=T),
    sd_ssss_info_fm1 = q51a_fm1,
    sd_ssss_inst_fm1 = q51b_fm1,
    sd_ssss_positive_fm1 = sd(c_across(q51a_fm1:q51f_fm1), na.rm=T),
    sd_ssss_negative_fm1 = sd(c_across(q51g_fm1:q51j_fm1), na.rm=T),
    sd_ssss_negative_fm1r = sd(c_across(q51g_fm1:q51h_fm1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q51")))
# # # # # View(t1_q_ssss_fm)

# DUKE RELIGIOSITY FM
t1_q_DUREL_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, 
                                       starts_with(c("q39","q40"))) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate(religaffil_fm1 = case_when(
    q39_fm1 == 7 | is.na(q39_fm1) ~ 0,
    TRUE ~ 1
  )
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q39-40")))
# # # # # View(t1_q_DUREL_fm)

# SOCIAL CONSTRAINT FM
t1_q_SCC_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, 
                                     starts_with("q41")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    constraint_fm1 = sum(c_across(q41a_fm1:last_col()), na.rm=T),
    sd_constraint_fm1 = sd(c_across(q41a_fm1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q41")))
# # # # # View(t1_q_SCC_fm)

# DYADIC ADJUSTMENT FM
t1_q_DAS4_fm = t1_q_fm_orig |> 
  select(BiPs_IID, BiPs_DID, q52_fm1:q55_fm1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q52_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      . == 6 ~ 5,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q53_fm1:q54_fm1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      . == 6 ~ 0,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q55_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      . == 6 ~ 5,
      . == 7 ~ 6,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    das_fm1 = sum(c_across(q52_fm1:last_col()), na.rm=T),
    sd_das_fm1 = sd(c_across(q52_fm1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q52-55")))
# # # # # View(t1_q_DAS4_fm)

# RELATIONSHIP QUALITY FM
t1_q_RQS_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, 
                                     starts_with("q56")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    rqi_fm1 = mean(c_across(q56a_fm1:q56f_fm1), na.rm=T),
    sd_rqi_fm1 = sd(c_across(q56a_fm1:q56f_fm1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q56")))
# # # # # View(t1_q_RQS_fm)

# DIMENSIONS OF CARE TASK FM ONLY
t1_q_dimensions_fm <- t1_q_fm_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q42"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(ends_with("2_fm1")), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(ends_with("3_fm1")), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    CTask_1 = rowSums(across(ends_with("1_fm1")) == 1, na.rm=T),
    CTaskEMO_1 = rowSums(across(c(q42a1_fm1, q42g1_fm1, q42i1_fm1, q42k1_fm1)) == 1, na.rm=T),
    CTaskINS_1 = rowSums(across(c(q42d1_fm1, q42l1_fm1, q42o1_fm1, q42p1_fm1)) == 1, na.rm=T), 
    CTaskTAN_1 = rowSums(across(c(q42b1_fm1, q42c1_fm1, q42f1_fm1, q42j1_fm1, q42n1_fm1)) == 1, na.rm=T),
    CTaskMED_1 = rowSums(across(c(q42e1_fm1, q42h1_fm1, q42m1_fm1, q42q1_fm1)) == 1, na.rm=T),
    
    CTaskfreq_EMO_1 = mean(c_across(c(q42a2_fm1, q42g2_fm1, q42i2_fm1, q42k2_fm1)), na.rm=T),
    CTaskfreq_INS_1 = mean(c_across(c(q42d2_fm1, q42l2_fm1, q42o2_fm1, q42p2_fm1)), na.rm=T),
    CTaskfreq_TAN_1 = mean(c_across(c(q42b2_fm1, q42c2_fm1, q42f2_fm1, q42j2_fm1, q42n2_fm1)), na.rm=T),  
    CTaskfreq_MED_1 = mean(c_across(c(q42e2_fm1, q42h2_fm1, q42m2_fm1, q42q2_fm1)), na.rm=T),
    
    CTaskdiff_EMO_1 = mean(c_across(c(q42a3_fm1, q42g3_fm1, q42i3_fm1, q42k3_fm1)), na.rm=T),
    CTaskdiff_INS_1 = mean(c_across(c(q42d3_fm1, q42l3_fm1, q42o3_fm1, q42p3_fm1)), na.rm=T),
    CTaskdiff_TAN_1 = mean(c_across(c(q42b3_fm1, q42c3_fm1, q42f3_fm1, q42j3_fm1, q42n3_fm1)), na.rm=T),  
    CTaskdff_MED_1 = mean(c_across(c(q42e3_fm1, q42h3_fm1, q42m3_fm1, q42q3_fm1)), na.rm=T),
    
    sd_CTask_1 = sd(c_across(ends_with("1_fm1")), na.rm=T),
    sd_CTaskEMO_1 = sd(c_across(c(q42a1_fm1, q42g1_fm1, q42i1_fm1, q42k1_fm1)), na.rm=T),
    sd_CTaskINS_1 = sd(c_across(c(q42d1_fm1, q42l1_fm1, q42o1_fm1, q42p1_fm1)), na.rm=T), 
    sd_CTaskTAN_1 = sd(c_across(c(q42b1_fm1, q42c1_fm1, q42f1_fm1, q42j1_fm1, q42n1_fm1)), na.rm=T),
    sd_CTaskMED_1 = sd(c_across(c(q42e1_fm1, q42h1_fm1, q42m1_fm1, q42q1_fm1)), na.rm=T),
    
    sd_CTaskfreq_EMO_1 = sd(c_across(c(q42a2_fm1, q42g2_fm1, q42i2_fm1, q42k2_fm1)), na.rm=T),
    sd_CTaskfreq_INS_1 = sd(c_across(c(q42d2_fm1, q42l2_fm1, q42o2_fm1, q42p2_fm1)), na.rm=T),
    sd_CTaskfreq_TAN_1 = sd(c_across(c(q42b2_fm1, q42c2_fm1, q42f2_fm1, q42j2_fm1, q42n2_fm1)), na.rm=T),  
    sd_CTaskfreq_MED_1 = sd(c_across(c(q42e2_fm1, q42h2_fm1, q42m2_fm1, q42q2_fm1)), na.rm=T),
    
    sd_CTaskdiff_EMO_1 = sd(c_across(c(q42a3_fm1, q42g3_fm1, q42i3_fm1, q42k3_fm1)), na.rm=T),
    sd_CTaskdiff_INS_1 = sd(c_across(c(q42d3_fm1, q42l3_fm1, q42o3_fm1, q42p3_fm1)), na.rm=T),
    sd_CTaskdiff_TAN_1 = sd(c_across(c(q42b3_fm1, q42c3_fm1, q42f3_fm1, q42j3_fm1, q42n3_fm1)), na.rm=T),  
    sd_CTaskdff_MED_1 = sd(c_across(c(q42e3_fm1, q42h3_fm1, q42m3_fm1, q42q3_fm1)), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q42")))
# # # # View(t1_q_dimensions_fm)


# ACTIVITIES OF DAILY LIVING (ADL) & IADL FM ONLY
t1_q_ADL_fm <- t1_q_fm_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q43"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(ends_with("3_fm1")), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    ADL_1 = rowSums(across(ends_with("1_fm1")) == 2, na.rm=T),
    ADL_fm1 = rowSums(across(ends_with("2_fm1")) == 2, na.rm=T),
    ADL_upset1 = mean(c_across(ends_with("3_fm1")), na.rm=T),
    
    sd_ADL_1 = sd(across(ends_with("1_fm1")) == 2, na.rm=T),
    sd_ADL_fm1 = sd(across(ends_with("2_fm1")) == 2, na.rm=T),
    sd_ADL_upset1 = sd(c_across(ends_with("3_fm1")), na.rm=T)
    
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q43")))
# # # # View(t1_q_ADL_fm)

# CAREGIVING DURATION PER DAY FM ONLY
t1_q_caredur_fm <- t1_q_fm_orig |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    care_dur_fm1 = q44_fm1 
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q44")))
# # # # View(t1_q_caredur_fm)

# CAREGIVING STRESS FM ONLY
t1_q_carestress_fm <- t1_q_fm_orig |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    c_overload1 = mean(c_across(starts_with("q45")), na.rm=T),
    
    sd_c_overload1 = sd(c_across(starts_with("q45")), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q45")))
# # # # View(t1_q_carestress_fm)

# CAREGIVING MOTIVATION FM ONLY
t1_q_caremot_fm <- t1_q_fm_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q46"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    mot_auto1_fm1 = mean(c_across(c(q46h_fm1, q46d_fm1, q46a_fm1, q46e_fm1)), na.rm=T),
    mot_ex1_fm1 = mean(c_across(c(q46b_fm1, q46g_fm1)), na.rm=T),
    mot_ij1_fm1 = mean(c_across(c(q46c_fm1, q46f_fm1)), na.rm=T),
    mot_cont1_fm1 = mean(c_across(c(q46b_fm1, q46g_fm1, q46c_fm1, q46f_fm1)), na.rm=T),
    
    sd_mot_auto1_fm1 = sd(c_across(c(q46h_fm1, q46d_fm1, q46a_fm1, q46e_fm1)), na.rm=T),
    sd_mot_ex1_fm1 = sd(c_across(c(q46b_fm1, q46g_fm1)), na.rm=T),
    sd_mot_ij1_fm1 = sd(c_across(c(q46c_fm1, q46f_fm1)), na.rm=T),
    sd_mot_cont1_fm1 = sd(c_across(c(q46b_fm1, q46g_fm1, q46c_fm1, q46f_fm1)), na.rm=T)) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q46")))
# # # # View(t1_q_caremot_fm)


# CAREGIVING EXPERIENCES FM ONLY
t1_q_cra_fm = t1_q_fm_orig |> 
  select(BiPs_IID, BiPs_DID, q47a_fm1:q49_fm1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q47e_fm1, q47i_fm1, q47j_fm1, q47r_fm1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ . )
  ) |> 
  mutate_at(
    vars(q48_fm1), ~ case_when(
      . == 2 ~ 1,
      is.na(q48_fm1) ~ 0,
      TRUE ~ 0 )
  ) |>
  mutate_at(
    vars(q49_fm1), ~ case_when(
      . == 1 ~ 1,
      . == 2 ~ 0,
      is.na(q49_fm1) ~ 0,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    cra_EST_1 = mean(c_across(c(q47a_fm1, q47e_fm1, q47g_fm1, q47i_fm1, q47o_fm1, q47p_fm1, q47q_fm1)), na.rm=T),
    cra_LFS_1 = mean(c_across(c(q47b_fm1, q47d_fm1, q47f_fm1, q47j_fm1, q47m_fm1)), na.rm=T), 
    cra_IOS_1 = mean(c_across(c(q47c_fm1, q47h_fm1, q47k_fm1, q47l_fm1, q47n_fm1)), na.rm=T),
    cra_IOF_1 = mean(c_across(c(q47r_fm1, q47s_fm1, q47t_fm1)), na.rm=T),
    
    hpothill_fm1 = q48_fm1,
    help_others_fm1 = q49_fm1,
    
    sd_cra_EST_1 = sd(c_across(c(q47a_fm1, q47e_fm1, q47g_fm1, q47i_fm1, q47o_fm1, q47p_fm1, q47q_fm1)), na.rm=T),
    sd_cra_LFS_1 = sd(c_across(c(q47b_fm1, q47d_fm1, q47f_fm1, q47j_fm1, q47m_fm1)), na.rm=T), 
    sd_cra_IOS_1 = sd(c_across(c(q47c_fm1, q47h_fm1, q47k_fm1, q47l_fm1, q47n_fm1)), na.rm=T),
    sd_cra_IOF_1 = sd(c_across(c(q47r_fm1, q47s_fm1, q47t_fm1)), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q47-49")))
# # # # View(t1_q_cra_fm)



##########
# Questionnaire Section 3
# PERCEIVED STRESS FM
t1_q_PSS_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q57")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q57a_fm1:q57c_fm1, q57f_fm1, q57i_fm1:q57j_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q57d_fm1:q57e_fm1, q57g_fm1:q57h_fm1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      . == 5 ~ 0,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    pss_fm1 = sum(c_across(q57a_fm1:last_col()), na.rm=T),
    sd_pss_fm1 = sd(c_across(q57a_fm1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q57")))
# # # # # View(t1_q_PSS_fm)

# B-COPE: Brief COPE + Emotional Approach and Emotional Processing FM
t1_q_bcope_fm <- t1_q_fm_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q58"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    # syntax has labels for composites
    bcope_SD_fm1 = mean(c_across(c(q58a_fm1, q58o_fm1)), na.rm=T),
    bcope_AC_fm1 = mean(c_across(c(q58b_fm1, q58f_fm1)), na.rm=T), 
    bcope_DN_fm1 = mean(c_across(c(q58c_fm1, q58h_fm1)), na.rm=T),  
    bcope_SU_fm1 = mean(c_across(c(q58d_fm1, q58i_fm1)), na.rm=T), 
    bcope_ES_fm1 = mean(c_across(c(q58w_fm1, q58aa_fm1)), na.rm=T), 
    bcope_IS_fm1 = mean(c_across(c(q58y_fm1, q58ee_fm1)), na.rm=T), 
    bcope_BD_fm1 = mean(c_across(c(q58e_fm1, q58m_fm1)), na.rm=T), 
    bcope_VT_fm1 = mean(c_across(c(q58x_fm1, q58dd_fm1)), na.rm=T),
    bcope_PR_fm1 = mean(c_across(c(q58z_fm1, q58bb_fm1)), na.rm=T), 
    bcope_PL_fm1 = mean(c_across(c(q58l_fm1, q58r_fm1)), na.rm=T), 
    bcope_HR_fm1 = mean(c_across(c(q58n_fm1, q58u_fm1)), na.rm=T),
    bcope_ACC_fm1 = mean(c_across(c(q58cc_fm1, q58ff_fm1)), na.rm=T),
    bcope_RL_fm1 = mean(c_across(c(q58q_fm1, q58t_fm1)), na.rm=T), 
    bcope_SB_fm1 = mean(c_across(c(q58k_fm1, q58s_fm1)), na.rm=T), 
    cope_ee_fm1 = mean(c_across(c(q58j_fm1, q58v_fm1)), na.rm=T),
    cope_ep_fm1 = mean(c_across(c(q58g_fm1, q58p_fm1)), na.rm=T),
    
    
    sd_bcope_SD_fm1 = sd(c_across(c(q58a_fm1, q58o_fm1)), na.rm=T),
    sd_bcope_AC_fm1 = sd(c_across(c(q58b_fm1, q58f_fm1)), na.rm=T), 
    sd_bcope_DN_fm1 = sd(c_across(c(q58c_fm1, q58h_fm1)), na.rm=T),  
    sd_bcope_SU_fm1 = sd(c_across(c(q58d_fm1, q58i_fm1)), na.rm=T), 
    sd_bcope_ES_fm1 = sd(c_across(c(q58w_fm1, q58aa_fm1)), na.rm=T), 
    sd_bcope_IS_fm1 = sd(c_across(c(q58y_fm1, q58ee_fm1)), na.rm=T), 
    sd_bcope_BD_fm1 = sd(c_across(c(q58e_fm1, q58m_fm1)), na.rm=T), 
    sd_bcope_VT_fm1 = sd(c_across(c(q58x_fm1, q58dd_fm1)), na.rm=T),
    sd_bcope_PR_fm1 = sd(c_across(c(q58z_fm1, q58bb_fm1)), na.rm=T), 
    sd_bcope_PL_fm1 = sd(c_across(c(q58l_fm1, q58r_fm1)), na.rm=T), 
    sd_bcope_HR_fm1 = sd(c_across(c(q58n_fm1, q58u_fm1)), na.rm=T),
    sd_bcope_ACC_fm1 = sd(c_across(c(q58cc_fm1, q58ff_fm1)), na.rm=T),
    sd_bcope_RL_fm1 = sd(c_across(c(q58q_fm1, q58t_fm1)), na.rm=T), 
    sd_bcope_SB_fm1 = sd(c_across(c(q58k_fm1, q58s_fm1)), na.rm=T), 
    sd_cope_ee_fm1 = sd(c_across(c(q58j_fm1, q58v_fm1)), na.rm=T),
    sd_cope_ep_fm1 = sd(c_across(c(q58g_fm1, q58p_fm1)), na.rm=T)) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q58")))
# # # # # View(t1_q_bcope_fm)

# LONELINESS FM
t1_q_loneliness_fm = t1_q_fm_orig |> 
  select(BiPs_IID, BiPs_DID, starts_with("q59")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q59c_fm1:q59d_fm1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |> 
  transmute(
    BiPs_IID, 
    BiPs_DID,
    loneliness_fm1 = mean(c_across(q59a_fm1:last_col()), na.rm=T),
    sd_loneliness_fm1 = sd(c_across(q59a_fm1:last_col()), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q59")))
# # # # # View(t1_q_loneliness_fm)



#  ISEL - Interpersonal support evaluation list (perceived social support) FM
t1_q_ISEL_fm = t1_q_fm_orig |> 
  select(BiPs_IID, BiPs_DID, starts_with(c("q60","q61"))) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q60c_fm1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q61a_fm1:q61c_fm1), ~ case_when(
      . == 1 ~ NA_real_,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    ISEL_fm1 = mean(c_across(q60a_fm1:q60f_fm1), na.rm=T),
    ISEL_SSAA_fm1 = mean(c_across(c(q60b_fm1,q60f_fm1)), na.rm=T),
    ISEL_SSAB_fm1 = mean(c_across(c(q60a_fm1, q60e_fm1)), na.rm=T),
    ISEL_SSAT_fm1  = mean(c_across(q60c_fm1:q60d_fm1), na.rm=T),
    ISELST_fm1 = mean(c_across(q61a_fm1:q61c_fm1), na.rm=T),
    
    sd_ISEL_fm1 = sd(c_across(q60a_fm1:q60f_fm1), na.rm=T),
    sd_ISEL_SSAA_fm1 = sd(c_across(c(q60b_fm1,q60f_fm1)), na.rm=T),
    sd_ISEL_SSAB_fm1 = sd(c_across(c(q60a_fm1, q60e_fm1)), na.rm=T),
    sd_ISEL_SSAT_fm1  = sd(c_across(q60c_fm1:q60d_fm1), na.rm=T),
    sd_ISELST_fm1 = sd(c_across(q61a_fm1:q61c_fm1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q60-61")))
# # # # # View(t1_q_ISEL_fm)

# CANCER RELATED STRESS APPRAISAL FM
t1_q_castress_fm = t1_q_fm_orig |> 
  select(BiPs_IID, BiPs_DID, starts_with("q62")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q62.1b_fm1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    castress_SF_fm1 = mean(c_across(q62.1a_fm1:q62.1b_fm1), na.rm=T),
    castress_FA_fm1 = mean(c_across(q62.2a_fm1:q62.2e_fm1), na.rm=T),
    castress_tot_fm1 = mean(c_across(q62.1a_fm1:q62.2e_fm1), na.rm=T),
    
    sd_castress_SF_fm1 = sd(c_across(q62.1a_fm1:q62.1b_fm1), na.rm=T),
    sd_castress_FA_fm1 = sd(c_across(q62.2a_fm1:q62.2e_fm1), na.rm=T),
    sd_castress_tot_fm1 = sd(c_across(q62.1a_fm1:q62.2e_fm1), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q62")))
# # # # # View(t1_q_castress_fm)

# BAS/BIS Scale FM
t1_q_BAS_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q63")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q63a_fm1, q63r_fm1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      TRUE ~ . )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    drive_fm1 = sum(c_across(c(q63b_fm1, q63g_fm1, q63i_fm1, q63q_fm1)), 
                    na.rm=T),
    funseek_fm1 = sum(c_across(c(q63d_fm1, q63h_fm1, q63l_fm1, q63p_fm1)), 
                      na.rm=T),
    reward_fm1 = sum(c_across(c(q63c_fm1, q63e_fm1, q63k_fm1, q63n_fm1, 
                                q63s_fm1)), na.rm=T),
    inhibition_fm1 = sum(c_across(c(q63a_fm1, q63f_fm1, q63j_fm1, q63m_fm1, 
                                    q63o_fm1, q63r_fm1, q63t_fm1)), na.rm=T),
    
    
    sd_drive_fm1 = sd(c_across(c(q63b_fm1, q63g_fm1, q63i_fm1, q63q_fm1)), 
                      na.rm=T),
    sd_funseek_fm1 = sd(c_across(c(q63d_fm1, q63h_fm1, q63l_fm1, q63p_fm1)), 
                        na.rm=T),
    sd_reward_fm1 = sd(c_across(c(q63c_fm1, q63e_fm1, q63k_fm1, q63n_fm1, 
                                  q63s_fm1)), na.rm=T),
    sd_inhibition_fm1 = sd(c_across(c(q63a_fm1, q63f_fm1, q63j_fm1, q63m_fm1, 
                                      q63o_fm1, q63r_fm1, q63t_fm1)), na.rm=T)
  ) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q63")))
# # # # # View(t1_q_BAS_fm)

# Biculturalism FM 
# this derives predictors for *both* composite and all sub-scales, as per SPSS
t1_q_biculturalism_fm <- t1_q_fm_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q64"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    # this should follow exactly from SPSS syntax given
    languse_fm1 = mean(c_across(q64a_fm1:q64e_fm1), na.rm=T),
    media_fm1 = mean(c_across(q64f_fm1:q64h_fm1), na.rm=T),
    relations_fm1 = mean(c_across(q64i_fm1:q64l_fm1), na.rm=T),
    accultur_fm1 = mean(c_across(q64a_fm1:q64l_fm1), na.rm=T),
    # NOTE SD versions for diagnostic only , to be removed later in modeling
    sd_languse_fm1 = sd(c_across(q64a_fm1:q64e_fm1), na.rm=T),
    sd_media_fm1 = sd(c_across(q64f_fm1:q64h_fm1), na.rm=T),
    sd_relations_fm1 = sd(c_across(q64i_fm1:q64l_fm1), na.rm=T),
    sd_accultur_fm1 = sd(c_across(q64a_fm1:q64l_fm1), na.rm=T)) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q64")))
# # View(t1_q_biculturalism_fm)

# TODO the above code chunk can be repeated for each of the FM items in the above
# todo list. BiPs_IID and BiPs_DID should be included in the derived FM tables 
# (as shown above) so that the individual tables can be joined to form 
# a table of predictors for the questionnaire DB (containing both PT and FM)
# NOTE 1: let us derive predictors for *both* composite and all sub-scales 
# NOTE 2: some predictors will be available only for PT *or* FM, not both, e.g.,
# Caregiving Experiences is available only for FM. 

# Social Support Network Size FM
# Recoding
t1_q_SSN_r_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, q65_fm1:q68_fm1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(across(q65_fm1:q68_fm1, ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    . == 3 ~ 2,
    . == 4 ~ 3.5,
    . == 5 ~ 6.5,
    . == 6 ~ 9,
    TRUE ~ . )),
    BiPs_IID, 
    BiPs_DID) |> 
  transmute(
    BiPs_IID, 
    SSNS_quant_fm1 = sum(c_across(c(q65_fm1, q67_fm1)), na.rm=T),
    SSNS_rlose_fm1 = sum(c_across(c(q66_fm1, q68_fm1)), na.rm=T),
    sd_SSNS_quant_fm1 = sd(c_across(c(q65_fm1, q67_fm1)), na.rm=T),
    sd_SSNS_rlose_fm1 = sd(c_across(c(q66_fm1, q68_fm1)), na.rm=T),
    
  )

# NOTE: certain composites are calculated BEFORE reordering
t1_q_SSN_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, q65_fm1:q70_fm1) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    SSNS_fm1 = mean(c_across(q65_fm1:q68_fm1), na.rm=T),
    SSNS_fam_fm1 = mean(c_across(q65_fm1:q66_fm1), na.rm=T),
    SSNS_friend_fm1 = mean(c_across(q67_fm1:q68_fm1), na.rm=T),
    SSNTQ_fm1 = mean(c_across(q69_fm1:q70_fm1), na.rm=T),
    
    sd_SSNS_fm1 = sd(c_across(q65_fm1:q68_fm1), na.rm=T),
    sd_SSNS_fam_fm1 = sd(c_across(q65_fm1:q66_fm1), na.rm=T),
    sd_SSNS_friend_fm1 = sd(c_across(q67_fm1:q68_fm1), na.rm=T),
    sd_SSNTQ_fm1 = sd(c_across(q69_fm1:q70_fm1), na.rm=T)) |>
  
  merge(t1_q_SSN_r_fm, by = "BiPs_IID", all=TRUE) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q65-70")))
# # # # # View(t1_q_SSN_fm)

# FAMILISM FM
t1_q_familism_fm <- t1_q_fm_orig |>
  select( BiPs_IID, BiPs_DID, starts_with("q71"))|>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    familism_tot_fm1 = sum(c_across(q71a_fm1:q71n_fm1), na.rm=T),
    famob_fm1 = sum(c_across(q71a_fm1:q71f_fm1), na.rm=T),
    famsupport_fm1 = sum(c_across(c(q71g_fm1,q71h_fm1,q71n_fm1)), na.rm=T),
    famref_fm1 = sum(c_across(q71i_fm1:q71m_fm1), na.rm=T),
    
    sd_familism_tot_fm1 = sd(c_across(q71a_fm1:q71n_fm1), na.rm=T),
    sd_famob_fm1 = sd(c_across(q71a_fm1:q71f_fm1), na.rm=T),
    sd_famsupport_fm1 = sd(c_across(c(q71g_fm1,q71h_fm1,q71n_fm1)), na.rm=T),
    sd_famref_fm1 = sd(c_across(q71i_fm1:q71m_fm1), na.rm=T)) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_q71")))
# # View(t1_q_familism_fm)

################################################################################
# OUTCOMES

########## PT OUTCOME EXTRACTIONS

# PROMIS-29 PT
t1_q_promis_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q22")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q22a_pt1:q22d_pt1, q22q_pt1:q22r_pt1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ NA_real_ )
  ) |>
  
  transmute(
    BiPs_IID, 
    BiPs_DID,
    pro_phy_pt1 = sum(c_across(q22a_pt1:q22d_pt1), na.rm=T),
    pro_anx_pt1 = sum(c_across(q22e_pt1:q22h_pt1), na.rm=T),
    pro_dep_pt1 = sum(c_across(q22i_pt1:q22l_pt1), na.rm=T),
    pro_fat_pt1 = sum(c_across(q22m_pt1:q22p_pt1), na.rm=T),
    pro_slp_pt1 = sum(c_across(q22q_pt1:q22t_pt1), na.rm=T),
    pro_sor_pt1 = sum(c_across(q22u_pt1:q22x_pt1), na.rm=T),
    pro_paininfr_pt1 = sum(c_across(q22y_pt1:q22bb_pt1), na.rm=T),
    pro_painit_pt1 = q22cc_pt1,
    
    Tpro_phy_pt1 = case_when(
      pro_phy_pt1  ==  4 ~ 22.5,
      pro_phy_pt1 == 5 ~ 26.6,
      pro_phy_pt1 == 6 ~ 28.9,
      pro_phy_pt1 == 7 ~ 30.5,
      pro_phy_pt1 == 8 ~ 31.9,
      pro_phy_pt1 == 9 ~ 33.2,
      pro_phy_pt1 == 10 ~ 34.4,
      pro_phy_pt1 == 11 ~ 35.6,
      pro_phy_pt1 == 12 ~ 36.7,
      pro_phy_pt1 == 13 ~ 37.9,
      pro_phy_pt1 == 14 ~ 39.2,
      pro_phy_pt1 == 15 ~ 40.5,
      pro_phy_pt1 == 16 ~ 41.9,
      pro_phy_pt1 == 17 ~ 43.5,
      pro_phy_pt1 == 18 ~ 45.5,
      pro_phy_pt1 == 19 ~ 48.3,
      pro_phy_pt1 == 20 ~ 57.0,
      TRUE ~ NA_real_ ),
    Tpro_anx_pt1 = case_when(
      pro_anx_pt1 == 4 ~ 40.3,
      pro_anx_pt1 == 5 ~ 48.0,
      pro_anx_pt1 == 6 ~ 51.2,
      pro_anx_pt1 == 7 ~ 53.7,
      pro_anx_pt1 == 8 ~ 55.8,
      pro_anx_pt1 == 9 ~ 57.7,
      pro_anx_pt1 == 10 ~ 59.5,
      pro_anx_pt1 == 11 ~ 61.4,
      pro_anx_pt1 == 12 ~ 63.4,
      pro_anx_pt1 == 13 ~ 65.3,
      pro_anx_pt1 == 14 ~ 67.3,
      pro_anx_pt1 == 15 ~ 69.3,
      pro_anx_pt1 == 16 ~ 71.2,
      pro_anx_pt1 == 17 ~ 73.3,
      pro_anx_pt1 == 18 ~ 75.4,
      pro_anx_pt1 == 19 ~ 77.9,
      pro_anx_pt1 == 20 ~ 81.6,
      TRUE ~ NA_real_ ),
    Tpro_dep_pt1 = case_when(
      pro_dep_pt1 == 4 ~ 41.0,
      pro_dep_pt1 == 5 ~ 49.0,
      pro_dep_pt1 == 6 ~ 51.8,
      pro_dep_pt1 == 7 ~ 53.9,
      pro_dep_pt1 == 8 ~ 55.7,
      pro_dep_pt1 == 9 ~ 57.3,
      pro_dep_pt1 == 10 ~ 58.9,
      pro_dep_pt1 == 11 ~ 60.5,
      pro_dep_pt1 == 12 ~ 62.2,
      pro_dep_pt1 == 13 ~ 63.9,
      pro_dep_pt1 == 14 ~ 65.7,
      pro_dep_pt1 == 15 ~ 67.5,
      pro_dep_pt1 == 16 ~ 69.4,
      pro_dep_pt1 == 17 ~ 71.2,
      pro_dep_pt1 == 18 ~ 73.3,
      pro_dep_pt1 == 19 ~ 75.7,
      pro_dep_pt1 == 20 ~ 79.4,
      TRUE ~ NA_real_ ),
    Tpro_fat_pt1 = case_when(
      pro_fat_pt1 == 4 ~ 33.7,
      pro_fat_pt1 == 5 ~ 39.7,
      pro_fat_pt1 == 6 ~ 43.1,
      pro_fat_pt1 == 7 ~ 46.0,
      pro_fat_pt1 == 8 ~ 48.6,
      pro_fat_pt1 == 9 ~ 51.0,
      pro_fat_pt1 == 10 ~ 53.1,
      pro_fat_pt1 == 11 ~ 55.1,
      pro_fat_pt1 == 12 ~ 57.0,
      pro_fat_pt1 == 13 ~ 58.8,
      pro_fat_pt1 == 14 ~ 60.7,
      pro_fat_pt1 == 15 ~ 62.7,
      pro_fat_pt1 == 16 ~ 64.6,
      pro_fat_pt1 == 17 ~ 66.7,
      pro_fat_pt1 == 18 ~ 69.0,
      pro_fat_pt1 == 19 ~ 71.6,
      pro_fat_pt1 == 20 ~ 75.8,
      TRUE ~ NA_real_ ),
    Tpro_slp_pt1 = case_when(
      pro_slp_pt1 == 4 ~ 32.0,
      pro_slp_pt1 == 5 ~ 37.5,
      pro_slp_pt1 == 6 ~ 41.1,
      pro_slp_pt1 == 7 ~ 43.8,
      pro_slp_pt1 == 8 ~ 46.2,
      pro_slp_pt1 == 9 ~ 48.4,
      pro_slp_pt1 == 10 ~ 50.5,
      pro_slp_pt1 == 11 ~ 52.4,
      pro_slp_pt1 == 12 ~ 54.3,
      pro_slp_pt1 == 13 ~ 56.1,
      pro_slp_pt1 == 14 ~ 57.9,
      pro_slp_pt1 == 15 ~ 59.8,
      pro_slp_pt1 == 16 ~ 61.7,
      pro_slp_pt1 == 17 ~ 63.8,
      pro_slp_pt1 == 18 ~ 66.0,
      pro_slp_pt1 == 19 ~ 68.8,
      pro_slp_pt1 == 20 ~ 73.3,
      TRUE ~ NA_real_ ),
    Tpro_sor_pt1 = case_when(
      pro_sor_pt1 == 4 ~ 27.5,
      pro_sor_pt1 == 5 ~ 31.8,
      pro_sor_pt1 == 6 ~ 34.0,
      pro_sor_pt1 == 7 ~ 35.7,
      pro_sor_pt1 == 8 ~ 37.3,
      pro_sor_pt1 == 9 ~ 38.8,
      pro_sor_pt1 == 10 ~ 40.5,
      pro_sor_pt1 == 11 ~ 42.3,
      pro_sor_pt1 == 12 ~ 44.2,
      pro_sor_pt1 == 13 ~ 46.2,
      pro_sor_pt1 == 14 ~ 48.1,
      pro_sor_pt1 == 15 ~ 50.0,
      pro_sor_pt1 == 16 ~ 51.9,
      pro_sor_pt1 == 17 ~ 53.7,
      pro_sor_pt1 == 18 ~ 55.8,
      pro_sor_pt1 == 19 ~ 58.3,
      pro_sor_pt1 == 20 ~ 64.2,
      TRUE ~ NA_real_ ),
    Tpro_paininfr_pt1 = case_when(
      pro_paininfr_pt1 == 4 ~ 41.6,
      pro_paininfr_pt1 == 5 ~ 49.6,
      pro_paininfr_pt1 == 6 ~ 52.0,
      pro_paininfr_pt1 == 7 ~ 53.9,
      pro_paininfr_pt1 == 8 ~ 55.6,
      pro_paininfr_pt1 == 9 ~ 57.1,
      pro_paininfr_pt1 == 10 ~ 58.5,
      pro_paininfr_pt1 == 11 ~ 59.9,
      pro_paininfr_pt1 == 12 ~ 61.2,
      pro_paininfr_pt1 == 13 ~ 62.5,
      pro_paininfr_pt1 == 14 ~ 63.8,
      pro_paininfr_pt1 == 15 ~ 65.2,
      pro_paininfr_pt1 == 16 ~ 66.6,
      pro_paininfr_pt1 == 17 ~ 68.0,
      pro_paininfr_pt1 == 18 ~ 69.7,
      pro_paininfr_pt1 == 19 ~ 71.6,
      pro_paininfr_pt1 == 20 ~ 75.6,
      TRUE ~ NA_real_ ),
    
    sd_pro_phy_pt1 = sd(c_across(q22a_pt1:q22d_pt1), na.rm=T),
    sd_pro_anx_pt1 = sd(c_across(q22e_pt1:q22h_pt1), na.rm=T),
    sd_pro_dep_pt1 = sd(c_across(q22i_pt1:q22l_pt1), na.rm=T),
    sd_pro_fat_pt1 = sd(c_across(q22m_pt1:q22p_pt1), na.rm=T),
    sd_pro_slp_pt1 = sd(c_across(q22q_pt1:q22t_pt1), na.rm=T),
    sd_pro_sor_pt1 = sd(c_across(q22u_pt1:q22x_pt1), na.rm=T),
    sd_pro_paininfr_pt1 = sd(c_across(q22y_pt1:q22bb_pt1), na.rm=T)
  ) |>
  
  mutate(
    totprosm_pt1 = sum(c_across(pro_phy_pt1:pro_painit_pt1), na.rm=T),
    sd_totprosm_pt1 = sd(c_across(pro_phy_pt1:pro_painit_pt1), na.rm=T),
    pro_painitt_pt1 = pro_painit_pt1 * 10,
    pranx_pt1 = 100 - Tpro_anx_pt1,
    prdep_pt1 = 100 - Tpro_dep_pt1,
    prfat_pt1 = 100 - Tpro_fat_pt1,
    prslp_pt1 = 100 - Tpro_slp_pt1,
    prpnif_pt1 = 100 - Tpro_paininfr_pt1
  ) |>
  
  mutate(
    prpnit_pt1 = 100 - pro_painitt_pt1,
    totpromn_pt1 = mean(c_across(c(Tpro_phy_pt1,pranx_pt1,Tpro_dep_pt1,
            Tpro_fat_pt1,Tpro_slp_pt1,Tpro_sor_pt1, prpnif_pt1,prpnit_pt1)), na.rm=T),
    sd_totpromn_pt1 = sd(c_across(c(Tpro_phy_pt1,pranx_pt1,Tpro_dep_pt1,
             Tpro_fat_pt1,Tpro_slp_pt1,Tpro_sor_pt1, prpnif_pt1,prpnit_pt1)), na.rm=T)
  ) |>
  
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq22")))
# # View(t1_q_promis_pt)

# Physical symptopms PT
t1_q_sympt_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q23")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q23a_pt1:q23j_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    sympt_pt1 = mean(c_across(q23a_pt1:q23j_pt1), na.rm=T),
    sd_sympt_pt1 = sd(c_across(q23a_pt1:q23j_pt1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq23")))
# # View(t1_q_sympt_pt)

# Satisfaction with life PT
t1_q_lsat_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q24")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    lsat_pt1 = sum(c_across(q24a_pt1:q24e_pt1), na.rm=T),
    sd_lsat_pt1 = sd(c_across(q24a_pt1:q24e_pt1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq24")))
# # View(t1_q_lsat_pt)

# PSQI: Pittsburgh Sleep Quality Index PT
t1_q_PSQI_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q25")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q25_5a_pt1:q25_5j_pt1, q25_6a_pt1:q25_9e_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      TRUE ~ NA_real_ )
  ) |>
  mutate_at(
    vars(q25_1_hr_pt1), ~ifelse((. == 12 && q25_1_apm_pt1 == 2) | is.na(.), 0, .)
  ) |>
  
  mutate(
    psqi_sbqual_pt1 = q25_7_pt1,
    psol_pt1 = (coalesce(q25_2_hr_pt1, 0) * 60) + coalesce(q25_2_mn_pt1, 0),
    
    psqi_sdmn_pt1 = (coalesce(q25_4_hr_pt1, 0) * 60) + coalesce(q25_4_mn_pt1, 0),
    psqi_sdhr_pt1 = ((coalesce(q25_4_hr_pt1, 0) * 60) + coalesce(q25_4_mn_pt1, 0)) / 60,
    
    slpwithn_pt1 = q25_5a_pt1, # already recoded above
    
    psqi_tb_h_pt1 = case_when(
      q25_1_apm_pt1 == 1 && q25_3_apm_pt1 == 2 ~ (12 - q25_1_hr_pt1) + q25_3_hr_pt1,
      q25_1_apm_pt1 == 2 && q25_3_apm_pt1 == 1 ~ ifelse(q25_3_hr_pt1 == 12, q25_3_hr_pt1 - q25_1_hr_pt1, (12 + q25_3_hr_pt1) - q25_1_hr_pt1),
      q25_1_apm_pt1 == 1 && q25_3_apm_pt1 == 1 ~ (q25_3_hr_pt1 + 12) + (12 - q25_1_hr_pt1),
      q25_1_apm_pt1 == 2 && q25_3_apm_pt1 == 2 ~ q25_3_hr_pt1 - q25_1_hr_pt1 ),
    psqi_tb_mn_pt1 = coalesce(q25_3_mn_pt1, 0) - coalesce(q25_1_mn_pt1, 0),
    
    psqi_distb_pt1 = sum(c_across(q25_5b_pt1:q25_5j_pt1), na.rm=T),
    sd_psqi_distb_pt1 = sd(c_across(q25_5b_pt1:q25_5j_pt1), na.rm=T),
    
    sleepmed_pt1 = q25_6a_pt1, # already recoded above
    
    psqi_dysf_pt1 = sum(c_across(c(q25_6b_pt1, q25_8_pt1)), na.rm=T),
    sd_psqi_dysf_pt1 = sd(c_across(c(q25_6b_pt1, q25_8_pt1)), na.rm=T),
  ) |>
  mutate(
    psolgr_pt1 = case_when(
      psol_pt1 <= 15 ~ 0,
      psol_pt1 >= 16 && psol_pt1 <= 30 ~ 1,
      psol_pt1 >= 31 && psol_pt1 <= 60 ~ 2,
      psol_pt1 > 60 ~ 3 ),
    
    goodsdhr_pt1 = case_when(
      psqi_sdhr_pt1 < 7 ~ 0,
      psqi_sdhr_pt1 >= 7 && psqi_sdhr_pt1 <= 9 ~ 1,
      psqi_sdhr_pt1 > 9 ~ 2 ),
    psqi_sdgr_pt1 = case_when(
      psqi_sdhr_pt1 < 5 ~ 3,
      psqi_sdhr_pt1 >= 5 && psqi_sdhr_pt1 <= 6 ~ 2,
      psqi_sdhr_pt1 >= 6.0001 && psqi_sdhr_pt1 <= 7 ~ 1,
      psqi_sdhr_pt1 > 7 ~ 0 ),
    
    psqi_tb_hr_pt1 = (psqi_tb_h_pt1*60 + psqi_tb_mn_pt1)/60,
    psqi_se_pt1 = ifelse((psqi_sdhr_pt1/psqi_tb_hr_pt1)*100 == 0, 0, 
                         (psqi_sdhr_pt1/psqi_tb_hr_pt1)*100),
    
    psqi_distbgr_pt1 = case_when(
      psqi_distb_pt1 == 0 ~ 0,
      psqi_distb_pt1 >= 1 && psqi_distb_pt1 <= 9 ~ 1,
      psqi_distb_pt1 >= 10 && psqi_distb_pt1 <= 18 ~ 2,
      psqi_distb_pt1 >= 19 && psqi_distb_pt1 <= 27 ~ 3),
    
    psqi_dysfgr_pt1 = case_when(
      psqi_dysf_pt1 == 0 ~ 0,
      psqi_dysf_pt1 >= 1 && psqi_dysf_pt1 <= 2 ~ 1,
      psqi_dysf_pt1 >= 3 && psqi_dysf_pt1 <= 4 ~ 2,
      psqi_dysf_pt1 >= 5 && psqi_dysf_pt1 <= 6 ~ 3)
  ) |>
  
  mutate(
    psqi_latnc_pt1 = sum(c_across(c(psolgr_pt1,slpwithn_pt1)), na.rm=T),
    sd_psqi_latnc_pt1 = sd(c_across(c(psolgr_pt1,slpwithn_pt1)), na.rm=T),
    
    goodse_pt1 = case_when(
      psqi_se_pt1 < 85 ~ 0,
      psqi_se_pt1 >= 85 ~ 1 ),
    psqi_segr_pt1 = case_when(
      psqi_se_pt1 < 65 ~ 3,
      psqi_se_pt1 >= 65 && psqi_se_pt1 <= 74.99 ~ 2,
      psqi_se_pt1 >= 75 && psqi_se_pt1 <= 84.99 ~ 1,
      psqi_se_pt1 >= 85 ~ 0)
  ) |>
  mutate(
    psqi_latncgr_pt1 = case_when(
      psqi_latnc_pt1 == 0 ~ 0,
      psqi_latnc_pt1 >= 1 && psqi_latnc_pt1 <= 2 ~ 1,
      psqi_latnc_pt1 >= 3 && psqi_latnc_pt1 <= 4 ~ 2,
      psqi_latnc_pt1 >= 5 && psqi_latnc_pt1 <= 6 ~ 3 )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    # Component 1: Subjective sleep quality
    psqi_sbqual_pt1,
    # Component 2: Sleep latency
    psol_pt1, psolgr_pt1, slpwithn_pt1, psqi_latnc_pt1, psqi_latncgr_pt1, sd_psqi_latnc_pt1,
    # Component 3: Sleep duration
    psqi_sdmn_pt1, psqi_sdhr_pt1, goodsdhr_pt1, psqi_sdgr_pt1,
    # Component 4: Habitual Sleep Efficiency
    psqi_tb_h_pt1, psqi_tb_mn_pt1, psqi_tb_hr_pt1, psqi_se_pt1, goodse_pt1, psqi_segr_pt1,
    # Component 5: Sleep disturbances
    psqi_distb_pt1, psqi_distbgr_pt1, sd_psqi_distb_pt1,
    # Component 6: Use of sleeping medication
    sleepmed_pt1,
    # Component 7: Daytime dysfunction
    psqi_dysf_pt1, psqi_dysfgr_pt1, sd_psqi_dysf_pt1,
    
    psqi_gl_pt1 = sum(c_across(c(psqi_sbqual_pt1,psqi_latncgr_pt1,psqi_sdgr_pt1,
                  psqi_segr_pt1,psqi_distbgr_pt1,sleepmed_pt1,psqi_dysfgr_pt1)),na.rm=T),
    sd_psqi_gl_pt1 = sd(c_across(c(psqi_sbqual_pt1,psqi_latncgr_pt1,psqi_sdgr_pt1,
                  psqi_segr_pt1,psqi_distbgr_pt1,sleepmed_pt1,psqi_dysfgr_pt1)),na.rm=T)
  ) |>
  mutate(
    psqi_gdslp = ifelse(psqi_gl_pt1 < 4.99, 1, ifelse(psqi_gl_pt1 > 4.99999, 0, NA_real_))
  ) |> 
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq25")))
# # View(t1_q_PSQI_pt)

# Distress thermometer PT
t1_q_distr_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q26")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    distr_therm_pt1 = case_when(
      q26_pt1  %in% 0:3 ~ 0,
      q26_pt1  %in% 4:10 ~ 1)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq26")))
# # View(t1_q_distr_pt)

# Depressive symptoms CES-D PT 
CESDvars1_pt = c("q27d_pt1", "q27h_pt1", "q27l_pt1", "q27p_pt1")
CESDvars2_pt = names(t1_q_pt_orig |> select(starts_with("q27")) |> select(-all_of(CESDvars1_pt)))
t1_q_cesd_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q27")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(all_of(CESDvars2_pt)), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      TRUE ~ NA_real_  )
  ) |>
  mutate_at(
    vars(all_of(CESDvars1_pt)), ~ case_when(
      . == 4 ~ 0,
      . == 3 ~ 1,
      . == 2 ~ 2,
      . == 1 ~ 3,
      TRUE ~ NA_real_ )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    CESD_pt1 = sum(c_across(q27a_pt1:q27t_pt1), na.rm=T),
    sd_CESD_pt1 = sd(c_across(q27a_pt1:q27t_pt1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq27")))
# # View(t1_q_cesd_pt)

# FACT-C (AND FACT-G) PT ONLY
t1_q_factc_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q28")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    FACT_PWB_pt1 = mean(c_across(q28a_pt1:q28g_pt1), na.rm=T),
    FACT_SFWB_pt1 = mean(c_across(c(q28h_pt1:q28m_pt1, q28n_pt1)), na.rm=T),
    FACT_EWB_pt1 = mean(c_across(q28o_pt1:q28t_pt1), na.rm=T),
    FACT_FWB_pt1 = mean(c_across(q28u_pt1:q28aa_pt1), na.rm=T),
    
    FACT_CCS_pt1 = case_when(
      q28ii_pt1 == 2 ~ mean(c_across(c(q28bb_pt1:q28hh_pt1, q28jj_pt1:q28kk_pt1)), na.rm=T),
      q28ii_pt1 == 1 ~ mean(c_across(q28bb_pt1:q28hh_pt1), na.rm=T) ),
    
    sd_FACT_PWB_pt1 = sd(c_across(q28a_pt1:q28g_pt1), na.rm=T),
    sd_FACT_SFWB_pt1 = sd(c_across(c(q28h_pt1:q28m_pt1, q28n_pt1)), na.rm=T),
    sd_FACT_EWB_pt1 = sd(c_across(q28o_pt1:q28t_pt1), na.rm=T),
    sd_FACT_FWB_pt1 = sd(c_across(q28u_pt1:q28aa_pt1), na.rm=T),
    
    sd_FACT_CCS_pt1 = case_when(
      q28ii_pt1 == 2 ~ sd(c_across(c(q28bb_pt1:q28hh_pt1, q28jj_pt1:q28kk_pt1)), na.rm=T),
      q28ii_pt1 == 1 ~ sd(c_across(q28bb_pt1:q28hh_pt1), na.rm=T) )
  ) |>
  mutate(
    FACT_G_pt1 = mean(c_across(FACT_PWB_pt1:FACT_FWB_pt1), na.rm=T),
    sd_FACT_G_pt1 = sd(c_across(FACT_PWB_pt1:FACT_FWB_pt1), na.rm=T)
  ) |>
  mutate(
    FACT_C_pt1 = mean(c_across(c(FACT_G_pt1, FACT_CCS_pt1)), na.rm=T),
    sd_FACT_C_pt1 = sd(c_across(c(FACT_G_pt1, FACT_CCS_pt1)), na.rm=T),
  ) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq28")))
# View(t1_q_factc_pt)

# FACIT sp: spirituality PT
t1_q_factsp_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q29")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q29a_pt1:q29l_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q29d_pt1, q29h_pt1), ~ case_when(
      . == 4 ~ 0,
      . == 3 ~ 1,
      . == 2 ~ 2,
      . == 1 ~ 3,
      . == 0 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  mutate(
    AHnotna_pt = rowSums(!is.na(across(q29a_pt1:q29h_pt1))),
    ILnotna_pt = rowSums(!is.na(across(q29i_pt1:q29l_pt1))),
    ALnotna_pt = rowSums(!is.na(across(q29a_pt1:q29l_pt1)))
  ) |>
  mutate(
    FACITSp12MP_pt1 = ifelse(AHnotna_pt > 4, sum(c_across(q29a_pt1:q29h_pt1), na.rm=T) 
                             * (8/AHnotna_pt), NA_real_),
    FACITSp12F_pt1 = ifelse(ILnotna_pt > 2, sum(c_across(q29i_pt1:q29l_pt1), na.rm=T) 
                             * (4/ILnotna_pt), NA_real_)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    FACITSp12MP_pt1,
    FACITSp12F_pt1,
    FACITSp12_pt1 = FACITSp12MP_pt1 + FACITSp12F_pt1,
    
    sd_FACITSp12MP_pt1 = ifelse(AHnotna_pt > 4, sd(c_across(q29a_pt1:q29h_pt1), na.rm=T), NA_real_),
    sd_FACITSp12F_pt1 = ifelse(ILnotna_pt > 2, sd(c_across(q29i_pt1:q29l_pt1), na.rm=T), NA_real_)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq29")))
# # View(t1_q_factsp_pt)

# ISI: Insomnia Severity Index PT
t1_q_ISI_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q30")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q30a_pt1:q30g_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  mutate(
    isi_pt1 = sum(c_across(q30a_pt1:q30g_pt1), na.rm=T),
    sd_isi_pt1 = sd(c_across(q30a_pt1:q30g_pt1), na.rm=T)
  )|>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    isi_pt1,
    isi_gr_pt1 = case_when(
      isi_pt1 >= 0 && isi_pt1 <= 7 ~ 0,
      isi_pt1 >= 8 && isi_pt1 <= 14 ~ 1,
      isi_pt1 >= 15 && isi_pt1 <= 21 ~ 2,
      isi_pt1 >= 22 && isi_pt1 <= 28 ~ 3 ),
    sd_isi_pt1
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq30")))
# # View(t1_q_ISI_pt)

# ESS: Epworth Sleepiness Scale PT
t1_q_ESS_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q31")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q31a_pt1:q31h_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      TRUE ~ . )
  ) |>
  mutate(
    ess_pt1 = sum(c_across(q31a_pt1:q31h_pt1), na.rm=T),
    sd_ess_pt1 = sd(c_across(q31a_pt1:q31h_pt1), na.rm=T)
  )|>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    ess_pt1,
    ess_gr_pt1 = case_when(
      ess_pt1 > -1 && ess_pt1 < 5.00001 ~ 0,
      ess_pt1 > 5.999999 && ess_pt1 < 10.00001 ~ 1,
      ess_pt1 > 10.999999 && ess_pt1 < 12.00001 ~ 2,
      ess_pt1 > 12.999999 && ess_pt1 < 15.00001 ~ 3,
      ess_pt1 > 15.999999 && ess_pt1 < 24.00001 ~ 3 ),
    sd_ess_pt1
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq31")))
# # View(t1_q_ESS_pt)

# Benefit Finding Scale PT
t1_q_BF_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q32")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    BF_AC_pt1 = mean(c_across(q32a_pt1:q32c_pt1), na.rm=T),
    BF_EM_pt1 = mean(c_across(q32f_pt1:q32i_pt1), na.rm=T),
    BF_AP_pt1 = mean(c_across(q32l_pt1:q32n_pt1), na.rm=T),
    BF_FA_pt1 = mean(c_across(c(q32d_pt1,q32e_pt1)), na.rm=T),
    BF_SV_pt1 = mean(c_across(c(q32j_pt1,q32k_pt1,q32q_pt1)), na.rm=T),
    BF_RE_pt1 = mean(c_across(q32o_pt1:q32p_pt1), na.rm=T),
    bf_pt1 = mean(c_across(q32a_pt1:q32p_pt1), na.rm=T),
    
    sd_BF_AC_pt1 = sd(c_across(q32a_pt1:q32c_pt1), na.rm=T),
    sd_BF_EM_pt1 = sd(c_across(q32f_pt1:q32i_pt1), na.rm=T),
    sd_BF_AP_pt1 = sd(c_across(q32l_pt1:q32n_pt1), na.rm=T),
    sd_BF_FA_pt1 = sd(c_across(c(q32d_pt1,q32e_pt1)), na.rm=T),
    sd_BF_SV_pt1 = sd(c_across(c(q32j_pt1,q32k_pt1,q32q_pt1)), na.rm=T),
    sd_BF_RE_pt1 = sd(c_across(q32o_pt1:q32p_pt1), na.rm=T),
    sd_bf_pt1 = sd(c_across(q32a_pt1:q32q_pt1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq32")))
# # View(t1_q_BF_pt)

# POMS-SF 30: Profile of Mood States-Short Form PT
t1_q_POMS_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q33")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q33a_pt1:q33dd_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  mutate_at(
    vars(q33z_pt1), ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0,
      TRUE ~ NA_real_ )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    POMS_TA_pt1 = sum(c_across(c(q33a_pt1, q33f_pt1,  q33l_pt1, q33p_pt1, q33t_pt1)), na.rm=T),
    POMS_DP_pt1 = sum(c_across(c(q33g_pt1, q33k_pt1, q33o_pt1, q33q_pt1, q33u_pt1)), na.rm=T),
    POMS_AH_pt1 = sum(c_across(c(q33b_pt1, q33i_pt1, q33n_pt1, q33y_pt1, q33bb_pt1)), na.rm=T),
    POMS_VA_pt1 = sum(c_across(c(q33d_pt1, q33h_pt1, q33j_pt1, q33aa_pt1,  q33dd_pt1)), na.rm=T),
    POMS_FI_pt1 = sum(c_across(c(q33c_pt1, q33m_pt1, q33s_pt1, q33v_pt1, q33w_pt1)), na.rm=T),
    POMS_CB_pt1 = sum(c_across(c(q33e_pt1, q33r_pt1	, q33x_pt1, q33z_pt1, q33cc_pt1)), na.rm=T),
    
    sd_POMS_TA_pt1 = sd(c_across(c(q33a_pt1, q33f_pt1,  q33l_pt1, q33p_pt1, q33t_pt1)), na.rm=T),
    sd_POMS_DP_pt1 = sd(c_across(c(q33g_pt1, q33k_pt1, q33o_pt1, q33q_pt1, q33u_pt1)), na.rm=T),
    sd_POMS_AH_pt1 = sd(c_across(c(q33b_pt1, q33i_pt1, q33n_pt1, q33y_pt1, q33bb_pt1)), na.rm=T),
    sd_POMS_VA_pt1 = sd(c_across(c(q33d_pt1, q33h_pt1, q33j_pt1, q33aa_pt1,  q33dd_pt1)), na.rm=T),
    sd_POMS_FI_pt1 = sd(c_across(c(q33c_pt1, q33m_pt1, q33s_pt1, q33v_pt1, q33w_pt1)), na.rm=T),
    sd_POMS_CB_pt1 = sd(c_across(c(q33e_pt1, q33r_pt1	, q33x_pt1, q33z_pt1, q33cc_pt1)), na.rm=T)
  ) |>
  mutate(
    POMS_TMD_pt1 = POMS_TA_pt1+POMS_DP_pt1+POMS_AH_pt1+POMS_FI_pt1+POMS_CB_pt1-POMS_VA_pt1
  ) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq33")))
# # View(t1_q_POMS_pt)

# MASQ-30: Anxiety Symptoms Subscale PT
t1_q_MASQ_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, starts_with("q34")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q34a_pt1:q34j_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    masq_anx_pt1 = sum(c_across(q34a_pt1:q34j_pt1), na.rm=T),
    sd_masq_anx_pt1 = sd(c_across(q34a_pt1:q34j_pt1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq34")))
# # View(t1_q_MASQ_pt)

# IES-R: Impact of Event Scale PT
POMSvars = names(t1_q_pt_orig |> select(starts_with("q33")) 
                |> select(-c("q33d_pt1","q33h_pt1","q33j_pt1","q33aa_pt1","q33dd_pt1")))
t1_q_IES_pt = t1_q_pt_orig |> select(BiPs_IID, BiPs_DID, 
                                      starts_with(c("q27","q29","q33","q35","q51"))) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q35a_pt1:q35v_pt1, q33a_pt1:q33dd_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  mutate_at(
    vars(q33z_pt1), ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0,
      TRUE ~ NA_real_ )
  ) |>
  mutate_at(
    vars(q27i_pt1, q27g_pt1, q27n_pt1, q51k_pt1, q51s_pt1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1.3,
      . == 3 ~ 2.7,
      . == 4 ~ 4,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q29a_pt1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      . == 5 ~ 0,
      TRUE ~ . )
  ) |>
  mutate(
    IES_AVD_pt1 = mean(c_across(c(q35e_pt1,q35g_pt1,q35h_pt1,q35k_pt1,q35l_pt1,q35m_pt1,q35q_pt1,q35v_pt1)), na.rm=T),
    IES_INT_pt1 = mean(c_across(c(q35a_pt1,q35b_pt1,q35c_pt1	,q35f_pt1,q35i_pt1, q35n_pt1,q35p_pt1,q35t_pt1)), na.rm=T),
    IES_HYP_pt1 = mean(c_across(c(q35d_pt1,q35j_pt1, q35o_pt1,q35r_pt1, q35s_pt1,q35u_pt1)), na.rm=T),
    IES_TOTAL_pt1 = sum(c_across(q35a_pt1:q35v_pt1), na.rm=T),
    
    POMS_dis_pt1 = mean(c_across(all_of(POMSvars)), na.rm=T),
  ) |>
  mutate(
    PTS_nacm_pt1 = mean(c_across(c(q27i_pt1, q27g_pt1, q27n_pt1, q51k_pt1, q51s_pt1, q29a_pt1, POMS_dis_pt1)), na.rm=T)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    IES_AVD_pt1, IES_INT_pt1, IES_HYP_pt1, IES_TOTAL_pt1,
    POMS_dis_pt1,
    PTS_nacm_pt1,
    
    sd_IES_AVD_pt1 = sd(c_across(c(q35e_pt1,q35g_pt1,q35h_pt1,q35k_pt1,q35l_pt1,q35m_pt1,q35q_pt1,q35v_pt1)), na.rm=T),
    sd_IES_INT_pt1 = sd(c_across(c(q35a_pt1,q35b_pt1,q35c_pt1	,q35f_pt1,q35i_pt1, q35n_pt1,q35p_pt1,q35t_pt1)), na.rm=T),
    sd_IES_HYP_pt1 = sd(c_across(c(q35d_pt1,q35j_pt1, q35o_pt1,q35r_pt1, q35s_pt1,q35u_pt1)), na.rm=T),
    sd_IES_TOTAL_pt1 = sd(c_across(q35a_pt1:q35v_pt1), na.rm=T),
    
    sd_POMS_dis_pt1 = sd(c_across(all_of(POMSvars)), na.rm=T),
    sd_PTS_nacm_pt1 = sd(c_across(c(q27i_pt1, q27g_pt1, q27n_pt1, q51k_pt1, q51s_pt1, q29a_pt1, POMS_dis_pt1)), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq35")))
# # View(t1_q_IES_pt)


# -------------------------------------------------------------------
########## FM OUTCOME EXTRACTIONS

# PROMIS-29 FM
t1_q_promis_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q22")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q22a_fm1:q22d_fm1, q22q_fm1:q22r_fm1), ~ case_when(
      . == 1 ~ 5,
      . == 2 ~ 4,
      . == 3 ~ 3,
      . == 4 ~ 2,
      . == 5 ~ 1,
      TRUE ~ NA_real_ )
  ) |>
  
  transmute(
    BiPs_IID, 
    BiPs_DID,
    pro_phy_fm1 = sum(c_across(q22a_fm1:q22d_fm1), na.rm=T),
    pro_anx_fm1 = sum(c_across(q22e_fm1:q22h_fm1), na.rm=T),
    pro_dep_fm1 = sum(c_across(q22i_fm1:q22l_fm1), na.rm=T),
    pro_fat_fm1 = sum(c_across(q22m_fm1:q22p_fm1), na.rm=T),
    pro_slp_fm1 = sum(c_across(q22q_fm1:q22t_fm1), na.rm=T),
    pro_sor_fm1 = sum(c_across(q22u_fm1:q22x_fm1), na.rm=T),
    pro_paininfr_fm1 = sum(c_across(q22y_fm1:q22bb_fm1), na.rm=T),
    pro_painit_fm1 = q22cc_fm1,
    
    Tpro_phy_fm1 = case_when(
      pro_phy_fm1  ==  4 ~ 22.5,
      pro_phy_fm1 == 5 ~ 26.6,
      pro_phy_fm1 == 6 ~ 28.9,
      pro_phy_fm1 == 7 ~ 30.5,
      pro_phy_fm1 == 8 ~ 31.9,
      pro_phy_fm1 == 9 ~ 33.2,
      pro_phy_fm1 == 10 ~ 34.4,
      pro_phy_fm1 == 11 ~ 35.6,
      pro_phy_fm1 == 12 ~ 36.7,
      pro_phy_fm1 == 13 ~ 37.9,
      pro_phy_fm1 == 14 ~ 39.2,
      pro_phy_fm1 == 15 ~ 40.5,
      pro_phy_fm1 == 16 ~ 41.9,
      pro_phy_fm1 == 17 ~ 43.5,
      pro_phy_fm1 == 18 ~ 45.5,
      pro_phy_fm1 == 19 ~ 48.3,
      pro_phy_fm1 == 20 ~ 57.0,
      TRUE ~ NA_real_ ),
    Tpro_anx_fm1 = case_when(
      pro_anx_fm1 == 4 ~ 40.3,
      pro_anx_fm1 == 5 ~ 48.0,
      pro_anx_fm1 == 6 ~ 51.2,
      pro_anx_fm1 == 7 ~ 53.7,
      pro_anx_fm1 == 8 ~ 55.8,
      pro_anx_fm1 == 9 ~ 57.7,
      pro_anx_fm1 == 10 ~ 59.5,
      pro_anx_fm1 == 11 ~ 61.4,
      pro_anx_fm1 == 12 ~ 63.4,
      pro_anx_fm1 == 13 ~ 65.3,
      pro_anx_fm1 == 14 ~ 67.3,
      pro_anx_fm1 == 15 ~ 69.3,
      pro_anx_fm1 == 16 ~ 71.2,
      pro_anx_fm1 == 17 ~ 73.3,
      pro_anx_fm1 == 18 ~ 75.4,
      pro_anx_fm1 == 19 ~ 77.9,
      pro_anx_fm1 == 20 ~ 81.6,
      TRUE ~ NA_real_ ),
    Tpro_dep_fm1 = case_when(
      pro_dep_fm1 == 4 ~ 41.0,
      pro_dep_fm1 == 5 ~ 49.0,
      pro_dep_fm1 == 6 ~ 51.8,
      pro_dep_fm1 == 7 ~ 53.9,
      pro_dep_fm1 == 8 ~ 55.7,
      pro_dep_fm1 == 9 ~ 57.3,
      pro_dep_fm1 == 10 ~ 58.9,
      pro_dep_fm1 == 11 ~ 60.5,
      pro_dep_fm1 == 12 ~ 62.2,
      pro_dep_fm1 == 13 ~ 63.9,
      pro_dep_fm1 == 14 ~ 65.7,
      pro_dep_fm1 == 15 ~ 67.5,
      pro_dep_fm1 == 16 ~ 69.4,
      pro_dep_fm1 == 17 ~ 71.2,
      pro_dep_fm1 == 18 ~ 73.3,
      pro_dep_fm1 == 19 ~ 75.7,
      pro_dep_fm1 == 20 ~ 79.4,
      TRUE ~ NA_real_ ),
    Tpro_fat_fm1 = case_when(
      pro_fat_fm1 == 4 ~ 33.7,
      pro_fat_fm1 == 5 ~ 39.7,
      pro_fat_fm1 == 6 ~ 43.1,
      pro_fat_fm1 == 7 ~ 46.0,
      pro_fat_fm1 == 8 ~ 48.6,
      pro_fat_fm1 == 9 ~ 51.0,
      pro_fat_fm1 == 10 ~ 53.1,
      pro_fat_fm1 == 11 ~ 55.1,
      pro_fat_fm1 == 12 ~ 57.0,
      pro_fat_fm1 == 13 ~ 58.8,
      pro_fat_fm1 == 14 ~ 60.7,
      pro_fat_fm1 == 15 ~ 62.7,
      pro_fat_fm1 == 16 ~ 64.6,
      pro_fat_fm1 == 17 ~ 66.7,
      pro_fat_fm1 == 18 ~ 69.0,
      pro_fat_fm1 == 19 ~ 71.6,
      pro_fat_fm1 == 20 ~ 75.8,
      TRUE ~ NA_real_ ),
    Tpro_slp_fm1 = case_when(
      pro_slp_fm1 == 4 ~ 32.0,
      pro_slp_fm1 == 5 ~ 37.5,
      pro_slp_fm1 == 6 ~ 41.1,
      pro_slp_fm1 == 7 ~ 43.8,
      pro_slp_fm1 == 8 ~ 46.2,
      pro_slp_fm1 == 9 ~ 48.4,
      pro_slp_fm1 == 10 ~ 50.5,
      pro_slp_fm1 == 11 ~ 52.4,
      pro_slp_fm1 == 12 ~ 54.3,
      pro_slp_fm1 == 13 ~ 56.1,
      pro_slp_fm1 == 14 ~ 57.9,
      pro_slp_fm1 == 15 ~ 59.8,
      pro_slp_fm1 == 16 ~ 61.7,
      pro_slp_fm1 == 17 ~ 63.8,
      pro_slp_fm1 == 18 ~ 66.0,
      pro_slp_fm1 == 19 ~ 68.8,
      pro_slp_fm1 == 20 ~ 73.3,
      TRUE ~ NA_real_ ),
    Tpro_sor_fm1 = case_when(
      pro_sor_fm1 == 4 ~ 27.5,
      pro_sor_fm1 == 5 ~ 31.8,
      pro_sor_fm1 == 6 ~ 34.0,
      pro_sor_fm1 == 7 ~ 35.7,
      pro_sor_fm1 == 8 ~ 37.3,
      pro_sor_fm1 == 9 ~ 38.8,
      pro_sor_fm1 == 10 ~ 40.5,
      pro_sor_fm1 == 11 ~ 42.3,
      pro_sor_fm1 == 12 ~ 44.2,
      pro_sor_fm1 == 13 ~ 46.2,
      pro_sor_fm1 == 14 ~ 48.1,
      pro_sor_fm1 == 15 ~ 50.0,
      pro_sor_fm1 == 16 ~ 51.9,
      pro_sor_fm1 == 17 ~ 53.7,
      pro_sor_fm1 == 18 ~ 55.8,
      pro_sor_fm1 == 19 ~ 58.3,
      pro_sor_fm1 == 20 ~ 64.2,
      TRUE ~ NA_real_ ),
    Tpro_paininfr_fm1 = case_when(
      pro_paininfr_fm1 == 4 ~ 41.6,
      pro_paininfr_fm1 == 5 ~ 49.6,
      pro_paininfr_fm1 == 6 ~ 52.0,
      pro_paininfr_fm1 == 7 ~ 53.9,
      pro_paininfr_fm1 == 8 ~ 55.6,
      pro_paininfr_fm1 == 9 ~ 57.1,
      pro_paininfr_fm1 == 10 ~ 58.5,
      pro_paininfr_fm1 == 11 ~ 59.9,
      pro_paininfr_fm1 == 12 ~ 61.2,
      pro_paininfr_fm1 == 13 ~ 62.5,
      pro_paininfr_fm1 == 14 ~ 63.8,
      pro_paininfr_fm1 == 15 ~ 65.2,
      pro_paininfr_fm1 == 16 ~ 66.6,
      pro_paininfr_fm1 == 17 ~ 68.0,
      pro_paininfr_fm1 == 18 ~ 69.7,
      pro_paininfr_fm1 == 19 ~ 71.6,
      pro_paininfr_fm1 == 20 ~ 75.6,
      TRUE ~ NA_real_ ),
    
    sd_pro_phy_fm1 = sd(c_across(q22a_fm1:q22d_fm1), na.rm=T),
    sd_pro_anx_fm1 = sd(c_across(q22e_fm1:q22h_fm1), na.rm=T),
    sd_pro_dep_fm1 = sd(c_across(q22i_fm1:q22l_fm1), na.rm=T),
    sd_pro_fat_fm1 = sd(c_across(q22m_fm1:q22p_fm1), na.rm=T),
    sd_pro_slp_fm1 = sd(c_across(q22q_fm1:q22t_fm1), na.rm=T),
    sd_pro_sor_fm1 = sd(c_across(q22u_fm1:q22x_fm1), na.rm=T),
    sd_pro_paininfr_fm1 = sd(c_across(q22y_fm1:q22bb_fm1), na.rm=T)
  ) |>
  
  # OLD FM VERSION?
  # mutate(
  #   totprosm_fm1 = sum(c_across(pro_phy_fm1:pro_painit_fm1), na.rm=T),
  #   totpromn_fm1 = mean(c_across(pro_phy_fm1:pro_painit_fm1), na.rm=T),
  # 
  #   sd_totprosm_fm1 = sd(c_across(pro_phy_fm1:pro_painit_fm1), na.rm=T),
  #   sd_totpromn_fm1 = sd(c_across(pro_phy_fm1:pro_painit_fm1), na.rm=T),
  #   
  #   pro_painitt_fm1 = pro_painit_fm1 * 10,
  #   pranx_fm1 = 100 - Tpro_anx_fm1,
  #   prdep_fm1 = 100 - Tpro_dep_fm1,
  #   prfat_fm1 = 100 - Tpro_fat_fm1,
  #   prslp_fm1 = 100 - Tpro_slp_fm1,
  #   prpnif_fm1 = 100 - Tpro_paininfr_fm1,
  #   prpnit_fm1 = 100 - (pro_painit_fm1 * 10)
  #   
  # ) |>
  
  mutate(
    totprosm_fm1 = sum(c_across(pro_phy_fm1:pro_painit_fm1), na.rm=T),
    sd_totprosm_fm1 = sd(c_across(pro_phy_fm1:pro_painit_fm1), na.rm=T),
    pro_painitt_fm1 = pro_painit_fm1 * 10,
    pranx_fm1 = 100 - Tpro_anx_fm1,
    prdep_fm1 = 100 - Tpro_dep_fm1,
    prfat_fm1 = 100 - Tpro_fat_fm1,
    prslp_fm1 = 100 - Tpro_slp_fm1,
    prpnif_fm1 = 100 - Tpro_paininfr_fm1
  ) |>
  
  mutate(
    prpnit_fm1 = 100 - pro_painitt_fm1,
    totpromn_fm1 = mean(c_across(c(Tpro_phy_fm1,pranx_fm1,Tpro_dep_fm1,
            Tpro_fat_fm1,Tpro_slp_fm1,Tpro_sor_fm1, prpnif_fm1,prpnit_fm1)), na.rm=T),
    sd_totpromn_fm1 = sd(c_across(c(Tpro_phy_fm1,pranx_fm1,Tpro_dep_fm1,
             Tpro_fat_fm1,Tpro_slp_fm1,Tpro_sor_fm1, prpnif_fm1,prpnit_fm1)), na.rm=T)
  ) |>
  
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq22")))
# # View(t1_q_promis_fm)

# Physical symptopms FM
t1_q_sympt_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q23")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q23a_fm1:q23j_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    sympt_fm1 = mean(c_across(q23a_fm1:q23j_fm1), na.rm=T),
    sd_sympt_fm1 = sd(c_across(q23a_fm1:q23j_fm1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq23")))
# # View(t1_q_sympt_fm)

# Satisfaction with life FM
t1_q_lsat_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q24")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    lsat_fm1 = sum(c_across(q24a_fm1:q24e_fm1), na.rm=T),
    sd_lsat_fm1 = sd(c_across(q24a_fm1:q24e_fm1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq24")))
# # View(t1_q_lsat_fm)

# PSQI: Pittsburgh Sleep Quality Index FM
t1_q_PSQI_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q25")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q25_5a_fm1:q25_5j_fm1, q25_6a_fm1:q25_9e_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      TRUE ~ NA_real_ )
  ) |>
  mutate_at(
    vars(q25_1_hr_fm1), ~ifelse((. == 12 && q25_1_apm_fm1 == 2) | is.na(.), 0, .)
  ) |>
  
  mutate(
    psqi_sbqual_fm1 = q25_7_fm1,
    psol_fm1 = (coalesce(q25_2_hr_fm1, 0) * 60) + coalesce(q25_2_mn_fm1, 0),
    
    psqi_sdmn_fm1 = (coalesce(q25_4_hr_fm1, 0) * 60) + coalesce(q25_4_mn_fm1, 0),
    psqi_sdhr_fm1 = ((coalesce(q25_4_hr_fm1, 0) * 60) + coalesce(q25_4_mn_fm1, 0)) / 60,
    
    slpwithn_fm1 = q25_5a_fm1, # already recoded above
    
    psqi_tb_h_fm1 = case_when(
      q25_1_apm_fm1 == 1 && q25_3_apm_fm1 == 2 ~ (12 - q25_1_hr_fm1) + q25_3_hr_fm1,
      q25_1_apm_fm1 == 2 && q25_3_apm_fm1 == 1 ~ ifelse(q25_3_hr_fm1 == 12, q25_3_hr_fm1 - q25_1_hr_fm1, (12 + q25_3_hr_fm1) - q25_1_hr_fm1),
      q25_1_apm_fm1 == 1 && q25_3_apm_fm1 == 1 ~ (q25_3_hr_fm1 + 12) + (12 - q25_1_hr_fm1),
      q25_1_apm_fm1 == 2 && q25_3_apm_fm1 == 2 ~ q25_3_hr_fm1 - q25_1_hr_fm1 ),
    psqi_tb_mn_fm1 = coalesce(q25_3_mn_fm1, 0) - coalesce(q25_1_mn_fm1, 0),
    
    psqi_distb_fm1 = sum(c_across(q25_5b_fm1:q25_5j_fm1), na.rm=T),
    sd_psqi_distb_fm1 = sd(c_across(q25_5b_fm1:q25_5j_fm1), na.rm=T),
    
    sleepmed_fm1 = q25_6a_fm1, # already recoded above
    
    psqi_dysf_fm1 = sum(c_across(c(q25_6b_fm1, q25_8_fm1)), na.rm=T),
    sd_psqi_dysf_fm1 = sd(c_across(c(q25_6b_fm1, q25_8_fm1)), na.rm=T),
  ) |>
  mutate(
    psolgr_fm1 = case_when(
      psol_fm1 <= 15 ~ 0,
      psol_fm1 >= 16 && psol_fm1 <= 30 ~ 1,
      psol_fm1 >= 31 && psol_fm1 <= 60 ~ 2,
      psol_fm1 > 60 ~ 3 ),
    
    goodsdhr_fm1 = case_when(
      psqi_sdhr_fm1 < 7 ~ 0,
      psqi_sdhr_fm1 >= 7 && psqi_sdhr_fm1 <= 9 ~ 1,
      psqi_sdhr_fm1 > 9 ~ 2 ),
    psqi_sdgr_fm1 = case_when(
      psqi_sdhr_fm1 < 5 ~ 3,
      psqi_sdhr_fm1 >= 5 && psqi_sdhr_fm1 <= 6 ~ 2,
      psqi_sdhr_fm1 >= 6.0001 && psqi_sdhr_fm1 <= 7 ~ 1,
      psqi_sdhr_fm1 > 7 ~ 0 ),
    
    psqi_tb_hr_fm1 = (psqi_tb_h_fm1*60 + psqi_tb_mn_fm1)/60,
    psqi_se_fm1 = ifelse((psqi_sdhr_fm1/psqi_tb_hr_fm1)*100 == 0, 0, 
                         (psqi_sdhr_fm1/psqi_tb_hr_fm1)*100),
    
    psqi_distbgr_fm1 = case_when(
      psqi_distb_fm1 == 0 ~ 0,
      psqi_distb_fm1 >= 1 && psqi_distb_fm1 <= 9 ~ 1,
      psqi_distb_fm1 >= 10 && psqi_distb_fm1 <= 18 ~ 2,
      psqi_distb_fm1 >= 19 && psqi_distb_fm1 <= 27 ~ 3),
    
    psqi_dysfgr_fm1 = case_when(
      psqi_dysf_fm1 == 0 ~ 0,
      psqi_dysf_fm1 >= 1 && psqi_dysf_fm1 <= 2 ~ 1,
      psqi_dysf_fm1 >= 3 && psqi_dysf_fm1 <= 4 ~ 2,
      psqi_dysf_fm1 >= 5 && psqi_dysf_fm1 <= 6 ~ 3)
  ) |>
  
  mutate(
    psqi_latnc_fm1 = sum(c_across(c(psolgr_fm1,slpwithn_fm1)), na.rm=T),
    sd_psqi_latnc_fm1 = sd(c_across(c(psolgr_fm1,slpwithn_fm1)), na.rm=T),
    
    goodse_fm1 = case_when(
      psqi_se_fm1 < 85 ~ 0,
      psqi_se_fm1 >= 85 ~ 1 ),
    psqi_segr_fm1 = case_when(
      psqi_se_fm1 < 65 ~ 3,
      psqi_se_fm1 >= 65 && psqi_se_fm1 <= 74.99 ~ 2,
      psqi_se_fm1 >= 75 && psqi_se_fm1 <= 84.99 ~ 1,
      psqi_se_fm1 >= 85 ~ 0)
  ) |>
  mutate(
    psqi_latncgr_fm1 = case_when(
      psqi_latnc_fm1 == 0 ~ 0,
      psqi_latnc_fm1 >= 1 && psqi_latnc_fm1 <= 2 ~ 1,
      psqi_latnc_fm1 >= 3 && psqi_latnc_fm1 <= 4 ~ 2,
      psqi_latnc_fm1 >= 5 && psqi_latnc_fm1 <= 6 ~ 3 )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    # Component 1: Subjective sleep quality
    psqi_sbqual_fm1,
    # Component 2: Sleep latency
    psol_fm1, psolgr_fm1, slpwithn_fm1, psqi_latnc_fm1, psqi_latncgr_fm1, sd_psqi_latnc_fm1,
    # Component 3: Sleep duration
    psqi_sdmn_fm1, psqi_sdhr_fm1, goodsdhr_fm1, psqi_sdgr_fm1,
    # Component 4: Habitual Sleep Efficiency
    psqi_tb_h_fm1, psqi_tb_mn_fm1, psqi_tb_hr_fm1, psqi_se_fm1, goodse_fm1, psqi_segr_fm1,
    # Component 5: Sleep disturbances
    psqi_distb_fm1, psqi_distbgr_fm1, sd_psqi_distb_fm1,
    # Component 6: Use of sleeping medication
    sleepmed_fm1,
    # Component 7: Daytime dysfunction
    psqi_dysf_fm1, psqi_dysfgr_fm1, sd_psqi_dysf_fm1,
    
    psqi_gl_fm1 = sum(c_across(c(psqi_sbqual_fm1,psqi_latncgr_fm1,psqi_sdgr_fm1,
                                 psqi_segr_fm1,psqi_distbgr_fm1,sleepmed_fm1,psqi_dysfgr_fm1)),na.rm=T),
    sd_psqi_gl_fm1 = sd(c_across(c(psqi_sbqual_fm1,psqi_latncgr_fm1,psqi_sdgr_fm1,
                                   psqi_segr_fm1,psqi_distbgr_fm1,sleepmed_fm1,psqi_dysfgr_fm1)),na.rm=T)
  ) |>
  mutate(
    psqi_gdslp = ifelse(psqi_gl_fm1 < 4.99, 1, ifelse(psqi_gl_fm1 > 4.99999, 0, NA_real_))
  ) |> 
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq25")))
# # View(t1_q_PSQI_fm)

# Distress thermometer FM
t1_q_distr_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q26")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    distr_therm_fm1 = case_when(
      q26_fm1  %in% 0:3 ~ 0,
      q26_fm1  %in% 4:10 ~ 1)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq26")))
# # View(t1_q_distr_fm)

# Depressive symptoms CES-D FM
CESDvars1_fm = c("q27d_fm1", "q27h_fm1", "q27l_fm1", "q27p_fm1")
CESDvars2_fm = names(t1_q_fm_orig |> select(starts_with("q27")) |> select(-all_of(CESDvars1_fm)))
t1_q_cesd_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q27")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(all_of(CESDvars2_fm)), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      TRUE ~ NA_real_  )
  ) |>
  mutate_at(
    vars(all_of(CESDvars1_fm)), ~ case_when(
      . == 4 ~ 0,
      . == 3 ~ 1,
      . == 2 ~ 2,
      . == 1 ~ 3,
      TRUE ~ NA_real_ )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    CESD_fm1 = sum(c_across(q27a_fm1:q27t_fm1), na.rm=T),
    sd_CESD_fm1 = sd(c_across(q27a_fm1:q27t_fm1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq27")))
# # View(t1_q_cesd_fm)

# FACIT sp: spirituality fm
t1_q_factsp_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q28")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q28a_fm1:q28l_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q28d_fm1, q28h_fm1), ~ case_when(
      . == 4 ~ 0,
      . == 3 ~ 1,
      . == 2 ~ 2,
      . == 1 ~ 3,
      . == 0 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  mutate(
    AHnotna_fm = rowSums(!is.na(across(q28a_fm1:q28h_fm1))),
    ILnotna_fm = rowSums(!is.na(across(q28i_fm1:q28l_fm1))),
    ALnotna_fm = rowSums(!is.na(across(q28a_fm1:q28l_fm1)))
  ) |>
  mutate(
    FACITSp12MP_fm1 = ifelse(AHnotna_fm > 4, sum(c_across(q28a_fm1:q28h_fm1), na.rm=T) 
                             * (8/AHnotna_fm), NA_real_),
    FACITSp12F_fm1 = ifelse(ILnotna_fm > 2, sum(c_across(q28i_fm1:q28l_fm1), na.rm=T) 
                            * (4/ILnotna_fm), NA_real_)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    FACITSp12MP_fm1,
    FACITSp12F_fm1,
    FACITSp12_fm1 = FACITSp12MP_fm1 + FACITSp12F_fm1,
    
    sd_FACITSp12MP_fm1 = ifelse(AHnotna_fm > 4, sd(c_across(q28a_fm1:q28h_fm1), na.rm=T), NA_real_),
    sd_FACITSp12F_fm1 = ifelse(ILnotna_fm > 2, sd(c_across(q28i_fm1:q28l_fm1), na.rm=T), NA_real_)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq28")))
# # View(t1_q_factsp_fm)

# ISI: Insomnia Severity Index FM
t1_q_ISI_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q29")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q29a_fm1:q29g_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ . )
  ) |>
  mutate(
    isi_fm1 = sum(c_across(q29a_fm1:q29g_fm1), na.rm=T),
    sd_isi_fm1 = sd(c_across(q29a_fm1:q29g_fm1), na.rm=T)
  )|>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    isi_fm1,
    isi_gr_fm1 = case_when(
      isi_fm1 >= 0 && isi_fm1 <= 7 ~ 0,
      isi_fm1 >= 8 && isi_fm1 <= 14 ~ 1,
      isi_fm1 >= 15 && isi_fm1 <= 21 ~ 2,
      isi_fm1 >= 22 && isi_fm1 <= 28 ~ 3 ),
    sd_isi_fm1 
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq29")))
# # View(t1_q_ISI_fm)

# ESS: Epworth Sleepiness Scale FM
t1_q_ESS_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q30")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q30a_fm1:q30h_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      TRUE ~ . )
  ) |>
  mutate(
    ess_fm1 = sum(c_across(q30a_fm1:q30h_fm1), na.rm=T),
    sd_ess_fm1 = sd(c_across(q30a_fm1:q30h_fm1), na.rm=T)
  )|>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    ess_fm1,
    ess_gr_fm1 = case_when(
      ess_fm1 > -1 && ess_fm1 < 5.00001 ~ 0,
      ess_fm1 > 5.999999 && ess_fm1 < 10.00001 ~ 1,
      ess_fm1 > 10.999999 && ess_fm1 < 12.00001 ~ 2,
      ess_fm1 > 12.999999 && ess_fm1 < 15.00001 ~ 3,
      ess_fm1 > 15.999999 && ess_fm1 < 24.00001 ~ 3 ),
    sd_ess_fm1
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq30")))
# # View(t1_q_ESS_fm) 

# Benefit Finding Scale FM
t1_q_BF_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q31")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    BF_AC_fm1 = mean(c_across(q31a_fm1:q31c_fm1), na.rm=T),
    BF_EM_fm1 = mean(c_across(q31f_fm1:q31i_fm1), na.rm=T),
    BF_AP_fm1 = mean(c_across(q31l_fm1:q31n_fm1), na.rm=T),
    BF_FA_fm1 = mean(c_across(c(q31d_fm1,q31e_fm1)), na.rm=T),
    BF_SV_fm1 = mean(c_across(c(q31j_fm1,q31k_fm1,q31q_fm1)), na.rm=T),
    BF_RE_fm1 = mean(c_across(q31o_fm1:q31p_fm1), na.rm=T),
    bf_fm1 = mean(c_across(q31a_fm1:q31p_fm1), na.rm=T),
    
    sd_BF_AC_fm1 = sd(c_across(q31a_fm1:q31c_fm1), na.rm=T),
    sd_BF_EM_fm1 = sd(c_across(q31f_fm1:q31i_fm1), na.rm=T),
    sd_BF_AP_fm1 = sd(c_across(q31l_fm1:q31n_fm1), na.rm=T),
    sd_BF_FA_fm1 = sd(c_across(c(q31d_fm1,q31e_fm1)), na.rm=T),
    sd_BF_SV_fm1 = sd(c_across(c(q31j_fm1,q31k_fm1,q31q_fm1)), na.rm=T),
    sd_BF_RE_fm1 = sd(c_across(q31o_fm1:q31p_fm1), na.rm=T),
    sd_bf_fm1 = sd(c_across(q31a_fm1:q31q_fm1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq31")))
# # View(t1_q_BF_fm)

# POMS-SF 30: Profile of Mood States-Short Form fm
t1_q_POMS_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q32")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q32a_fm1:q32dd_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  mutate_at(
    vars(q32z_fm1), ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0,
      TRUE ~ NA_real_ )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    POMS_TA_fm1 = sum(c_across(c(q32a_fm1, q32f_fm1,  q32l_fm1, q32p_fm1, q32t_fm1)), na.rm=T),
    POMS_DP_fm1 = sum(c_across(c(q32g_fm1, q32k_fm1, q32o_fm1, q32q_fm1, q32u_fm1)), na.rm=T),
    POMS_AH_fm1 = sum(c_across(c(q32b_fm1, q32i_fm1, q32n_fm1, q32y_fm1, q32bb_fm1)), na.rm=T),
    POMS_VA_fm1 = sum(c_across(c(q32d_fm1, q32h_fm1, q32j_fm1, q32aa_fm1,  q32dd_fm1)), na.rm=T),
    POMS_FI_fm1 = sum(c_across(c(q32c_fm1, q32m_fm1, q32s_fm1, q32v_fm1, q32w_fm1)), na.rm=T),
    POMS_CB_fm1 = sum(c_across(c(q32e_fm1, q32r_fm1	, q32x_fm1, q32z_fm1, q32cc_fm1)), na.rm=T),
    
    sd_POMS_TA_fm1 = sd(c_across(c(q32a_fm1, q32f_fm1,  q32l_fm1, q32p_fm1, q32t_fm1)), na.rm=T),
    sd_POMS_DP_fm1 = sd(c_across(c(q32g_fm1, q32k_fm1, q32o_fm1, q32q_fm1, q32u_fm1)), na.rm=T),
    sd_POMS_AH_fm1 = sd(c_across(c(q32b_fm1, q32i_fm1, q32n_fm1, q32y_fm1, q32bb_fm1)), na.rm=T),
    sd_POMS_VA_fm1 = sd(c_across(c(q32d_fm1, q32h_fm1, q32j_fm1, q32aa_fm1,  q32dd_fm1)), na.rm=T),
    sd_POMS_FI_fm1 = sd(c_across(c(q32c_fm1, q32m_fm1, q32s_fm1, q32v_fm1, q32w_fm1)), na.rm=T),
    sd_POMS_CB_fm1 = sd(c_across(c(q32e_fm1, q32r_fm1	, q32x_fm1, q32z_fm1, q32cc_fm1)), na.rm=T)
  ) |>
  mutate(
    POMS_TMD_fm1 = POMS_TA_fm1+POMS_DP_fm1+POMS_AH_fm1+POMS_FI_fm1+POMS_CB_fm1-POMS_VA_fm1
  ) |>
  select(-starts_with("sd_"), everything()) |>
  ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq32")))
# # View(t1_q_POMS_fm)

# MASQ-30: Anxiety Symfmoms Subscale FM
t1_q_MASQ_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, starts_with("q33")) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q33a_fm1:q33j_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    masq_anx_fm1 = sum(c_across(q33a_fm1:q33j_fm1), na.rm=T),
    sd_masq_anx_fm1 = sd(c_across(q33a_fm1:q33j_fm1), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq33")))
# # View(t1_q_MASQ_fm)

# IES-R: Impact of Event Scale fm
POMSvars = names(t1_q_fm_orig |> select(starts_with("q32")) 
                 |> select(-c("q32d_fm1","q32h_fm1","q32j_fm1","q32aa_fm1","q32dd_fm1")))
t1_q_IES_fm = t1_q_fm_orig |> select(BiPs_IID, BiPs_DID, 
                                     starts_with(c("q27","q28","q32","q34","q58"))) |>
  mutate_all(remove_val_labels) |>
  rowwise() |>
  mutate_at(
    vars(q34a_fm1:q34v_fm1, q32a_fm1:q32dd_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1,
      . == 3 ~ 2,
      . == 4 ~ 3,
      . == 5 ~ 4,
      TRUE ~ NA_real_ )
  ) |>
  mutate_at(
    vars(q32z_fm1), ~ case_when(
      . == 0 ~ 4,
      . == 1 ~ 3,
      . == 2 ~ 2,
      . == 3 ~ 1,
      . == 4 ~ 0,
      TRUE ~ NA_real_ )
  ) |>
  mutate_at(
    vars(q27i_fm1, q27g_fm1, q27n_fm1, q58k_fm1, q58s_fm1), ~ case_when(
      . == 1 ~ 0,
      . == 2 ~ 1.3,
      . == 3 ~ 2.7,
      . == 4 ~ 4,
      TRUE ~ . )
  ) |>
  mutate_at(
    vars(q28a_fm1), ~ case_when(
      . == 1 ~ 4,
      . == 2 ~ 3,
      . == 3 ~ 2,
      . == 4 ~ 1,
      . == 5 ~ 0,
      TRUE ~ . )
  ) |>
  mutate(
    IES_AVD_fm1 = mean(c_across(c(q34e_fm1,q34g_fm1,q34h_fm1,q34k_fm1,q34l_fm1,q34m_fm1,q34q_fm1,q34v_fm1)), na.rm=T),
    IES_INT_fm1 = mean(c_across(c(q34a_fm1,q34b_fm1,q34c_fm1	,q34f_fm1,q34i_fm1, q34n_fm1,q34p_fm1,q34t_fm1)), na.rm=T),
    IES_HYP_fm1 = mean(c_across(c(q34d_fm1,q34j_fm1, q34o_fm1,q34r_fm1, q34s_fm1,q34u_fm1)), na.rm=T),
    IES_TOTAL_fm1 = sum(c_across(q34a_fm1:q34v_fm1), na.rm=T),
    
    POMS_dis_fm1 = mean(c_across(all_of(POMSvars)), na.rm=T),
  ) |>
  mutate(
    fmS_nacm_fm1 = mean(c_across(c(q27i_fm1, q27g_fm1, q27n_fm1, q58k_fm1, q58s_fm1, q28a_fm1, POMS_dis_fm1)), na.rm=T)
  ) |>
  transmute(
    BiPs_IID, 
    BiPs_DID,
    
    IES_AVD_fm1, IES_INT_fm1, IES_HYP_fm1, IES_TOTAL_fm1,
    POMS_dis_fm1,
    fmS_nacm_fm1,
    
    sd_IES_AVD_fm1 = sd(c_across(c(q34e_fm1,q34g_fm1,q34h_fm1,q34k_fm1,q34l_fm1,q34m_fm1,q34q_fm1,q34v_fm1)), na.rm=T),
    sd_IES_INT_fm1 = sd(c_across(c(q34a_fm1,q34b_fm1,q34c_fm1	,q34f_fm1,q34i_fm1, q34n_fm1,q34p_fm1,q34t_fm1)), na.rm=T),
    sd_IES_HYP_fm1 = sd(c_across(c(q34d_fm1,q34j_fm1, q34o_fm1,q34r_fm1, q34s_fm1,q34u_fm1)), na.rm=T),
    sd_IES_TOTAL_fm1 = sd(c_across(q34a_fm1:q34v_fm1), na.rm=T),
    
    sd_POMS_dis_fm1 = sd(c_across(all_of(POMSvars)), na.rm=T),
    sd_fmS_nacm_fm1 = sd(c_across(c(q27i_fm1, q27g_fm1, q27n_fm1, q58k_fm1, q58s_fm1, q28a_fm1, POMS_dis_fm1)), na.rm=T)
  ) |> ungroup() |>
  rename_with(~if_else(.x %in% c("BiPs_IID", "BiPs_DID"), .x, paste0(.x, "_outq34")))
# # View(t1_q_IES_fm)
