library(tidyverse)
library(haven)
library(labelled)
library(surveytoolbox)
library(dplyr)

# @prereq, needs BiPs_PTFM_T1_composite_full_060424.csv in folder "integrated"
# this is to be generated using fulltable.R script 
# copies of the integrated CSV files to be made available on Box 

# CANT READ TO CSV WITHOUT LOSING LABELS
#df_full <- read_csv("integrated/BiPs_PTFM_T1_integrated_full_082124.csv")

#df_full <- read_csv("integrated/BiPs_PTFM_T1_integrated_full_101424.csv")

# df_full <- read_csv("integrated/BiPs_PTFM_T1_integrated_full_012025.csv")

df_full <- read_csv("integrated/BiPs_PTFM_T1_integrated_full_012325.csv")

#----DATASET 1: predictors + SLEEP 
# QUESTIONAIRE: 196+1 PT columns, 197+13+2+5+1 FM columns, 415 total 
## CTask variables FM only -- 13
## ADL variables FM only -- 2
## caregiving stress and experience FM only -- 5
## Pittsburgh Sleep Quality Index PT/FM -- 2
##  TODO needs to be renamed "psqi_gdslp_q25.x" and "psqi_gdslp_q25.y"
##  TODO dyads B761 and B781 need to be deleted 

df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_q.*$")) |>
  select(contains("_pt_")|contains("_pt1_"))

df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_q.*$")) |>
  select(contains("_fm_")|contains("_fm1_"))

df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_q.*$")) |>
  select(!contains("_pt_") & !contains("_pt1_")  & 
           !contains("_fm_") & !contains("_fm1_")  )

# SCREENER: 17 PT columns, 17 FM columns, 34 total 
df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_screen$")) |>
  select(contains("_pt_")|contains("_pt1_"))

df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_screen$")) |>
  select(contains("_fm_")|contains("_fm1_"))

# ECG/CARDIO: 6 PT, 6 FM, 12 total
df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_cardio$")) 

# SLEEP LOG: 6 PT, 6 FM, 12 total 
df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_sleep$")) 

# ACTIGRAPH COMP: 5 PT, 5 FM, 10 total
df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_actcomp$")) 

# ACTIGRAPH RHYTHM: 3 PT, 3 FM, 6 total
df_full |>
  select(-starts_with("sd_")) |>
  select(matches("_actrhythm$")) 

# QUESTIONAIRE: 196+1 PT columns, 197+13+2+5+1 FM columns, 415 total 
# SCREENER: 17 PT columns, 17 FM columns, 34 total 
# ECG/CARDIO: 6 PT, 6 FM, 12 total
# SLEEP LOG: 6 PT, 6 FM, 12 total 
# ACTIGRAPH COMP: 5 PT, 5 FM, 10 total
# ACTIGRAPH RHYTHM: 3 PT, 3 FM, 6 total
# 1 ID variable (BiPs_DID)
## dataset 1: 153 observations, 489 variables , 1 ID
# with_sleep <- df_full |>
#   select(-starts_with("sd_")) |>
#   select(BiPs_DID|matches("_q.*$")|matches("_screen$")|matches("_cardio$")|
#            matches("_sleep$")|matches("_actcomp$")|matches("_actrhythm$"))
# 
# with_sleep |>
#   write_csv("integrated/BiPs_PTFM_T1_integrated_sleep_060424.csv")
#------------
# DATASET 2: TBA
# ... 

#---------------------------------------------------------------------------
pred_list = c("_q", "_screen", "_cardio", "_co", "_clo")

with_sleep = df_full |> select(BiPs_DID, contains(c(pred_list, "_sleep", "_actcomp", "_actrhythm")), -starts_with("sd_"))
# with_sleep |>
#   write_csv("integrated/BiPs_PTFM_T1_integrated_sleep_082124.csv")
# with_sleep |>
#   write_csv("integrated/BiPs_PTFM_T1_integrated_sleep_101424.csv")
# with_sleep |>
#   write_csv("integrated/BiPs_PTFM_T1_integrated_sleep_012025.csv")
with_sleep |>
  write_csv("integrated/BiPs_PTFM_T1_integrated_sleep_012325.csv")

#### Dataset 2: [ALL PREDICTORS] + [MOOD OUTCOMES]
# with_mood = df_full |> select(BiPs_DID, contains(c(pred_list, "_mood")), -starts_with("sd_"))
# with_mood |>
#   write_csv("integrated/BiPs_PTFM_T1_integrated_mood_062224.csv")

#### Dataset 3: [ALL PREDICTORS] + [ACUTE AFFECT OUTCOMES]
# with_affect = df_full |> select(BiPs_DID, contains(pred_list), ends_with("_affect"), -starts_with("sd_"))
# with_affect |>
#   write_csv("integrated/BiPs_PTFM_T1_integrated_affect_062224.csv")

#### Dataset 4: [ALL PREDICTORS] + [QUESTIONNAIRE SECTION 2
# with_quest = df_full |> select(BiPs_DID, contains(c(pred_list, "_outq")), -starts_with("sd_"))
# with_quest |>
#   write_csv("integrated/BiPs_PTFM_T1_integrated_quest_062224.csv")