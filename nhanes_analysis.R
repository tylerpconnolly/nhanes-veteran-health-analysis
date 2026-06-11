# 01 Load Packages ----
library(nhanesA)
library(tidyverse)
library(gtsummary)
library(survey)

# 02 Import Data ----
demo <- nhanes('DEMO_J')

# 03 Clean and Recode ----
# Create vector of variable names for trimmed data set
demo_selection <- c(
  seqn      = "SEQN",
  veteran   = "DMQMILIZ",
  age_years = "RIDAGEYR",
  gender    = "RIAGENDR",
  ethnicity = "RIDRETH3",
  hh_income = "INDHHIN2",
  education = "DMDEDUC2"
)
# Pass in variable vector to trim data set, change yes/no to veteran/non-veteran
# filter out missing data. 
demo_clean <- demo %>%
  select(all_of(demo_selection)) %>%
  mutate(
    veteran = case_when(
      veteran == "Yes" ~ "Veteran",
      veteran == "No" ~ "Non-Veteran",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(veteran))
# Blood pressure and cholesterol var selection and trimming
bpq_clean <- nhanes('BPQ_J') %>%
  select(SEQN, BPQ020, BPQ080) %>%
  rename(
    seqn = SEQN,
    hypertension = BPQ020,
    high_cholest = BPQ080
  )
# Diabetes var selection and trimming
diq_clean <- nhanes('DIQ_J') %>%
  select(SEQN, DIQ010) %>%
  rename(
    seqn = SEQN,
    diabetes = DIQ010
  )
# Medical conditions var selection and trimming
# Single cardio_burden var created based on 4 conditions
mcq_clean <- nhanes('MCQ_J') %>%
  select(SEQN, MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F, MCQ220) %>%
  rename(
    seqn = SEQN,
    heart_fail = MCQ160B,
    heart_disease = MCQ160C,
    angina = MCQ160D,
    heart_attack = MCQ160E,
    stroke = MCQ160F,
    cancer = MCQ220
  ) %>%
  mutate(
    cardio_burden = case_when(
      heart_fail == "Yes" | heart_disease == "Yes" |
      angina == "Yes" | heart_attack == "Yes" ~ "Cardio Burden",
      heart_fail == "No" & heart_disease == "No" &
      angina == "No" & heart_attack == "No" ~ "No Cardio Burden",
      .default = NA_character_
    )
  )

# Change categorical vars into numeric scale for PHQ-9 calculation
dpq_clean <- nhanes('DPQ_J') %>%
  rename(seqn = SEQN) %>%
  mutate(
    across(
      .cols = c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, 
                DPQ060, DPQ070, DPQ080, DPQ090),
      .fns = ~ case_when(
        .x == "Not at all" ~ 0,
        .x == "Several days" ~ 1,
        .x == "More than half the days" ~ 2,
        .x == "Nearly every day" ~ 3,
        .default = NA_real_
      ),
      .names = "{.col}_num"
    )
  ) %>% 
  mutate(
    phq9_score = rowSums(across(ends_with("_num")), na.rm = FALSE)
  ) %>%
  mutate(
    phq9_category = case_when(
      between(phq9_score, 0, 4) ~ "No/Minimal depression",
      between(phq9_score, 5, 9) ~ "Mild depression",
      between(phq9_score, 10, 14) ~ "Moderate depression",
      between(phq9_score, 15, 27) ~ "Moderately severe/Severe depression",
      .default = NA_character_
      
    )
  ) 

# Combining all cleaned data frames 
df_list <- list(demo_clean, dpq_clean, diq_clean, bpq_clean, mcq_clean)
df_complete <- df_list %>% reduce(left_join, by = 'seqn')
# Setting refused or don't knows to NA
df_clean <- df_complete %>%
  mutate(
    across(
      .cols = c(hypertension, high_cholest, diabetes, stroke, cancer),
      .fns = ~ case_when(
        .x == "Yes" ~ "Yes",
        .x == "No" ~ "No",
        .default = NA_character_
      )
    )
  ) %>%
  mutate(
    education = case_when(
      education == "Less than 9th grade" ~ "Less than high school",
      education == "9-11th grade (Includes 12th grade with no diploma)" ~
        "Less than high school",
      education == "High school graduate/GED or equivalent" ~ 
        "High school graduate/GED", 
      education == "Some college or AA degree" ~ "Some college or AA degree", 
      education == "College graduate or above" ~ "College graduate or above",
      .default = NA_character_
      
    )
  ) %>%
  mutate(
    hh_income = case_when(
      hh_income == "$ 0 to $ 4,999" ~ "Low income",
      hh_income == "$ 5,000 to $ 9,999" ~ "Low income",
      hh_income == "$10,000 to $14,999" ~ "Low income",
      hh_income == "$15,000 to $19,999" ~ "Low income",
      hh_income == "Under $20,000" ~ "Low income",
      hh_income == "$20,000 and Over" ~ "Lower middle",
      hh_income == "$20,000 to $24,999" ~ "Lower middle",
      hh_income == "$25,000 to $34,999" ~ "Lower middle",
      hh_income == "$35,000 to $44,999" ~ "Lower middle",
      hh_income == "$45,000 to $54,999" ~ "Upper middle",
      hh_income == "$55,000 to $64,999" ~ "Upper middle",
      hh_income == "$65,000 to $74,999" ~ "Upper middle",
      hh_income == "$75,000 to $99,999" ~ "High income",
      hh_income == "$100,000 and Over" ~ "High income",
      .default = NA_character_
    )
  )

df_clean |>
  tbl_summary(
    by = veteran, 
    include = c(age_years, gender, ethnicity, hh_income,
                education, phq9_category, , phq9_score, diabetes,
                hypertension, high_cholest, cardio_burden,
                stroke, cancer)
  ) |>
  add_p(test = list(education ~ "chisq.test"))
