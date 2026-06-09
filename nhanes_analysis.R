# 01 Load Packages ----
library(nhanesA)
library(tidyverse)
library(gtsummary)
library(survey)

# 02 Import Data ----
demo <- nhanes('DEMO_J')

# 03 Clean and Recode ----
# Create vector of variable names for trimmed dataset
selection <- c(
  veteran = "DMQMILIZ",
  age_years = "RIDAGEYR",
  gender = "RIAGENDR",
  ethnicity = "RIDRETH3",
  hh_income = "INDHHIN2",
  education = "DMDEDUC2"
)
# Pass in variable vector to trim data set, change yes/no to veteran/non-veteran
# filter out missing data. 
demo_clean <- demo %>%
  select(all_of(selection)) %>%
  mutate(
    veteran = case_when(
      veteran == "Yes" ~ "Veteran",
      veteran == "No" ~ "Non-Veteran",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(veteran))


