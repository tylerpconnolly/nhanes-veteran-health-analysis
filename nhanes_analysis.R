# 01 Load Packages ----
library(nhanesA)
library(tidyverse)
library(gtsummary)
library(survey)
library(broom)
library(pROC)

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
    across(                                             # Applies mutate to all cols
      .cols = c(DPQ010, DPQ020, DPQ030, DPQ040, DPQ050, # Cols to apply
                DPQ060, DPQ070, DPQ080, DPQ090),
      .fns = ~ case_when(                               # function applied
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
    phq9_score = rowSums(across(ends_with("_num")), na.rm = FALSE) # phq9 calculation
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
  mutate(                   # Collapsing education categories
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
  mutate(                  # Collapsing household income to 4 categories
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
  ) %>%
  mutate(                 # Binary depression scale 
    phq9_binary = case_when(
      phq9_score < 10 ~ "Not Depressed",
      phq9_score >= 10 ~ "Depressed",
      .default = NA_character_
    ),
    phq9_binary = factor(phq9_binary, levels = c("Not Depressed", "Depressed"))
  )
# Setting reference ethnicity to Non-hispanic White
df_clean <- df_clean %>%
  mutate(
    ethnicity = relevel(factor(ethnicity), ref = "Non-Hispanic White")
  )

# 04 Table Summary and Logistic Regression Model ----
# Table with statistical significance separated by veteran status
df_clean %>%
  tbl_summary(
    by = veteran, 
    include = c(age_years, gender, ethnicity, hh_income,
                education, phq9_category, , phq9_score, diabetes,
                hypertension, high_cholest, cardio_burden,
                stroke, cancer),
    label = list(
      age_years ~ "Age (Years)",
      gender ~ "Gender",
      ethnicity ~ "Ethnicity",
      hh_income ~ "Household Income",
      education ~ "Education", 
      phq9_category ~ "PHQ9 Depression Scale", 
      phq9_score ~ "PHQ9 Depression Score",
      diabetes ~ "Diabetes", 
      hypertension ~ "Hypertension", 
      high_cholest ~ "High Cholesterol", 
      cardio_burden ~ "Cardio Burden", 
      stroke ~ "Stroke", 
      cancer ~ "Cancer"
    )
  ) %>%
  add_p()


# Unadjusted odds ratio only for depression scale and veteran status
model_unadjusted <- glm(
  phq9_binary ~ veteran,
  data = df_clean,
  family = "binomial"
)


# Adjusted logistic regression model including covariates
model <- glm(
  phq9_binary ~ veteran + age_years + gender + ethnicity + hh_income+
    education + diabetes + hypertension + high_cholest + cardio_burden +
    stroke + cancer, 
  data = df_clean,
  family = "binomial"
) 

# Model summary, estimate, std dev, z value
summary(model)
# Exponentiated model with intercept and coefficients
exp(coef(model))
# Confidence intervals of exponentiated model
exp(confint(model))
# Odds ratio table with 95% CIs and p-values 
model %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      veteran      ~ "Veteran Status",
      age_years    ~ "Age (Years)",
      gender       ~ "Gender",
      ethnicity    ~ "Race/Ethnicity",
      hh_income    ~ "Household Income",
      education    ~ "Education Level",
      diabetes     ~ "Diabetes",
      hypertension ~ "Hypertension",
      high_cholest ~ "High Cholesterol",
      cardio_burden ~ "Cardiovascular Burden",
      stroke       ~ "Stroke",
      cancer       ~ "Cancer"
    )
  )
# Dataframe produced from model results
model_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    label = case_when(
      term == "veteranVeteran"                               ~ "Veteran (vs Non-Veteran)",
      term == "age_years"                                    ~ "Age (years)",
      term == "genderFemale"                                 ~ "Female (vs Male)",
      term == "ethnicityMexican American"                    ~ "Mexican American (vs Non-Hispanic White)",
      term == "ethnicityOther Hispanic"                      ~ "Other Hispanic (vs Non-Hispanic White)",
      term == "ethnicityNon-Hispanic Black"                  ~ "Non-Hispanic Black (vs Non-Hispanic White)",
      term == "ethnicityNon-Hispanic Asian"                  ~ "Non-Hispanic Asian (vs Non-Hispanic White)",
      term == "ethnicityOther Race - Including Multi-Racial" ~ "Other/Multi-Racial (vs Non-Hispanic White)",
      term == "hh_incomeLow income"                          ~ "Low Income (vs High Income)",
      term == "hh_incomeLower middle"                        ~ "Lower Middle Income (vs High Income)",
      term == "hh_incomeUpper middle"                        ~ "Upper Middle Income (vs High Income)",
      term == "educationHigh school graduate/GED"            ~ "High School/GED (vs College Graduate)",
      term == "educationLess than high school"               ~ "Less Than High School (vs College Graduate)",
      term == "educationSome college or AA degree"           ~ "Some College (vs College Graduate)",
      term == "diabetesYes"                                  ~ "Diabetes (vs No Diabetes)",
      term == "hypertensionYes"                              ~ "Hypertension (vs No Hypertension)",
      term == "high_cholestYes"                              ~ "High Cholesterol (vs No High Cholesterol)",
      term == "cardio_burdenNo Cardio Burden"                ~ "No Cardiovascular Burden (vs Burden)",
      term == "strokeYes"                                    ~ "Stroke (vs No Stroke)",
      term == "cancerYes"                                    ~ "Cancer (vs No Cancer)"
    ),
    significant = case_when(
      p.value < 0.05 ~ "Significant",
      TRUE ~ "Not Significant"
    )
  )

model_data <- model$model
# 05 Plotting Model Outcome ----
# Forest plot displaying adjusted odds ratio for each predictor
ggplot(data = model_results) +
  geom_point(mapping = aes(x = estimate, y = reorder(label, estimate, FUN = mean),
                           color = significant), size = 3) +
  geom_errorbarh(aes(y = reorder(label, estimate),
                     xmin = conf.low, xmax = conf.high), width = 0) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Predictors of Depression in US Adults: Adjusted Odds Ratios (NHANES 2017-2018)",
    x = "Odds Ratio Estimate",
    y = NULL,
    color = "Statistical Significance"
    
  ) + 
  scale_color_manual(
    values = c("Significant" = "steelblue", "Not Significant" = "salmon"),
    labels = c("Significant" = "Significant (p < 0.05)", 
               "Not Significant" = "Not Significant (p ≥ 0.05)")
  ) + 
  theme_minimal()
  
  
# ROC curve 
roc_curve <- roc(model$y, fitted(model))

roc_df <- data.frame(
  specificity = 1 - roc_curve$specificities,
  sensitivity = roc_curve$sensitivities
)
  
ggplot(roc_df, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  annotate("text", x = 0.6, y = 0.2, 
           label = paste("AUC =", round(auc(roc_curve), 3))) +
  labs(
    title = "ROC Curve - Depression Prediction Model", 
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal()

# Boxplot for veteran and non-veteran PHQ-9 scores
filter_phq9 <- filter(df_clean, !is.na(phq9_score))
ggplot(filter_phq9, aes(x = veteran, y = phq9_score, fill = veteran)) +
  geom_jitter(alpha = 0.05, width = 0.2) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(
    title = "PHQ-9 Scores of Veterans and Non-veterans", 
    subtitle = "Veterans show slightly lower median PHQ-9 score than non-veterans",
    x = "Veteran Status",
    y = "PHQ-9 Scores"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 
  

