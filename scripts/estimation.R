
# Created on: February 22, 2022
# DEPENDS ON: data_samples.R

# This script runs the main regressions to be reported in the paper

# Load saved data
gt2_sample <- read_csv("data/gt2_sample.csv")
gt3_sample <- read_csv("data/gt3_sample.csv")

# First-Stage ####

# 2+ sample

# Unconditional:
m1_u <- lm(no_kids ~ same_sex_12, data = gt2_sample)
m2_u <- lm(no_kids ~ boy_1 + boy_2 + same_sex_12, data = gt2_sample)
m3_u <- lm(no_kids ~ boy_1 + boy_12 + girl_12, data = gt2_sample)
m4_u <- lm(no_kids ~ twins_2, data = gt2_sample)

stargazer(
  m1_u, m2_u, m3_u, m4_u, 
  type = "text", 
  keep.stat = c("n","rsq")
  )

# Conditional:
m1_c <- lm(
  no_kids ~ same_sex_12 + child_age_month + boy + district + moth_educ +
    moth_pp_group + moth_age_year + moth_income + fath_educ + fath_income,
  data = gt2_sample
)

m2_c <- lm(
  no_kids ~ boy_1 + boy_2 + same_sex_12 + child_age_month + boy +
    district + moth_educ + moth_pp_group + moth_age_year +
    moth_income + fath_educ + fath_income,
  data = gt2_sample
)

m3_c <- lm(
  no_kids ~ boy_1 + boy_12 + girl_12 + child_age_month + boy + district +
    moth_educ + moth_pp_group + moth_age_year + moth_income + fath_educ +
    fath_income,
  data = gt2_sample
)

m4_c <- lm(
  no_kids ~ twins_2 + child_age_month + boy + district + moth_educ +
    moth_pp_group + moth_age_year + moth_income + fath_educ + fath_income,
  data = gt2_sample
)

stargazer(
  m1_c, m2_c, m3_c, m4_c, 
  keep = c(
    "boy_1", "boy_2", "same_sex_12", "boy_12", "girl_12", "twins_2"
    ),
  type = "text", 
  keep.stat = c("n","rsq")
  ) 

stargazer( 
  m1_u, m2_u, m3_u, m4_u, m1_c, m2_c, m3_c, m4_c, 
  keep = c(
    "boy_1", "boy_2", "same_sex_12", "boy_12", "girl_12", "twins_2"
  ),
  type = "text", 
  keep.stat = c("n","rsq")
) 











