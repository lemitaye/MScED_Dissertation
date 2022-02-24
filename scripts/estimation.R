
# Created on: February 22, 2022
# DEPENDS ON: data_samples.R

# This script runs the main regressions to be reported in the paper

rm(list = ls())

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

covars <- c(
  "child_age_month", "boy", "district", "moth_educ", "moth_pp_group", 
  "moth_age_year", "moth_income", "fath_inhh"
)

fm1_c <- as.formula(
  paste("no_kids ~ same_sex_12 + ", 
        paste(covars, collapse = "+"))
  )

m1_c <- lm( fm1_c, data = gt2_sample )


fm2_c <- as.formula(
  paste("no_kids ~ boy_1 + boy_2 + same_sex_12 + ", 
        paste(covars, collapse = "+"))
)

m2_c <- lm( fm2_c, data = gt2_sample )

fm3_c <- as.formula(
  paste("no_kids ~ boy_1 + boy_12 + girl_12 + ", 
        paste(covars, collapse = "+"))
)

m3_c <- lm( fm3_c, data = gt2_sample )


fm4_c <- as.formula(
  paste("no_kids ~ twins_2 + child_age_month +", 
        paste(covars, collapse = "+"))
)

m4_c <- lm( fm4_c, data = gt2_sample)


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

# F-tests 
linearHypothesis(m1_c, c("same_sex_12"))
linearHypothesis(m2_c, c("boy_1", "boy_2", "same_sex_12"))
linearHypothesis(m3_c, c("boy_1", "boy_12", "girl_12"))
linearHypothesis(m4_c, c("twins_2"))


# 2SLS/IV regressions ####

## 2+ sample ####

### Educational attainment ####

fOLS_A1 <- as.formula(
  paste("educ_attain ~ no_kids + ", paste(covars, collapse = "+"))
)

fIV_A1 <- as.formula( # twins_2 instrument
  paste(
    "educ_attain ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | twins_2 + ", paste(covars, collapse = "+"))
  )
)

fIV_A2 <- as.formula( # same_sex instrument
  paste(
    "educ_attain ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | same_sex_12 + ", paste(covars, collapse = "+") )
  )
)

fIV_A3 <- as.formula( # same_sex instrument disaggregated
  paste(
    "educ_attain ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | boy_12 + girl_12 + ", paste(covars, collapse = "+") )
  )
)

fIV_A4 <- as.formula( # all instruments
  paste(
    "educ_attain ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | twins_2 + boy_12 + girl_12 + ", paste(covars, collapse = "+") )
  )
)

OLS_A1 <- lm(fOLS_A1, data = gt2_sample)
IV_A1 <- ivreg(fIV_A1, data = gt2_sample)
IV_A2 <- ivreg(fIV_A2, data = gt2_sample)
IV_A3 <- ivreg(fIV_A3, data = gt2_sample)
IV_A4 <- ivreg(fIV_A4, data = gt2_sample)

stargazer(
  OLS_A1, IV_A1, IV_A2, IV_A3, IV_A4,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


### Private school attendance ####

fOLS_A2 <- as.formula(
  paste("private_school ~ no_kids + ", paste(covars, collapse = "+"))
)

fIV_A5 <- as.formula( # twins_2 instrument
  paste(
    "private_school ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | twins_2 + ", paste(covars, collapse = "+") )
  )
)

fIV_A6 <- as.formula( # same_sex instrument
  paste(
    "private_school ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | same_sex_12 + ", paste(covars, collapse = "+") )
  )
)

fIV_A7 <- as.formula( # same_sex instrument
  paste(
    "private_school ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | boy_12 + girl_12 + ", paste(covars, collapse = "+") )
  )
)

fIV_A8 <- as.formula( # same_sex instrument
  paste(
    "private_school ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | twins_2 + boy_12 + girl_12 + ", paste(covars, collapse = "+") )
  )
)

OLS_A2 <- lm(fOLS_A2, data = gt2_sample)
IV_A5  <- ivreg(fIV_A5, data = gt2_sample)
IV_A6  <- ivreg(fIV_A6, data = gt2_sample)
IV_A7  <- ivreg(fIV_A7, data = gt2_sample)
IV_A8  <- ivreg(fIV_A8, data = gt2_sample)

stargazer(
  OLS_A2, IV_A5, IV_A6, IV_A7, IV_A8,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


### Mothers' LFP ####

fOLS_A3 <- as.formula(
  paste("moth_inlf ~ no_kids + ", paste(covars, collapse = "+"))
)

fIV_A9 <- as.formula( # twins_2 instrument
  paste(
    "moth_inlf ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | twins_2 + ", paste(covars, collapse = "+") )
  )
)

fIV_A10 <- as.formula( # same_sex instrument
  paste(
    "moth_inlf ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | same_sex_12 + ", paste(covars, collapse = "+") )
  )
)

fIV_A11 <- as.formula( # same_sex instrument
  paste(
    "moth_inlf ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | boy_12 + girl_12 + ", paste(covars, collapse = "+") )
  )
)

fIV_A12 <- as.formula( # same_sex instrument
  paste(
    "moth_inlf ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | twins_2 + boy_12 + girl_12 + ", paste(covars, collapse = "+") )
  )
)

OLS_A3 <- lm(fOLS_A3, data = gt2_sample)
IV_A9  <- ivreg(fIV_A9, data = gt2_sample)
IV_A10 <- ivreg(fIV_A10, data = gt2_sample)
IV_A11 <- ivreg(fIV_A11, data = gt2_sample)
IV_A12 <- ivreg(fIV_A12, data = gt2_sample)

stargazer(
  OLS_A3, IV_A9, IV_A10, IV_A11, IV_A12,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)

# 3+ sample #####

# Angrist et al. (2010) cluster the standard errors for the regressions using
# the 3+ sample by mohter's ID. (see p. 791)
m5 <- lm(no_kids ~ same_sex_123 + boy, data = gt3_sample)
m6 <- lm(no_kids ~ boy_123 + girl_123 + boy_3:I(1-same_sex_12), 
         data = gt3_sample)
m7 <- lm(no_kids ~ twins_3, data = gt3_sample)
stargazer(m5, m6, m7, type = "text", keep.stat = c("n","rsq"))

# Educational attainment

make_formula <- function(dep_var, instrument = 0) {
  
  covar1 <- c("child_age_month", "boy", "moth_age_year", "fath_inhh")
  covar2 <- c("district", "moth_educ", "moth_pp_group", "moth_income")
  covar <- paste( paste(covar1, collapse = "+"), "|",  
                  paste(covar2, collapse = "+") )
  
  if (instrument == 0) {
    f <- as.formula( paste( dep_var, " ~ no_kids + ", covar, 
                            " | 0 | moth_no" ) )
  } else {
    f <- as.formula( paste( dep_var, " ~ ", covar, 
                            " | (no_kids ~ ", instrument, ")", " | moth_no" ) )
  }
  
  return(f)
}

fOLS_B1 <- make_formula("educ_attain")
fIV_B1 <- make_formula("educ_attain", "twins_3")
fIV_B2 <- make_formula("educ_attain", "same_sex_123")
fIV_B3 <- make_formula("educ_attain", "boy_123 + girl_123")
fIV_B4 <- make_formula("educ_attain", "twins_3 + boy_123 + girl_123")

fOLS_B1 <- as.formula( paste("educ_attain ~ no_kids + ", covar, 
                             " | 0 | moth_no") )

fIV_B1 <- as.formula( paste("educ_attain ~ ", covar, 
                  " | (no_kids ~ twins_3)", " | moth_no" ) )

fIV_B2 <- as.formula( paste("educ_attain ~ ", covar, 
                  " | (no_kids ~ same_sex_123)", " | moth_no") )

fIV_B3 <- as.formula( paste("educ_attain ~ ", covar, 
                  " | (no_kids ~ boy_123 + girl_123)", 
                  " | moth_no") )

fIV_B4 <- as.formula( paste("educ_attain ~ ", covar, 
                  " | (no_kids ~ twins_3 + boy_123 + girl_123)", 
                  " | moth_no") )

OLS_B1 <- felm(fOLS_B1, data = gt3_sample)
IV_B1 <- felm(fIV_B1, data = gt3_sample)
IV_B2 <- felm(fIV_B2, data = gt3_sample)
IV_B3 <- felm(fIV_B3, data = gt3_sample)
IV_B4 <- felm(fIV_B4, data = gt3_sample)

stargazer(
  OLS_B1, IV_B1, IV_B2, IV_B3, IV_B4,
  keep = c("no_kids"), 
  type = "text", 
  keep.stat = c("n","rsq"))

# Private school attendance
OLS_B2 <- lm(private_school ~ no_kids, data = gt3_sample)
IV_B5 <- ivreg(private_school ~ no_kids | 
                 twins_3, data = gt3_sample)
IV_B6 <- ivreg(private_school ~ no_kids | 
                 same_sex_123, data = gt3_sample)
IV_B7 <- ivreg(private_school ~ no_kids | 
                 boy_123 + girl_123, data = gt3_sample)
IV_B8 <- ivreg(private_school ~ no_kids | 
                 twins_3 + boy_123 + girl_123, data = gt3_sample)
stargazer(OLS_B2, IV_B5, IV_B6, IV_B7, IV_B8, 
          type = "text", keep.stat = c("n","rsq"))


# test

covars <- c(
  "child_age_month", "boy", "district", "moth_educ", "moth_pp_group", 
  "moth_age_year", "moth_income", "fath_inhh"
)

fiv <- as.formula( # same_sex instrument
  paste(
    "moth_inlf ~ no_kids + ", paste(covars, collapse = "+"),
    paste(" | boy_12 + girl_12 + ", paste(covars, collapse = "+") )
  )
)

ivest <- felm(
  educ_attain ~ no_kids + child_age_month + boy + moth_age_year + fath_inhh | 
    district + moth_educ + moth_pp_group + moth_income  
  data = gt2_sample
  )


stargazer(IV_A1, type = "text", keep = c("no_kids"), keep.stat = c("n","rsq"))
stargazer(
  summary(ivest), 
  type = "text", 
  keep = c("no_kids"), 
  keep.stat = c("n","rsq")
  )

j <- felm(
  educ_attain|private_school|moth_inlf ~ child_age_month + boy + 
    moth_age_year + fath_inhh | district + moth_educ + moth_pp_group + 
    moth_income | (no_kids ~ twins_2), 
  data = gt2_sample
)

J_educ <- summary(j, lhs = "educ_attain", robust = TRUE)
as(J_ed)

stargazer(J_educ, 
          lhs = "educ_attain",
          type = "text", 
          keep = c("no_kids"), 
          keep.stat = c("n","rsq"))









