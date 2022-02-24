
# Created on: February 22, 2022
# DEPENDS ON: data_samples.R

# This script runs the main regressions to be reported in the paper

rm(list = ls())

# Load saved data
gt2_sample <- read_csv("data/gt2_sample.csv")
gt3_sample <- read_csv("data/gt3_sample.csv")

# convert all character columns into factors
gt2_sample <- gt2_sample %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    district = factor(district),
    municip = factor(municip),
    moth_no = factor(moth_no)
    )

gt3_sample <- gt3_sample %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    district = factor(district),
    municip = factor(municip),
    moth_no = factor(moth_no)
  )


# Functions for constructing formulas ####

make_formula_frst_stg <- function(dep_var, instrument, clus = FALSE) {
  # Define a vector of covariates
  covar1 <- c("child_age_month", "boy", "moth_age_year", "fath_inhh")
  covar2 <- c("district", "moth_educ", "moth_pp_group", "moth_income")
  covar <- paste( paste(covar1, collapse = "+"), "|",  
                  paste(covar2, collapse = "+") )
  
  if (clus) {
    f <- as.formula( paste( dep_var, " ~ ", instrument, " + ", covar,
                            " | 0 | moth_no" ) )
  } else {
    f <- as.formula( paste( dep_var, " ~ ", instrument, " + ", covar ) )
  }
  
  return(f)
}

make_formula_gt2 <- function(dep_var, instrument = 0) {
  # Define a vector of covariates
  covar1 <- c("child_age_month", "boy", "moth_age_year", "fath_inhh")
  covar2 <- c("district", "moth_educ", "moth_pp_group", "moth_income")
  covar <- paste( paste(covar1, collapse = "+"), "|",  
                  paste(covar2, collapse = "+") )
  
  if (instrument == 0) {
    f <- as.formula( paste( dep_var, " ~ no_kids + ", covar ) )
  } else {
    f <- as.formula( paste( dep_var, " ~ ", covar, 
                            " | (no_kids ~ ", instrument, ")" ) )
  }
  
  return(f)
}

make_formula_gt3 <- function(dep_var, instrument = 0) {
  
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



# First-Stage ####

## 2+ sample ####

fm_a1 <- make_formula_frst_stg("no_kids", "twins_2")
fm_a2 <- make_formula_frst_stg("no_kids", "same_sex_12")
fm_a3 <- make_formula_frst_stg("no_kids", "boy_12 + girl_12")
fm_a4 <- make_formula_frst_stg("no_kids", "twins_2 + boy_12 + girl_12")

ma_1 <- felm( fm_a1, data = gt2_sample )
ma_2 <- felm( fm_a2, data = gt2_sample )
ma_3 <- felm( fm_a3, data = gt2_sample )
ma_4 <- felm( fm_a4, data = gt2_sample )

stargazer(
  ma_1, ma_2, ma_3, ma_4, 
  keep = c(
    "boy_1", "same_sex_12", "boy_12", "girl_12", "twins_2"
  ),
  type = "text", 
  keep.stat = c("n","rsq")
) 

# F-tests 
linearHypothesis(ma_1, c("same_sex_12"))
linearHypothesis(ma_2, c("boy_1", "boy_2", "same_sex_12"))
linearHypothesis(ma_3, c("boy_1", "boy_12", "girl_12"))
linearHypothesis(ma_4, c("twins_2"))

## 3+ sample ####

# These are all clustere by mother's id
fm_b1 <- make_formula_frst_stg("no_kids", "twins_3", clus = TRUE)
fm_b2 <- make_formula_frst_stg("no_kids", "same_sex_123", clus = TRUE)
fm_b3 <- make_formula_frst_stg("no_kids", "boy_123 + girl_123", clus = TRUE)
fm_b4 <- make_formula_frst_stg("no_kids", "twins_3 + boy_123 + girl_123", 
                               clus = TRUE)

mb_1 <- felm( fm_b1, data = gt3_sample )
mb_2 <- felm( fm_b2, data = gt3_sample )
mb_3 <- felm( fm_b3, data = gt3_sample )
mb_4 <- felm( fm_b4, data = gt3_sample )

stargazer(
  mb_1, mb_2, mb_3, mb_4, 
  keep = c(
    "boy_1", "same_sex_123", "boy_123", "girl_123", "twins_3"
  ),
  type = "text", 
  keep.stat = c("n","rsq")
) 

# 2SLS/IV regressions ####

## 2+ sample ####

### Educational attainment ####

fOLS_A1 <- make_formula_gt2("educ_attain")
fIV_A1 <- make_formula_gt2("educ_attain", "twins_2")
fIV_A2 <- make_formula_gt2("educ_attain", "same_sex_12")
fIV_A3 <- make_formula_gt2("educ_attain", "boy_12 + girl_12")
fIV_A4 <- make_formula_gt2("educ_attain", "twins_2 + boy_12 + girl_12")

OLS_A1 <- felm(fOLS_A1, data = gt2_sample)
IV_A1 <- felm(fIV_A1, data = gt2_sample)
IV_A2 <- felm(fIV_A2, data = gt2_sample)
IV_A3 <- felm(fIV_A3, data = gt2_sample)
IV_A4 <- felm(fIV_A4, data = gt2_sample)

stargazer(
  OLS_A1, IV_A1, IV_A2, IV_A3, IV_A4,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


### Private school attendance ####
fOLS_A2 <- make_formula_gt2("private_school")
fIV_A5 <- make_formula_gt2("private_school", "twins_2")
fIV_A6 <- make_formula_gt2("private_school", "same_sex_12")
fIV_A7 <- make_formula_gt2("private_school", "boy_123 + girl_123")
fIV_A8 <- make_formula_gt2("private_school", "twins_2 + boy_12 + girl_12")

OLS_A2 <- felm(fOLS_A2, data = gt2_sample)
IV_A5  <- felm(fIV_A5, data = gt2_sample)
IV_A6  <- felm(fIV_A6, data = gt2_sample)
IV_A7  <- felm(fIV_A7, data = gt2_sample)
IV_A8  <- felm(fIV_A8, data = gt2_sample)

stargazer(
  OLS_A2, IV_A5, IV_A6, IV_A7, IV_A8,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


### Mothers' LFP ####

fOLS_A3 <- make_formula_gt2("moth_inlf")
fIV_A9  <- make_formula_gt2("moth_inlf", "twins_2")
fIV_A10 <- make_formula_gt2("moth_inlf", "same_sex_12")
fIV_A11 <- make_formula_gt2("moth_inlf", "boy_12 + girl_12")
fIV_A12 <- make_formula_gt2("moth_inlf", "twins_2 + boy_12 + girl_12")

OLS_A3 <- felm(fOLS_A3, data = gt2_sample)
IV_A9  <- felm(fIV_A9, data = gt2_sample)
IV_A10 <- felm(fIV_A10, data = gt2_sample)
IV_A11 <- felm(fIV_A11, data = gt2_sample)
IV_A12 <- felm(fIV_A12, data = gt2_sample)

stargazer(
  OLS_A3, IV_A9, IV_A10, IV_A11, IV_A12,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)

## 3+ sample #####

### Educational attainment ####

fOLS_B1 <- make_formula_gt3("educ_attain")
fIV_B1 <- make_formula_gt3("educ_attain", "twins_3")
fIV_B2 <- make_formula_gt3("educ_attain", "same_sex_123")
fIV_B3 <- make_formula_gt3("educ_attain", "boy_123 + girl_123")
fIV_B4 <- make_formula_gt3("educ_attain", "twins_3 + boy_123 + girl_123")

OLS_B1 <- felm(fOLS_B1, data = gt3_sample)
IV_B1 <- felm(fIV_B1, data = gt3_sample)
IV_B2 <- felm(fIV_B2, data = gt3_sample)
IV_B3 <- felm(fIV_B3, data = gt3_sample)
IV_B4 <- felm(fIV_B4, data = gt3_sample)

stargazer(
  OLS_B1, IV_B1, IV_B2, IV_B3, IV_B4,
  keep = c("no_kids"), 
  type = "text", 
  keep.stat = c("n","rsq")
  )

### Private school attendance ####
fOLS_B2 <- make_formula_gt3("private_school")
fIV_B5  <- make_formula_gt3("private_school", "twins_3")
fIV_B6  <- make_formula_gt3("private_school", "same_sex_123")
fIV_B7  <- make_formula_gt3("private_school", "boy_123 + girl_123")
fIV_B8  <- make_formula_gt3("private_school", "twins_3 + boy_123 + girl_123")

OLS_B2 <- felm(fOLS_B2, data = gt3_sample)
IV_B5  <- felm(fIV_B5, data = gt3_sample)
IV_B6  <- felm(fIV_B6, data = gt3_sample)
IV_B7  <- felm(fIV_B7, data = gt3_sample)
IV_B8  <- felm(fIV_B8, data = gt3_sample)

stargazer(
  OLS_B2, IV_B5, IV_B6, IV_B7, IV_B8, 
  keep = c("no_kids"), 
  type = "text", 
  keep.stat = c("n","rsq")
  )

### Mothers' LFP ####
fOLS_B3 <- make_formula_gt3("moth_inlf")
fIV_B9  <- make_formula_gt3("moth_inlf", "twins_3")
fIV_B10 <- make_formula_gt3("moth_inlf", "same_sex_123")
fIV_B11 <- make_formula_gt3("moth_inlf", "boy_123 + girl_123")
fIV_B12 <- make_formula_gt3("moth_inlf", "twins_3 + boy_123 + girl_123")

OLS_B3 <- felm(fOLS_B3, data = gt3_sample)
IV_B9  <- felm(fIV_B9, data = gt3_sample)
IV_B10 <- felm(fIV_B10, data = gt3_sample)
IV_B11 <- felm(fIV_B11, data = gt3_sample)
IV_B12 <- felm(fIV_B12, data = gt3_sample)

stargazer(
  OLS_B3, IV_B9, IV_B10, IV_B11, IV_B12, 
  keep = c("no_kids"), 
  type = "text", 
  keep.stat = c("n","rsq")
)


# Think of ways of getting robust std. errors for the 2+ sample

# Sub-sample analysis






