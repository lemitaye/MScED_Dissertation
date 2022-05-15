
# Created on: February 22, 2022
# DEPENDS ON: data_samples.R

# This script runs the main regressions to be reported in the paper

rm(list = ls())
source("scripts/ggplot_theme_Publication-2.R")  

# Load saved data
gt2_sample <- read_csv("data/gt2_sample.csv")
gt3_sample <- read_csv("data/gt3_sample.csv")

# convert all character columns into factors
gt2_sample <- gt2_sample %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    district = factor(district),
    municip = factor(municip),
  )

gt3_sample <- gt3_sample %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    district = factor(district),
    municip = factor(municip),
    moth_no = factor(moth_no),
    birth_order = factor(birth_order)
  )


# Functions for constructing formulas ####

make_formula_frst_stg <- function(dep_var, instrument, clus = FALSE) {
  # Define a vector of covariates
  covar1 <- c("child_age_month", "boy", "moth_age_year", "fath_inhh",
              "moth_age_fstbr")
  covar2 <- c("district", "moth_educ", "moth_pp_group", "moth_income", 
              "moth_marital")
  covar <- paste(
    paste(covar1, collapse = "+"), "|",
    paste(covar2, collapse = "+")
  )

  if (clus) {
    f <- as.formula(paste(
      dep_var, " ~ ", instrument, " + ", covar,
      " | 0 | moth_no"
    ))
  } else {
    f <- as.formula(paste(dep_var, " ~ ", instrument, " + ", covar))
  }

  return(f)
}

make_formula_gt2 <- function(dep_var, instrument = 0) {
  # Define a vector of covariates
  covar1 <- c("child_age_month", "boy", "moth_age_year", "fath_inhh",
              "moth_age_fstbr")
  covar2 <- c("district", "moth_educ", "moth_pp_group", "moth_income", 
              "moth_marital")
  covar <- paste(
    paste(covar1, collapse = "+"), "|",
    paste(covar2, collapse = "+")
  )

  if (instrument == 0) {
    f <- as.formula(paste(dep_var, " ~ no_kids + ", covar))
  } else {
    f <- as.formula(paste(
      dep_var, " ~ ", covar,
      " | (no_kids ~ ", instrument, ")"
    ))
  }

  return(f)
}

make_formula_gt3 <- function(dep_var, instrument = 0) {
  covar1 <- c("child_age_month", "boy", "moth_age_year", "fath_inhh",
              "moth_age_fstbr")
  covar2 <- c("birth_order", "district", "moth_educ", "moth_pp_group", 
              "moth_income", "moth_marital")
  covar <- paste(
    paste(covar1, collapse = "+"), "|",
    paste(covar2, collapse = "+")
  )

  if (instrument == 0) {
    f <- as.formula(paste(
      dep_var, " ~ no_kids + ", covar,
      " | 0 | moth_no"
    ))
  } else {
    f <- as.formula(paste(
      dep_var, " ~ ", covar,
      " | (no_kids ~ ", instrument, ")", " | moth_no"
    ))
  }

  return(f)
}



# First-Stage ####

## 2+ sample ####

fm_a1 <- make_formula_frst_stg("no_kids", "twins_2")
fm_a2 <- make_formula_frst_stg("no_kids", "same_sex_12")
fm_a3 <- make_formula_frst_stg("no_kids", "boy_12 + girl_12")
fm_a4 <- make_formula_frst_stg("no_kids", "twins_2 + boy_12 + girl_12")

ma_1 <- felm(fm_a1, data = gt2_sample)
ma_2 <- felm(fm_a2, data = gt2_sample)
ma_3 <- felm(fm_a3, data = gt2_sample)
ma_4 <- felm(fm_a4, data = gt2_sample)

stargazer(
  ma_1, ma_2, ma_3, ma_4,
  keep = c(
    "boy_1", "same_sex_12", "boy_12", "girl_12", "twins_2"
  ),
  type = "text",
  keep.stat = c("n","rsq")
)

## 3+ sample ####

# These are all clustered by mother's id
fm_b1 <- make_formula_frst_stg(
  "no_kids", "twins_3 + birth_order", clus = TRUE
  )
fm_b2 <- make_formula_frst_stg(
  "no_kids", "same_sex_123 + birth_order", clus = TRUE
  )
fm_b3 <- make_formula_frst_stg(
  "no_kids", "boy_123 + girl_123 + birth_order", clus = TRUE
  )
fm_b4 <- make_formula_frst_stg(
  "no_kids", "twins_3 + boy_123 + girl_123 + birth_order", clus = TRUE
)

mb_1 <- felm(fm_b1, data = gt3_sample)
mb_2 <- felm(fm_b2, data = gt3_sample)
mb_3 <- felm(fm_b3, data = gt3_sample)
mb_4 <- felm(fm_b4, data = gt3_sample)

stargazer(
  mb_1, mb_2, mb_3, mb_4,
  keep = c(
    "boy_1", "same_sex_123", "boy_123", "girl_123", "twins_3"
  ),
  type = "text",
  keep.stat = c("n", "rsq")
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


### Left Behind ####

fOLS_A4 <- make_formula_gt2("behind")
fIV_A13 <- make_formula_gt2("behind", "twins_2")
fIV_A14 <- make_formula_gt2("behind", "same_sex_12")
fIV_A15 <- make_formula_gt2("behind", "boy_12 + girl_12")
fIV_A16 <- make_formula_gt2("behind", "twins_2 + boy_12 + girl_12")

OLS_A4 <- felm(fOLS_A4, data = gt2_sample)
IV_A13 <- felm(fIV_A13, data = gt2_sample)
IV_A14 <- felm(fIV_A14, data = gt2_sample)
IV_A15 <- felm(fIV_A15, data = gt2_sample)
IV_A16 <- felm(fIV_A16, data = gt2_sample)

stargazer(
  OLS_A4, IV_A13, IV_A14, IV_A15, IV_A16,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


### Private school attendance ####
fOLS_A2 <- make_formula_gt2("private_school")
fIV_A5 <- make_formula_gt2("private_school", "twins_2")
fIV_A6 <- make_formula_gt2("private_school", "same_sex_12")
fIV_A7 <- make_formula_gt2("private_school", "boy_12 + girl_12")
fIV_A8 <- make_formula_gt2("private_school", "twins_2 + boy_12 + girl_12")

OLS_A2 <- felm(fOLS_A2, data = gt2_sample)
IV_A5 <- felm(fIV_A5, data = gt2_sample)
IV_A6 <- felm(fIV_A6, data = gt2_sample)
IV_A7 <- felm(fIV_A7, data = gt2_sample)
IV_A8 <- felm(fIV_A8, data = gt2_sample)

stargazer(
  OLS_A2, IV_A5, IV_A6, IV_A7, IV_A8,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


### Mothers' LFP ####

fOLS_A3 <- make_formula_gt2("moth_inlf")
fIV_A9 <- make_formula_gt2("moth_inlf", "twins_2")
fIV_A10 <- make_formula_gt2("moth_inlf", "same_sex_12")
fIV_A11 <- make_formula_gt2("moth_inlf", "boy_12 + girl_12")
fIV_A12 <- make_formula_gt2("moth_inlf", "twins_2 + boy_12 + girl_12")

OLS_A3 <- felm(fOLS_A3, data = gt2_sample)
IV_A9 <- felm(fIV_A9, data = gt2_sample)
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
  keep.stat = c("n", "rsq")
)


### Left Behind ####
fOLS_B4 <- make_formula_gt3("behind")
fIV_B13 <- make_formula_gt3("behind", "twins_3")
fIV_B14 <- make_formula_gt3("behind", "same_sex_123")
fIV_B15 <- make_formula_gt3("behind", "boy_123 + girl_123")
fIV_B16 <- make_formula_gt3("behind", "twins_3 + boy_123 + girl_123")

OLS_B4 <- felm(fOLS_B4, data = gt3_sample)
IV_B13 <- felm(fIV_B13, data = gt3_sample)
IV_B14 <- felm(fIV_B14, data = gt3_sample)
IV_B15 <- felm(fIV_B15, data = gt3_sample)
IV_B16 <- felm(fIV_B16, data = gt3_sample)

stargazer(
  OLS_B4, IV_B13, IV_B14, IV_B15, IV_B16,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)



### Private school attendance ####
fOLS_B2 <- make_formula_gt3("private_school")
fIV_B5 <- make_formula_gt3("private_school", "twins_3")
fIV_B6 <- make_formula_gt3("private_school", "same_sex_123")
fIV_B7 <- make_formula_gt3("private_school", "boy_123 + girl_123")
fIV_B8 <- make_formula_gt3("private_school", "twins_3 + boy_123 + girl_123")

OLS_B2 <- felm(fOLS_B2, data = gt3_sample)
IV_B5 <- felm(fIV_B5, data = gt3_sample)
IV_B6 <- felm(fIV_B6, data = gt3_sample)
IV_B7 <- felm(fIV_B7, data = gt3_sample)
IV_B8 <- felm(fIV_B8, data = gt3_sample)

stargazer(
  OLS_B2, IV_B5, IV_B6, IV_B7, IV_B8,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)

### Mothers' LFP ####
fOLS_B3 <- make_formula_gt3("moth_inlf")
fIV_B9 <- make_formula_gt3("moth_inlf", "twins_3")
fIV_B10 <- make_formula_gt3("moth_inlf", "same_sex_123")
fIV_B11 <- make_formula_gt3("moth_inlf", "boy_123 + girl_123")
fIV_B12 <- make_formula_gt3("moth_inlf", "twins_3 + boy_123 + girl_123")

OLS_B3 <- felm(fOLS_B3, data = gt3_sample)
IV_B9 <- felm(fIV_B9, data = gt3_sample)
IV_B10 <- felm(fIV_B10, data = gt3_sample)
IV_B11 <- felm(fIV_B11, data = gt3_sample)
IV_B12 <- felm(fIV_B12, data = gt3_sample)

stargazer(
  OLS_B3, IV_B9, IV_B10, IV_B11, IV_B12,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


## Sub-sample analysis ####

### Whites vs. Non-whites ####

#### First stages #####
ma_SS_W1 <- felm(
  fm_a1, data = gt2_sample, subset = (moth_pp_group == "White")
  )

ma_SS_NW1 <- felm(
  fm_a1, data = gt2_sample, subset = (moth_pp_group != "White")
)

mb_SS_W1 <- felm(
  fm_b1, data = gt3_sample, subset = (moth_pp_group == "White")
)

mb_SS_NW1 <- felm(
  fm_b1, data = gt3_sample, subset = (moth_pp_group != "White")
)

stargazer(
  ma_SS_W1, ma_SS_NW1, mb_SS_W1, mb_SS_NW1,
  keep = c(
    "twins_2", "twins_3"
  ),
  type = "text",
  keep.stat = c("n","rsq")
)


#### Educational attainment ####

# OLS and IVs for Whites

# 2+
OLS_SS_AW1 <- felm( fOLS_A1, data = gt2_sample,
                   subset = (moth_pp_group == "White") )

IV_SS_AW1 <- felm( fIV_A1, data = gt2_sample, 
                   subset = (moth_pp_group == "White") )

# 3+
OLS_SS_BW1 <- felm( fOLS_B1, data = gt3_sample, 
                    subset = (moth_pp_group == "White") )

IV_SS_BW1 <- felm( fIV_B1, data = gt3_sample, 
                   subset = (moth_pp_group == "White") )

# OLS and IVs for Non-Whites

# 2+
OLS_SS_ANW1 <- felm( fOLS_A1, data = gt2_sample, 
                     subset = (moth_pp_group != "White") )

IV_SS_ANW1 <- felm( fIV_A1, data = gt2_sample, 
                    subset = (moth_pp_group != "White") )

# 3+
OLS_SS_BNW1 <- felm( fOLS_B1, data = gt3_sample, 
                     subset = (moth_pp_group != "White") )

IV_SS_BNW1 <- felm( fIV_B1, data = gt3_sample, 
                    subset = (moth_pp_group != "White") )

stargazer(
  OLS_SS_AW1, IV_SS_AW1, OLS_SS_ANW1, IV_SS_ANW1,
  OLS_SS_BW1, IV_SS_BW1, OLS_SS_BNW1, IV_SS_BNW1,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


#### Left Behind ####

# 2+
OLS_SS_AW4 <- felm( fOLS_A4, data = gt2_sample,
                    subset = (moth_pp_group == "White") )

IV_SS_AW13 <- felm( fIV_A13, data = gt2_sample, 
                   subset = (moth_pp_group == "White") )

# 3+
OLS_SS_BW4 <- felm( fOLS_B4, data = gt3_sample, 
                    subset = (moth_pp_group == "White") )

IV_SS_BW13 <- felm( fIV_B13, data = gt3_sample, 
                   subset = (moth_pp_group == "White") )

# OLS and IVs for Non-Whites

# 2+
OLS_SS_ANW4 <- felm( fOLS_A4, data = gt2_sample, 
                     subset = (moth_pp_group != "White") )

IV_SS_ANW13 <- felm( fIV_A13, data = gt2_sample, 
                    subset = (moth_pp_group != "White") )

# 3+
OLS_SS_BNW4 <- felm( fOLS_B4, data = gt3_sample, 
                     subset = (moth_pp_group != "White") )

IV_SS_BNW13 <- felm( fIV_B13, data = gt3_sample, 
                    subset = (moth_pp_group != "White") )

stargazer(
  OLS_SS_AW4, IV_SS_AW13, OLS_SS_ANW4, IV_SS_ANW13, 
  OLS_SS_BW4, IV_SS_BW13, OLS_SS_BNW4, IV_SS_BNW13,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


#### Private School Attendance ####

# 2+
OLS_SS_AW2 <- felm( fOLS_A2, data = gt2_sample,
                    subset = (moth_pp_group == "White") )

IV_SS_AW5 <- felm( fIV_A5, data = gt2_sample, 
                    subset = (moth_pp_group == "White") )

# 3+
OLS_SS_BW2 <- felm( fOLS_B2, data = gt3_sample, 
                    subset = (moth_pp_group == "White") )

IV_SS_BW5 <- felm( fIV_B5, data = gt3_sample, 
                    subset = (moth_pp_group == "White") )

# OLS and IVs for Non-Whites

# 2+
OLS_SS_ANW2 <- felm( fOLS_A2, data = gt2_sample, 
                     subset = (moth_pp_group != "White") )

IV_SS_ANW5 <- felm( fIV_A5, data = gt2_sample, 
                     subset = (moth_pp_group != "White") )

# 3+
OLS_SS_BNW2 <- felm( fOLS_B2, data = gt3_sample, 
                     subset = (moth_pp_group != "White") )

IV_SS_BNW5 <- felm( fIV_B5, data = gt3_sample, 
                     subset = (moth_pp_group != "White") )

stargazer(
  OLS_SS_AW2, IV_SS_AW5, OLS_SS_ANW2, IV_SS_ANW5,
  OLS_SS_BW2, IV_SS_BW5, OLS_SS_BNW2, IV_SS_BNW5,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)



#### Mothers' LFP ####

# 2+
OLS_SS_AW3 <- felm( fOLS_A3, data = gt2_sample,
                    subset = (moth_pp_group == "White") )

IV_SS_AW9 <- felm( fIV_A9, data = gt2_sample, 
                   subset = (moth_pp_group == "White") )

# 3+
OLS_SS_BW3 <- felm( fOLS_B3, data = gt3_sample, 
                    subset = (moth_pp_group == "White") )

IV_SS_BW9 <- felm( fIV_B9, data = gt3_sample, 
                   subset = (moth_pp_group == "White") )

# OLS and IVs for Non-Whites

# 2+
OLS_SS_ANW3 <- felm( fOLS_A3, data = gt2_sample, 
                     subset = (moth_pp_group != "White") )

IV_SS_ANW9 <- felm( fIV_A9, data = gt2_sample, 
                    subset = (moth_pp_group != "White") )

# 3+
OLS_SS_BNW3 <- felm( fOLS_B3, data = gt3_sample, 
                     subset = (moth_pp_group != "White") )

IV_SS_BNW9 <- felm( fIV_B9, data = gt3_sample, 
                    subset = (moth_pp_group != "White") )

stargazer(
  OLS_SS_AW3, IV_SS_AW9, OLS_SS_ANW3, IV_SS_ANW9, 
  OLS_SS_BW3, IV_SS_BW9, OLS_SS_BNW3, IV_SS_BNW9,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


## Analysis of no-first stage sample ####

gt2_sample %>% 
  count(moth_age_fstbr)

fm_a1 <- make_formula_frst_stg("no_kids", "twins_2")

fm_a1_t1 <- make_formula_frst_stg("educ_attain", "twins_2 + no_kids")
fm_a2_t1 <- make_formula_frst_stg("behind", "twins_2 + no_kids")
fm_a3_t1 <- make_formula_frst_stg("private_school", "twins_2 + no_kids")
fm_a4_t1 <- make_formula_frst_stg("moth_inlf", "twins_2 + no_kids")

subsamp_twins1 <- gt2_sample %>% 
  filter(
    spacing < 2,
    # moth_educ %in% c("No schooling"),
    # moth_age_fstbr < 22,
    # moth_pp_group %in% c("Black African"),
    no_kids >= 3
  ) 

subsamp_twins2 <- gt2_sample %>% 
  filter(
    # spacing < 2,
    moth_educ %in% c("No schooling"),
    # moth_age_fstbr < 21,
    # moth_pp_group %in% c("Black African"),
    no_kids >= 3
  ) 

ma_t1 <- felm(fm_a1, data = subsamp_twins1)
ma_t2 <- felm(fm_a1, data = subsamp_twins2)

rma_1_t1 <- felm(fm_a1_t1, data = subsamp_twins1)
rma_1_t2 <- felm(fm_a1_t1, data = subsamp_twins2)

rma_2_t1 <- felm(fm_a2_t1, data = subsamp_twins1)
rma_2_t2 <- felm(fm_a2_t1, data = subsamp_twins2)

rma_3_t1 <- felm(fm_a3_t1, data = subsamp_twins1)
rma_3_t2 <- felm(fm_a3_t1, data = subsamp_twins2)

rma_4_t1 <- felm(fm_a4_t1, data = subsamp_twins1)
rma_4_t2 <- felm(fm_a4_t1, data = subsamp_twins2)


stargazer(
  ma_t1, rma_1_t1, rma_2_t1, rma_3_t1, rma_4_t1,
  keep = c("twins_2"),
  type = "text",
  keep.stat = c("n","rsq")
)

stargazer(
  ma_t2, rma_1_t2, rma_2_t2, rma_3_t2, rma_4_t2,
  keep = c("twins_2"),
  type = "text",
  keep.stat = c("n","rsq")
)

# Look for whether first-born boys are affected:

fm_a3 <- make_formula_frst_stg("no_kids", "boy_12")

fm_b1_t1 <- make_formula_frst_stg("educ_attain", "boy_12 + no_kids")
fm_b2_t1 <- make_formula_frst_stg("behind", "boy_12 + no_kids")
fm_b3_t1 <- make_formula_frst_stg("private_school", "boy_12 + no_kids")
fm_b4_t1 <- make_formula_frst_stg("moth_inlf", "boy_12 + no_kids")

subsamp_boy1 <- gt2_sample %>% 
  filter(
    boy == 1,
    spacing < 2#,
    # moth_educ %in% c("No schooling"),
    # moth_age_fstbr < 22,
    # moth_pp_group %in% c("Black African"),
    # no_kids >= 3
  ) 

subsamp_boy2 <- gt2_sample %>% 
  filter(
    boy == 1,
    # spacing <= 2,
    moth_educ %in% c("No schooling")#,
    # moth_age_fstbr < 22,
    # moth_pp_group %in% c("Black African"),
    # no_kids >= 3
  ) 

mb_t1 <- felm(fm_a3, data = subsamp_boy1)
mb_t2 <- felm(fm_a3, data = subsamp_boy2)
  
rmb_1_t1 <- felm(fm_b1_t1, data = subsamp_boy1)
rmb_1_t2 <- felm(fm_b1_t1, data = subsamp_boy2)
  
rmb_2_t1 <- felm(fm_b2_t1, data = subsamp_boy1)
rmb_2_t2 <- felm(fm_b2_t1, data = subsamp_boy2)
  
rmb_3_t1 <- felm(fm_b3_t1, data = subsamp_boy1)
rmb_3_t2 <- felm(fm_b3_t1, data = subsamp_boy2)
  
rmb_4_t1 <- felm(fm_b4_t1, data = subsamp_boy1)
rmb_4_t2 <- felm(fm_b4_t1, data = subsamp_boy2)
  
stargazer(
  mb_t1, rmb_1_t1, rmb_2_t1, rmb_3_t1, rmb_4_t1,
  keep = c("boy_12"),
  type = "text",
  keep.stat = c("n","rsq")
)

stargazer(
  mb_t2, rmb_1_t2, rmb_2_t2, rmb_3_t2, rmb_4_t2,
  keep = c("boy_12"),
  type = "text",
  keep.stat = c("n","rsq")
)


# A Partial Check on the Monotonicity of Same Sex Instrument ####

# Variables to group by:
# * Age at first birth (intervals)
# * Mother's population group
# * Mother's education level


formula_frst_stg_samesex <- function(dep_var, instrument, clus = FALSE) {
  # Define a vector of covariates
  covar1 <- c("child_age_month", "boy", "moth_age_year", "fath_inhh")
  covar2 <- c("district", "moth_income", "moth_marital")
  covar <- paste(
    paste(covar1, collapse = "+"), "|",
    paste(covar2, collapse = "+")
  )
  
  if (clus) {
    f <- as.formula(paste(
      dep_var, " ~ ", instrument, " + ", covar,
      " | 0 | moth_no"
    ))
  } else {
    f <- as.formula(paste(dep_var, " ~ ", instrument, " + ", covar))
  }
  
  return(f)
}

fm_a2_monot <- formula_frst_stg_samesex("no_kids", "same_sex_12")

model_frst_stg <- function(tbl) {
  # if_else( nrow(tbl) > 300, felm(fm_a2_monot, data = tbl), NULL ) 
  lm(no_kids ~ same_sex_12, data = tbl)
}

get_confs <- function(mod) {
  confint(mod) %>% 
    as_tibble()
}

nested <- gt2_sample %>% 
  mutate(
    
    pop_group =
      case_when(
        moth_pp_group == "Black African" ~ "Black African",
        moth_pp_group == "White" ~ "White",
        TRUE ~ "Coloured, Indian or Asian, and Other"
      ) %>% factor(),
    
    age_frst_br = 
      case_when(
        between(moth_age_fstbr, 15, 19) ~ "15-19",
        between(moth_age_fstbr, 20, 24) ~ "20-24",
        between(moth_age_fstbr, 25, 29) ~ "25-29",
        moth_age_fstbr >= 30 ~ "30+"
      ) %>% factor(),
    
    moth_educ = fct_lump(moth_educ, n=3) %>% 
      fct_recode("Primary or Less" = "Other")
  ) %>% 
  group_by(pop_group, age_frst_br, moth_educ) %>% 
  mutate(n = n()) %>% 
  filter(!is.na(age_frst_br), n >= 50) %>% 
  select(pop_group, moth_educ, age_frst_br, everything()) %>% 
  arrange(pop_group, moth_educ, age_frst_br) %>% 
  nest()

run_frst <- function(var) {
  tbl <- nested %>% 
    mutate(
      model = map(
        data, ~lm(as.formula(paste("no_kids ~", var)), data = .)
        ), 
      summaries = map(model, tidy), 
      conf_ints = map(model, get_confs)
    ) %>% 
    unnest(c(summaries, conf_ints)) %>% 
    select(-data, -model) %>%
    filter(term == as.character(var)) 
  
  return(tbl)
}

samesex_frst <- bind_rows(
  run_frst("same_sex_12"), run_frst("boy_12"), run_frst("girl_12")
  ) %>% 
  rename(iv = "term") %>% 
  ungroup() 
  
samesex_coefs <- samesex_frst %>% 
  mutate(term = str_c(
    str_sub(pop_group, 1, 1), str_sub(moth_educ, 1, 1), age_frst_br, 
    sep = "_")) %>% 
  rename("conf.low" = `2.5 %`, "conf.high" = `97.5 %`) %>% 
  select(-c(pop_group, moth_educ, age_frst_br))


plot_frst <- function(var, label) {
  p <- samesex_coefs %>% 
    filter(iv == var) %>%
    ggcoef(color = "blue", sort = "ascending") + 
    facet_wrap(~ iv, labeller = as_labeller(label)) +
    labs(x = "Coefficient", y = "")
  
  return(p)
}

f1 <- plot_frst("same_sex_12", label = c("same_sex_12" = "SameSex12"))
f2 <- plot_frst("boy_12", label = c("boy_12" = "Boy12"))
f3 <- plot_frst("girl_12", label = c("girl_12" = "Girl12"))

fig2 <- ggarrange(
  f1, f2, f3, labels = c("A.", "B.", "C."),
  ncol = 3
  ) 

ggsave(
  filename = "tex/figures/monot.pdf",
  plot = fig2,
  device = cairo_pdf,
  width = 270,
  height = 180,
  units = "mm"
)



# First stage effects at different parities ####

first_gt2 <- gt2_sample %>% 
  mutate(
    d_3 = (no_kids >= 3), d_4 = (no_kids >= 4),
    d_5 = (no_kids >= 5), d_6 = (no_kids >= 6),
    d_7 = (no_kids >= 7), d_8 = (no_kids >= 8), d_9 = (no_kids >= 9)
    ) %>%
  select(d_3:d_9, twins_2, same_sex_12, boy_12, girl_12, no_kids)

first_gt3 <- gt3_sample %>% 
  mutate(
    d_4 = (no_kids >= 4), d_5 = (no_kids >= 5), 
    d_6 = (no_kids >= 6), d_7 = (no_kids >= 7), 
    d_8 = (no_kids >= 8), d_9 = (no_kids >= 9)
  ) %>%
  select(d_4:d_9, twins_3, same_sex_123, boy_123, girl_123, no_kids)

deps_gt2 <- c("d_3", "d_4", "d_5", "d_6", "d_7", "d_8", "d_9")
deps_gt3 <- deps_gt2[-1]

models_gt2 <- list(); models_gt3 <- list()

for (var in deps_gt2) {
  mod1 <- lm(as.formula(paste(var, "~ twins_2")), data = first_gt2)
  mod2 <- lm(as.formula(paste(var, "~ same_sex_12")), data = first_gt2)
  mod3 <- lm(as.formula(paste(var, "~ boy_12")), data = first_gt2)
  mod4 <- lm(as.formula(paste(var, "~ girl_12")), data = first_gt2)
  
  x <- list(mod1, mod2, mod3, mod4)
  names(x) <- rep(var, 4)
  models_gt2 <- append(models_gt2, x)
}

for (var in deps_gt3) {
  mod1 <- lm(as.formula(paste(var, "~ twins_3")), data = first_gt3)
  mod2 <- lm(as.formula(paste(var, "~ same_sex_123")), data = first_gt3)
  mod3 <- lm(as.formula(paste(var, "~ boy_123")), data = first_gt3)
  mod4 <- lm(as.formula(paste(var, "~ girl_123")), data = first_gt3)
  
  x <- list(mod1, mod2, mod3, mod4)
  names(x) <- rep(var, 4)
  models_gt3 <- append(models_gt3, x)
}

# Thnik of writing a function for the above two for loops.

tidy_modelII <- function(model_list) {
  tidy <- map(model_list, tidy, conf.int = TRUE) %>%
    map(~ filter(., term != "(Intercept)")) %>%
    bind_rows() %>%
    mutate(
      parity = factor(names(model_list)),
      parity = fct_recode(parity,
        "3+" = "d_3", "4+" = "d_4", "5+" = "d_5", "6+" = "d_6",
        "7+" = "d_7", "8+" = "d_8", "9+" = "d_9"
      )
    )

  return(tidy)
}



acr_2 <- tidy_modelII(models_gt2) %>% 
  mutate(sample = "gt2") %>% 
  select(sample, parity, term, everything())

acr_3 <- tidy_modelII(models_gt3) %>% 
  mutate(sample = "gt3") %>% 
  select(sample, parity, term, everything()) %>% 
  suppressWarnings() # throws out a warning about "d_3"

acr <- bind_rows(acr_2, acr_3)

extrct <- function(var, tbl) {
  
  mod <- lm(as.formula(paste("no_kids ~", var)), data = tbl) 
  
  est <- coef(summary(mod))[var, 1] %>% round(3)
  se <- coef(summary(mod))[var, 2] %>% round(3)  
  
  txt <- paste0("Overall: ", est, "", "(", se, ")")
  
  return(txt)
}

plot_acr <- function(vars, rows = 1, labels, x, y) {
  
  anno <- data.frame(
    x = x, y = y, # coordinates to place annotation
    lab = c(extrct(vars[1], first_gt2), extrct(vars[2], first_gt3)),
    term = vars
  )
  
  p <- acr %>%   
    filter(term %in% vars) %>% 
    ggplot(aes(parity, estimate)) + 
    geom_point(color = "blue") +
    geom_line(aes(group = 1)) +
    geom_line(aes(y = conf.low, group = 1), linetype = "dashed") +
    geom_line(aes(y = conf.high, group = 1), linetype = "dashed") +
    geom_hline(
      aes(yintercept = 0), color = "gray50", size = 1, linetype = "dotted"
      ) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = 1), 
                fill = "grey", alpha = .3) +
    geom_text(data = anno, aes(x = x,  y = y, label = lab)) +
    facet_wrap(~term, scale = "free", nrow = rows,
               labeller = as_labeller(labels)
    ) +
    labs(x = "", y = "") +
    scale_colour_Publication() + 
    theme_Publication()
  
  return(p)
}

r1 <- plot_acr(c("twins_2", "twins_3"), 
               labels = c("twins_2" = "Twins2", "twins_3" = "Twins3"),
               x= c(5, 4), y = c(0.4, 0.4)) 

r2 <- plot_acr(c("same_sex_12", "same_sex_123"), 
               labels = c("same_sex_12" = "SameSex12", "same_sex_123" = "SameSex123"),
               x= c(5, 4), y = c(0.02, 0.02))
  
r3 <- plot_acr(c("boy_12", "boy_123"), 
               labels = c("boy_12" = "Boy12", "boy_123" = "Boy123"),
               x= c(5, 4), y = c(0.005, -0.01))

r4 <- plot_acr(c("girl_12", "girl_123"), 
               labels = c("girl_12" = "Girl12", "girl_123" = "Girl123"),
               x= c(5, 4), y = c(0.03, 0.03)) + labs(x = "Number of Children", y = "")

fig1 <- ggarrange(
r1, r2, r3, r4,
labels = c("A.", "B.", "C.", "D."),
nrow = 4, heights = c(1.2, 1.2, 1.2, 1.2)
) 

ggsave(
  filename = "tex/figures/acrs.pdf",
  plot = fig1,
  device = cairo_pdf,
  width = 200,
  height = 297,
  units = "mm"
)


# Think of a way to add the overall first stage
lm(no_kids ~ twins_2, data = first_gt2) %>% 
  summary() %>% 
  coef() %>% 
  .["twins_2", 2] %>% 
  round(3)

lm(as.formula(paste("no_kids ~", "twins_2")), data = first_gt2)

acr_2 %>% filter(term == "twins_2") %>% pull(estimate) %>% sum()

lm(no_kids ~ twins_3, data = first_gt3) %>% coef()
acr_3 %>% filter(term == "twins_3") %>% pull(estimate) %>% sum()






# A check on the randomness of the samesex instrument

samesex12 <- lm(
  same_sex_12 ~ moth_age_year + moth_pp_group + moth_educ + moth_income + 
    moth_inlf + fath_inhh + district + moth_marital,
   data = gt2_sample)

samesex123 <- lm(
  same_sex_123 ~ moth_age_year + moth_pp_group + moth_educ + moth_income + 
    moth_inlf + fath_inhh + district + moth_marital,
  data = gt3_sample, subset = (same_sex_12 == 1 & birth_order == 1))

stargazer(samesex12, samesex123, type = "text", keep.stat = c("n", "rsq"))



# Overidentification test

# 2+ Sample
resid3_educ <- residuals(IV_A4)
resid2_behind <- residuals(IV_A16)
resid2_private <- residuals(IV_A8)
resid2_mlfp <- residuals(IV_A12)

gt2_sample$resid2_educ <- resid2_educ
gt2_sample$resid2_behind <- resid2_behind
gt2_sample$resid2_private <- resid2_private
gt2_sample$resid2_mlfp <- resid2_mlfp

fIV_A4_OR <- make_formula_frst_stg("resid2_educ", "twins_2 + boy_12 + girl_12")
fIV_A16_OR <- make_formula_frst_stg("resid2_behind", "twins_2 + boy_12 + girl_12")
fIV_A8_OR <- make_formula_frst_stg("resid2_private", "twins_2 + boy_12 + girl_12")
fIV_A12_OR <- make_formula_frst_stg("resid2_mlfp", "twins_2 + boy_12 + girl_12")

IV_A4_OR <- felm(fIV_A4_OR, data = gt2_sample)
IV_A16_OR <- felm(fIV_A16_OR, data = gt2_sample)
IV_A8_OR <- felm(fIV_A8_OR, data = gt2_sample)
IV_A12_OR <- felm(fIV_A12_OR, data = gt2_sample)

IV_A4_OR_test <- linearHypothesis(IV_A4_OR,
  c("twins_2 = 0", "boy_12 = 0", "girl_12 = 0"),
  test = "Chisq"
)
IV_A16_OR_test <- linearHypothesis(IV_A16_OR,
  c("twins_2 = 0", "boy_12 = 0", "girl_12 = 0"),
  test = "Chisq"
)
IV_A8_OR_test <- linearHypothesis(IV_A8_OR,
  c("twins_2 = 0", "boy_12 = 0", "girl_12 = 0"),
  test = "Chisq"
)
IV_A12_OR_test <- linearHypothesis(IV_A12_OR,
  c("twins_2 = 0", "boy_12 = 0", "girl_12 = 0"),
  test = "Chisq"
)

# df = m - k, where m is the number of instruments and k is the number 
# of endogenous regressors. Thereofre, m - k = 3 - 1 = 2

pchisq(IV_A4_OR_test[2, 3], df = 2, lower.tail = FALSE)
pchisq(IV_A16_OR_test[2, 3], df = 2, lower.tail = FALSE)
pchisq(IV_A8_OR_test[2, 3], df = 2, lower.tail = FALSE)
pchisq(IV_A12_OR_test[2, 3], df = 2, lower.tail = FALSE)


# 3+ Sample:
resid3_educ <- residuals(IV_B4)
resid3_behind <- residuals(IV_B16)
resid3_private <- residuals(IV_B8)
resid3_mlfp <- residuals(IV_B12)

gt3_sample$resid3_educ <- resid3_educ
gt3_sample$resid3_behind <- resid3_behind
gt3_sample$resid3_private <- resid3_private
gt3_sample$resid3_mlfp <- resid3_mlfp

fIV_B4_OR <- make_formula_frst_stg("resid3_educ", "twins_3 + boy_123 + girl_123")
fIV_B16_OR <- make_formula_frst_stg("resid3_behind", "twins_3 + boy_123 + girl_123")
fIV_B8_OR <- make_formula_frst_stg("resid3_private", "twins_3 + boy_123 + girl_123")
fIV_B12_OR <- make_formula_frst_stg("resid3_mlfp", "twins_3 + boy_123 + girl_123")

IV_B4_OR <- felm(fIV_B4_OR, data = gt3_sample)
IV_B16_OR <- felm(fIV_B16_OR, data = gt3_sample)
IV_B8_OR <- felm(fIV_B8_OR, data = gt3_sample)
IV_B12_OR <- felm(fIV_B12_OR, data = gt3_sample)

IV_B4_OR_test <- linearHypothesis(IV_B4_OR,
                                  c("twins_3 = 0", "boy_123 = 0", "girl_123 = 0"),
                                  test = "Chisq"
)
IV_B16_OR_test <- linearHypothesis(IV_B16_OR,
                                   c("twins_3 = 0", "boy_123 = 0", "girl_123 = 0"),
                                   test = "Chisq"
)
IV_B8_OR_test <- linearHypothesis(IV_B8_OR,
                                  c("twins_3 = 0", "boy_123 = 0", "girl_123 = 0"),
                                  test = "Chisq"
)
IV_B12_OR_test <- linearHypothesis(IV_B12_OR,
                                   c("twins_3 = 0", "boy_123 = 0", "girl_123 = 0"),
                                   test = "Chisq"
)

# df = m - k, where m is the number of instruments and k is the number 
# of endogenous regressors. Thereofre, m - k = 3 - 1 = 2

pchisq(IV_B4_OR_test[2, 3], df = 2, lower.tail = FALSE)
pchisq(IV_B16_OR_test[2, 3], df = 2, lower.tail = FALSE)
pchisq(IV_B8_OR_test[2, 3], df = 2, lower.tail = FALSE)
pchisq(IV_B12_OR_test[2, 3], df = 2, lower.tail = FALSE)


