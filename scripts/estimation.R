
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

OLS_A1 <- felm(fOLS_A1, data = gt2_sample, subset = (moth_pp_group == "White"))
IV_A1 <- felm(fIV_A1, data = gt2_sample, subset = (moth_pp_group == "White"))
IV_A2 <- felm(fIV_A2, data = gt2_sample, subset = (moth_pp_group == "White"))
IV_A3 <- felm(fIV_A3, data = gt2_sample, subset = (moth_pp_group == "White"))
IV_A4 <- felm(fIV_A4, data = gt2_sample, subset = (moth_pp_group == "White"))

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

OLS_A4 <- felm(fOLS_A4, data = gt2_sample, subset = (moth_pp_group != "White"))
IV_A13 <- felm(fIV_A13, data = gt2_sample, subset = (moth_pp_group != "White"))
IV_A14 <- felm(fIV_A14, data = gt2_sample, subset = (moth_pp_group != "White"))
IV_A15 <- felm(fIV_A15, data = gt2_sample, subset = (moth_pp_group != "White"))
IV_A16 <- felm(fIV_A16, data = gt2_sample, subset = (moth_pp_group != "White"))

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

OLS_A2 <- felm(fOLS_A2, data = gt2_sample, subset = (child_age_year > 9))
IV_A5 <- felm(fIV_A5, data = gt2_sample, subset = (child_age_year > 9))
IV_A6 <- felm(fIV_A6, data = gt2_sample, subset = (child_age_year > 9))
IV_A7 <- felm(fIV_A7, data = gt2_sample, subset = (child_age_year > 9))
IV_A8 <- felm(fIV_A8, data = gt2_sample, subset = (child_age_year > 9))

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

OLS_A3 <- felm(fOLS_A3, data = gt2_sample, subset = (child_age_year > 9))
IV_A9 <- felm(fIV_A9, data = gt2_sample, subset = (child_age_year > 9))
IV_A10 <- felm(fIV_A10, data = gt2_sample, subset = (child_age_year > 9))
IV_A11 <- felm(fIV_A11, data = gt2_sample, subset = (child_age_year > 9))
IV_A12 <- felm(fIV_A12, data = gt2_sample, subset = (child_age_year > 9))

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

OLS_B1 <- felm(fOLS_B1, data = gt3_sample, subset = (child_age_year > 9))
IV_B1 <- felm(fIV_B1, data = gt3_sample, subset = (child_age_year > 9))
IV_B2 <- felm(fIV_B2, data = gt3_sample, subset = (child_age_year > 9))
IV_B3 <- felm(fIV_B3, data = gt3_sample, subset = (child_age_year > 9))
IV_B4 <- felm(fIV_B4, data = gt3_sample, subset = (child_age_year > 9))

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

OLS_B4 <- felm(fOLS_B4, data = gt3_sample, subset = (child_age_year > 9))
IV_B13 <- felm(fIV_B13, data = gt3_sample, subset = (child_age_year > 9))
IV_B14 <- felm(fIV_B14, data = gt3_sample, subset = (child_age_year > 9))
IV_B15 <- felm(fIV_B15, data = gt3_sample, subset = (child_age_year > 9))
IV_B16 <- felm(fIV_B16, data = gt3_sample, subset = (child_age_year > 9))

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

OLS_B2 <- felm(fOLS_B2,
  data = gt3_sample,
  subset = (child_age_year > 9) & (moth_pp_group != "Black African")
)
IV_B5 <- felm(fIV_B5,
  data = gt3_sample,
  subset = (child_age_year > 9) & (moth_pp_group != "Black African")
)
IV_B6 <- felm(fIV_B6,
  data = gt3_sample,
  subset = (child_age_year > 9) & (moth_pp_group != "Black African")
)
IV_B7 <- felm(fIV_B7,
  data = gt3_sample,
  subset = (child_age_year > 9) & (moth_pp_group != "Black African")
)
IV_B8 <- felm(fIV_B8,
  data = gt3_sample,
  subset = (child_age_year > 9) & (moth_pp_group != "Black African")
)

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

### Educational attainment ####

# OLS_SS_B1 <- felm(fOLS_A1,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group == "Black African")
# )
# IV_SS_B1 <- felm(fIV_A1,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group == "Black African")
# )
# IV_SS_B3 <- felm(fIV_A3,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group == "Black African")
# )
# IV_SS_B4 <- felm(fIV_A4,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group == "Black African")
# )
# 
# OLS_SS_NB1 <- felm(fOLS_A1,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group != "Black African")
# )
# IV_SS_NB1 <- felm(fIV_A1,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group != "Black African")
# )
# IV_SS_NB3 <- felm(fIV_A3,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group != "Black African")
# )
# IV_SS_NB4 <- felm(fIV_A4,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group != "Black African")
# )
# 
# stargazer(
#   OLS_SS_B1, IV_SS_B1, IV_SS_B3, IV_SS_B4, OLS_SS_NB1, IV_SS_NB3, IV_SS_NB4,
#   keep = c("no_kids"),
#   type = "text",
#   keep.stat = c("n", "rsq")
# )
# 
# 
# ### Left Behind ####
# 
# OLS_SS_B2 <- felm(fOLS_A4,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group == "Black African")
# )
# IV_SS_B5 <- felm(fIV_A13,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group == "Black African")
# )
# IV_SS_B6 <- felm(fIV_A15,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group == "Black African")
# )
# IV_SS_B7 <- felm(fIV_A16,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group == "Black African")
# )
# 
# OLS_SS_NB2 <- felm(fOLS_A4,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group != "Black African")
# )
# IV_SS_NB5 <- felm(fIV_A13,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group != "Black African")
# )
# IV_SS_NB6 <- felm(fIV_A15,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group != "Black African")
# )
# IV_SS_NB7 <- felm(fIV_A16,
#   data = gt2_sample,
#   subset = (child_age_year > 9) & (moth_pp_group != "Black African")
# )
# 
# stargazer(
#   OLS_SS_B2, IV_SS_B5, IV_SS_B6, IV_SS_B7, OLS_SS_NB2, IV_SS_NB5, IV_SS_NB6, IV_SS_NB7,
#   keep = c("no_kids"),
#   type = "text",
#   keep.stat = c("n", "rsq")
# )

## Analysis of no-first stage sample ####

gt2_sample %>% 
  count(spacing)

ma_1_t1 <- felm(fm_a1, data = gt2_sample, 
                subset = (no_kids > 4))
mb_2_t1 <- felm(fm_b1, data = gt3_sample, 
                subset = (no_kids > 4))

ma_3_t1 <- felm(fm_a3, data = gt2_sample, 
                subset = (no_kids > 4))
ma_4_t1 <- felm(fm_a4, data = gt2_sample, 
                subset = (no_kids > 4))

stargazer(
  ma_1_t1, mb_2_t1,
  # ma_2_t1, ma_3_t1, ma_4_t1,
  keep = c(
    "same_sex_12", "boy_12", "girl_12", "twins_2", "twins_3"
  ),
  type = "text",
  keep.stat = c("n","rsq")
)

# Reduced form first stage

fm_a1_t1 <- make_formula_frst_stg("educ_attain", "twins_2")
fm_a2_t1 <- make_formula_frst_stg("behind", "twins_2")
fm_a3_t1 <- make_formula_frst_stg("private_school", "twins_2")
fm_a4_t1 <- make_formula_frst_stg("moth_inlf", "twins_2")

rma_1_t1 <- felm(fm_a1_t1, data = gt2_sample, 
                subset = (no_kids > 4))
rma_2_t1 <- felm(fm_a2_t1, data = gt2_sample, 
                subset = (no_kids > 4))
rma_3_t1 <- felm(fm_a3_t1, data = gt2_sample, 
                subset = (no_kids > 4))
rma_4_t1 <- felm(fm_a4_t1, data = gt2_sample, 
                subset = (no_kids > 4))

stargazer(
  rma_1_t1, rma_2_t1, rma_3_t1, rma_4_t1,
  keep = c("twins_2"),
  type = "text",
  keep.stat = c("n","rsq")
)


fm_b1_t1 <- make_formula_frst_stg("educ_attain", "twins_3")
fm_b2_t1 <- make_formula_frst_stg("behind", "twins_3")
fm_b3_t1 <- make_formula_frst_stg("private_school", "twins_3")
fm_b4_t1 <- make_formula_frst_stg("moth_inlf", "twins_3")

rmb_1_t1 <- felm(fm_b1_t1, data = gt3_sample, 
                 subset = (no_kids > 4))
rmb_2_t1 <- felm(fm_b2_t1, data = gt3_sample, 
                 subset = (no_kids > 4))
rmb_3_t1 <- felm(fm_b3_t1, data = gt3_sample, 
                 subset = (no_kids > 4))
rmb_4_t1 <- felm(fm_b4_t1, data = gt3_sample, 
                 subset = (no_kids > 4))

stargazer(
  rmb_1_t1, rmb_2_t1, rmb_3_t1, rmb_4_t1,
  keep = c("twins_3"),
  type = "text",
  keep.stat = c("n","rsq")
)

# all as expected. what a relief!


















