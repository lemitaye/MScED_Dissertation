
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
    moth_no = factor(moth_no),
    behind = as.numeric(behind)
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

ma_1 <- felm( fm_a1, data = gt2_sample, subset = (child_age_year > 9) )
ma_2 <- felm( fm_a2, data = gt2_sample, subset = (child_age_year > 9) )
ma_3 <- felm( fm_a3, data = gt2_sample, subset = (child_age_year > 9) )
ma_4 <- felm( fm_a4, data = gt2_sample, subset = (child_age_year > 9) )

stargazer(
  ma_1, ma_2, ma_3, ma_4, 
  keep = c(
    "boy_1", "same_sex_12", "boy_12", "girl_12", "twins_2"
  ),
  type = "text", 
  keep.stat = c("n","rsq")
) 

# F-tests 
# linearHypothesis(ma_1, c("same_sex_12"))
# linearHypothesis(ma_2, c("boy_1", "boy_2", "same_sex_12"))
# linearHypothesis(ma_3, c("boy_1", "boy_12", "girl_12"))
# linearHypothesis(ma_4, c("twins_2"))

## 3+ sample ####

# These are all clustered by mother's id
fm_b1 <- make_formula_frst_stg("no_kids", "twins_3", clus = TRUE)
fm_b2 <- make_formula_frst_stg("no_kids", "same_sex_123", clus = TRUE)
fm_b3 <- make_formula_frst_stg("no_kids", "boy_123 + girl_123", clus = TRUE)
fm_b4 <- make_formula_frst_stg("no_kids", "twins_3 + boy_123 + girl_123", 
                               clus = TRUE)

mb_1 <- felm( fm_b1, data = gt3_sample, subset = (child_age_year > 9) )
mb_2 <- felm( fm_b2, data = gt3_sample, subset = (child_age_year > 9) )
mb_3 <- felm( fm_b3, data = gt3_sample, subset = (child_age_year > 9) )
mb_4 <- felm( fm_b4, data = gt3_sample, subset = (child_age_year > 9) )

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

OLS_A1 <- felm(fOLS_A1, data = gt2_sample, subset = (child_age_year > 9))
IV_A1 <- felm(fIV_A1, data = gt2_sample, subset = (child_age_year > 9))
IV_A2 <- felm(fIV_A2, data = gt2_sample, subset = (child_age_year > 9))
IV_A3 <- felm(fIV_A3, data = gt2_sample, subset = (child_age_year > 9))
IV_A4 <- felm(fIV_A4, data = gt2_sample, subset = (child_age_year > 9))

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

OLS_A4 <- felm(fOLS_A4, data = gt2_sample, subset = (child_age_year > 9))
IV_A13 <- felm(fIV_A13, data = gt2_sample, subset = (child_age_year > 9))
IV_A14 <- felm(fIV_A14, data = gt2_sample, subset = (child_age_year > 9))
IV_A15 <- felm(fIV_A15, data = gt2_sample, subset = (child_age_year > 9))
IV_A16 <- felm(fIV_A16, data = gt2_sample, subset = (child_age_year > 9))

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
IV_A5  <- felm(fIV_A5, data = gt2_sample, subset = (child_age_year > 9))
IV_A6  <- felm(fIV_A6, data = gt2_sample, subset = (child_age_year > 9))
IV_A7  <- felm(fIV_A7, data = gt2_sample, subset = (child_age_year > 9))
IV_A8  <- felm(fIV_A8, data = gt2_sample, subset = (child_age_year > 9))

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

OLS_A3 <- felm(fOLS_A3, data = gt2_sample, subset = (child_age_year > 9))
IV_A9  <- felm(fIV_A9, data = gt2_sample, subset = (child_age_year > 9))
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
  keep.stat = c("n","rsq")
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
  keep.stat = c("n","rsq")
)



### Private school attendance ####
fOLS_B2 <- make_formula_gt3("private_school")
fIV_B5  <- make_formula_gt3("private_school", "twins_3")
fIV_B6  <- make_formula_gt3("private_school", "same_sex_123")
fIV_B7  <- make_formula_gt3("private_school", "boy_123 + girl_123")
fIV_B8  <- make_formula_gt3("private_school", "twins_3 + boy_123 + girl_123")

OLS_B2 <- felm(fOLS_B2, data = gt3_sample, 
               subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_B5  <- felm(fIV_B5, data = gt3_sample, 
               subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_B6  <- felm(fIV_B6, data = gt3_sample, 
               subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_B7  <- felm(fIV_B7, data = gt3_sample, 
               subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_B8  <- felm(fIV_B8, data = gt3_sample, 
               subset = (child_age_year > 9) & (moth_pp_group != "Black African"))

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


## Sub-sample analysis ####

### Educational attainment ####

OLS_SS_B1 <- felm(fOLS_A1, data = gt2_sample,
                subset = (child_age_year > 9) & (moth_pp_group == "Black African"))
IV_SS_B1 <- felm(fIV_A1, data = gt2_sample, 
                subset = (child_age_year > 9) & (moth_pp_group == "Black African"))
IV_SS_B3 <- felm(fIV_A3, data = gt2_sample, 
                subset = (child_age_year > 9) & (moth_pp_group == "Black African"))
IV_SS_B4 <- felm(fIV_A4, data = gt2_sample, 
                subset = (child_age_year > 9) & (moth_pp_group == "Black African"))

OLS_SS_NB1 <- felm(fOLS_A1, data = gt2_sample,
                 subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_SS_NB1 <- felm(fIV_A1, data = gt2_sample, 
                subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_SS_NB3 <- felm(fIV_A3, data = gt2_sample, 
                subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_SS_NB4 <- felm(fIV_A4, data = gt2_sample, 
                subset = (child_age_year > 9) & (moth_pp_group != "Black African"))

stargazer(
  OLS_SS_B1, IV_SS_B1, IV_SS_B3, IV_SS_B4, OLS_SS_NB1, IV_SS_NB3, IV_SS_NB4,  
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)


### Left Behind ####

OLS_SS_B2 <- felm(fOLS_A4, data = gt2_sample,
                  subset = (child_age_year > 9) & (moth_pp_group == "Black African"))
IV_SS_B5 <- felm(fIV_A13, data = gt2_sample, 
                 subset = (child_age_year > 9) & (moth_pp_group == "Black African"))
IV_SS_B6 <- felm(fIV_A15, data = gt2_sample, 
               subset = (child_age_year > 9) & (moth_pp_group == "Black African"))
IV_SS_B7 <- felm(fIV_A16, data = gt2_sample, 
                 subset = (child_age_year > 9) & (moth_pp_group == "Black African"))

OLS_SS_NB2 <- felm(fOLS_A4, data = gt2_sample,
                   subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_SS_NB5 <- felm(fIV_A13, data = gt2_sample, 
                  subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_SS_NB6 <- felm(fIV_A15, data = gt2_sample, 
                  subset = (child_age_year > 9) & (moth_pp_group != "Black African"))
IV_SS_NB7 <- felm(fIV_A16, data = gt2_sample, 
                  subset = (child_age_year > 9) & (moth_pp_group != "Black African"))

stargazer(
  OLS_SS_B2, IV_SS_B5, IV_SS_B6, IV_SS_B7, OLS_SS_NB2, IV_SS_NB5, IV_SS_NB6, IV_SS_NB7,  
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq")
)



# Think of ways of getting robust std. errors for the 2+ sample

# Sub-sample analysis
#  * population group (e.g., black and non-black)
#  * mother's age (e.g., >30 and <= 30)

# Run both OLS & IV regs for each age and plot in a graph 

# Try out:

## Load example dataset
data(tli)

## Demonstrate data.frame
tli.table <- xtable(tli[1:20, ])
print(tli.table, 
      file="D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table1.tex")
print(tli.table, type = "html")
xtable(mtcars)
mt.table <- xtable(mtcars, auto = TRUE)
print(mt.table, 
      file="D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table2.tex")


"D:/MSc_ED/Thesis/SA_2011_Census/outline/"



ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
print(xtable(lm.D9),
      file="D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table3.tex")
print(xtable(anova(lm.D9)))


## Demonstrate include.rownames, include.colnames,
## only.contents and add.to.row arguments
library(xtable)
set.seed(2345)
x <- matrix(sample(0:9, size = 4*3, replace = TRUE), nrow = 4, ncol = 3)
rownames(x) <- c("foo", "baa", "here", "there")
colnames(x) <- c("(1)", "(2)", "(3)")

xres <- xtable(x)

print(xres, file="D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table4.tex")

digits(xres) <- rep(0, 7)
addtorow <- list()
addtorow$pos <- list()
addtorow$pos[[1]] <- c(0, 2)
addtorow$pos[[2]] <- 4
addtorow$command <- c('\vspace{2mm} \n', '\vspace{10mm} \n')
print(xres, add.to.row = addtorow, include.rownames = FALSE,
      include.colnames = TRUE, only.contents = TRUE,
      hline.after = c(0, 0, 9, 9),
      file="D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table4.tex")

res <- stargazer(
  OLS_A1, 
  # IV_A1, IV_A2, IV_A3, IV_A4,
  keep = c("no_kids"),
  type = "text",
  keep.stat = c("n", "rsq") 
)

library(textTools)

str_rm_non_alphanumeric(c("test 67890 * % $ "))
str_rm_blank_space(c("test 67890 * % $ "))

res[[8]] %>% 
  str_rm_words("no_kids") %>% 
  str_rm_blank_space() 


OLS <- lm(no_kids ~ educ_attain + behind + private_school + moth_inlf, 
          data = gt2_sample)

iv <- ivreg(no_kids ~ educ_attain + behind + private_school + moth_inlf 
            | twins_2 + behind + private_school + moth_inlf, 
          data = gt2_sample)

coef(summary(OLS_A1))
coef(summary(OLS_A2))
coef(summary(OLS_A3))
coef(summary(OLS_A4))

rownames( coef(summary(OLS_A1)) ) <- c(
  "educ_attain",  "behind",  "private_school", "moth_inlf", "place holder"
  )

x <- matrix(sample(0:9, size = 4*3, replace = TRUE), nrow = 5, ncol = 1)

rownames(x) <- c("educ_attain",  "behind",  "private_school", "moth_inlf", "place holder")
colnames(x) <- c("no_kids")

OLS$coefficients <- x
OLS$lhs <- c("no_kids")

rownames(A) <- c("educ_attain",  "behind",  "private_school", "moth_inlf", "place holder")




#######

set.seed()
x <- matrix(sample(0:9, size = 4*3, replace = TRUE), nrow = 4, ncol = 3)

## Create a fictitious data
set.seed(2345)
n <- 1e3

d <- data.frame(
  no_kids = sample(1000, n, replace=TRUE),
  educ_attain = rnorm(n) + 10,
  behind = sample(c(0,1), n, replace=TRUE),
  private_school = rnorm(n),
  moth_inlf = rnorm(n),
  iv = rnorm(n) + 5
)

ols <- lm(no_kids ~ 0 + educ_attain + behind + private_school + moth_inlf, 
          data = d)

iv <- ivreg(no_kids ~ 0 + educ_attain + behind + private_school + moth_inlf 
            | iv + behind + private_school + moth_inlf, 
            data = d)

# Consider writing a loop

### coefficients ####

coef_list <- list(
# OLS
ols = c(
coef(summary(OLS_A1, robust = TRUE))[1,1],
coef(summary(OLS_A4, robust = TRUE))[1,1],
coef(summary(OLS_A2, robust = TRUE))[1,1],
coef(summary(OLS_A3, robust = TRUE))[1,1]
),
# IV -twins_2
iv_twins = c(
coef(summary(IV_A1))[5,1],
coef(summary(IV_A13))[5,1],
coef(summary(IV_A5))[5,1],
coef(summary(IV_A9))[5,1]
),
# IV - same_sex_12
iv_same_sex = c(
  coef(summary(IV_A2))[5,1],
  coef(summary(IV_A14))[5,1],
  coef(summary(IV_A6))[5,1],
  coef(summary(IV_A10))[5,1]
),
# IV - boy_12 + girl_12
iv_boy_girl = c(
  coef(summary(IV_A3))[5,1],
  coef(summary(IV_A15))[5,1],
  coef(summary(IV_A7))[5,1],
  coef(summary(IV_A11))[5,1]
),
# IV - twins_2 + boy_12 + girl_12 
iv_all = c(
  coef(summary(IV_A4))[5,1],
  coef(summary(IV_A16))[5,1],
  coef(summary(IV_A8))[5,1],
  coef(summary(IV_A12))[5,1]
)
)

### std. errors ####

se_list <- list(
  # OLS
  c(
    coef(summary(OLS_A1, robust = TRUE))[1,2],
    coef(summary(OLS_A4, robust = TRUE))[1,2],
    coef(summary(OLS_A2, robust = TRUE))[1,2],
    coef(summary(OLS_A3, robust = TRUE))[1,2]
  ),
  # IV - twins_2
  c(
    coef(summary(IV_A1))[5,2],
    coef(summary(IV_A13))[5,2],
    coef(summary(IV_A5))[5,2],
    coef(summary(IV_A9))[5,2]
  ),
  # IV - same_sex_12
  c(
    coef(summary(IV_A2))[5,2],
    coef(summary(IV_A14))[5,2],
    coef(summary(IV_A6))[5,2],
    coef(summary(IV_A10))[5,2]
  ),
  # IV - boy_12 + girl_12
  c(
    coef(summary(IV_A3))[5,2],
    coef(summary(IV_A15))[5,2],
    coef(summary(IV_A7))[5,2],
    coef(summary(IV_A11))[5,2]
  ),
  # IV - twins_2 + boy_12 + girl_12 
  c(
    coef(summary(IV_A4))[5,2],
    coef(summary(IV_A16))[5,2],
    coef(summary(IV_A7))[5,2],
    coef(summary(IV_A12))[5,2]
  )
)

### p-values ####

p_list <- list(
  # OLS
  c(
    coef(summary(OLS_A1, robust = TRUE))[1,4],
    coef(summary(OLS_A4, robust = TRUE))[1,4],
    coef(summary(OLS_A2, robust = TRUE))[1,4],
    coef(summary(OLS_A3, robust = TRUE))[1,4]
  ),
  # IV - twins_2
  c(
    coef(summary(IV_A1))[5,4],
    coef(summary(IV_A13))[5,4],
    coef(summary(IV_A5))[5,4],
    coef(summary(IV_A9))[5,4]
  ),
  # IV - same_sex_12
  c(
    coef(summary(IV_A2))[5,4],
    coef(summary(IV_A14))[5,4],
    coef(summary(IV_A6))[5,4],
    coef(summary(IV_A10))[5,4]
  ),
  # IV - boy_12 + girl_12
  c(
    coef(summary(IV_A3))[5,4],
    coef(summary(IV_A15))[5,4],
    coef(summary(IV_A7))[5,4],
    coef(summary(IV_A11))[5,4]
  ),
  # IV - twins_2 + boy_12 + girl_12 
  c(
    coef(summary(IV_A4))[5,4],
    coef(summary(IV_A16))[5,4],
    coef(summary(IV_A7))[5,4],
    coef(summary(IV_A12))[5,4]
  )
)

### stargazer ####

models <- list(
  ols_results = list(OLS_A1, OLS_A4, OLS_A2, OLS_A3), 
  iv_twins = list(IV_A1, IV_A13, IV_A5, IV_A9),
  iv_same_sex = list(IV_A2, IV_A14, IV_A6, IV_A10),
  iv_boy_girl = list(IV_A3, IV_A15, IV_A7, IV_A11),
  iv_all = list(IV_A4, IV_A16, IV_A8, IV_A12)
  # add estimation objects from 3+ sample here
)

# Create empty lists
make_list <- function() {
  x <- list(ols = double(4), iv_twins = double(4), 
              iv_same_sex = double(4), iv_boy_girl = double(4), 
              iv_all = double(4)) # expand this to accommodate 3+ estimations
  
  return(x)
}

coef_list <- make_list()
se_list <- make_list()
p_list <- make_list()

# Collect coefficients, standard errors, and p-values
for (i in seq_along(models)) {
  for (j in seq_along(models[[i]])) {
    if (i == 1) {
      coef_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[1,1]
      se_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[1,2]
      p_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[1,4]
    } else {
      coef_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[5,1]
      se_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[5,2]
      p_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[5,4]
    }
  }
}

stargazer(
  OLS, iv, iv, iv, iv,
  coef = coef_list,
  se = se_list,
  p = p_list,
  type = "text",
  omit.stat = "all",
  style = "aer"
  # ,
  # out = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table5.tex"
)





















