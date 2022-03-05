
# Created on: March 04, 2022
# DEPENDS ON: estimation.R

# This script produces LaTex tables for regression outputs

# Regression Tables #######

models <- list(
  ols_2 = list(OLS_A1, OLS_A4, OLS_A2, OLS_A3), 
  iv_twins_2 = list(IV_A1, IV_A13, IV_A5, IV_A9),
  iv_same_sex_2 = list(IV_A2, IV_A14, IV_A6, IV_A10),
  iv_boy_girl_2 = list(IV_A3, IV_A15, IV_A7, IV_A11),
  iv_all_2 = list(IV_A4, IV_A16, IV_A8, IV_A12),
  ols_3 = list(OLS_B1, OLS_B4, OLS_B2, OLS_B3), 
  iv_twins_3 = list(IV_B1, IV_B13, IV_B5, IV_B9),
  iv_same_sex_3 = list(IV_B2, IV_B14, IV_B6, IV_B10),
  iv_boy_girl_3 = list(IV_B3, IV_B15, IV_B7, IV_B11),
  iv_all_3 = list(IV_B4, IV_B16, IV_B8, IV_B12)
  # add estimation objects from 3+ sample here
)

# Create empty lists
make_list <- function() {
  x <- list(
    ols_2 = double(4), iv_twins_2 = double(4), iv_same_sex_2 = double(4), 
    iv_boy_girl_2 = double(4), iv_all_2 = double(4),
    ols_3 = double(4), iv_twins_3 = double(4), iv_same_sex_3 = double(4), 
    iv_boy_girl_3 = double(4), iv_all_3 = double(4)
  ) 
  
  return(x)
}

coef_list <- make_list()
se_list <- make_list()
p_list <- make_list()

# Collect coefficients, standard errors, and p-values
for (i in seq_along(models)) {
  for (j in seq_along(models[[i]])) {
    # The first and sixth models are OLS; needed robust s.e.
    if (i == 1 | i == 6) {
      coef_list[[i]][[j]] <- coef(summary(models[[i]][[j]], robust = TRUE))[1,1]
      se_list[[i]][[j]] <- coef(summary(models[[i]][[j]], robust = TRUE))[1,2]
      p_list[[i]][[j]] <- coef(summary(models[[i]][[j]], robust = TRUE))[1,4]
    } else {
      coef_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[5,1]
      se_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[5,2]
      p_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))[5,4]
    }
  }
}

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

# Run reg. using fictitious data as placeholders
ols <- lm(no_kids ~ 0 + educ_attain + behind + private_school + moth_inlf, 
          data = d)

iv <- ivreg(no_kids ~ 0 + educ_attain + behind + private_school + moth_inlf 
            | iv + behind + private_school + moth_inlf, 
            data = d)

labels <- c(
  "Educational Attainment",
  "Left Behind",
  "Private School",
  "Mothers LFP"
)

last_lines = list(
  c("IV used", "-", "Twins2", "SameSex12", "Boy12", "Twins2, Boy12",
    "-", "Twins3", "SameSex123", "Boy123", "Twins3, Boy123"),
  c(" ", " ", " ", " ", "Girl12", "Girl12",
    " ", " ", " ", " ", "Girl123", "Girl123")
)

stargazer(
  ols, iv, iv, iv, iv, ols, iv, iv, iv, iv,
  coef = coef_list,
  se = se_list,
  p = p_list,
  # type = "text",
  omit.stat = "all",
  style = "aer",
  covariate.labels = labels,
  column.labels   = c("2+ Sample", "3+ Sample"),
  column.separate = c(5, 5),
  dep.var.labels.include = FALSE,
  model.names = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  float = FALSE,
  add.lines = last_lines
  ,
  out = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table5.tex"
)


# First Stages

# F-tests
t1 <- linearHypothesis(ma_1_o, c("twins_2"))
t2 <- linearHypothesis(ma_2_o, c("same_sex_12"))
t3 <- linearHypothesis(ma_3_o, c("boy_12", "girl_12"))
t4 <- linearHypothesis(ma_4_o, c("twins_2", "boy_12", "girl_12"))

F_stats <- round( c(t1[2,5], t2[2,5], t3[2,5], t4[2,5]), 2)

stargazer(
  ma_1_o, ma_2_o, ma_3_o, ma_4_o,
  keep = c(
    "boy_1", "same_sex_12", "boy_12", "girl_12", "twins_2"
  ),
  type = "text",
  keep.stat = c("n","rsq"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  add.lines = F_stats
)


##latex example

data(mtcars)
mod.mtcars.1 <- lm(mpg ~ hp + wt, mtcars)
mod.mtcars.2 <- lm(mpg ~ hp + wt + cyl, mtcars)
mod.mtcars.3 <- lm(hp ~ wt + cyl, mtcars)

stargazer(
  mod.mtcars.1, mod.mtcars.2, mod.mtcars.3, 
  type = "latex",
  keep.stat = c("n", "rsq")
) %>% 
star_insert_row(
  c("Controls? & No & No & No \\\\",
              "Linear Model? & Yes & Yes & Yes \\\\"),
                insert.after = c(29)
  ) %>% 
  star_tex_write(
    file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table6.tex"
    )
  















