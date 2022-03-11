
# Created on: March 04, 2022
# DEPENDS ON: estimation.R

# This script produces LaTex tables for regression outputs

# Regression Tables #######

models <- list(
  ols_2 = list(OLS_A1, OLS_A4, OLS_A2, OLS_A3), 
  iv_twins_2 = list(IV_A1, IV_A13, IV_A5, IV_A9),
  # iv_same_sex_2 = list(IV_A2, IV_A14, IV_A6, IV_A10),
  iv_boy_girl_2 = list(IV_A3, IV_A15, IV_A7, IV_A11),
  iv_all_2 = list(IV_A4, IV_A16, IV_A8, IV_A12),
  ols_3 = list(OLS_B1, OLS_B4, OLS_B2, OLS_B3), 
  iv_twins_3 = list(IV_B1, IV_B13, IV_B5, IV_B9),
  # iv_same_sex_3 = list(IV_B2, IV_B14, IV_B6, IV_B10),
  iv_boy_girl_3 = list(IV_B3, IV_B15, IV_B7, IV_B11),
  iv_all_3 = list(IV_B4, IV_B16, IV_B8, IV_B12)
  # add estimation objects from 3+ sample here
)

# Create empty lists
make_list <- function() {
  x <- list(
    ols_2 = double(4), iv_twins_2 = double(4), 
    iv_boy_girl_2 = double(4), iv_all_2 = double(4),
    ols_3 = double(4), iv_twins_3 = double(4), 
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
    if (i == 1 | i == 5) { # Toggle this when removing columns
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
  c("IV used", "-", "Twins2", "Boy12", "Twins2, Boy12",
    "-", "Twins3", "Boy123", "Twins3, Boy123"),
  c(" ", " ", " ", "Girl12", "Girl12",
    " ", " ", "Girl123", "Girl123")
)

star.out <- stargazer(
  ols, iv, iv, iv, ols, iv, iv, iv, 
  coef = coef_list,
  se = se_list,
  p = p_list,
  # type = "text",
  omit.stat = "all",
  # style = "aer",
  dep.var.caption  = "",
  covariate.labels = labels,
  column.labels   = c("OLS", "IV", "OLS", "IV"),
  column.separate = c(1, 3, 1, 3),
  dep.var.labels.include = FALSE,
  model.names = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  add.lines = last_lines,
  notes = NULL
)

long_note = "*** Significant at 0.1\\%, ** Significant at 1\\%, * Significant at 5\\%. Robust standard errors are in parentheses.  
Covariates for all models include age (in months) and sex of child, mother's education, 
dummies for mother's population group and income range, dummies for districts, and a dummy for whether the father resides in the household. 
The regressions for the 3+ sample are clustered by mother's ID."

star_sidewaystable(star.out) %>% 
star_notes_tex(
  note.type = "caption", #Use the latex 'caption' package for notes
  note = long_note) %>% 
  star_tex_write(
    file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table5.tex"
  )

# First Stages ####

# 3+ sample
fa1 <- waldtest(ma_1, ~ twins_2)[["F"]]
fa2 <- waldtest(ma_2, ~ same_sex_12)[["F"]]
fa3 <- waldtest(ma_3, ~ boy_12 | girl_12)[["F"]]
fa4 <- waldtest(ma_4, ~ twins_2 | boy_12 | girl_12)[["F"]]

F_stats_a <- sprintf( c(0, fa1, fa2, fa3, fa4), fmt='%#.4g')
F_stats_a[1] <- "F"
F_line_a <- list(F_stats_a)


frst_stg2 <- stargazer(
  ma_1, ma_2, ma_3, ma_4,
  keep = c(
    "same_sex_12", "boy_12", "girl_12", "twins_2"
  ),
  # type = "text",
  keep.stat = c("rsq", "n"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  add.lines = F_line_a,
  title = "First Stage Regressions",
  dep.var.labels = "Number of Children",
  covariate.labels = c("Twins2", "SameSex12", "Boy12", "Girl12")
)

# 3+ sample
fb1 <- waldtest(mb_1, ~ twins_3)[["F"]]
fb2 <- waldtest(mb_2, ~ same_sex_123)[["F"]]
fb3 <- waldtest(mb_3, ~ boy_123 | girl_123)[["F"]]
fb4 <- waldtest(mb_4, ~ twins_3 | boy_123 | girl_123)[["F"]]

F_stats_b <- sprintf( c(0, fb1, fb2, fb3, fb4), fmt='%3.4g')
F_stats_b[1] <- "F"
F_line_b <- list(F_stats_b)


frst_stg3 <- stargazer(
  mb_1, mb_2, mb_3, mb_4,
  keep = c(
    "same_sex_123", "boy_123", "girl_123", "twins_3"
  ),
  # type = "text",
  keep.stat = c("rsq", "n"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  add.lines = F_line_b,
  covariate.labels = c("Twins3", "SameSex123", "Boy123", "Girl123")
)

long_note_frst <- "Covariates for all regressions include age (in months) and sex of child, 
mother's characteristics, dummies for districts, and a dummy for whether the father resides in the household.
The regressisons for the 3+ sample in addition include dummies for birth order. F statistics are from the Wald 
test for exclusion restrictions of the relevant instrument(s) from the regression. Robust standard errors 
(for the 2+ sample) and cluster robust standard errors (for the 3+ sample; clustered by mother's ID) are in parenthesis. 
*** Significant at 0.1\\%, ** Significant at 1\\%, * Significant at 5\\%."


frst_panel <- star_panel(
  frst_stg2, frst_stg3,
  same.summary.stats = FALSE, 
  panel.label.fontface = "bold.italic",
  panel.names = c("2+ Sample", "3+ Sample")
) %>% 
  star_notes_tex(
  note.type = "threeparttable", #Use the latex 'caption' package for notes
  note = long_note_frst
  )

star_tex_write(
  frst_panel, 
  file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table11.tex"
)


#####



##latex example

data(mtcars)
mod.mtcars.1 <- lm(mpg ~ hp + wt, mtcars)
mod.mtcars.2 <- lm(mpg ~ hp + wt + cyl, mtcars)
mod.mtcars.3 <- lm(hp ~ wt + cyl, mtcars)

stargazer(
  mod.mtcars.1, mod.mtcars.2, mod.mtcars.3, 
  type = "latex",
  keep.stat = c("n", "rsq"),
  add
) %>% 
star_insert_row(
  c("Controls? & No & No & No \\\\",
              "Linear Model? & Yes & Yes & Yes \\\\"),
                insert.after = c(29)
  ) %>% 
  star_tex_write(
    file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table6.tex"
    )
  



data(mtcars)
star.out <- stargazer(mtcars)
star_sidewaystable(star.out) %>% 
  star_tex_write(
    file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table9noh.tex"
  )











