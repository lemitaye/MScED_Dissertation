
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
      coef_list[[i]][[j]] <- coef(summary(models[[i]][[j]], robust = TRUE))["no_kids", 1]
      se_list[[i]][[j]] <- coef(summary(models[[i]][[j]], robust = TRUE))["no_kids", 2]
      p_list[[i]][[j]] <- coef(summary(models[[i]][[j]], robust = TRUE))["no_kids", 4]
    } else {
      coef_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))["`no_kids(fit)`", 1]
      se_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))["`no_kids(fit)`", 2]
      p_list[[i]][[j]] <- coef(summary(models[[i]][[j]]))["`no_kids(fit)`", 4]
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

star.out %>% 
star_notes_tex(
  note.type = "threeparttable", #Use the latex 'caption' package for notes
  note = long_note) %>% 
  star_sidewaystable() %>% 
  star_tex_write(
    file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table5.tex"
  )

# First Stages ####

## 2+ sample ####
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
  # star.cutoffs = c(0.05, 0.01, 0.001),
  add.lines = F_line_a,
  title = "First Stage Regressions",
  dep.var.labels = "Number of Children",
  covariate.labels = c("Twins2", "SameSex12", "Boy12", "Girl12")
)

## 3+ sample ####
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
  # star.cutoffs = c(0.05, 0.01, 0.001),
  add.lines = F_line_b,
  covariate.labels = c("Twins3", "SameSex123", "Boy123", "Girl123")
)

long_note_frst <- "Covariates for all regressions include age (in months) and sex of child, 
mother's characteristics, dummies for districts, and a dummy for whether the father resides in the household.
The regressisons for the 3+ sample in addition include dummies for birth order. F statistics are from the Wald 
test for exclusion restrictions of the relevant instrument(s) from the regression. Robust standard errors 
(for the 2+ sample) and cluster robust standard errors (for the 3+ sample; clustered by mother's ID) are in parenthesis. 
*** Significant at 1\\%, ** Significant at 5\\%, * Significant at 10\\%."


frst_panel <- star_panel(
  frst_stg2, frst_stg3,
  same.summary.stats = FALSE, 
  panel.label.fontface = "bold",
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


# Sub-Sample Analysis - I: Whites vs. Non-Whites ######

models_ssI <- list(
  ols_2W = list(OLS_SS_AW1, OLS_SS_AW4, OLS_SS_AW2, OLS_SS_AW3), 
  iv_2W = list(IV_SS_AW1, IV_SS_AW13, IV_SS_AW5, IV_SS_AW9),
  ols_2NW = list(OLS_SS_ANW1, OLS_SS_ANW4, OLS_SS_ANW2, OLS_SS_ANW3), 
  iv_2NW = list(IV_SS_ANW1, IV_SS_ANW13, IV_SS_ANW5, IV_SS_ANW9),
  ols_3W = list(OLS_SS_BW1, OLS_SS_BW4, OLS_SS_BW2, OLS_SS_BW3), 
  iv_3W = list(IV_SS_BW1, IV_SS_BW13, IV_SS_BW5, IV_SS_BW9),
  ols_3NW = list(OLS_SS_BNW1, OLS_SS_BNW4, OLS_SS_BNW2, OLS_SS_BNW3), 
  iv_3NW = list(IV_SS_BNW1, IV_SS_BNW13, IV_SS_BNW5, IV_SS_BNW9)
)

# Create empty lists
make_list_ssI <- function() {
  x <- list(
    ols_2W = double(4), iv_2W = double(4), 
    ols_2NW = double(4), iv_2NW = double(4),
    ols_3W = double(4), iv_3W = double(4), 
    ols_3NW = double(4), iv_3NW = double(4)
  ) 
  
  return(x)
}

coef_list_ssI <- make_list_ssI()
se_list_ssI <- make_list_ssI()
p_list_ssI <- make_list_ssI()

# Collect coefficients, standard errors, and p-values
for (i in seq_along(models_ssI)) {
  for (j in seq_along(models_ssI[[i]])) {
    # Odd numbered models are OLS; needed robust s.e.
    if (i %in% c(1, 3, 5, 7)) { # Toggle this when removing columns
      # use the row name "no_kids" to be safe
      coef_list_ssI[[i]][[j]] <- coef(summary(models_ssI[[i]][[j]], robust = TRUE))["no_kids",1]
      se_list_ssI[[i]][[j]] <- coef(summary(models_ssI[[i]][[j]], robust = TRUE))["no_kids", 2]
      p_list_ssI[[i]][[j]] <- coef(summary(models_ssI[[i]][[j]], robust = TRUE))["no_kids", 4]
    } else {
      # use the row name "`no_kids(fit)`" to be safe
      coef_list_ssI[[i]][[j]] <- coef(summary(models_ssI[[i]][[j]]))["`no_kids(fit)`", 1]
      se_list_ssI[[i]][[j]] <- coef(summary(models_ssI[[i]][[j]]))["`no_kids(fit)`", 2]
      p_list_ssI[[i]][[j]] <- coef(summary(models_ssI[[i]][[j]]))["`no_kids(fit)`", 4]
    }
  }
}

# Prepare the pieces
labels_ss_I <- c(
  "Educational Attainment",
  "Left Behind",
  "Private School",
  "Mothers LFP"
)

nobs_wrap <- function(mod) {
  paste0("\\multicolumn{2}{c}{", nobs(mod), "}" )
}

last_lines_ss = c(
    " $ N $ ", nobs_wrap(OLS_SS_AW1), nobs_wrap(OLS_SS_ANW1), 
    nobs_wrap(OLS_SS_BW1), nobs_wrap(OLS_SS_BNW1)
    )

last_lines_ss <- paste( paste(last_lines_ss, collapse = " & "), "\\\\" )

header <- c(
  " & \\multicolumn{4}{c}{2+ Sample} & \\multicolumn{4}{c}{3+ Sample} \\\\",
  "\\cline{2-5}  \\cline{6-9} \\\\",
  " & \\multicolumn{2}{c}{Whites} & \\multicolumn{2}{c}{Non-Whites} & 
    \\multicolumn{2}{c}{Whites} & \\multicolumn{2}{c}{Non-Whites} \\\\",
  "\\cline{2-3}  \\cline{4-5} \\cline{6-7} \\cline{8-9} \\\\[-1.8ex]" #,
  # "\\hline \\\\[-1.8ex] "
)

long_note_ssI = "*** Significant at 0.1\\%, ** Significant at 1\\%, * Significant at 5\\%. \\\\[-1.8ex] 

$ \\dag $ Columns 1-4 have the coefficients for the Twins2
instrument (in the 2+ sample) and in columns 5-8 are the coefficients for the
Twins3 instrument (3+ sample). The regressions were run using the same set 
of controls as in Table ?. Robust standard errors are in parentheses and the numbers 
in square brackets below the s.e. are F stats for the exclusion of the Twins2 or
the Twins 3 variable from the model. \\\\[-1.8ex]
 
$ \\ddag $ The regressions control for the same set of covariates as in Table ? (see notes there). 
The regressions for the 3+ sample are clustered by mother's ID. 
"

f_wrap <- function(mod, sample = 2) {
  if (sample == 2) {
    x <- paste0("\\multicolumn{2}{c}{ [", 
                sprintf( waldtest(mod, ~ twins_2)[["F"]], fmt='%#.4g'), 
                "] }" )
  } else if (sample == 3) {
    x <- paste0("\\multicolumn{2}{c}{ [", 
                sprintf( waldtest(mod, ~ twins_3)[["F"]], fmt='%#.4g'), 
                "] }" )
  }
  
}

f_line_ssI <- c(
  " \\multicolumn{1}{c}{$F$} ", f_wrap(ma_SS_W1, 2), f_wrap(ma_SS_NW1, 2), 
  f_wrap(mb_SS_W1, 3), f_wrap(mb_SS_NW1, 3)
)

f_line_ssI <- paste( paste(f_line_ssI, collapse = " & "), "\\\\" )

## Run the below #####

# Upper Table
star_trial <- stargazer(
  ma_SS_W1, ols, ma_SS_NW1, ols, mb_SS_W1, ols, mb_SS_NW1, ols,
  keep = c(
    "twins_2", "twins_3" 
  ),
  # type = "text",
  omit.stat = "all",
  dep.var.caption  = "",
  dep.var.labels.include = FALSE,
  header = FALSE,
  model.names = FALSE,
  column.sep.width = "8pt",
  star.cutoffs = c(0.05, 0.01, 0.001),
  title = "Heterogeneity by Mother's Population Group (Whites vs. Non-Whites)"
) 

star_trial[5] <- "\\begin{tabular}{@{\\extracolsep{8pt}}lcc@{\\hskip 0.3in}cc@{\\hskip 0.3in}cc@{\\hskip 0.3in}cc} "

star_trial[10:12] <- c(
  " Twins2/ & \\multicolumn{2}{c}{ 0.942$^{***}$ }  & \\multicolumn{2}{c}{ 0.809$^{***}$ } 
  &  \\multicolumn{2}{c}{ 0.861$^{***}$ }  &  \\multicolumn{2}{c}{ 0.756$^{***}$ }  \\\\ ",
  " Twins3 & \\multicolumn{2}{c}{ (0.044) }  & \\multicolumn{2}{c}{ (0.030) } 
  &  \\multicolumn{2}{c}{ (0.068) }  &  \\multicolumn{2}{c}{ (0.037) }  \\\\ ",
  f_line_ssI
) 

# star_trial <- star_trial %>% 
#   star_rhs_names(pattern = "Twins", line1 = "Twins2/", line2 = "Twins3")

# Bottom Table
star_ssI <- stargazer(
  ols, iv, ols, iv, ols, iv, ols, iv, 
  coef = coef_list_ssI,
  se = se_list_ssI,
  p = p_list_ssI,
  # type = "text",
  omit.stat = "all",
  # style = "aer",
  dep.var.caption  = "",
  covariate.labels = labels_ss_I,
  column.labels   = c("OLS", "IV", "OLS", "IV", "OLS", "IV", "OLS", "IV"),
  dep.var.labels.include = FALSE,
  model.names = FALSE,
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes = NULL,
  header = FALSE
)

# Putting the everything together
star_print <- star_panel(
  star_trial, star_ssI,
  same.summary.stats = FALSE, 
  panel.label.fontface = "bold",
  panel.names = c("First Stage$^{\\dag}$", "OLS \\& 2SLS$^{\\ddag}$")
) %>% 
  star_insert_row(
    header,
    insert.after = c(7)
  ) %>% 
  star_notes_tex(
    note.type = "threeparttable",
    note = long_note_ssI
  ) 
 
star_print <- star_print %>% 
  star_insert_row(
    c(
      " & OLS & IV & OLS & IV & OLS & IV & OLS & IV \\\\",
      " \\hline \\\\",
      last_lines_ss
    ),
    insert.after = c(26, 26, 41)
  ) 

star_print <- star_print[-c(19:24, 41:42)]

star_sidewaystable(star_print) %>% 
  star_tex_write(
    file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table12.tex"
  )


# No First Stages Samples #####

models_nofrst <- list(
  twins_ls2 = list(rma_1_t1, rma_2_t1, rma_3_t1, rma_4_t1), 
  twins_nosch = list(rma_1_t2, rma_2_t2, rma_3_t2, rma_4_t2),
  boy_ls2 = list(rmb_1_t1, rmb_2_t1, rmb_3_t1, rmb_4_t1),
  boy_nosch = list(rmb_1_t2, rmb_2_t2, rmb_3_t2, rmb_4_t2)
)

# Create empty lists
make_list_nofrst <- function() {
  x <- list(
    twins_ls2 = double(4), twins_nosch = double(4), 
    boy_ls2 = double(4), boy_nosch = double(4)
  ) 
  
  return(x)
}

coef_list_nofrst <- make_list_nofrst()
se_list_nofrst <- make_list_nofrst()
p_list_nofrst <- make_list_nofrst()


# Collect coefficients, standard errors, and p-values
for (i in seq_along(models_nofrst)) {
  for (j in seq_along(models_nofrst[[i]])) {
    # The first and second models are for twins, the last two are for boy12
    if (i == 1 | i == 2) { # Toggle this when removing columns
      coef_list_nofrst[[i]][[j]] <- coef(summary(models_nofrst[[i]][[j]], robust = TRUE))["twins_2", 1]
      se_list_nofrst[[i]][[j]] <- coef(summary(models_nofrst[[i]][[j]], robust = TRUE))["twins_2", 2]
      p_list_nofrst[[i]][[j]] <- coef(summary(models_nofrst[[i]][[j]], robust = TRUE))["twins_2", 4]
    } else if (i == 3 | i == 4) {
      coef_list_nofrst[[i]][[j]] <- coef(summary(models_nofrst[[i]][[j]], robust = TRUE))["boy_12", 1]
      se_list_nofrst[[i]][[j]] <- coef(summary(models_nofrst[[i]][[j]], robust = TRUE))["boy_12", 2]
      p_list_nofrst[[i]][[j]] <- coef(summary(models_nofrst[[i]][[j]], robust = TRUE))["boy_12", 4]
    }
  }
}

# The bottom table
nofrst_bottom <- stargazer(
  ols, ols, ols, ols,
  coef = coef_list_nofrst,
  se = se_list_nofrst,
  p = p_list_nofrst,
  # type = "text",
  keep.stat = c("n"),
  # style = "aer",
  dep.var.caption  = "",
  covariate.labels = labels,
  dep.var.labels.include = FALSE,
  model.names = FALSE
  # star.cutoffs = c(0.05, 0.01, 0.001),
  # add.lines = last_lines,
  # notes = NULL
)


# The top table

mb_t1 <- star_change_felm_rhs_names(
  mb_t1, old = "boy_12", new = "twins_2"
  )

mb_t2 <- star_change_felm_rhs_names(
  mb_t2, old = "boy_12", new = "twins_2"
)

nofrst_top <- stargazer(
  ma_t1, ma_t2, mb_t1, mb_t2,
  # type = "text",
  omit.stat = "all",
  keep = c("twins_2", "boy_12"),
  covariate.labels = "No. Children",
  header = FALSE,
  dep.var.caption  = "",
  dep.var.labels.include = FALSE
)


nofrst_panel <- star_panel(
  nofrst_top, nofrst_bottom,
  same.summary.stats = TRUE, 
  # panel.label.fontface = "bold",
  panel.names = c("First Stage", "Reduced Form")
) 

star_tex_write(
  nofrst_panel, 
  file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table13.tex"
)












