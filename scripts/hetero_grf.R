
# Date Created: April 7, 2022

# A script to train and fit a "Generalized Random Forest" 
# from Athey et al. (2019) to carry out heterogeneity analysis


# Load necessary packages
library(tidyverse)
library(grf)
library(fastDummies)

gt2_sample <- read_csv("data/gt2_sample.csv")

# gt2_sample %>% 
#   glimpse()

X <- gt2_sample %>% 
  select(-c(
    no_kids, child_no, moth_no, child_dob, boy, birth_order, boy_1, boy_2, 
    twins_1, child_age_year, child_sch_attend, municip, moth_age_month, 
    moth_dob, moth_ceb, moth_age_scndbr, child_educ_gen, mean_educ_agg, 
    mode_educ_agg, twins_2, district, boy_12, girl_12, same_sex_12,
    private_school, birth_month, educ_attain, behind, child_pop_group,
    child_private
  )) %>% 
  mutate(
    moth_pp_group =
      case_when(
        moth_pp_group == "Black African" ~ "Black African",
        moth_pp_group == "White" ~ "White",
        moth_pp_group %in% c("Coloured", "Indian or Asian", "Other") 
        ~ "Coloured, Indian or Asian, and Other"
      ) %>% factor(),
    moth_income = fct_lump(factor(moth_income), 8),
  ) %>% 
  dummy_cols(
    c("child_sex", "province", "moth_pp_group", 
      "moth_educ", "moth_marital", "moth_employ", "moth_income")
  ) %>% 
  select(
    -c(child_sex, province, moth_pp_group, moth_educ,
       moth_marital, moth_employ, moth_income)
  )

X %>% glimpse()

W <- gt2_sample$no_kids
Z <- gt2_sample$twins_2

Y1 <- gt2_sample$educ_attain
Y2 <- gt2_sample$behind
Y3 <- gt2_sample$private_school

tau.educ_attain <- instrumental_forest(X, Y1, W, Z, num.trees = 100, mtry = 7)
tau.behind <- instrumental_forest(X, Y2, W, Z, num.trees = 100, mtry = 7)
tau.private_school <- instrumental_forest(X, Y3, W, Z, num.trees = 100, mtry = 7)


# Vary Mother's population group and Mother's age at first birth

comb <- expand.grid(
  moth_age_fstbr = 16:34, 
  moth_pp_group = c(
    "Black African", "White", "Coloured, Indian or Asian, and Other"
    )) %>% 
  dummy_cols("moth_pp_group")

median <- map_dbl(X, median)

X.pred <- matrix(nrow = nrow(comb), ncol = ncol(X)) 
colnames(X.pred) <- names(median)

for (i in 1:nrow(comb)) {
  X.pred[i, ] <- median
}

X.pred <- as_tibble(X.pred) %>% 
  mutate(
    moth_age_fstbr = comb$moth_age_fstbr,
    `moth_pp_group_Black African` = comb$`moth_pp_group_Black African`,
    moth_pp_group_White = comb$moth_pp_group_White,
    `moth_pp_group_Coloured, Indian or Asian, and Other` = 
      comb$`moth_pp_group_Coloured, Indian or Asian, and Other`
  )

# Predict on the tesing sample
pred.educ_attain <- predict(tau.educ_attain, X.pred, estimate.variance = TRUE)
pred.behind <- predict(tau.behind, X.pred, estimate.variance = TRUE)
pred.private_school <- predict(tau.private_school, X.pred, estimate.variance = TRUE)

pred.educ_attain <- pred.educ_attain %>% 
  rename(pred_educ = "predictions", var_educ = "variance.estimates")

pred.behind <- pred.behind %>% 
  rename(pred_behind = "predictions", var_behind = "variance.estimates")

pred.private_school <- pred.private_school %>% 
  rename(pred_private = "predictions", var_private = "variance.estimates")

final <- X.pred %>% 
  as_tibble() %>% 
  mutate(moth_pp_group = comb$moth_pp_group) %>% 
  select(moth_age_fstbr, contains("moth_pp_group")) %>% 
  bind_cols(pred.educ_attain, pred.behind, pred.private_school) %>% 
  mutate(
    sigma.hat_educ = sqrt(var_educ),
    sigma.hat_behind = sqrt(var_behind),
    sigma.hat_private = sqrt(var_private),
    upper_educ = pred_educ + 1.96 * sigma.hat_educ,
    lower_educ = pred_educ - 1.96 * sigma.hat_educ,
    upper_behind = pred_behind + 1.96 * sigma.hat_behind,
    lower_behind = pred_behind - 1.96 * sigma.hat_behind,
    upper_private = pred_private + 1.96 * sigma.hat_private,
    lower_private = pred_private - 1.96 * sigma.hat_private,
  )

plot_out <- function(pred_outcome, upper, lower) {
  final %>% 
    ggplot(aes(moth_age_fstbr, {{pred_outcome}})) +
    geom_line(aes(group = 1)) +
    geom_line(aes(y = {{upper}}, group = 1), linetype = "dashed") +
    geom_line(aes(y = {{lower}}, group = 1), linetype = "dashed") +
    geom_hline(aes(yintercept = 0), color = "red", size = .5, linetype = "dashed") +
    facet_wrap(~ moth_pp_group, scales = "free_y")
}

plot_out(pred_educ, upper_educ, lower_educ)
plot_out(pred_behind, upper_behind, lower_behind)
plot_out(pred_private, upper_private, lower_private)

final %>% 
  ggplot(aes(moth_age_fstbr, pred_educ)) +
  geom_line(aes(group = 1)) +
  geom_line(aes(y = upper_educ, group = 1), linetype = "dashed") +
  geom_line(aes(y = lower_educ, group = 1), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), color = "red", size = .5, linetype = "dashed") +
  facet_wrap(~ moth_pp_group)




# we are making some progress
# issues to fix: 
# * X.train needs to include dummies for all cats., (x)
# * facet plot by mother's population group. (x)
# * Out-of-bag predictions?
# * reduce "mtry" to 7 or lower (x)
# * Tuning; e.g., min. leaf size ?
# * variable importance plot 
# * look at more dimensions of heterogeneity and more outcomes
#   - Education of the mother and population group
# Think of how to extend this to 2SLS using same sex instruments

























