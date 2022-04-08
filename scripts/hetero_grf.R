
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

comb.1 <- expand.grid(
  moth_age_fstbr = 16:34, 
  moth_pp_group = c(
    "Black African", "White", "Coloured, Indian or Asian, and Other"
    )) %>% 
  dummy_cols("moth_pp_group")


moth_educ <- gt2_sample %>% count(moth_educ) %>% pull(moth_educ)

comb.2 <- expand.grid(
  moth_educ = moth_educ, 
  moth_pp_group = c(
    "Black African", "White", "Coloured, Indian or Asian, and Other"
  )) %>% 
  dummy_cols(c("moth_educ", "moth_pp_group"))


median <- map_dbl(X, median)

X.pred.1 <- matrix(nrow = nrow(comb.1), ncol = ncol(X)) 
colnames(X.pred.1) <- names(median)

X.pred.2 <- matrix(nrow = nrow(comb.2), ncol = ncol(X)) 
colnames(X.pred.2) <- names(median)

for (i in 1:nrow(comb.1)) {
  X.pred.1[i, ] <- median
}

for (i in 1:nrow(comb.2)) {
  X.pred.2[i, ] <- median
}

X.pred.1 <- as_tibble(X.pred.1) %>% 
  mutate(
    moth_age_fstbr = comb$moth_age_fstbr,
    `moth_pp_group_Black African` = comb$`moth_pp_group_Black African`,
    moth_pp_group_White = comb$moth_pp_group_White,
    `moth_pp_group_Coloured, Indian or Asian, and Other` = 
      comb$`moth_pp_group_Coloured, Indian or Asian, and Other`
  )

X.pred.2 <- as_tibble(X.pred.2) %>% 
  mutate(
    `moth_pp_group_Black African` = comb.2$`moth_pp_group_Black African`,
    moth_pp_group_White = comb.2$moth_pp_group_White,
    `moth_pp_group_Coloured, Indian or Asian, and Other` = 
      comb.2$`moth_pp_group_Coloured, Indian or Asian, and Other`,
    `moth_educ_No schooling` = comb.2$`moth_educ_No schooling`,
    `moth_educ_Some primary` = comb.2$`moth_educ_Some primary`,
    `moth_educ_Completed primary` = comb.2$`moth_educ_Completed primary`,
    `moth_educ_Some secondary` = comb.2$`moth_educ_Some secondary`,
    `moth_educ_Grade 12/Std 10` = comb.2$`moth_educ_Grade 12/Std 10`,
    moth_educ_Higher = comb.2$moth_educ_Higher,
  )

# Predict on the tesing sample along the first dimensions of hetero.
pred.educ_attain.1 <- predict(tau.educ_attain, X.pred.1, estimate.variance = TRUE)
pred.behind.1 <- predict(tau.behind, X.pred.1, estimate.variance = TRUE)
pred.private_school.1 <- predict(tau.private_school, X.pred.1, estimate.variance = TRUE)

pred.educ_attain.1 <- pred.educ_attain.1 %>% 
  rename(pred_educ = "predictions", var_educ = "variance.estimates")

pred.behind.1 <- pred.behind.1 %>% 
  rename(pred_behind = "predictions", var_behind = "variance.estimates")

pred.private_school.1 <- pred.private_school.1 %>% 
  rename(pred_private = "predictions", var_private = "variance.estimates")

final.1 <- X.pred.1 %>% 
  as_tibble() %>% 
  mutate(moth_pp_group = comb$moth_pp_group) %>% 
  select(moth_age_fstbr, contains("moth_pp_group")) %>% 
  bind_cols(pred.educ_attain.1, pred.behind.1, pred.private_school.1) %>% 
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

# Predict on the tesing sample along the second dimensions of hetero.

pred.educ_attain.2 <- predict(tau.educ_attain, X.pred.2, estimate.variance = TRUE)
pred.behind.2 <- predict(tau.behind, X.pred.2, estimate.variance = TRUE)
pred.private_school.2 <- predict(tau.private_school, X.pred.2, estimate.variance = TRUE)

pred.educ_attain.2 <- pred.educ_attain.2 %>% 
  rename(pred_educ = "predictions", var_educ = "variance.estimates")

pred.behind.2 <- pred.behind.2 %>% 
  rename(pred_behind = "predictions", var_behind = "variance.estimates")

pred.private_school.2 <- pred.private_school.2 %>% 
  rename(pred_private = "predictions", var_private = "variance.estimates")


final.2 <- X.pred.2 %>% 
  as_tibble() %>% 
  mutate(moth_pp_group = comb.2$moth_pp_group, moth_educ = comb.2$moth_educ) %>% 
  select(contains("moth_educ"), contains("moth_pp_group")) %>% 
  select(
    -c(`moth_educ_Completed primary`:`moth_educ_Some secondary`),
    -c(`moth_pp_group_Black African`:moth_pp_group_White),
    -contains(c("var_", "sigma.hat"))
    ) %>% 
  bind_cols(pred.educ_attain.2, pred.behind.2, pred.private_school.2) %>% 
  pivot_longer(
    pred_educ:var_private, 
    names_to = c("type" ,"outcome"),
    names_sep = "_",
    values_to = "value"
    ) %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(
    upper = pred + 1.96 * sqrt(var),
    lower = pred - 1.96 * sqrt(var),
  )

# plot
final.2 %>% 
  ggplot(aes(moth_educ, pred_educ, color = moth_pp_group)) +
  geom_point(size = 1.75, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower_educ, ymax = upper_educ), 
                width = .2, position = position_dodge(width = 0.9)) +
  geom_hline(aes(yintercept = 0), color = "red", 
             linetype = "dashed") +
  coord_flip()
# facet_wrap(~moth_pp_group)














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

























