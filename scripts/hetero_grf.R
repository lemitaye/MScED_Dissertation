
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
    child_no, moth_no, child_dob, boy, birth_order, boy_1, boy_2, 
    twins_1, child_age_year, child_sch_attend, municip, moth_age_month, 
    moth_dob, moth_ceb, moth_age_scndbr, child_educ_gen, mean_educ_agg, 
    mode_educ_agg, twins_2, district, boy_12, girl_12, same_sex_12,
    private_school, birth_month, educ_attain, behind, child_pop_group
  )) %>% 
  dummy_cols(
    c("child_sex", "child_private", "province", "moth_pp_group", 
      "moth_educ", "moth_marital", "moth_employ", "moth_income")
  ) %>% 
  select(
    -c(child_sex, child_private, province, moth_pp_group, moth_educ,
       moth_marital, moth_employ, moth_income)
  )

X <- model.matrix(~ ., data = X)
head(X)

set.seed (1212)
train <- sample(nrow(X), round(0.6*nrow(X))) # use 60% for training
X.train <- X[train, ]
X.test <- X[-train, ]

W <- gt2_sample$no_kids[train]
Z <- gt2_sample$twins_2[train]

Y1 <- gt2_sample$educ_attain[train]
Y2 <- gt2_sample$behind[train]

tau.forest <- instrumental_forest(X.train, Y1, W, Z, num.trees = 500)

# Mother's population group and Mother's age at first birth

median <- map_dbl(as_tibble(X.test), median)

X.pred <- matrix(nrow = nrow(X.test), ncol = ncol(X.test))
colnames(X.pred) <- names(median)

for (i in 1:nrow(X.test)) {
  X.pred[i, ] <- median
}

X.pred <- X.pred %>% 
  as_tibble() %>% 
  mutate(
    moth_age_fstbr = X.test[,"moth_age_fstbr"],
    moth_pp_groupColoured = X.test[,"moth_pp_groupColoured"],
    "moth_pp_groupIndian or Asian" = X.test[,"moth_pp_groupIndian or Asian"],
    moth_pp_groupOther = X.test[,"moth_pp_groupOther"],
    moth_pp_groupWhite = X.test[,"moth_pp_groupWhite"]
  ) %>% 
  as.matrix() 

# Predict on the tesing sample
tau.hat <- predict(tau.forest, X.pred, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)

final <- X.pred %>% 
  as_tibble() %>% 
  select(contains("moth_pp_group"), moth_age_fstbr) %>% 
  bind_cols(tau.hat) %>% 
  mutate(
    sigma.hat = sqrt(variance.estimates),
    upper = predictions + 1.96 * sigma.hat,
    lower = predictions - 1.96 * sigma.hat
  )

final %>% 
  filter(predictions < 200) %>% 
  filter(between(moth_age_fstbr, 16, 37)) %>% 
  ggplot(aes(moth_age_fstbr, predictions)) +
  geom_line(aes(group = 1)) #+
# geom_line(aes(y = upper, group = 1), linetype = "dashed") +
# geom_line(aes(y = lower, group = 1), linetype = "dashed")

final %>% 
  filter(predictions < 100) %>% 
  ggplot(aes(predictions)) +
  geom_histogram()


final %>% 
  filter(predictions < 200) %>% 
  filter(between(moth_age_fstbr, 18, 37)) %>% 
  group_by(moth_age_fstbr) %>% 
  summarise(
    mean_pred = median(predictions),
    mean_upper = median(upper),
    mean_lower = median(lower)
  ) %>% 
  ggplot(aes(moth_age_fstbr, mean_pred)) +
  geom_line(aes(group = 1)) +
  geom_line(aes(y = mean_upper, group = 1), linetype = "dashed") +
  geom_line(aes(y = mean_lower, group = 1), linetype = "dashed")

# we are making some progress
# issues to fix: X.train needs to include dummies for all cats., facet plot
# by mother's population group. Out-of-bag predictions?
# reduce "mtry" to 7 or lower, min. leaf size = ?
# variable importance plot 
# Think of how to extend this to 2SLS.