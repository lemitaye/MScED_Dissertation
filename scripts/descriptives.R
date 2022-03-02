
# Date created: March 01, 2022
# DEPENDS ON: data_samples.R

# Descriptive statistics and graphics

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


# Reg. for each age group

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

fOLS_A1 <- make_formula_gt2("educ_attain")
fIV_A1 <- make_formula_gt2("educ_attain", "twins_2")

model_ols <- function(tbl) {
  felm(fOLS_A1, data = tbl)
}

model_twins <- function(tbl) {
  felm(fIV_A1, data = tbl)
}

get_confs <- function(mod) {
  confint(mod) %>% 
    as_tibble()
}

data_model_ols <- gt2_sample %>%
  filter(child_age_year > 7) %>% 
  group_by(child_age_year) %>% 
  nest() %>% 
  arrange(child_age_year) %>% 
  mutate(
    model = map(data, model_ols), 
    summaries = map(model, tidy), 
    conf_ints = map(model, get_confs)
  ) %>% 
  unnest(c(summaries, conf_ints)) %>% 
  select(-data, -model) %>% 
  filter(term == "no_kids") %>% 
  rename("conf_low" = `2.5 %`, "conf_high" = `97.5 %`) 

data_model_twins <- gt2_sample %>%
  filter(child_age_year > 7) %>% 
  group_by(child_age_year) %>% 
  nest() %>% 
  arrange(child_age_year) %>% 
  mutate(
    model = map(data, model_twins), 
    summaries = map(model, tidy), 
    conf_ints = map(model, get_confs)
  ) %>% 
  unnest(c(summaries, conf_ints)) %>% 
  select(-data, -model) %>% 
  filter(term == "`no_kids(fit)`") %>% 
  rename("conf_low" = `2.5 %`, "conf_high" = `97.5 %`) %>% 
  mutate(term = str_replace(term, fixed("`no_kids(fit)`"), "no_kids_iv"))

data_model_ols %>% 
  bind_rows(data_model_twins)


data_model_ols %>% 
  bind_rows(data_model_twins) %>% 
  # mutate(
  #   term = recode(
  #     factor(term), "no_kids" = "OLS", "no_kids_iv" = "IV (twins2)"
  #     )
  # ) %>% 
  ggplot(aes(factor(child_age_year), estimate)) + 
  geom_point(color = "red") + 
  facet_wrap(~ term) +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), 
                width = .1, position = "dodge") +
  geom_hline(aes(yintercept = 0)) +
  # coord_flip() +
  labs(
    title = "Plot of 2SLS Coefficients using Twins2 instrument",
    x = "Age of Child",
    y = "Coefficient",
    caption = "The red dots indicate coefficient estimate and the lines are 95% confidence intervals"
  )










































