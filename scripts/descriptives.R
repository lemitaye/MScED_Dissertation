
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

# Tables of Descriptive Statistics ####

gt2_descript <- gt2_sample %>% 
  select(
    no_kids,
    twins_2,
    same_sex_12,
    boy_12,
    girl_12,
    male = boy,
    child_age_year,
    child_age_month,
    child_educ,
    educ_attain,
    behind,
    private_school,
    moth_age_year,
    moth_age_fstbr,
    moth_pp_group,
    moth_educ,
    moth_marital,
    moth_inlf,
    fath_inhh,
    province
    ) %>% dummy_cols(
      c("moth_pp_group", "moth_educ", "moth_marital", "province")
      ) %>% 
  select(-c(moth_pp_group, moth_educ, province, moth_marital))

gt3_descript <- gt3_sample %>% 
  filter(!is.na(moth_pp_group)) %>% 
  select(
    no_kids,
    twins_3,
    same_sex_123,
    boy_123,
    girl_123,
    male = boy,
    child_age_year,
    child_age_month,
    child_educ,
    educ_attain,
    behind,
    private_school,
    moth_age_year,
    moth_age_fstbr,
    moth_pp_group,
    moth_educ,
    moth_marital,
    moth_inlf,
    fath_inhh,
    province,
    birth_order
  ) 

gt3_descript_frstbrn <- gt3_descript %>% 
  filter(birth_order == 1) %>% 
  dummy_cols(
    c("moth_pp_group", "moth_educ", "moth_marital", "province")
  ) %>% 
  select(
    -c(moth_pp_group, moth_educ, province, moth_marital, birth_order)
  )

gt3_descript_both <- gt3_descript %>%
  dummy_cols(
  c("moth_pp_group", "moth_educ", "moth_marital", "province")
) %>% 
  select(
    -c(moth_pp_group, moth_educ, province, moth_marital, birth_order)
    )

sd_rm_bin <- function(x) {
  
  if ( min(x, na.rm = TRUE) == 0 && max(x, na.rm = TRUE) == 1 ) {
    res <- NA
  } else {
    res <- sd(x, na.rm = TRUE)
  }

  res
}

mean_2 <- gt2_descript %>% 
  map_dbl(mean, na.rm = TRUE) %>% 
  round(3)

sd_2 <- gt2_descript %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

mean_3_frstbrn <- gt3_descript_frstbrn %>% 
  map_dbl(mean, na.rm = TRUE) %>% 
  round(3)

sd_3_frstbrn <- gt3_descript_frstbrn %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

mean_3_both <- gt3_descript_both %>% 
  map_dbl(mean, na.rm = TRUE) %>% 
  round(3)

sd_3_both <- gt3_descript_both %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

x <- cbind(mean_2, sd_2) %>% 
  data.frame() %>% 
  rownames_to_column("variable")

y <- cbind(mean_3_frstbrn, sd_3_frstbrn, mean_3_both, sd_3_both) %>% 
  data.frame() %>% 
  rownames_to_column("variable")

z <- full_join(x, y, by = "variable")
z$empty0 <- NA
z$empty1 <- NA
z$empty2 <- NA
z <- select(z, empty0, variable, mean_2, sd_2, empty1, mean_3_frstbrn,
            sd_3_frstbrn, empty2, mean_3_both, sd_3_both)

cols <- pull(z, "variable")  
cbind(cols) # to see the vector
cols_arrg <- cols[
  c(1, 6, 7, 8, 9, 2, 17, 3, 4, 5, 18, 19, 20, 10, 11, 12, 13, 14, 16, 15)
]

# cols <- c(
#   "no_kids", "male", "child_age_year", "child_age_month", "child_educ", "twins_2", 
#   "twins_3", "same_sex_12", "boy_12", "girl_12", "same_sex_123", "boy_123", "girl_123",
#   "moth_age_year", "moth_age_fstbr", "fath_in_hh", "educ_attain", "behind"
# )

z$variable <- factor(z$variable, levels = cols_arrg)
z <- z[order(z$variable), ]

names <- c("Number of Kids", "Male", "Age in years", "Age in months")


xtab <- xtable(z, caption = "Summary Statistics") 

# align(xtab) <- "rllccp{0.5cm}cc" {rllrrlrrlrr}
# digits(xtab) <- 3

addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 16, 21, 28, 34)
addtorow$command <- c(
  " & & \\multicolumn{2}{c}{2+ Sample} & & \\multicolumn{4}{c}{3+ Sample}  \\\\",
  "\\cline{3-4}  \\cline{6-10}",
  " & & \\multicolumn{2}{c}{Firstborns} & & \\multicolumn{2}{c}{Firstborns} & & \\multicolumn{2}{c}{Secondborns} \\\\",
  " & & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} & & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} & & 
  \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} \\\\",
  "\\multicolumn{2}{l}{Mother's Population Group} &  &  &  & & & & \\\\",
  "\\multicolumn{2}{l}{Mother's Level of Education} &  &  &  & & & & \\\\",
  "\\multicolumn{2}{l}{Mother's Marital Status} &  &  &  & & & & \\\\",
  "\\multicolumn{2}{l}{Province} &  &  &  & & & & \\\\"
  )

print(
  xtab,   
  add.to.row = addtorow,
  include.rownames = FALSE,
  include.colnames = FALSE,
  booktabs = TRUE,
  floating = FALSE,
  tabular.environment = "longtable",
  # floating.environment = "sidewaystable",
  file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table10.tex"
)
# Xtable

display(xtab) <- c("s", "s", "s", "g", "g", "s", "g", "g", "s", "g", "g")
digits(xtab) <- 4


xtable(mtcars, align = xalign(mtcars), digits = xdigits(mtcars),
       display = xdisplay(mtcars))


# include birth order in the 3+ sample
# table for first stage, descriptive stat. and main regressions
# Foucs on first stage
# marital status of the mother in regs.

# Heterogeneity of First Stage ####

gt2_sample %>% 
  group_by(twins_2) %>% 
  summarise(
    n = n(),
    mean_age_frstbr = mean(moth_age_fstbr_gen),
    sd_age_frstbr = sd(moth_age_fstbr_gen)        
    )

ha1 <- lm(moth_age_fstbr ~ twins_2, data = gt2_sample)
summary(ha1)

ha2 <- lm(moth_age_fstbr ~ twins_3, data = gt3_sample, 
          subset = birth_order == 1)
summary(ha2)





# Statistically significant d/ce in mean in the 2+ sample
t.test(moth_age_fstbr ~ twins_2, data = gt2_sample) %>% tidy()

gt3_sample %>% 
  group_by(twins_3) %>% 
  summarise(
    n = n(),
    mean_age_frstbr = mean(moth_age_fstbr_gen, na.rm = TRUE),
    sd_age_frstbr = sd(moth_age_fstbr_gen, na.rm = TRUE)        
  )

# Not that much robust in the 3+ sample
t.test(moth_age_fstbr_gen ~ twins_3, data = gt3_sample) %>% tidy()

# Test if twinning is correlated with other demographics
# E.g., population group, socio-economic status

gt2_sample %>% 
  group_by(moth_income) %>% 
  summarise(
    twins = mean(twins_2), sd = sd(twins_2), n = n()
  )

gt3_sample %>% 
  group_by(moth_pp_group) %>% 
  summarise(
    twins = mean(twins_3), sd = sd(twins_3), n = n()
  )

# 2+ sample
# Some d/ce in population groups
aov_pp_2 <- aov(twins_2 ~ moth_pp_group, data = gt2_sample)
summary(aov_pp_2)
TukeyHSD(aov_pp_2)

# No d/ce in income group
aov_inc_2 <- aov(twins_2 ~ moth_income, data = gt2_sample)
summary(aov_inc_2)
TukeyHSD(aov_inc_2)

# Some d/ce in mother's education
aov_educ_2 <- aov(twins_2 ~ moth_educ, 
                  data = gt2_sample %>% filter(moth_pp_group != "White"))
summary(aov_educ_2)
TukeyHSD(aov_educ_2)

# 3+ sample
# No d/ce in population groups
aov_pp_3 <- aov(twins_3 ~ moth_pp_group, data = gt3_sample)
summary(aov_pp_3)
TukeyHSD(aov_pp_3)

# No d/ce in income group
aov_inc_3 <- aov(twins_3 ~ moth_income, data = gt3_sample)
summary(aov_inc_3)
TukeyHSD(aov_inc_3)

# Some d/ce in mother's education
aov_educ_3 <- aov(twins_3 ~ moth_educ, data = gt3_sample)
summary(aov_educ_3)
TukeyHSD(aov_educ_3)

# => Occurence of twins is higher among older mothers and 
# whites

library("ggpubr")
ggline(gt2_sample, x = "moth_pp_group", y = "twins_2", 
       add = c("mean_se", "jitter"), 
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "Twins2", xlab = "Mother's Population Group")


all_births <- all_persons %>%
  select(sn, dob)  %>% 
  mutate(year = year(dob)) %>% 
  count(year, name = "all")

uni_mults <- all_persons %>%
  select(sn, dob) %>% 
  group_by(sn) %>% 
  filter(duplicated(dob)) %>% 
  ungroup() %>% 
  distinct(mults, sn ,dob) %>% 
  mutate(year = year(dob))

uni_mults %>% 
  count(year) %>% 
  filter(year > 1970) %>% 
  ggplot( aes(year, n) ) + 
  geom_line()

all_births %>% 
  filter(year > 1950) %>% 
  ggplot( aes(year, n) ) + 
  geom_line()

uni_mults <- read_csv("data/uni_mults.csv")

uni_mults %>% 
  filter(year %>% between(1970, 2011)) %>%
  mutate(prop = (mult/all)*1000) %>% 
  ggplot(aes(year, prop)) +
  geom_line() +
  labs(
    title = "Multiple Births Per 1000 Live Births",
    x = "Year", 
    y = ""
    )

## Comparison of twins birth by pp. group and educ. level ####

gt2_sample %>%
  filter(moth_pp_group != "Other") %>% 
  group_by(moth_pp_group) %>%
  summarize(prop_twins = mean(twins_2)) %>%
  mutate(
    moth_pp_group = fct_reorder(moth_pp_group, prop_twins) %>% fct_rev()
  ) %>%
  ggplot(aes(moth_pp_group, prop_twins)) +
  geom_col() +
  scale_y_continuous(label = percent)

gt3_sample %>%
  filter(moth_pp_group != "Other") %>% 
  group_by(moth_pp_group) %>%
  summarize(prop_twins = mean(twins_3)) %>%
  mutate(
    moth_pp_group = fct_reorder(moth_pp_group, prop_twins) %>% fct_rev()
  ) %>%
  ggplot(aes(moth_pp_group, prop_twins)) +
  geom_col() +
  scale_y_continuous(label = percent)

gt2_sample %>%
  group_by(moth_educ) %>%
  summarize(prop_twins = mean(twins_2)) %>%
  mutate(
    moth_educ = fct_reorder(moth_educ, prop_twins) %>% fct_rev()
  ) %>%
  ggplot(aes(moth_educ, prop_twins)) +
  geom_col() +
  scale_y_continuous(label = percent)

gt3_sample %>%
  filter(moth_educ != "Other") %>% 
  group_by(moth_educ) %>%
  summarize(prop_twins = mean(twins_3)) %>%
  mutate(
    moth_educ = fct_reorder(moth_educ, prop_twins) %>% fct_rev()
  ) %>%
  ggplot(aes(moth_educ, prop_twins)) +
  geom_col() +
  scale_y_continuous(label = percent)

# # of kids by pp. group

gt2_sample %>%
  filter(moth_pp_group != "Other") %>% 
  group_by(moth_pp_group) %>%
  # mutate(
  #   moth_pp_group = fct_reorder(moth_pp_group, prop_twins) %>% fct_rev()
  # ) %>%
  ggplot(aes(moth_pp_group, no_kids)) +
  geom_boxplot() 

gt2_sample %>% 
  filter(no_kids > 4) %>% 
  count(twins_2)

# mother's starting birth at a younger age are likely to have more kids
# gt2_sample %>% 
#   filter(moth_age_fstbr %>% between(14, 35)) %>% 
#   ggplot(aes(moth_age_fstbr, no_kids)) +
#   geom_smooth()

gt2_sample %>%
  filter(moth_age_fstbr %>% between(14, 38)) %>%
  group_by(moth_age_fstbr) %>% 
  summarise(prop_twins = mean(twins_2)) %>% 
  ggplot(aes(moth_age_fstbr, prop_twins)) +
  geom_col() +
  scale_y_continuous(label = percent)

















