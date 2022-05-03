
# Date created: March 01, 2022
# DEPENDS ON: data_samples.R

# Descriptive statistics and graphics

rm(list = ls())

# Load saved data ####
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


# Plotting Regression by Mother's Age ####

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

fOLS_A4 <- make_formula_gt2("behind")
fIV_A13 <- make_formula_gt2("behind", "twins_2")

fOLS_A2 <- make_formula_gt2("private_school")
fIV_A5 <- make_formula_gt2("private_school", "twins_2")

fOLS_A3 <- make_formula_gt2("moth_inlf")
fIV_A9 <- make_formula_gt2("moth_inlf", "twins_2")

# specify the age range here:
# age_range <- 21:40
# # create an empty list to store data sets
# age_data <- vector("list", length = length(age_range))

# for (age in age_range) {
#   # create a list containing all the data
#   index <- which(age_range == age)
#   age_data[[index]] <- filter(gt2_sample, moth_age_year >= age)
# }

age_data <- list(
  filter(gt2_sample, moth_age_fstbr %>% between(15, 19)),
  filter(gt2_sample, moth_age_fstbr %>% between(20, 24)),
  filter(gt2_sample, moth_age_fstbr %>% between(25, 29)),
  filter(gt2_sample, moth_age_fstbr >= 30)
)


# loop through the tibbles running the appropriate model 
ols_educ <- map( age_data, ~felm(fOLS_A1, data = .) )
iv_educ <- map( age_data, ~felm(fIV_A1, data = .) )

ols_behind <- map( age_data, ~felm(fOLS_A4, data = .) )
iv_behind <- map( age_data, ~felm(fIV_A13, data = .) )

ols_private <- map( age_data, ~felm(fOLS_A2, data = .) )
iv_private <- map( age_data, ~felm(fIV_A5, data = .) )

ols_lfp <- map( age_data, ~felm(fOLS_A3, data = .) )
iv_lfp <- map( age_data, ~felm(fIV_A9, data = .) )

# Tidy and collect results
tidy_model <- function(model, name) {
  
  tidy <- map(model, tidy, conf.int = TRUE) %>% 
    map( ~filter( . , term %in% c("no_kids", "`no_kids(fit)`") ) ) %>% 
    bind_rows() %>% 
    mutate( 
      term = c("15-19", "20-24", "25-29", "30+"), 
      type = name 
      )
  
  return(tidy)
  
}

mods_list <- list(
  ols_educ = ols_educ, iv_educ = iv_educ, ols_behind = ols_behind,
  iv_behind = iv_behind, ols_private = ols_private,
  iv_private = iv_private, ols_lfp = ols_lfp, iv_lfp = iv_lfp
)

mod_all <- tibble()

for (i in seq_along(mods_list)) {
  
  tidy_data <- tidy_model(mods_list[[i]], names(mods_list)[i])

  mod_all <- bind_rows(mod_all, tidy_data)
  
}

# New Figure

mod_all <- mod_all %>% 
  arrange(term) %>% 
  separate(type, c("model", "var"), "_") %>% 
  mutate( 
    var = factor(var, levels = c("educ", "behind", "private", "lfp")) 
  )

to_string <- as_labeller(
  c(
    "educ" = "Educational Attainment",
    "behind" = "Left Behind", 
    "private" = "Private School", 
    "lfp" = "Mother's LFP"
  )
)

p <- mod_all %>% 
  mutate(model = case_when(
    model == "ols" ~ "OLS", model == "iv" ~ "IV: Twins2"
  ) %>% factor(levels = c("IV: Twins2", "OLS"))
    ) %>% 
  ggplot(aes(term, estimate, color = model)) +
  geom_point(position = position_dodge(width = .25), aes(shape = model)) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high), 
    position = position_dodge(width = .25),
    width = 0
    ) +
  geom_hline(
    aes(yintercept = 0), color = "gray50", 
    size = 1, linetype = "dotted"
    ) +
  theme(legend.position = "top") +
  facet_wrap( ~ var, scale = "free_y", nrow = 2, labeller = to_string) +
  scale_shape_manual(values = c(15, 16)) +
  labs( x = "Mother's Age at First Birth", y = "", color = "", shape = "" )

ggsave(
  filename = "tex/figures/age_mods.pdf",
  plot = p,
  device = cairo_pdf,
  width = 210,
  height = 150,
  units = "mm"
)


# Regression by child's age #### 
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

# Tables of Summary Statistics ####

gt2_descript <- gt2_sample %>% 
  select(
    no_kids, twins_2, same_sex_12, boy_12, girl_12, male = boy,
    child_age_year, child_age_month, child_educ, educ_attain,
    behind, private_school, moth_age_year, moth_age_fstbr,
    moth_pp_group, moth_educ, moth_marital, moth_inlf, fath_inhh
    ) %>% dummy_cols(
      c("moth_pp_group", "moth_educ", "moth_marital")
      ) %>% 
  select(-c(moth_pp_group, moth_educ, moth_marital))

gt3_descript <- gt3_sample %>% 
  filter(!is.na(moth_pp_group)) %>% 
  select(
    no_kids, twins_3, same_sex_123, boy_123, girl_123, male = boy,
    child_age_year, child_age_month, child_educ, educ_attain, behind,
    private_school, moth_age_year, moth_age_fstbr, moth_pp_group,
    moth_educ, moth_marital, moth_inlf, fath_inhh, birth_order
  ) 

gt3_descript_frstbrn <- gt3_descript %>% 
  filter(birth_order == 1) %>% 
  dummy_cols(
    c("moth_pp_group", "moth_educ", "moth_marital")
  ) %>% 
  select(
    -c(moth_pp_group, moth_educ, moth_marital, birth_order)
  )

gt3_descript_both <- gt3_descript %>%
  dummy_cols(
  c("moth_pp_group", "moth_educ", "moth_marital")
) %>% 
  select(
    -c(moth_pp_group, moth_educ, moth_marital, birth_order)
    )

sd_rm_bin <- function(x) {
  
  if ( min(x) == 0 && max(x) == 1 ) {
    res <- NA
  } else {
    res <- sd(x)
  }

  res
}

mean_2 <- gt2_descript %>% 
  map_dbl(mean) %>% 
  round(3)

sd_2 <- gt2_descript %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

mean_3_frstbrn <- gt3_descript_frstbrn %>% 
  map_dbl(mean) %>% 
  round(3)

sd_3_frstbrn <- gt3_descript_frstbrn %>% 
  map_dbl(sd_rm_bin) %>% 
  round(3)

mean_3_both <- gt3_descript_both %>% 
  map_dbl(mean) %>% 
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

z <- z %>% 
  mutate(
    variable = str_remove(
      variable, "moth_pp_group_|moth_educ_|moth_marital_"
    ),
    variable = str_replace_all(
      variable,
      c(
        "no_kids" = "Number of Children",
        "twins_2" = "Twins2",
        "same_sex_12" = "SameSex12",
        "boy_12" = "Boy12",
        "girl_12" = "Girl12",
        "male" = "Male",
        "child_age_year" = "Age (Years)",
        "child_age_month" = "Age (Months)",
        "child_educ" = "Grade Achieved",
        "educ_attain" = "Educational Attainment",
        "behind" = "Left Behind",
        "private_school" = "Private School",
        "moth_age_year" = "Mother's Age (Years)",
        "moth_age_fstbr" = "Age at First Birth",
        "moth_inlf" = "Mother in the Labour Force",
        "fath_inhh" = "Father in Household",
        "Some primary" = "Some Primary",
        "Completed primary" = "Completed Primary",
        "Some secondary" = "Some Secondary",
        "Living together" = "Living Together",
        "Never married" = "Never Married",
        "Widower/widow" = "Widower",
        "twins_3" = "Twins3",
        "same_sex_123" = "SameSex123",
        "boy_123" = "Boy123",
        "girl_123" = "Girl123"
        )
    )
    )  
  
cols <- pull(z, "variable")
cbind(cols) # to see in a vertical list

cols_arrg <- c(
  "Number of Children", "Male", "Age (Years)", "Age (Months)", "Twins2" ,"SameSex12",                 
  "Boy12", "Girl12", "Twins3", "SameSex123", "Boy123", "Girl123", "Grade Achieved",
  "Educational Attainment", "Left Behind", "Private School", "Mother's Age (Years)",
  "Age at First Birth", "Mother in the Labour Force", "Father in Household",
  "Black African", "Coloured", "White", "Indian or Asian", "Other", 
  "No schooling", "Some Primary", "Completed Primary", "Some Secondary",
  "Grade 12/Std 10", "Higher",  
  "Never Married", "Married", "Living Together", "Separated", "Divorced", "Widower"
)

z$variable <- factor(z$variable, levels = cols_arrg)
z <- z[order(z$variable), ]
row.names(z) <- NULL  # reset the rownames

# 
# names <- c("Number of Kids", "Male", "Age in years", "Age in months")


xtab <- xtable(
  z, display = c("s", "s", "s", "g", "g", "s", "g", "g", "s", "g", "g"),
  digits = 4, caption = "Summary Statistics", label = "tab:01"
  ) 

# align(xtab) <- "rllccp{0.5cm}cc" {rllrrlrrlrr}
# digits(xtab) <- 3

nobs_descr_wrap <- function(chr, n = 2, a = "c") {
  
  n <- as.character(n)
  chr <- as.character(chr)
  
  paste0( "\\multicolumn{", n, "}{", a, "}{", chr, "}" )
  
}

obs_descr <- c(
  nobs_descr_wrap( comma( nrow(gt2_descript) ) ), 
  nobs_descr_wrap( comma( nrow(gt3_descript_frstbrn) ) ), 
  nobs_descr_wrap( comma( nrow(gt3_descript_both) ) )
)

obs_descr <- paste( paste(obs_descr, collapse = " & & "), "\\\\" )
obs_descr <- paste( nobs_descr_wrap( " $ N $ ", a = "l" ), " & ", 
                    obs_descr )

comm <- paste0(" \n \\\\[-1.8ex] \\multicolumn{10}{l}",
               "{\\scriptsize{The standard deviations for proportions is 
               not presented.}} \n")


addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 0, 0, 0, 0, 20, 25, 31, 37, 37, 37, 37)
addtorow$command <- c(
  " \\\\[-1.8ex]",
  " & & \\multicolumn{2}{c}{2+ Sample} & & \\multicolumn{5}{c}{3+ Sample}  \\\\[0.2ex]",
  "\\cline{3-4}  \\cline{6-10}  \\\\[-1.2ex]" ,
  " & & & & & & & & \\multicolumn{2}{c}{First and} \\\\",
  " & & \\multicolumn{2}{c}{Firstborns} & & \\multicolumn{2}{c}{Firstborns} & & \\multicolumn{2}{c}{Second Borns} \\\\",
  "\\cline{3-4}  \\cline{6-7} \\cline{9-10}  \\\\[-1.2ex]" ,
  " & & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} & & \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} & & 
  \\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{sd} \\\\",
  " \\\\[-1.8ex] &  & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} &  & 
  \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} &  & 
  \\multicolumn{1}{c}{(5)} & \\multicolumn{1}{c}{(6)}  \\\\ ",
  "\\multicolumn{2}{l}{Mother's Population Group} &  &  &  & & & & \\\\",
  "\\multicolumn{2}{l}{Mother's Level of Education} &  &  &  & & & & \\\\",
  "\\multicolumn{2}{l}{Mother's Marital Status} &  &  &  & & & & \\\\",
  " \\\\[-1.8ex] \\hline \\\\[-1.8ex] ",
  obs_descr,
  " \\bottomrule ",
  comm
  )


print(
  xtab, add.to.row = addtorow,
  include.rownames = FALSE, include.colnames = FALSE, 
  booktabs = TRUE, caption.placement = "top", hline.after = c(-1, 0),
  file = "D:/MSc_ED/Thesis/SA_2011_Census/outline/tables/table10.tex" 
  )


# Line Graph ####

uni_mults <- read_csv("data/uni_mults.csv")

line_gr <- uni_mults %>% 
  mutate(pop_group = factor( pop_group,
    levels = c("White", "Black African", "Coloured, Indian or Asian, and Other")
    )) %>%
  ggplot(aes(year, prop,linetype = pop_group, color = pop_group)) +
  geom_line(size = 0.5) +
  # scale_colour_brewer(palette = "Set1") +
  # theme_bw() +
  theme(legend.position = "top") +
  labs(
    # title = "Multiple Births Per 1000 Live Births",
    x = "Year", 
    y = "Multiple Births Per 1000 Live Births",
    color = "",
    linetype = ""
  )

ggsave(
  filename = "tex/figures/line_pp.pdf",
  plot = line_gr,
  device = cairo_pdf,
  width = 160,
  height = 130,
  units = "mm"
)



# Histogram of Educ. Attainment Index ####

hist2 <- gt2_sample %>% 
  mutate(samp = "2+ Sample") %>% 
  # filter(educ_attain <= 1.75) %>%
  ggplot(aes(x = educ_attain)) +
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept = mean(educ_attain)),
             color="red", linetype="dashed", size=1) +
  facet_wrap(~ samp) +
  theme_bw() +
  labs(x = "", y = "")

hist3 <- gt3_sample %>% 
  mutate(samp = "3+ Sample") %>% 
  # filter(educ_attain <= 1.75) %>%
  ggplot(aes(x = educ_attain)) +
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept = mean(educ_attain)),
             color="red", linetype="dashed", size=1) +
  facet_wrap(~ samp) +
  theme_bw() +
  labs(x = "", y = "")

figure <- ggarrange(hist2, hist3, ncol = 2) 
  
figure_annon <- annotate_figure( 
  figure, 
  # bottom = "Educational Attainment Index",
  left = "Count"
  )

ggsave(
  filename = "tex/figures/hists.pdf",
  plot = figure_annon,
  device = cairo_pdf,
  width = 220,
  height = 90,
  units = "mm"
)


# Comparison of twins birth by pp. group and educ. level ####

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
gt2_sample %>%
  filter(moth_age_fstbr %>% between(14, 35)) %>%
  ggplot(aes(moth_age_fstbr, no_kids)) +
  geom_smooth()

gt2_sample %>% count(spacing)
  ggplot(aes(spacing, no_kids)) +
  geom_smooth()

gt2_sample %>%
  filter(moth_age_fstbr %>% between(14, 38)) %>%
  group_by(moth_age_fstbr) %>% 
  summarise(prop_twins = mean(twins_2)) %>% 
  ggplot(aes(moth_age_fstbr, prop_twins)) +
  geom_col() +
  scale_y_continuous(label = percent)


gt2_sample %>%
  filter(moth_educ != "Other") %>% 
  group_by(moth_educ) %>%
  summarize(avg_kids = mean(no_kids)) %>%
  mutate(
    moth_educ = fct_reorder(moth_educ, avg_kids) 
  ) %>% 
  ggplot(aes(moth_educ, avg_kids)) +
  geom_col() +
  coord_flip()

gt2_sample %>%
  filter(moth_educ != "Other") %>% 
  # mutate(
  #   moth_educ = fct_reorder(moth_educ, avg_kids) 
  # ) %>% 
  ggplot(aes(moth_educ, no_kids)) +
  geom_boxplot() 

gt2_sample %>% 
  # filter(moth_pp_group != "Other") %>%
  group_by(moth_pp_group) %>%
  summarize(avg_kids = mean(no_kids)) %>%
  ggplot(aes(moth_pp_group, avg_kids)) +
  geom_col() 


gt2_sample %>% 
  ggplot(aes(x = educ_attain)) +
  geom_boxplot()


# Statistically significant d/ce in mean in the 2+ sample
t.test(moth_age_fstbr ~ twins_2, data = gt2_sample) %>% tidy()

gt3_sample %>% 
  group_by(twins_3) %>% 
  summarise(
    n = n(),
    mean_age_frstbr = mean(moth_age_fstbr_gen),
    sd_age_frstbr = sd(moth_age_fstbr_gen)        
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














