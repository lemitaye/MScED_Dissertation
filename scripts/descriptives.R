
# Date created: March 01, 2022
# DEPENDS ON: data_samples.R

# Descriptive statistics and graphics

rm(list = ls())

# Load themes for pulication ready plots:
source("scripts/ggplot_theme_Publication-2.R")  

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

# create a list of tibbles by age:
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
  facet_wrap( ~ var, scale = "free_y", nrow = 2, labeller = to_string) +
  scale_shape_manual(values = c(15, 16)) +
  labs( x = "Mother's Age at First Birth", y = "", color = "", shape = "" ) +
  scale_colour_Publication() + 
  theme_Publication() +
  theme(
    legend.position = "top",
    legend.margin=margin(t = -0.5, unit='cm'),
    axis.title=element_text(size=13),
    plot.margin=unit(c(1, 1, 0.5, 1), units="line")  # top, right, bottom, & left
    )

ggsave(
  filename = "tex/figures/age_mods.pdf",
  plot = p,
  device = cairo_pdf,
  width = 210,
  height = 140,
  units = "mm"
)


# Table of Summary Statistics ####

gt2_descript <- gt2_sample %>% 
  select(
    no_kids, twins_2, same_sex_12, boy_12, girl_12, male = boy,
    child_age_year, child_age_month, child_educ, educ_attain,
    behind, private_school, moth_age_year, moth_age_fstbr,
    moth_pp_group, moth_educ, moth_inlf, fath_inhh
    ) %>% dummy_cols(
      c("moth_pp_group", "moth_educ")
      ) %>% 
  select(-c(moth_pp_group, moth_educ))

gt3_descript <- gt3_sample %>% 
  filter(!is.na(moth_pp_group)) %>% 
  select(
    no_kids, twins_3, same_sex_123, boy_123, girl_123, male = boy,
    child_age_year, child_age_month, child_educ, educ_attain, behind,
    private_school, moth_age_year, moth_age_fstbr, moth_pp_group,
    moth_educ, moth_inlf, fath_inhh, birth_order
  ) 

gt3_descript_frstbrn <- gt3_descript %>% 
  filter(birth_order == 1) %>% 
  dummy_cols(
    c("moth_pp_group", "moth_educ")
  ) %>% 
  select(
    -c(moth_pp_group, moth_educ, birth_order)
  )

gt3_descript_both <- gt3_descript %>%
  dummy_cols(
  c("moth_pp_group", "moth_educ")
) %>% 
  select(
    -c(moth_pp_group, moth_educ, birth_order)
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
z <- select(z, variable, empty0, mean_2, sd_2, empty1, mean_3_frstbrn,
            sd_3_frstbrn, empty2, mean_3_both, sd_3_both)

z <- z %>% 
  mutate(
    variable = str_remove(
      variable, "moth_pp_group_|moth_educ_"
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
  "Grade 12/Std 10", "Higher"
)

z$variable <- factor(z$variable, levels = cols_arrg)
z <- z[order(z$variable), ]
row.names(z) <- NULL  # reset the rownames
z$variable <- as.character( z$variable )
z$variable[21:31] <- str_c("\\phantom{M}", as.character( z$variable[21:31] )) 


# Start building table
xtab <- xtable(
  z, display = c("s", "s", "s", "g", "g", "s", "g", "g", "s", "g", "g"),
  digits = 4, caption = "Summary Statistics", label = "tab:sum-stat"
  ) 

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
               "{\\footnotesize{\\textit{Note:} The standard deviations for proportions is 
               not presented.}} \n")


addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 0, 0, 0, 0, 20, 25, 31, 31, 31, 31)
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
  " \\\\[-1.8ex] \\hline \\\\[-1.8ex] ",
  obs_descr,
  " \\bottomrule ",
  comm
  )


print(
  xtab, add.to.row = addtorow,
  include.rownames = FALSE, include.colnames = FALSE, 
  booktabs = TRUE, caption.placement = "top", hline.after = c(-1, 0),
  sanitize.text.function = identity,
  table.placement = "t!",
  file = "tex/tables/sum-stat.tex"
  )


# Line Graph ####

uni_mults <- read_csv("data/uni_mults.csv")

# lines only:
line_gr <- uni_mults %>% 
  mutate(pop_group = factor( pop_group,
    levels = c("White", "Black African", "Coloured, Indian or Asian, and Other")
    ) %>% fct_recode(
      "Coloured, Indian/Asian, & Other" = "Coloured, Indian or Asian, and Other") ) %>%
  ggplot(aes(year, prop,linetype = pop_group, color = pop_group)) +
  geom_line(size = 1) +
  labs(
    # title = "Multiple Births Per 1000 Live Births",
    x = "Year", 
    y = "Multiple Births Per 1000 Live Births",
    color = "",
    linetype = ""
  ) +
  scale_colour_Publication() + 
  theme_Publication() +
  theme(legend.position = "top",
        legend.margin=margin(t = -0.5, unit='cm'),
        axis.title=element_text(size = 12.5),
        plot.margin=unit(c(1, 1, 0.5, 1), units="line")  # top, right, bottom, & left
        )

# lines + CI ribbons:
line_rib <- uni_mults %>% 
  mutate(
    pop_group = factor( pop_group,
                        levels = c("White", "Black African", "Coloured, Indian or Asian, and Other")
    ) %>% fct_recode(
      "Coloured, Indian/Asian, & Other" = "Coloured, Indian or Asian, and Other"),
    
    # We need the following to draw CI ribbons:
    p = prop/1000,
    score = 1.96*1000*sqrt( p*(1 - p)/all ),
    prop_lower = prop - score,
    prop_upper = prop + score
  ) %>%
  ggplot(aes(year, prop, linetype = pop_group, color = pop_group, fill = pop_group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = prop_lower, ymax = prop_upper), 
              alpha = .2, colour = NA) +
  labs(
    # title = "Multiple Births Per 1000 Live Births",
    x = "Year", 
    y = "Multiple Births Per 1000 Live Births",
    color = "",
    linetype = "",
    fill = ""
  ) +
  scale_colour_Publication() + 
  scale_fill_Publication() +
  theme_Publication() +
  theme(
    legend.position = "top",
    legend.margin = margin(t = -0.4, unit = "cm"),
    axis.title = element_text(size = 12.5),
    plot.margin = unit(c(1, 1, 0.5, 1), units = "line") # top, right, bottom, & left
  )

# Save both:
ggsave(
  filename = "tex/figures/line_pp.pdf",
  plot = line_gr,
  device = cairo_pdf,
  width = 180,
  height = 140,
  units = "mm"
)

ggsave(
  filename = "tex/figures/line_rib.pdf",
  plot = line_rib,
  device = cairo_pdf,
  width = 180,
  height = 140,
  units = "mm"
)

# Histogram of Educ. Attainment Index ####

hists_data <- gt2_sample %>% 
  select(educ_attain) %>% 
  mutate(samp = "2+ Sample") %>% 
  bind_rows(
    gt3_sample %>% 
      select(educ_attain) %>% 
      mutate(samp = "3+ Sample")
  )

hists <- hists_data %>% 
  ggplot(aes(x = educ_attain)) +
  geom_histogram(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept = mean(educ_attain)),
             color="red", linetype="dashed", size=1) +
  facet_wrap(~ samp, scales = "free_y") +
  labs(x = "", y = "Count") +
  theme_Publication() +
  theme(legend.position = "top") +
  theme(
    axis.title=element_text(size=12),
    plot.margin=unit(c(-0.05, 1, -0.5, 1), units="line")  # top, right, bottom, & left
  )

ggsave(
  filename = "tex/figures/hists.pdf",
  plot = hists,
  device = cairo_pdf,
  width = 220,
  height = 80,
  units = "mm"
)


# Comparison of twins birth by pp. group and educ. level ####

# Statistically significant d/ce in mean in the 2+ sample:
t.test(moth_age_fstbr ~ twins_2, data = gt2_sample) %>% tidy()

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
