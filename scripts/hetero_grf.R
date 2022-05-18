
# Date Created: April 7, 2022

# A script to train and fit a "Generalized Random Forest" 
# from Athey et al. (2019) to carry out heterogeneity analysis

# Load themes for graphing:
source("scripts/ggplot_theme_Publication-2.R")  

# Load data:
gt2_sample <- read_csv("data/gt2_sample.csv")


# Build the necessary components: 
X <- gt2_sample %>% 
  select(-c(
    no_kids, child_no, moth_no, child_dob, boy, birth_order, boy_1, boy_2, 
    twins_1, child_age_year, child_sch_attend, municip, moth_age_month, 
    moth_dob, moth_ceb, moth_age_scndbr, child_educ_gen, mean_educ_agg, 
    mode_educ_agg, twins_2, district, boy_12, girl_12, same_sex_12,
    private_school, birth_month, educ_attain, behind, child_pop_group,
    child_private, child_educ, province, spacing, moth_inlf
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
    c("child_sex", "moth_pp_group", "moth_educ", "moth_marital",
       "moth_employ", "moth_income")
  ) %>% 
  select(
    -c(child_sex, moth_pp_group, moth_educ, moth_marital, 
       moth_employ, moth_income)
  )


W <- gt2_sample$no_kids
Z1 <- gt2_sample$twins_2
Z2 <- gt2_sample$same_sex_12

# Outcomes
Y1 <- gt2_sample$educ_attain
Y2 <- gt2_sample$behind
Y3 <- gt2_sample$private_school


# Train Instrumental Forests ####
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

start.time <- proc.time()

# # Train instrumental forest using twins instrument (Z1)
# (Tuning with the default params. is the best option)

# tau.educ.twins <- instrumental_forest(X, Y1, W, Z1, num.trees = 5000)
# tau.behind.twins <- instrumental_forest(X, Y2, W, Z1, num.trees = 5000)
# tau.private.twins <- instrumental_forest(X, Y3, W, Z1, num.trees = 5000)


# Train instrumental forest using same sex instrument (Z2)
# (Supplied params. obtained from tuning)

tau.educ.samesx <- instrumental_forest(
  X, Y1, W, Z2, num.trees = 5000, 
  sample.fraction = 0.253441507730167,
  mtry = 7,
  min.node.size = 53,
  honesty.fraction = 0.536909526004456,
  honesty.prune.leaves = 1,
  alpha = 0.206129047670402,
  imbalance.penalty = 0.548535325594362
  )

tau.behind.samesx <- instrumental_forest(
  X, Y2, W, Z2, num.trees = 5000, 
  sample.fraction = 0.0807964854175225,
  mtry = 8,
  min.node.size = 71,
  honesty.fraction = 0.615930338646285,
  honesty.prune.leaves = 0,
  alpha = 0.0728798793861642,
  imbalance.penalty = 1.26205245650895
  )

tau.private.samesx <- instrumental_forest(
  X, Y3, W, Z2, num.trees = 5000, 
  sample.fraction = 0.0987349107163027,
  mtry = 9,
  min.node.size = 83,
  honesty.fraction = 0.623782520485111,
  honesty.prune.leaves = 0,
  alpha = 0.0982620577560738,
  imbalance.penalty = 3.12435664022772
  )

stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)

# Save (expensive) results:
saveRDS(tau.educ.twins, file = "data/tau.educ.twins.RData")
saveRDS(tau.behind.twins, file = "data/tau.behind.twins.RData")
saveRDS(tau.private.twins, file = "data/tau.private.twins.RData")

saveRDS(tau.educ.samesx, file = "data/tau.educ.samesx.RData")
saveRDS(tau.behind.samesx, file = "data/tau.behind.samesx.RData")
saveRDS(tau.private.samesx, file = "data/tau.private.samesx.RData")

# Read saved results
tau.educ.twins <- readRDS("data/tau.educ.twins.RData")
tau.behind.twins <- readRDS("data/tau.behind.twins.RData")
tau.private.twins <- readRDS("data/tau.private.twins.RData")

tau.educ.samesx <- readRDS("data/tau.educ.samesx.RData")
tau.behind.samesx <- readRDS("data/tau.behind.samesx.RData")
tau.private.samesx <- readRDS("data/tau.private.samesx.RData")



# Prepare prediction data: ####

# Vary Mother's population group and Mother's age at first birth
comb.1 <- expand.grid(
  moth_age_fstbr = 16:34, 
  moth_pp_group = c(
    "Black African", "White", "Coloured, Indian or Asian, and Other"
    )) %>% 
  dummy_cols("moth_pp_group")

# Vary Mother's population group and mother's level of education
moth_educ <- gt2_sample %>% count(moth_educ) %>% pull(moth_educ)

comb.2 <- expand.grid(
  moth_educ = moth_educ, 
  moth_pp_group = c(
    "Black African", "White", "Coloured, Indian or Asian, and Other"
  )) %>% 
  dummy_cols(c("moth_educ", "moth_pp_group"))

# compute the median value for all predictors:
median <- map_dbl(X, median)

X.pred.1 <- matrix(nrow = nrow(comb.1), ncol = ncol(X)) 
colnames(X.pred.1) <- names(median)

X.pred.2 <- matrix(nrow = nrow(comb.2), ncol = ncol(X)) 
colnames(X.pred.2) <- names(median)

# Fill all rows with median values
for (i in 1:nrow(comb.1)) {
  X.pred.1[i, ] <- median
}

for (i in 1:nrow(comb.2)) {
  X.pred.2[i, ] <- median
}

# Data with 1st dimenstion of hetero. (Moth pp group & Age at first birth)
X.pred.1 <- as_tibble(X.pred.1) %>% 
  mutate(
    moth_age_fstbr = comb.1$moth_age_fstbr,
    `moth_pp_group_Black African` = comb.1$`moth_pp_group_Black African`,
    moth_pp_group_White = comb.1$moth_pp_group_White,
    `moth_pp_group_Coloured, Indian or Asian, and Other` = 
      comb.1$`moth_pp_group_Coloured, Indian or Asian, and Other`
  )

# Data with second dimension of hetero. (Moth pp group & Moth educ.)
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


# Start prediction: ####

# Predict along the first dimensions of hetero. (i.e., X.pred.1) using twins instr.
pred.educ.twins.1 <- predict(tau.educ.twins, X.pred.1, estimate.variance = TRUE)
pred.behind.twins.1 <- predict(tau.behind.twins, X.pred.1, estimate.variance = TRUE)
pred.private.twins.1 <- predict(tau.private.twins, X.pred.1, estimate.variance = TRUE)

pred.educ.twins.1 <- pred.educ.twins.1 %>% 
  rename(pred_educ = "predictions", var_educ = "variance.estimates")

pred.behind.twins.1 <- pred.behind.twins.1 %>% 
  rename(pred_behind = "predictions", var_behind = "variance.estimates")

pred.private.twins.1 <- pred.private.twins.1 %>% 
  rename(pred_private = "predictions", var_private = "variance.estimates")

# Predict along the first dimensions of hetero. (i.e., X.pred.1) using same sex instr.
pred.educ.samesx.1 <- predict(tau.educ.samesx, X.pred.1, estimate.variance = TRUE)
pred.behind.samesx.1 <- predict(tau.behind.samesx, X.pred.1, estimate.variance = TRUE)
pred.private.samesx.1 <- predict(tau.private.samesx, X.pred.1, estimate.variance = TRUE)

pred.educ.samesx.1 <- pred.educ.samesx.1 %>% 
  rename(pred_educ = "predictions", var_educ = "variance.estimates")

pred.behind.samesx.1 <- pred.behind.samesx.1 %>% 
  rename(pred_behind = "predictions", var_behind = "variance.estimates")

pred.private.samesx.1 <- pred.private.samesx.1 %>% 
  rename(pred_private = "predictions", var_private = "variance.estimates")

# Function to construct a data frame of predictions
final_res_1 <- function(first, second, third) {
  
  final <- X.pred.1 %>% 
    as_tibble() %>% 
    mutate(moth_pp_group = comb.1$moth_pp_group) %>% 
    select(
      moth_age_fstbr, contains("moth_pp_group"), 
      -c(`moth_pp_group_Black African`:moth_pp_group_White)
    ) %>% 
    bind_cols(first, second, third) %>% 
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
  
  return(final)
}

final.twins.1 <- final_res_1(
  pred.educ.twins.1, pred.behind.twins.1, pred.private.twins.1
  )

final.samesx.1 <- final_res_1(
  pred.educ.samesx.1, pred.behind.samesx.1, pred.private.samesx.1
  )


# Predict along the second dimensions of hetero. (i.e., X.pred.2) using twins instr.
pred.educ.twins.2 <- predict(tau.educ.twins, X.pred.2, estimate.variance = TRUE)
pred.behind.twins.2 <- predict(tau.behind.twins, X.pred.2, estimate.variance = TRUE)
pred.private.twins.2 <- predict(tau.private.twins, X.pred.2, estimate.variance = TRUE)

pred.educ.twins.2 <- pred.educ.twins.2 %>% 
  rename(pred_educ = "predictions", var_educ = "variance.estimates")

pred.behind.twins.2 <- pred.behind.twins.2 %>% 
  rename(pred_behind = "predictions", var_behind = "variance.estimates")

pred.private.twins.2 <- pred.private.twins.2 %>% 
  rename(pred_private = "predictions", var_private = "variance.estimates")


# Predict along the second dimensions of hetero. (i.e., X.pred.2) using twins instr.
pred.educ.samesx.2 <- predict(tau.educ.samesx, X.pred.2, estimate.variance = TRUE)
pred.behind.samesx.2 <- predict(tau.behind.samesx, X.pred.2, estimate.variance = TRUE)
pred.private.samesx.2 <- predict(tau.private.samesx, X.pred.2, estimate.variance = TRUE)

pred.educ.samesx.2 <- pred.educ.samesx.2 %>% 
  rename(pred_educ = "predictions", var_educ = "variance.estimates")

pred.behind.samesx.2 <- pred.behind.samesx.2 %>% 
  rename(pred_behind = "predictions", var_behind = "variance.estimates")

pred.private.samesx.2 <- pred.private.samesx.2 %>% 
  rename(pred_private = "predictions", var_private = "variance.estimates")


# A similar function:
final_res_2 <- function(first, second, third) {
  
  final <- X.pred.2 %>% 
    as_tibble() %>% 
    mutate(moth_pp_group = comb.2$moth_pp_group, moth_educ = comb.2$moth_educ) %>% 
    select(contains("moth_educ"), contains("moth_pp_group")) %>% 
    select(
      -c(`moth_educ_Completed primary`:`moth_educ_Some secondary`),
      -c(`moth_pp_group_Black African`:moth_pp_group_White)
    ) %>% 
    bind_cols(first, second, third) %>% 
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
  
  return(final)
}

final.twins.2 <- final_res_2(
  pred.educ.twins.2, pred.behind.twins.2, pred.private.twins.2
)

final.samesx.2 <- final_res_2(
  pred.educ.samesx.2, pred.behind.samesx.2, pred.private.samesx.2
)


# Save dataframes containing predictions

write_csv(final.twins.1, file = "data/final.twins.1.csv")
write_csv(final.samesx.1, file = "data/final.samesx.1.csv")

write_csv(final.twins.2, file = "data/final.twins.2.csv")
write_csv(final.samesx.2, file = "data/final.samesx.2.csv")


# Read saved data ####
final.twins.1 <- read_csv("data/final.twins.1.csv")
final.samesx.1 <- read_csv("data/final.samesx.1.csv")

final.twins.2 <- read_csv("data/final.twins.2.csv")
final.samesx.2 <- read_csv("data/final.samesx.2.csv")

# Prepare the data
  


# Function to plot from the data frames above

to_string <- as_labeller(
  c(
    "educ" = "Education Attainment",
    "behind" = "Left Behind", 
    "private" = "Private School",
    "Black African" = "Black African",
    "Coloured, Indian or Asian, and Other" = "Coloured, Indian/\nAsian, & Other",
    "White" = "White"
  )
)

plot_1 <- function( tbl ) {
  
  p <- tbl %>% 
    mutate(
      moth_pp_group = factor(
        moth_pp_group,
        levels = c("Black African", "White", "Coloured, Indian or Asian, and Other")
      ),
      outcome = factor(outcome,
                       levels = c("educ", "behind", "private"))
    ) %>% 
    ggplot(aes(moth_age_fstbr, pred)) +
    geom_line(aes(group = 1), color = "darkblue", alpha = .8) +
    geom_line(aes(y = upper, group = 1), linetype = "dashed", color = "darkblue", alpha=0.5) +
    geom_line(aes(y = lower, group = 1), linetype = "dashed", color = "darkblue", alpha=0.5) +
    geom_hline(
      aes(yintercept = 0), color = "gray50", size = 1, linetype = "dotted"
    ) + 
    facet_grid(moth_pp_group ~ outcome, scales = "free_y", 
               labeller = to_string) +
    labs(
      x = "Mother's Age at First Birth", y = ""
    ) +
    scale_colour_Publication() + 
    theme_Publication() +
    theme(
      axis.title=element_text(size=12)
    )
  
  return(p)
}

plot_2 <- function( tbl ) {
  p <- tbl %>% 
    mutate(
      moth_pp_group = factor(
        moth_pp_group,
        levels = c("Black African", "White", "Coloured, Indian or Asian, and Other")
      ) %>% fct_recode(
        "Coloured, Indian/Asian, & Other" = "Coloured, Indian or Asian, and Other") %>% fct_rev(),
      moth_educ = factor(
        moth_educ,
        levels = c("No schooling", "Some primary", "Completed primary", 
                   "Some secondary", "Grade 12/Std 10", "Higher")
      ),
      outcome = factor(outcome,
                       levels = c("educ", "behind", "private"))
    ) %>% 
    ggplot(aes(pred, moth_educ, color = moth_pp_group, shape = moth_pp_group )) +
    geom_point(size = 1.75, position = position_dodge(width = 0.75) ) +
    geom_errorbar(aes(xmin = lower, xmax = upper), 
                  width = .2, position = position_dodge(width = 0.75)) +
    geom_vline(
      aes(xintercept = 0), color = "gray50", size = 1, linetype = "dotted"
    ) +
    scale_y_discrete(expand=c(.1, 0)) +
    facet_wrap(~outcome, labeller = to_string) +
    guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE) ) +
    labs(
      x = "", y = "Mother's Level of Education", color = "", shape = ""
    ) +
    # borrowed the following colours from "scale_colour_Publication" function:
    scale_color_manual( values = c("#7fc97f", "#f87f01", "#386cb0")  ) + 
    theme_Publication() +
    theme(
      legend.position = "top",
      legend.margin = margin(t = -0.5, unit='cm'),
      axis.title=element_text(size=12),
      plot.margin=unit(c(1, 1, -0.5, 0.5), units="line")  # top, right, bottom, & left
      )
  
  return(p)
}

# plots
a.t <- plot_1(final.twins.1)
b.t <- plot_2(final.twins.2)

a.s <- plot_1(final.samesx.1)
b.s <- plot_2(final.samesx.2)

fig5 <- ggarrange(
  a.t, NULL, b.t, 
  labels = c("A.", "", "B."),
  nrow = 3, heights = c(1, 0.05, 0.85)
) 

fig6 <- ggarrange(
  a.s, NULL, b.s, 
  labels = c("A.", "", "B."),
  nrow = 3, heights = c(1, 0.05, 0.85)
) 

ggsave(
  filename = "tex/figures/heter1.pdf",
  plot = fig5,
  device = cairo_pdf,
  width = 200,
  height = 256.5,
  units = "mm"
)

ggsave(
  filename = "tex/figures/heter2.pdf",
  plot = fig6,
  device = cairo_pdf,
  width = 200,
  height = 256.5,
  units = "mm"
)

# Making a table for tuning values

tuning.samesx <- data.frame(
  
educ = c(
  sample.fraction = 0.253441507730167,
  mtry = 7,
  min.node.size = 53,
  alpha = 0.206129047670402,
  imbalance.penalty = 0.548535325594362
),

behind = c(
  sample.fraction = 0.0807964854175225,
  mtry = 8,
  min.node.size = 71,
  alpha = 0.0728798793861642,
  imbalance.penalty = 1.26205245650895
),

private = c(
  sample.fraction = 0.0987349107163027,
  mtry = 9,
  min.node.size = 83,
  alpha = 0.0982620577560738,
  imbalance.penalty = 3.12435664022772
)

)

tuning.samesx <- t(tuning.samesx)  # transpose to save space
rownames(tuning.samesx) <- c(
  "Education Attainment",
  "Left Behind", 
  "Private School"
)

print(
  xtable(tuning.samesx, display = c("s", "g", "g", "g", "g", "g"),
         digits = 4), 
  floating = FALSE,
  file = "tex/tables/tuning-samesx.tex"
  )


