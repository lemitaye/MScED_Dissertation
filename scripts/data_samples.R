
# Created on: February 22, 2022
# DEPENDS ON: data_extract.R
# Outputs:
#   gt2_sample.csv
#   gt3_sample.csv

# This script loads the data produced by data_extract.R, processes it,
# and produces cleaned data ready for analysis.

rm(list = ls())

# Data Preparation ####

# Load saved data
data <- read_csv("data/kids_data.csv")

# convert all character columns into factors
# data <- data %>%
#   mutate_if(is.character, as.factor) %>%
#   mutate(
#     district = factor(district), 
#     municip = factor(municip),
#     moth_no = factor(moth_no)
#     )


# Get the raw 2+ and 3+ samples
gt2_sample0 <- data %>%
  filter(firstborn_age %>% between(6, 18)) %>%
  group_by(moth_no) %>%
  arrange(child_dob, .by_group = TRUE) %>%
  mutate(
    birth_order = row_number(child_dob),
    no_kids = n()
  ) %>%
  ungroup() %>%
  filter(no_kids >= 2)

gt3_sample0 <- gt2_sample0 %>%
  filter(no_kids >= 3) %>%
  filter(secondborn_age %>% between(6, 18))


# This function enables us to get data frames with all the instruments
get_parity <- function(tbl, n) {
  tbl <- tbl %>%
    select(moth_no, child_dob, birth_order, no_kids, child_sex) %>%
    # extend the vector up to 4 to get the 3+ sample
    filter(birth_order %in% 1:n) %>%
    pivot_wider(
      id_cols = moth_no,
      names_from = birth_order,
      values_from = c(child_sex, child_dob)
    ) %>%
    mutate(
      boy_1 = case_when(
        child_sex_1 == "Male" ~ 1,
        TRUE ~ 0
      ),
      boy_2 = case_when(
        child_sex_2 == "Male" ~ 1,
        TRUE ~ 0
      ),
      boy_12 = case_when(
        (child_sex_1 == "Male" & child_sex_2 == "Male") ~ 1,
        TRUE ~ 0
      ),
      girl_12 = case_when(
        (child_sex_1 == "Female" & child_sex_2 == "Female") ~ 1,
        TRUE ~ 0
      ),
      same_sex_12 = boy_12 + girl_12,
      twins_1 = case_when(
        (child_dob_1 == child_dob_2) ~ 1,
        TRUE ~ 0
      ),
      twins_2 = case_when(
        (child_dob_2 == child_dob_3) ~ 1,
        TRUE ~ 0
      )
    )

  # The following is relevant for the 3+ sample:
  if (n == 4) {
    tbl <- tbl %>%
      mutate(
        boy_3 = case_when(
          child_sex_3 == "Male" ~ 1,
          TRUE ~ 0
        ),
        boy_123 = case_when(
          (child_sex_1 == "Male" &
            child_sex_2 == "Male" &
            child_sex_3 == "Male") ~ 1,
          TRUE ~ 0
        ),
        girl_123 = case_when(
          (child_sex_1 == "Female" &
            child_sex_2 == "Female" &
            child_sex_3 == "Female") ~ 1,
          TRUE ~ 0
        ),
        same_sex_123 = boy_123 + girl_123,
        twins_3 = case_when(
          (child_dob_3 == child_dob_4) ~ 1,
          TRUE ~ 0
        )
      )
  }

  tbl <- tbl %>%
    select(-contains(c("child_sex", "child_dob")))

  return(tbl)
}


# get all the instruments we need for the respective sample
parity_gt2 <- get_parity(gt2_sample0, n = 3) # for the 2+ sample
parity_gt3 <- get_parity(gt3_sample0, n = 4) # for the 3+ sample


# Join with parity data frames and do some cleaning:
gt2_sample <- gt2_sample0 %>%
  left_join(parity_gt2, by = "moth_no") %>%
  filter(birth_order == 1) %>%
  # Filter out twins at first birth (and unrealistic obs.)
  filter(
    !(twins_1 == 1), no_kids < 10
    # , fath_age_year < 82
  ) %>%
  mutate(boy = case_when(
    child_sex == "Male" ~ 1,
    child_sex == "Female" ~ 0
  )) %>%
  select(
    child_no:child_sex, boy, birth_order:twins_2, no_kids,
    everything(), -(firstborn_dob:secondborn_age), -fath_pnr
  ) %>%
  filter(
    !is.na(moth_dob)
    # , !is.na(fath_dob)
  )


gt3_sample <- gt3_sample0 %>%
  left_join(parity_gt3, by = "moth_no") %>%
  # Filter out twins at first and second birth (and unrealistic obs.)
  filter(
    birth_order %in% c(1, 2),
    !(twins_1 == 1 | twins_2 == 1),
    no_kids < 10
  ) %>%
  mutate(boy = case_when(
    child_sex == "Male" ~ 1,
    child_sex == "Female" ~ 0
  )) %>%
  select(
    child_no:child_sex, boy, birth_order:twins_2, twins_3, no_kids,
    everything(), -(firstborn_dob:secondborn_age)
  )


# Construct Variables ####

gen_private <- function(tbl) {
  tbl <- tbl %>%
    filter(
      child_private %in% c("Private", "Public (government)")
    ) %>%
    mutate(
      private_school = case_when(
        child_private == "Private" ~ 1, TRUE ~ 0
      ))
  
  return(tbl)
}

get_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

gen_educ_attains <- function(tbl) {
  tbl <- tbl %>%
    filter(child_educ %in% 0:12 | child_educ == 98) %>%
    mutate(
      child_educ_gen = case_when(
        as.numeric(child_educ) == 98 ~ 1,
        TRUE ~ as.numeric(child_educ) + 2
      ),
      birth_month = month(child_dob)
    ) %>%
    # grouping by age in years, month of birth and province 
    group_by(child_age_year, birth_month, province) %>%
    mutate(
      mean_educ_agg = mean(child_educ_gen),
      mode_educ_agg = get_mode(child_educ_gen)
    ) %>%
    ungroup() %>%
    mutate(
      educ_attain = child_educ_gen / mean_educ_agg,
      behind = (child_educ_gen < mode_educ_agg)
    )
  
  return(tbl)
}

gt2_sample <- gen_private(gt2_sample)
gt3_sample <- gen_private(gt3_sample)

gt2_sample <- gen_educ_attains(gt2_sample)
gt3_sample <- gen_educ_attains(gt3_sample)



# save data
write_csv(gt2_sample, file = "data/gt2_sample.csv")
write_csv(gt3_sample, file = "data/gt3_sample.csv")

# Next:
# +3 sample
# alternative measure for educ: left behind
# heterogeneity analysis (>= 10 yrs. old)
