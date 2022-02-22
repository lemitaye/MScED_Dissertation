# Created on: February 22, 2022
# DEPENDS ON: data_extract.R
# Outputs:
#   gt2_analysis_sample.csv
#   gt3_analysis_sample.csv

# This script loads the data produced by data_extract.R, processes it,
# and produces cleaned data ready for analysis.

rm(list = ls())

# Load saved data 
data <- read_csv("data/kids_data.csv")


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
        child_sex_1 == 1 ~ 1,
        TRUE ~ 0
      ),
      boy_2 = case_when(
        child_sex_2 == 1 ~ 1,
        TRUE ~ 0
      ),
      boy_12 = case_when(
        (child_sex_1 == 1 & child_sex_2 == 1) ~ 1,
        TRUE ~ 0
      ),
      girl_12 = case_when(
        (child_sex_1 == 2 & child_sex_2 == 2) ~ 1,
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
          child_sex_3 == 1 ~ 1,
          TRUE ~ 0
        ),
        boy_123 = case_when(
          (child_sex_1 == 1 & child_sex_2 == 1 & child_sex_3 == 1) ~ 1,
          TRUE ~ 0
        ),
        girl_123 = case_when(
          (child_sex_1 == 2 & child_sex_2 == 2 & child_sex_3 == 2) ~ 1,
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
parity_gt2 <- get_parity(gt2_sample0, n = 3)  # for the 2+ sample
parity_gt3 <- get_parity(gt3_sample0, n = 4)  # for the 3+ sample


# Join with parity data frames and do some cleaning:
gt2_sample <- gt2_sample0 %>%
  left_join(parity_gt2, by = "moth_no") %>%
  filter(birth_order == 1) %>%
  # Filter out twins at first birth (and unrealistic obs.)
  filter(!(twins_1 == 1), no_kids < 10) %>%
  mutate(boy = case_when(child_sex == 1 ~ 1, child_sex == 2 ~ 0)) %>%
  select(
    moth_no:child_sex, boy, birth_order:twins_2, no_kids,
    everything(), -(firstborn_dob:secondborn_age)
  )

gt3_sample <- gt3_sample0 %>%
  left_join(parity_gt3, by = "moth_no") %>%
  # Filter out twins at first and second birth (and unrealistic obs.)
  filter(
    birth_order %in% c(1, 2),
    !(twins_1 == 1 | twins_2 == 1),
    no_kids < 10
  ) %>%
  mutate(boy = case_when(child_sex == 1 ~ 1, child_sex == 2 ~ 0)) %>%
  select(
    moth_no:child_sex, boy, birth_order:twins_2, twins_3, no_kids,
    everything(), -(firstborn_dob:secondborn_age)
  )

gt2_sample %>% 
  summarise_all( ~sum(is.na(.)) )

gt2_sample %>%
  filter(!is.na(moth_dob), !is.na(fath_dob), !is.na(child_private)) %>%
  select(-fath_employ, -moth_employ) %>% 
  summarise_all( ~sum(is.na(.)) )

gt3_sample %>% 
  select(-fath_employ, -moth_employ) %>% 
  filter(!is.na(moth_dob), !is.na(fath_dob), !is.na(child_private)) %>%
  summarise_all( ~sum(is.na(.)) )












