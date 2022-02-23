# Created on: February 22, 2022
# DEPENDS ON: data_extract.R
# Outputs:
#   gt2_analysis_sample.csv
#   gt3_analysis_sample.csv

# This script loads the data produced by data_extract.R, processes it,
# and produces cleaned data ready for analysis.

rm(list = ls())

# Data Preparation ####

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
  ) %>% 
  filter( !is.na(moth_dob), !is.na(fath_dob) ) 
  

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


# Construct Variables ####

## 2+ sample ####

### Parents' education & background ####

gt2_sample <- gt2_sample %>%
  mutate(
    moth_educ =
      case_when(
        moth_educ == 1 ~ "No schooling",
        moth_educ == 2 ~ "Some primary",
        moth_educ == 3 ~ "Completed primary",
        moth_educ == 4 ~ "Some secondary",
        moth_educ == 5 ~ "Grade 12/Std 10",
        moth_educ == 6 ~ "Higher",
        moth_educ == 7 ~ "Other"
      ) %>% factor(),
    fath_educ = 
      case_when(
        fath_educ == 1 ~ "No schooling",
        fath_educ == 2 ~ "Some primary",
        fath_educ == 3 ~ "Completed primary",
        fath_educ == 4 ~ "Some secondary",
        fath_educ == 5 ~ "Grade 12/Std 10",
        fath_educ == 6 ~ "Higher",
        fath_educ == 7 ~ "Other"
      ) %>% factor()
  )

gt2_sample <- gt2_sample %>%
  mutate(
    moth_pp_group =
      case_when(
        moth_pp_group == 1 ~ "Black African",
        moth_pp_group == 2 ~ "Coloured",
        moth_pp_group == 3 ~ "Indian or Asian",
        moth_pp_group == 4 ~ "White",
        moth_pp_group == 5 ~ "Other",
      ) %>% factor(),
    fath_pp_group =
      case_when(
        fath_pp_group == 1 ~ "Black African",
        fath_pp_group == 2 ~ "Coloured",
        fath_pp_group == 3 ~ "Indian or Asian",
        fath_pp_group == 4 ~ "White",
        fath_pp_group == 5 ~ "Other",
      ) %>% factor()
  )

gt2_sample <- gt2_sample %>% 
  mutate(
    moth_inlf = case_when(
      moth_employ %in% 1:2 ~ 1,
      moth_employ == 3 ~ 0
    ),
    fath_inlf = case_when(
      fath_employ %in% 1:2 ~ 1,
      fath_employ == 3 ~ 0
    ),
    moth_employ =
      case_when(
        moth_employ == 1 ~ "Employed",
        moth_employ == 2 ~ "Unemployed",
        moth_employ == 3 ~ "Not economically active"
      ) %>% factor(),
    fath_employ =
      case_when(
        fath_employ == 1 ~ "Employed",
        fath_employ == 2 ~ "Unemployed",
        fath_employ == 3 ~ "Not economically active"
      ) %>% factor()
  )

gt2_sample <- gt2_sample %>% 
  mutate( 
    province = case_when(
      province == 1 ~ "Western Cape",
      province == 2 ~ "Eastern Cape",
      province == 3 ~ "Northern Cape",
      province == 4 ~ "Free State",
      province == 5 ~ "Kwazulu-Natal",
      province == 6 ~ "North West",
      province == 7 ~ "Gauteng",
      province == 8 ~ "Mpumalanga",
      province == 9 ~ "Limpopo"
    ) %>% factor(),
    district = factor(district),
    municip = factor(municip)
    )

gt2_sample <- gt2_sample %>%
  mutate(
    moth_income =
      case_when(
        moth_income == 1 ~ "No income",
        moth_income == 2 ~ "R 1 - R 4800",
        moth_income == 3 ~ "R 4801 - R 9600",
        moth_income == 4 ~ "R 9601 - R 19200",
        moth_income == 5 ~ "R 19201 - R 38400",
        moth_income == 6 ~ "R 38401 - R 76800",
        moth_income == 7 ~ "R 76801 - R 153600",
        moth_income == 8 ~ "R 153601 - R 307200",
        moth_income == 9 ~ "R 307201 - R 614400",
        moth_income == 10 ~ "R 614401 - R 1228800",
        moth_income == 11 ~ "R 1228801 - R 2457600",
        moth_income == 12 ~ "R 2457601 or more",
        moth_income == 99 ~ "Unspecified"
      ) %>% factor(),
    fath_income =
      case_when(
        fath_income == 1 ~ "No income",
        fath_income == 2 ~ "R 1 - R 4800",
        fath_income == 3 ~ "R 4801 - R 9600",
        fath_income == 4 ~ "R 9601 - R 19200",
        fath_income == 5 ~ "R 19201 - R 38400",
        fath_income == 6 ~ "R 38401 - R 76800",
        fath_income == 7 ~ "R 76801 - R 153600",
        fath_income == 8 ~ "R 153601 - R 307200",
        fath_income == 9 ~ "R 307201 - R 614400",
        fath_income == 10 ~ "R 614401 - R 1228800",
        fath_income == 11 ~ "R 1228801 - R 2457600",
        fath_income == 12 ~ "R 2457601 or more",
        fath_income == 99 ~ "Unspecified"
      ) %>% factor()
  )



### outcome variables ####

# Dummy for private school attendance & child sex (factor)
gt2_sample <- gt2_sample %>%
  filter(child_private %in% c(1, 2)) %>%
  mutate(private_school = case_when(
    child_private == 2 ~ 1, TRUE ~ 0
  )) %>%
  mutate(child_sex = case_when(
    child_sex == 1 ~ "Male", child_sex == 2 ~ "Female"
  ) %>% factor())

# constructing educational attainment variable
gt2_sample <- gt2_sample %>%
  filter(child_educ %in% 0:12 | child_educ == 98) %>%
  mutate(
    child_educ_gen = case_when(
      as.numeric(child_educ) == 98 ~ 1,
      TRUE ~ as.numeric(child_educ) + 2
    )
  ) %>%
  group_by(child_age_year, boy) %>%
  mutate(mean_educ_age_sex = mean(child_educ_gen)) %>%
  ungroup() %>%
  mutate(educ_attain = child_educ_gen / mean_educ_age_sex)












































