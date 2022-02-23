# Created on: February 22, 2022
# DEPENDS ON: import_append.do
# outputs: kids_data.csv

# This script loads the data produced by Stata, processes it, and
# produces raw data for cleaning
# (make sure to load the packages in "master.R")


# Load the data ####

# (takes a while; data has > 2mil obs.)
memory.limit(9999999999) # expand the working memory
all_persons <- read_csv("data/all_persons.csv")
gc() # free unused memory

all_persons <- all_persons %>%
  clean_names() %>%
  mutate(dob = make_date(p01_year, p01_month, p01_day)) %>%
  # the census was conducted on 9-10 October, 2011
  mutate(
    age_month = interval(dob, ymd(20111010)) %/% months(1),
    age_year = interval(dob, ymd(20111010)) %/% years(1)
  ) %>%
  select(sn, f00_nr, dob, f03_sex, age_month, age_year, everything())


# prepare kids, mothers, and fathers data sets ####

kids <- all_persons %>%
  filter(p02_relation == 3) %>% # extracts sons/daughters
  filter(
    p14_motheralive == 1, # mother alive
    p15_fatheralive == 1, # father alive
    !(p14a_motherpnr %in% c(98, 99)) # mother in the hh
    # !(p15a_fatherpnr %in% c(98, 99)) # father in the hh
  ) %>%
  mutate(
    child_no = str_c(sn, f00_nr),
    moth_no = str_c(sn, p14a_motherpnr),
    fath_no = str_c(sn, p15a_fatherpnr)
  ) %>%
  select(
    # other variables need to be added (not final list)
    child_no,
    moth_no,
    fath_no,
    child_dob = dob,
    child_sex = f03_sex,
    child_age_year = age_year,
    child_age_month = age_month,
    child_sch_attend = p17_schoolattend,
    child_educ = p20_edulevel,
    child_private = p19_edupubpriv,
    child_pop_group = p05_pop_group,
    fath_pnr == p15a_fatherpnr,
    province = p_province,
    district = p_district,
    municip = p_munic
  )

mothers <- all_persons %>%
  mutate(moth_no = str_c(sn, f00_nr)) %>%
  # semi_join(x, y) keeps all obs. in x that have a match in y:
  semi_join(kids, by = "moth_no") %>%
  select(
    # other variables need to be added (not final list)
    moth_no,
    moth_age_year = age_year,
    moth_age_month = age_month,
    moth_dob = dob,
    moth_marital = p03_marital_st,
    moth_pp_group = p05_pop_group,
    moth_educ = derp_educational_level,
    moth_ceb = p32_childeverborn,
    moth_age_fstbr = p33_agefirstbirth,
    moth_employ = derp_employ_status_expanded,
    moth_income = p16_income
  )

# fathers <- all_persons %>%
#   mutate(fath_no = str_c(sn, f00_nr)) %>%
#   semi_join(kids, by = "fath_no") %>%
#   select(
#     # other variables need to be added (not final list)
#     fath_no,
#     fath_age_year = age_year,
#     fath_age_month = age_month,
#     fath_dob = dob,
#     fath_marital = p03_marital_st,
#     fath_pp_group = p05_pop_group,
#     fath_educ = derp_educational_level,
#     fath_employ = derp_employ_status_expanded,
#     fath_income =  p16_income
#   )


# join all three above
data <- kids %>%
  left_join(mothers, by = "moth_no") 
# %>%
#   left_join(fathers, by = "fath_no")

# free-up some memory
rm(list = c("fathers", "mothers", "kids", "all_persons"))
gc()

# The following function enables us to extract first- and second-borns
dobs <- function(tbl = data, n) {
  tbl <- tbl %>%
    select(moth_no, child_dob) %>%
    group_by(moth_no) %>%
    arrange(child_dob, .by_group = TRUE) %>%
    # n = 1 for "firstborn_dob" and n = 2 for "secondborn_dob"
    slice(n) %>%
    ungroup()

  # conditionally generate dobs for 1st and 2nd borns
  if (n == 1) {
    tbl <- tbl %>%
      rename(firstborn_dob = "child_dob") %>%
      # using the "interval()" function from lubridate to calc. age
      mutate(
        firstborn_age = interval(
          firstborn_dob, ymd(20111010)
        ) %/% years(1)
      )
  } else if (n == 2) {
    tbl <- tbl %>%
      rename(secondborn_dob = "child_dob") %>%
      mutate(
        secondborn_age = interval(
          secondborn_dob, ymd(20111010)
        ) %/% years(1)
      )
  }

  return(tbl)
}

#  Get first and second born obs. (very time-intensive)
firstborn_dob <- dobs(n = 1)
secondborn_dob <- dobs(n = 2)

# put everything together:
data <- data %>%
  left_join(firstborn_dob, by = "moth_no") %>%
  left_join(secondborn_dob, by = "moth_no") %>%
  filter(firstborn_age <= 18)

# label variables (labels are obtained from documentation and Stata file) ####
data <- data %>%
  mutate(
    child_sex = case_when(
      child_sex == 1 ~ "Male",
      child_sex == 2 ~ "Female"
    ) %>% factor(),
    child_sch_attend = case_when(
      child_sch_attend == 1 ~ "Yes",
      child_sch_attend == 2 ~ "No",
      child_sch_attend == 3 ~ "Don't know",
      child_sch_attend == 9 ~ "Unspecified"
    ),
    child_private = case_when(
      child_private == 1 ~ "Public (government)",
      child_private == 2 ~ "Private",
      child_private == 3 ~ "Don't know",
      child_private == 9 ~ "Unspecified"
    ),
    moth_educ =
      case_when(
        moth_educ == 1 ~ "No schooling",
        moth_educ == 2 ~ "Some primary",
        moth_educ == 3 ~ "Completed primary",
        moth_educ == 4 ~ "Some secondary",
        moth_educ == 5 ~ "Grade 12/Std 10",
        moth_educ == 6 ~ "Higher",
        moth_educ == 7 ~ "Other"
      ) %>% factor()
    # ,
    # fath_educ =
    #   case_when(
    #     fath_educ == 1 ~ "No schooling",
    #     fath_educ == 2 ~ "Some primary",
    #     fath_educ == 3 ~ "Completed primary",
    #     fath_educ == 4 ~ "Some secondary",
    #     fath_educ == 5 ~ "Grade 12/Std 10",
    #     fath_educ == 6 ~ "Higher",
    #     fath_educ == 7 ~ "Other"
    #   ) %>% factor()
  )

data <- data %>%
  mutate(
    child_pop_group =
      case_when(
        child_pop_group == 1 ~ "Black African",
        child_pop_group == 2 ~ "Coloured",
        child_pop_group == 3 ~ "Indian or Asian",
        child_pop_group == 4 ~ "White",
        child_pop_group == 5 ~ "Other",
      ) %>% factor(),
    moth_pp_group =
      case_when(
        moth_pp_group == 1 ~ "Black African",
        moth_pp_group == 2 ~ "Coloured",
        moth_pp_group == 3 ~ "Indian or Asian",
        moth_pp_group == 4 ~ "White",
        moth_pp_group == 5 ~ "Other",
      ) %>% factor()
    # ,
    # fath_pp_group =
    #   case_when(
    #     fath_pp_group == 1 ~ "Black African",
    #     fath_pp_group == 2 ~ "Coloured",
    #     fath_pp_group == 3 ~ "Indian or Asian",
    #     fath_pp_group == 4 ~ "White",
    #     fath_pp_group == 5 ~ "Other",
    #   ) %>% factor()
  )

data <- data %>%
  mutate(
    moth_inlf = case_when(
      moth_employ %in% 1:2 ~ 1,
      moth_employ == 3 ~ 0
    ),
    # fath_inlf = case_when(
    #   fath_employ %in% 1:2 ~ 1,
    #   fath_employ == 3 ~ 0
    # ),
    moth_employ =
      case_when(
        moth_employ == 1 ~ "Employed",
        moth_employ == 2 ~ "Unemployed",
        moth_employ == 3 ~ "Not economically active"
      ) %>% factor()
    # ,
    # fath_employ =
    #   case_when(
    #     fath_employ == 1 ~ "Employed",
    #     fath_employ == 2 ~ "Unemployed",
    #     fath_employ == 3 ~ "Not economically active"
    #   ) %>% factor()
  )

data <- data %>%
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

data <- data %>%
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
      ) %>% factor()
    # ,
    # fath_income =
    #   case_when(
    #     fath_income == 1 ~ "No income",
    #     fath_income == 2 ~ "R 1 - R 4800",
    #     fath_income == 3 ~ "R 4801 - R 9600",
    #     fath_income == 4 ~ "R 9601 - R 19200",
    #     fath_income == 5 ~ "R 19201 - R 38400",
    #     fath_income == 6 ~ "R 38401 - R 76800",
    #     fath_income == 7 ~ "R 76801 - R 153600",
    #     fath_income == 8 ~ "R 153601 - R 307200",
    #     fath_income == 9 ~ "R 307201 - R 614400",
    #     fath_income == 10 ~ "R 614401 - R 1228800",
    #     fath_income == 11 ~ "R 1228801 - R 2457600",
    #     fath_income == 12 ~ "R 2457601 or more",
    #     fath_income == 99 ~ "Unspecified"
    #   ) %>% factor()
  )

data <- data %>%
  mutate(
    moth_marital = case_when(
      moth_marital == 1 ~ "Married",
      moth_marital == 2 ~ "Living together",
      moth_marital == 3 ~ "Never married",
      moth_marital == 4 ~ "Widower/widow",
      moth_marital == 5 ~ "Separated",
      moth_marital == 6 ~ "Divorced"
    ) %>% factor()
    # ,
    # fath_marital = case_when(
    #   fath_marital == 1 ~ "Married",
    #   fath_marital == 2 ~ "Living together",
    #   fath_marital == 3 ~ "Never married",
    #   fath_marital == 4 ~ "Widower/widow",
    #   fath_marital == 5 ~ "Separated",
    #   fath_marital == 6 ~ "Divorced"
    # ) %>% factor()
  )


# save data
write_csv(data, file = "data/kids_data.csv")
