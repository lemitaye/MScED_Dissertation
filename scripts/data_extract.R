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
    !(p14a_motherpnr %in% c(98, 99)), # mother in the hh
    !(p15a_fatherpnr %in% c(98, 99)) # father in the hh
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

fathers <- all_persons %>%
  mutate(fath_no = str_c(sn, f00_nr)) %>%
  semi_join(kids, by = "fath_no") %>%
  select(
    # other variables need to be added (not final list)
    fath_no,
    fath_age_year = age_year,
    fath_age_month = age_month,
    fath_dob = dob,
    fath_marital = p03_marital_st,
    fath_pp_group = p05_pop_group,
    fath_educ = derp_educational_level,
    fath_employ = derp_employ_status_expanded,
    fath_income =  p16_income
  )


# join all three above
data <- kids %>%
  left_join(mothers, by = "moth_no") %>%
  left_join(fathers, by = "fath_no")

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

# save data
write_csv(data, file = "data/kids_data.csv")
