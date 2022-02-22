
# Created on: February 22, 2022
# DEPENDS ON: import_append.do

# This script loads the data produced by Stata, processes it, and  
# produces cleaned data for analysis.

# packages
library(haven)
library(tidyverse)
library(janitor)
library(lubridate)
library(AER)
library(stargazer)
library(scales)
library(broom)
library(car)

theme_set(theme_light())

# Load the data ####
# (takes a while; data has > 2mil obs.)
memory.limit(9999999999)   # expand the working memory
all_persons <- read_csv("data/all_persons.csv")
gc()   # free unused memory

all_persons <- all_persons %>%
  clean_names() %>%
  mutate(dob = make_date(p01_year, p01_month, p01_day)) %>%
  # the census was conducted on 9-10 October, 2011
  mutate(
    age_month = interval(dob, ymd(20111010)) %/% months(1),
    age_year = interval(dob, ymd(20111010)) %/% years(1)
  ) %>%
  select(sn, f00_nr, dob, f03_sex, age_month, age_year, everything())

gc()

# prepare kids, mothers, and fathers data sets ####
kids <- all_persons %>%
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
    child_pop_group = p05_pop_group
  )

gc()

mothers <- all_persons %>%
  mutate(moth_no = str_c(sn, f00_nr)) %>%
  ## semi_join(x, y) keeps all obs. in x that have a match in y:
  semi_join(kids, by = "moth_no") %>%
  # other variables need to be added (not final list)
  select(
    moth_no,
    moth_age_year = age_year,
    moth_age_month = age_month,
    moth_dob = dob,
    moth_marital = p03_marital_st,
    moth_pp_group = p05_pop_group,
    moth_educ = p20_edulevel,
    moth_ceb = p32_childeverborn,
    moth_age_fstbr = p33_agefirstbirth,
    moth_employ = derp_employ_status,
    moth_employ_official = derp_employ_status_official,
    moth_employ_extended = derp_employ_status_expanded
  )

gc()

fathers <- all_persons %>%
  mutate(fath_no = str_c(sn, f00_nr)) %>%
  semi_join(kids, by = "fath_no") %>%
  # other variables need to be added (not final list)
  select(
    fath_no,
    fath_age_year = age_year,
    fath_age_month = age_month,
    fath_dob = dob,
    fath_marital = p03_marital_st,
    fath_pp_group = p05_pop_group,
    fath_educ = p20_edulevel,
    fath_employ = derp_employ_status,
    fath_employ_official = derp_employ_status_official,
    fath_employ_extended = derp_employ_status_expanded
  )

gc()

data <- kids %>%
  left_join(mothers, by = "moth_no") %>%
  left_join(fathers, by = "fath_no")

# It is time to free-up some memory
rm(list = c("fathers", "mothers", "kids", "all_persons"))
gc()

# The following two definitely need a function
firstborn_dob <- data %>%
  select(moth_no, child_dob) %>%
  group_by(moth_no) %>%
  arrange(child_dob, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  rename(firstborn_dob = "child_dob") %>%
  mutate(
    firstborn_age = interval(
      firstborn_dob, ymd(20111010)
      ) %/% years(1)
    )

secondborn_dob <- data %>%
  select(moth_no, child_dob) %>%
  group_by(moth_no) %>%
  arrange(child_dob, .by_group = TRUE) %>%
  slice(2) %>%
  ungroup() %>%
  rename(secondborn_dob = "child_dob") %>%
  mutate(
    secondborn_age = interval(
      secondborn_dob, ymd(20111010)
      ) %/% years(1))

data <- data %>%
  left_join(firstborn_dob, by = "moth_no") %>%
  left_join(secondborn_dob, by = "moth_no") %>%
  filter(firstborn_age <= 18)




























