
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

# Load the data (takes a while; data has > 2mil obs.)
all_persons <- read_csv("data/all_persons.csv")