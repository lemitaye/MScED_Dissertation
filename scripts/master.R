########################################################################
# MSc Thesis of Lemi Taye Daba
# Univestiy of Oxford
# Title: "The Relationship Between ..."
#
# R Script "master"
# Date of this version: February 22, 2022
########################################################################

# Some preparations: ####

rm(list = ls())

# packages
library(tidyverse)
library(janitor)
library(lubridate)
library(AER)
library(stargazer)
library(scales)
library(broom)
library(car)

theme_set(theme_light())


# Call R scripts ####
source("scripts/data_extract.R",echo=TRUE,max=1000) # Data import and extract
source("scripts/data_samples.R",echo=TRUE,max=1000) # Data cleaning
source("scripts/descriptives.R",echo=TRUE,max=1000) # Descriptive statistics
source("scripts/estimation.R"  ,echo=TRUE,max=1000) # Estimation of model 
source("scripts/results.R"     ,echo=TRUE,max=1000) # Tables and Figures
