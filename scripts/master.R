########################################################################~
# MSc Thesis of Lemi Taye Daba
# Univestiy of Oxford
# 
# Title: "The Relationship Between Sibling Size and Educational Attainment 
#         of Children: Evidence from South Africa"
#
# R Script "master" (runs all other scripts)
# Date created: February 22, 2022
########################################################################~

# Some preparations: ####

rm(list = ls())

# packages
library(tidyverse)
library(janitor)
library(lubridate)
library(stargazer)
library(scales)
library(ggthemes)
library(broom)
library(car)
library(lfe)
library(starpolishr)
library(fastDummies)
library(xtable)
library(ggpubr)
library(grf)
library(doParallel)


# Call R scripts ####
source("scripts/data_extract.R", echo=TRUE, max=1000) # Data import and extract
source("scripts/data_samples.R", echo=TRUE, max=1000) # Data cleaning, get analysis samples
source("scripts/descriptives.R", echo=TRUE, max=1000) # Descriptive stats (table & figures)
source("scripts/estimation.R"  , echo=TRUE, max=1000) # Estimation of models
source("scripts/results.R"     , echo=TRUE, max=1000) # Export tables in LaTeX format
source("scripts/hetero_grf.R"  , echo=TRUE, max=1000) # Trains GRFs

