########################################################################~
# MSc Thesis of Lemi Taye Daba
# Univestiy of Oxford
# Title: "The Relationship Between ..."
#
# R Script "master"
# Date of this version: February 22, 2022
########################################################################~

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
library(lfe)
library(starpolishr)
library(fastDummies)
library(xtable)
library(ggpubr)
library(GGally)  # contains "ggcoef()" to plot reg. coefs.

# Credit goes to Koundinya Desiraju (https://github.com/koundy/ggplot_theme_Publication)
# for providing the following themes for publication ready plots:
source("scripts/ggplot_theme_Publication-2.R")  


# Call R scripts ####
source("scripts/data_extract.R",echo=TRUE,max=1000) # Data import and extract
source("scripts/data_samples.R",echo=TRUE,max=1000) # Data cleaning
source("scripts/descriptives.R",echo=TRUE,max=1000) # Descriptive statistics
source("scripts/estimation.R"  ,echo=TRUE,max=1000) # Estimation of model 
source("scripts/results.R"     ,echo=TRUE,max=1000) # Tables and Figures
