########################################################################
# MSc Thesis of Lemi Taye Daba
# Univestiy of Oxford
# Title: "The Relationship Between ..."
#
# R Script "master"
# Date of this version: February 22, 2022
########################################################################

# Some preparations:
rm(list = ls())

# Call R scripts
source("data.R"        ,echo=TRUE,max=1000) # Data import and cleaning
source("descriptives.R",echo=TRUE,max=1000) # Descriptive statistics
source("estimation.R"  ,echo=TRUE,max=1000) # Estimation of model 
source("results.R"     ,echo=TRUE,max=1000) # Tables and Figures