# MScED Dissertation

## Overview
Replication Files for my graduate dissertation at the University of Oxford for the program MSc in Economics for Development, 2021/22.


## Introduction
This repository contains replication files for my graduate dissertation titled "The Relationship Between Sibling Size and Educational Attainment of Children: Evidence from South Africa".

The raw data comes from the 2011 South African Census (10% public use sample), which can be freely accessed from [here](https://www.datafirst.uct.ac.za/dataportal/index.php/catalog/485). In the "Get Microdata" tab, you can find two zip files named "sa-census-2011-person-prov-1to5-v2-20150825-stata.zip" and "sa-census-2011-person-prov-6to9-v2-20150825-stata.zip", which contain individual level data in Stata format. 

We start with the Stata do file `import_append.do`, which reads in the raw data above, appends them, and exports a .csv file ready to be imported in R.

R takes over from here. The R script `master.R` loads all necesarry packages and calls the other scripts in sequence. See comments there for more detail.

The following is a list of the main scripts, in proper sequence, and their function in brief:

* `data_extract`: loads 
* `data_samples`:
* `descriptives`:
* `estimation`:
* `results`:
* `hetero_grf`:

## Credit

Statistics were done using R 4.1.2 \parencite{RCT2021}, the \texttt{tidyverse} \parencite{Wickham2019}, the \texttt{lubridate} \parencite{Grolemund2011} the \texttt{lfe} \parencite{Gaure2013,Gaure2021} and the \texttt{grf} \parencite{Tibshirani2022} packages. \texttt{statgazer} \parencite{Hlavac2022} and \texttt{xtable} \parencite{Dahl2019} were used to generate \LaTeX~tables.












