# MScED Dissertation

### Overview
Replication Files for my graduate dissertation at the University of Oxford for the program MSc in Economics for Development, 2021/22.


### Introduction
This repository contains replication files for my graduate dissertation titled "The Relationship Between Sibling Size and Educational Attainment of Children: Evidence from South Africa".

The raw data comes from the 2011 South African Census (10% public use sample), which can be freely accessed from [here](https://www.datafirst.uct.ac.za/dataportal/index.php/catalog/485). In the "Get Microdata" tab, you can find two zip files named "sa-census-2011-person-prov-1to5-v2-20150825-stata.zip" and "sa-census-2011-person-prov-6to9-v2-20150825-stata.zip", which contain individual level data in Stata format. 

We start with the Stata do file `import_append.do`, which reads in the raw data above, appends them, and exports a .csv file ready to be imported in R.

R takes over from here. The R script `master.R` loads all necesarry packages and calls the other scripts in sequence. See comments there for more detail.

The following is a list of the main scripts, in proper sequence, and their function in brief:

* `data_extract`: loads the data produced by Stata, processes it, and
produces raw data for further cleaning (Warning: some of the code chunks take considerably long time to run because of the size of the data.)
* `data_samples`: loads the data produced by `data_extract.R`, processes it, and produces cleaned data ready for analysis (these are named `gt2_sample.csv` and `gt3_sample.csv`).
* `descriptives`: produces table of summary statistics and some descriptive figures.
* `estimation`: runs the main regressions to be reported in the paper.
* `results`: produces LaTex tables for regression outputs
* `hetero_grf`: trains and fits Generalized Random Forests (GRFs)

### Credit

Statistics were done using R 4.1.2, the `tidyverse`, the `lubridate`  the `lfe`, and the `grf` packages. `statgazer` and `xtable` were used to generate $\LaTeX$~tables.

Credit goes to Koundinya Desiraju (https://github.com/koundy/ggplot_theme_Publication) for providing the following themes for publication ready plots: `ggplot_theme_Publication-2.R`












