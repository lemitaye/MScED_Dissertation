# MScED Dissertation

### Overview
Replication Files for my graduate dissertation at the University of Oxford for the program "MSc in Economics for Development", 2021/22.


### Introduction
This repository contains replication files for my graduate dissertation titled "The Relationship Between Sibling Size and Educational Attainment of Children: Evidence from South Africa" (the final version of the paper can be downloaded from [here](https://drive.google.com/file/d/1lz-9Tp55zYg3v-_-COC_jQDdhy0DhoIR/view?usp=sharing).

The raw data comes from the 2011 South African Census (10% public use sample), which can be freely accessed from [here](https://www.datafirst.uct.ac.za/dataportal/index.php/catalog/485). In the "Get Microdata" tab, you can find two zip files named "sa-census-2011-person-prov-1to5-v2-20150825-stata.zip" and "sa-census-2011-person-prov-6to9-v2-20150825-stata.zip", which contain individual level data in Stata format. 

We start with the Stata do file `import_append.do`, which reads in the raw data above, appends them, and exports a .csv file ready to be imported in R.

R takes over from here. The R script `master.R` loads all necesarry packages and calls the other scripts in sequence. See comments there for more detail.

The following is a list of the main scripts, in proper sequence, and their function in brief:

* `data_extract`: loads the data produced by Stata, processes it, and
produces raw data for further cleaning (*Warning*: some of the code chunks take considerably long time to run because of the size of the data.)
* `data_samples`: loads the data produced by `data_extract.R`, processes it, and produces cleaned data ready for analysis (these are named `gt2_sample.csv` and `gt3_sample.csv`).
* `descriptives`: produces table of summary statistics and some descriptive figures.
* `estimation`: runs the main regressions to be reported in the paper.
* `results`: produces LaTex tables for regression outputs
* `hetero_grf`: trains and fits Generalized Random Forests (GRFs) (*Warning*: fitting GRFs, particularly with large number of trees, is computationally intensive and requires a computer with powerful processors and a large memory.)

The repository also contains the Tex source code for the final document. 

### Credits

Statistics were done using R 4.1.2 ([R Core Team, 2021](https://www.R-project.org/)), the `tidyverse` ([Wickham et al., 2019](https://doi.org/10.21105/joss.01686)), the `lubridate` ([Grolemund and Wickham, 2011](https://www.jstatsoft.org/v40/i03/)),  the `lfe` ([Gaure, 2021](https://CRAN.R-project.org/package=lfe)), and the `grf` ([Tibshirani et al., 2022](https://CRAN.R-project.org/package=grf)) packages. `statgazer` ([Hlavac, 2022](https://CRAN.R-project.org/package=stargazer)) and `xtable` ([Dahl et al., 2019](https://CRAN.R-project.org/package=xtable)) were used to generate LaTeX tables.

Credit goes to Koundinya Desiraju (https://github.com/koundy/ggplot_theme_Publication) for providing the following themes for publication ready plots: `ggplot_theme_Publication-2.R`












