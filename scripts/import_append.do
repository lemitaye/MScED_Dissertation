* import_append.do  February 22, 2022 for Stata version 17.0

********** Purpose **********
* This do file is needed because the data is so large and directly loading it to R causes a considerable slowdown in runtime or even crashing. Here, we simply load the two raw files, append them, filter relevant observations and prepare a data that can easily and quicky be imported into R.

********** Setup **********
version 17
clear all
global cdpath = "D:\R_projects\MScED_Dissertation\data"   // change to where you save the current file on your drive
cd "$cdpath"

** Load raw data files
use "sa-census-2011-person-prov-1to5-v1.2-20150825.dta", clear
append using "sa-census-2011-person-prov-6to9-v1.2-20150825.dta"

* filter household quensionnaires 
keep if QN_TYPE == 1

** Write the data to a .csv file (faster to read in R, but takes time in Stata)
export delimited using "all_persons", nolabel replace
