* import_append.do  February 22, 2022 for Stata version 17.0

********** Purpose **********
* This do file is needed because the data is so large and directly loading it to
* R causes a considerable slowdown in runtime or even crashing. Here, we simply
* load the two raw files, append them, filter relevant observations and prepare
* a data that can easily and quicky be imported into R.

********** Setup **********
version 11
clear all