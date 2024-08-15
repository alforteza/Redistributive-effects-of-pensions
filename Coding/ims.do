/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/

/*
The data is contained in INE file: IMS C1 Gral emp M B08.xls. It was downloaded in November 2022 from INE webpage. ims.xlsx is a "clean" version of the INE original file. 
*/

* Average wage index

* 1. Monthly data 

clear
import excel "$inputs/INE/ims.xlsx", sheet("ims") firstrow
sort year month
save "$intermediate/ims_monthly.dta", replace

* 2. Annual data (April of each year)

keep if month == 4
keep year ims
sort year
save "$intermediate/ims_annual.dta", replace


