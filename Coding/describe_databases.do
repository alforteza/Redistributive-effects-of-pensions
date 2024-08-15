/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/


* Variables in the databases produced by the do files


/*
This do file produces databases containing the list of do files in the project and the list of databases produced with those do files that contain the variable selected in local varlist. The files "datasets_containing_`varlist'.dta" can be merged using the loop in part 3 of this program (the list in the last loop must be adjusted accordingly). The final integrated outcome is "datasets_containing_selected_variables.dta".  
*/

clear 

use "$intermediate/results.dta", clear

*******************************************************************************************************************************
* 1. Inputs
*******************************************************************************************************************************
local varlist "ECH_income"		// List selected variable
*local varlist "age"

*******************************************************************************************************************************
* 2. Program (do not touch!)
*******************************************************************************************************************************
quietly {
g dofile_number = .
g dc_`varlist' = ""
label var dc_`varlist' "datasets containing `varlist'"	

save "$intermediate/datasets_containing_`varlist'.dta", replace

local counter = 1   
foreach file in ech_wages ech_quintiles_classif matching_ids ech_densities bps_wages_densities ech_wages parameters pensions ech_labor_income_retired ceq_for_matching merging_ceq_2017 life_cycle_approach ech_income_concepts results {
	use "$intermediate/`file'.dta", clear 
	 quietly {
		ds  , varwidth(25) 
	 }
	g dofile_number = `counter' 
	g dc_`varlist' = ""	
	   local var "`r(varlist)'"
	   foreach x of local var {
			if "`x'" == "`varlist'" {
		*		display "ECH wages.dta, producido por ECH_wages.do contiene la variable: `x'"
				replace dc_`varlist' = "`file'"
			}				
	   }
	   keep dc_`varlist' dofile_number
	   keep if _n == 1
	save "$intermediate/aux.dta", replace
	use "$intermediate/datasets_containing_`varlist'.dta", clear
	append using $intermediate/aux.dta
	save "$intermediate//datasets_containing_`varlist'.dta", replace
	erase  "$intermediate/aux.dta"
	local counter = `counter' + 1
	clear
}
use "$intermediate/datasets_containing_`varlist'.dta", clear
g dofiles = ""	
label var dofiles "do files producing dataset in datasets_with_`varlist'"
replace dofiles = "ECH_wages" 					if _n == 1
replace dofiles = "ECH_quintiles_classif" 		if _n == 2
replace dofiles = "ECH_densities" 				if _n == 3 | _n == 4
replace dofiles = "BPS_wages_densities" 		if _n == 5

replace dofiles = "ECH_panel_wages_densities" 	if _n == 6
replace dofiles = "Parameters" 					if _n == 7
replace dofiles = "Pensions" 					if _n == 8
replace dofiles = "ECH_labor_income_retired" 	if _n == 9

replace dofiles = "Merging_CEQ_2017" 					if _n == 10
replace dofiles = "Merging_CEQ_2017" 					if _n == 11

replace dofiles = "Life_cycle_approach" 		if _n == 12
replace dofiles = "Income_concepts_ECH" 				if _n == 13
replace dofiles = "Results" 							if _n == 14
order dofile_number dofiles dc_`varlist'
sort dofile_number
save "$intermediate/datasets_containing_`varlist'.dta", replace 
}

*******************************************************************************************************************************
* 3. Merging several datasets_containing_`varlist'
*******************************************************************************************************************************

use "$intermediate/datasets_containing_ECH_income.dta", clear

foreach var in  `varlist' {
merge 1:1 dofile_number using "$intermediate/datasets_containing_`var'.dta"
drop _merge
}
save "$intermediate/datasets_containing_selected_variables.dta", replace 
