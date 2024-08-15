/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/

* Descriptive statistics of the databases

*********************************************************************************************************************
* 1. SS "deficit"
* Fraction of pensions expenditure that is not financed with current contributions in the payg pillar in 2017
*********************************************************************************************************************
*use  "$intermediate/panel_active_retired_3000hhs.dta", clear 
use "$intermediate/panel_active_retired.dta", clear 
merge m:1 id using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m

egen expenditure_pensions = sum(pen_in) if year == 2017
egen contributions = sum(con_ss_in) if year == 2017
g deficit_expenditure = (expenditure_pensions - contributions)/expenditure_pensions
sum deficit_expenditure [fw=weight ]

egen income_declared = sum(my_labor_in) if year == 2017
g tax = (expenditure_pensions - contributions)/income_declared
sum tax [fw=weight ]

* Results: 31% of simulated expenditure in pensions, representing about 7% of declared income, is not paid with contributions in  2017.
*********************************************************************************************************************
* 2. Retirement age is on average 62 
*********************************************************************************************************************
*use  "$intermediate/panel_active_retired_3000hhs.dta", clear 
use "$intermediate/panel_active_retired.dta", clear 
merge m:1 id using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m

sum age_ret [fw=weight ]

*********************************************************************************************************************
* 3. Densities of contribution by age
*********************************************************************************************************************

*********************************************************************************************************************
* 	3.1. All years
*********************************************************************************************************************
*use  "$intermediate/panel_active_retired_3000hhs.dta", clear 
use "$intermediate/panel_active_retired.dta", clear 
merge m:1 id using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m

* table age if age >=20 & age <=65 & age < age_ret , c(mean contributes_sim ) // STATA version 16 and older
table age if  age >=20 & age <=65 [fw=weight ] , statistic(mean contributes_sim )  // STATA 17

collapse contributes_sim  , by(age)
twoway (line contributes_sim age) if age >= 20 & age <= 65

* Results: simulated densities are relatively high: 68% on average (ages 20 to 65)
*********************************************************************************************************************
* 	3.2. In 2017 
*********************************************************************************************************************
*use  "$intermediate/panel_active_retired_3000hhs.dta", clear 
use "$intermediate/panel_active_retired.dta", clear 
merge m:1 id using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m

keep if year == 2017
* table age if year == 2017 & age >=20 & age <=65 & age < age_ret  , c(mean contributes_sim)  // STATA version 16 and older
table age  if age >=20 & age <=65  [fw=weight ], statistic(mean contributes_sim)  // STATA 17

* Results: densities in 2017 are 58% on average (ages 20 to 65). We should check why densities are so different in 2017 and in the whole panel dataset. Probably simulation of densities should be adjusted.

collapse contributes_sim  , by(age)
twoway (line contributes_sim age) if age >= 20 & age <= 65

* The age profiles of contributions are different in 2017 and the whole panel dataset (all years). The panel uses the contributive status (binary variable) and the probability of contributing in 2017 from the HH survey but the age profile from SS, so the cross section densities by age in 2017 can be very different from the longitudinal densities by age.

*********************************************************************************************************************
* 4. Proportion of the population with x or more periods of contribution at 60 to 65 years of age
*********************************************************************************************************************
use "$intermediate/panel_active_retired_section5.dta", clear
merge m:1 id using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m

capture drop cont_??ormore
g cont_30ormore = contributes_cum >= 30 if age == 60
g cont_25ormore = contributes_cum >= 25 if age == 60
g cont_20ormore = contributes_cum >= 20 if age == 60
forvalues i = 1(1)5 {
	replace cont_30ormore = contributes_cum >= 30 if age == (60 + `i')
	replace cont_25ormore = contributes_cum >= 25 if age == (60 + `i')
	replace cont_20ormore = contributes_cum >= 20 if age == (60 + `i')
}

table age if age >= 60 & age <= 65 [fw=weight ], statistic(mean cont_20ormore cont_25ormore cont_30ormore) nototals

*********************************************************************************************************************
* 5. Social Security Wealth: distribution of ssw/income
*********************************************************************************************************************
* 5.1. At the household level
*********************************************************************************************************************
use "$intermediate/life_cycle_hh.dta", clear
keep if year == 2017
merge 1:m hhid using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m

g lifetime_income_hh_capita = lifetime_income_hh/mofhh_c
g ssw_income = ssw/lifetime_income_hh
xtile pct = lifetime_income_hh_capita [fw=weight ], nq(10)
table pct [fw=weight ], statistic(mean ssw_income) statistic(median ssw_income) statistic(range ssw_income) statistic(sd ssw_income)
*********************************************************************************************************************
* 5.2. At the individual level
*********************************************************************************************************************
* Notes: (i) Both ssw and lifetime income are household aggregates, so members of the household have the same ssw-lifetime income ratio. (ii) Estimations in sections 5.1 and 5.2 differ in that percentiles are computed at the household and individual levels, respectively. 

use "$intermediate/life_cycle.dta", clear
keep if year == 2017
merge 1:1 id using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m

g lifetime_income_hh_capita = lifetime_income_hh/mofhh_c
g ssw_income = ssw/lifetime_income_hh
xtile pct = lifetime_income_hh_capita [fw=weight ], nq(10)
table pct [fw=weight ], statistic(mean ssw_income) statistic(median ssw_income) statistic(range ssw_income) statistic(sd ssw_income)

********************************************************************************************
* 6. Households per capita income with and without SS
********************************************************************************************
* 6.1. Merging databases
********************************************************************************************

use "$intermediate/life_cycle.dta", clear
keep if year == 2017
keep hhid memb_no id income_NSS income_SS income_NSS_pc income_SS_pc income_labor_pc income_other_pc income_vsc_NSS_pc income_vsc_SS_pc income_ms_SS_pc  income_vsb_pc 
g income_capital_NSS_pc = income_vsc_NSS_pc + income_vsb_pc // See equation {eq:income-bequests}
label var income_capital_NSS_pc "Income from savings WITHOUT SS, per capita"
g income_capital_SS_pc = income_vsc_SS_pc + income_vsb_pc 
label var income_capital_SS_pc "Income from savings WITH SS, per capita"
sort hhid memb_no
save "$intermediate/life_cycle_2017.dta", replace

use "$intermediate/ceq_income_concepts.dta", clear
keep hhid memb_no CEQ_NSS_pc CEQ_SS_pc lab_income_pc cap_income_pc net_pension_income_pc other_income_pc i_cppriv_in2 i_cppriv_in3 i_cppub_in2 i_cppub_in3 weight
g gross_pensions_ceq = i_cppriv_in2 + i_cppriv_in3 + i_cppub_in2 + i_cppub_in3

sort hhid
merge 1:1 hhid memb_no using "$intermediate/life_cycle_2017.dta"
keep if _merge == 3
drop _merge

* Renaming LC variables
rename income_NSS_pc total_nss_lc
rename income_SS_pc  total_ss_lc
rename income_labor_pc labor_lc
rename income_other_pc other_lc
rename income_ms_SS_pc pensions_lc 
rename income_capital_NSS_pc capital_nss_lc
rename income_capital_SS_pc capital_ss_lc
* Renaming CEQ variables
rename CEQ_NSS_pc total_nss_ceq
rename CEQ_SS_pc  total_ss_ceq
rename lab_income_pc labor_ceq
rename other_income_pc other_ceq
rename net_pension_income_pc pensions_ceq 
rename cap_income_pc capital_nss_ceq
g 	   capital_ss_ceq = capital_nss_ceq // Capital income is the same with and without SS in CEQ. 

save "$intermediate/life_cycle_ceq_2017.dta", replace

********************************************************************************************
* 6.2. Building table XX
********************************************************************************************

use "$intermediate/life_cycle_ceq_2017.dta", clear

g Concept = ""
replace Concept = "Total" if _n == 1
replace Concept = "Labor" if _n == 2
replace Concept = "Capital"	if _n == 3
replace Concept = "Pensions" if _n == 4
replace Concept = "Other sources" if _n == 5

g CEQ_SS = .
g CEQ_NSS = .
g LC_SS = .
g LC_NSS = .
g na = .

global CEQ_SS 	"total_ss_ceq labor_ceq capital_ss_ceq pensions_ceq other_ceq"
global CEQ_NSS 	"total_nss_ceq labor_ceq capital_nss_ceq na other_ceq"
global LC_SS 	"total_ss_lc labor_lc capital_ss_lc pensions_lc other_lc" 
global LC_NSS 	"total_nss_lc labor_lc capital_nss_lc na other_lc" 

global environment "CEQ_SS CEQ_NSS LC_SS LC_NSS"

foreach env in $environment {
	local row = 1
	foreach var in $`env' {
			sum `var' [fw=weight ]
			replace `env' = r(mean) if _n == `row'
			local ++row  
			dis `row'
	}
}

format Concept %-15s
format $environment %9.0f
list Concept $environment  if _n<= 5
export excel Concept $environment using "$results/Income components.xls" if _n <= 6, replace firstrow(variables)

********************************************************************************************
* 6.x. PROBANDO ALTERNATIVA DE SEPARAR ESCENARIO INICIAL Y FINAL EN DOS TABLAS. 
********************************************************************************************

* Table xx: HH per capita income in the "initial" scenario with pensions
use "$intermediate/life_cycle_ceq_2017.dta", clear

g Concept = ""

replace Concept = "Total" if _n == 1
replace Concept = "Labor" if _n == 2
replace Concept = "Capital"	if _n == 3
replace Concept = "Pensions" if _n == 4
replace Concept = "Other sources" if _n == 5

g ceq_mean = .
g ceq_p25 = .
g ceq_p75 = .
g lc_mean = .
g lc_p25 = .
g lc_p75 = .

global ceq 	"total_ss_ceq labor_ceq capital_ss_ceq pensions_ceq other_ceq"
global lc 	"total_ss_lc labor_lc capital_ss_lc pensions_lc other_lc" 
global environment "ceq lc"
global column "ceq_mean ceq_p25 ceq_p75  lc_mean lc_p25 lc_p75"

foreach env in $environment {
	local row = 1
	foreach var in $`env' {
			dis `row'
			sum `var' [fw=weight ], d
			replace `env'_mean = r(mean) if _n == `row'
			replace `env'_p25 = r(p25) if _n == `row'
			replace `env'_p75 = r(p75) if _n == `row'			
			local ++row  		
	}
}

format Concept %-15s
format $column %9.0f
list Concept $column  if _n<= 5
export excel Concept $column using "$results/Income with pensions.xls" if _n <= 5, replace firstrow(variables)

* Table yy: HH per capita income in the "final" scenario without pensions

use "$intermediate/life_cycle_ceq_2017.dta", clear

g Concept = ""

replace Concept = "Total" if _n == 1
replace Concept = "Labor" if _n == 2
replace Concept = "Capital"	if _n == 3
replace Concept = "Pensions" if _n == 4
replace Concept = "Other sources" if _n == 5

g ceq_mean = .
g ceq_p25 = .
g ceq_p75 = .
g lc_mean = .
g lc_p25 = .
g lc_p75 = .
g na = .

global ceq 	"total_nss_ceq labor_ceq capital_nss_ceq na other_ceq"
global lc 	"total_nss_lc labor_lc capital_nss_lc na other_lc" 
global environment "ceq lc"
global column "ceq_mean ceq_p25 ceq_p75  lc_mean lc_p25 lc_p75"

foreach env in $environment {
	local row = 1
	foreach var in $`env' {
			dis `row'
			sum `var'[fw=weight ], d
			replace `env'_mean = r(mean) if _n == `row'
			replace `env'_p25 = r(p25) if _n == `row'
			replace `env'_p75 = r(p75) if _n == `row'			
			local ++row  		
	}
}

format Concept %-15s
format $column %9.0f
list Concept $column  if _n<= 5
export excel Concept $column using "$results/Income without pensions.xls" if _n <= 5, replace firstrow(variables)
********************************************************************************************
* 6.3. Differences in hh per capita income in ceq and lc
********************************************************************************************

use "$intermediate/life_cycle_ceq_2017.dta", clear
g Concept = ""
replace Concept = "Total" if _n == 1
replace Concept = "Labor" if _n == 2
replace Concept = "Capital"	if _n == 3
replace Concept = "Pensions" if _n == 4
replace Concept = "Other sources" if _n == 5

global ss = "total_ss labor capital_ss pensions other"
global nss = "total_nss  labor capital_nss  pensions other"
global column "ss nss"

g ss_m = .
g nss_m = .
g ss_sd = .
g nss_sd = .

foreach column in $column {
	foreach var in $`column' {
		capture drop dif_`var'
		g dif_`var'= `var'_ceq - `var'_lc
	}
}

foreach column in $column {
	local row = 1
	foreach var in $`column' {
		sum dif_`var' [fw=weight ], d
		replace `column'_m = r(mean) if _n == `row'	
		replace `column'_sd = r(sd) if _n == `row'	
		sum `var'_ceq [fw=weight ]
		replace `column'_sd = `column'_sd/r(mean)
		local ++row  
		dis `row'
	}
}

format Concept %-15s
format ss_m nss_m  %9.0f
format nss_sd nss_sd %9.3f
list Concept ss_m ss_sd nss_m  nss_sd if _n<= 5

*****************


foreach var in labor other {
	ineq `var'_ceq  `var'_lc ,  index(gini)
}

********************************************************************************************
* 6.4. Household per-capita total income with and without SS 
********************************************************************************************
use "$intermediate/life_cycle_ceq_2017.dta", clear
reg total_ss_lc total_nss_lc [fw=weight ]
capture drop fitted
predict fitted
xtile ptile = total_nss_lc [fw=weight ],nq(100)
label var total_ss_lc "HH per capita income with SS"
g line45 = total_nss_lc
label var line45 "45º line"
twoway(scatter total_ss_lc total_nss_lc) (line line45 total_nss_lc) (line fitted total_nss_lc) if ptile <= 99, ytitle(HH per capita income with SS)

********************************************************************************************
* 7.  Crowding out
********************************************************************************************

use "$intermediate/life_cycle.dta", clear
keep if year == 2017
merge 1:m id using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m


********************************************************************************************
* 7.1. Tabulates
********************************************************************************************
g crowding_out = (income_vsc_NSS_pc > income_vsc_SS_pc)
label var crowding_out "=1 if there is crowding-out of savings"
g no_crowding_out = (income_vsc_NSS_pc == income_vsc_SS_pc)
label var no_crowding_out "=1 if there is no crowding-out of savings"
g crowding_in = (income_vsc_NSS_pc < income_vsc_SS_pc)
label var crowding_in "=1 if there is crowding-in of savings"
g partial_crowding_out = (income_vsc_NSS_pc > income_vsc_SS_pc) & (income_vsc_NSS_pc <= income_vsc_SS_pc + income_ms_SS_pc) 
label var partial_crowding_out "=1 if there is partial crowding-out of savings"
g over_crowding_out = (income_vsc_NSS_pc > income_vsc_SS_pc) & (income_vsc_NSS_pc > income_vsc_SS_pc + income_ms_SS_pc) 
label var over_crowding_out "=1 if there is over crowding-out of savings"

table [fw=weight ], statistic(mean crowding_in no_crowding_out crowding_out  partial_crowding_out over_crowding_out) // {table:crowding out}

********************************************************************************************
* 7.2. Graphs
********************************************************************************************
use "$intermediate/life_cycle.dta", clear
keep if year == 2017
merge 1:m id using "$intermediate/life_cycle_ceq_2017.dta", keepusing(weight)
drop if _m!=3
drop _m

g income_pc_volunt_SS_NSS = (income_vsc_SS_pc - income_vsc_NSS_pc)/income_NSS_pc
label var income_pc_volunt_SS_NSS "Capital income with minus without SS/total income without SS"

sum income_pc_volunt_SS_NSS [fw=weight ], d

capture drop ptile
xtile ptile = income_pc_volunt_SS_NSS [fw=weight ],nq(100)
histogram income_pc_volunt_SS_NSS if ptile >1, title(The crowding out of capital income by pensions)
graph export "$results/crowding_out_histogram.pdf" , replace

g income_pc_volunt_SS_NSS_absolute = (income_vsc_SS_pc - income_vsc_NSS_pc)
regress   income_pc_volunt_SS_NSS_absolute  income_ms_SS_pc [fw=weight ]
capture drop fitted
predict fitted 
g total_crowding_out = - income_ms_SS_pc
capture drop ptile
xtile ptile = income_ms_SS_pc [fw=weight ],nq(100)

twoway(scatter income_pc_volunt_SS_NSS_absolute income_ms_SS_pc) (line total_crowding_out income_ms_SS_pc) (line fitted income_ms_SS_pc) if ptile > 1



