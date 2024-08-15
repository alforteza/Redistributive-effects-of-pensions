/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 

This do file produces the tables and figures used in the paper. 
*/

********************************************************************************************
* 1. Merging ceq and life cycle databases
********************************************************************************************

use "$intermediate/life_cycle_yd_$scenario.dta", clear
merge m:1 hhid memb_no year using "$intermediate/life_cycle_ym_$scenario.dta", keepusing(income_ym_NSS_pc income_ym_SS_pc)
keep if _merge == 3
drop _merge
keep if year == 2017
drop year

g income_capital_NSS_pc = income_vsc_NSS_pc + income_vsb_pc // See equation {eq:income-bequests}
label var income_capital_NSS_pc "Income from savings WITHOUT SS, per capita"
g income_capital_SS_pc = income_vsc_SS_pc + income_vsb_pc 
label var income_capital_SS_pc "Income from savings WITH SS, per capita"

* Renaming LC variables
rename income_NSS_pc total_nss_pc_lc
rename income_SS_pc  total_ss_pc_lc
rename income_ym_NSS_pc total_ym_nss_pc_lc
rename income_ym_SS_pc  total_ym_ss_pc_lc

rename income_labor_pc labor_pc_lc
rename income_other_pc other_pc_lc
rename income_ms_SS_pc pensions_pc_lc 
rename income_capital_NSS_pc capital_nss_pc_lc
rename income_capital_SS_pc capital_ss_pc_lc
rename income_dtr_pc dtr_pc_lc
rename dtx_labor_pc dtx_labor_pc_lc
rename dtx_pensions_pc dtx_pensions_pc_lc
rename dtx_volunt_assets_NSS_pc dtx_capital_NSS_pc_lc
rename dtx_volunt_assets_SS_pc dtx_capital_SS_pc_lc

* Bringing ceq variables

merge 1:1 hhid memb_no using "$intermediate/hhs.dta", keepusing(CEQ_NSS_ym_pc CEQ_NSS_yd_pc CEQ_SS_ym_pc CEQ_SS_yd_pc lab_income_pc  cap_income_pc net_pension_income_pc other_income_pc i_cppriv* i_cppub* weight dtx_labor* dtx_pensions* dtx_capital* dtr_all_pc)
keep if _merge == 3
drop _merge

g gross_pensions_ceq = i_cppriv_in2 + i_cppriv_in3 + i_cppub_in2 + i_cppub_in3
label var gross_pensions_ceq "Gross pensions from CEQ"
sort hhid memb_no

* Renaming CEQ variables
rename CEQ_NSS_yd_pc total_nss_pc_ceq
rename CEQ_SS_yd_pc  total_ss_pc_ceq
rename CEQ_NSS_ym_pc total_ym_nss_pc_ceq
rename CEQ_SS_ym_pc  total_ym_ss_pc_ceq
rename lab_income_pc labor_pc_ceq
rename other_income_pc other_pc_ceq
rename net_pension_income_pc pensions_pc_ceq 
rename cap_income_pc capital_nss_pc_ceq
rename dtr_all_pc dtr_pc_ceq
rename dtx_labor_pc dtx_labor_pc_ceq
rename dtx_pensions_pc dtx_pensions_pc_ceq
rename dtx_capital_pc dtx_capital_pc_ceq

g 	capital_ss_pc_ceq = capital_nss_pc_ceq // Capital income is the same with and without SS in CEQ. 

save "$intermediate/life_cycle_ceq_2017_$scenario.dta", replace

********************************************************************************************
* 2. Households per capita income with and without SS
********************************************************************************************

********************************************************************************************
* 2.1. Table \label{table:income_ss}: Households per capita income in the initial scenario with pensions 
********************************************************************************************

clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear

*svyset  [iw=weight]
svyset hhid [iw=weight]

g dtx_ss_pc_ceq = dtx_labor_pc_ceq + dtx_pensions_pc_ceq + dtx_capital_pc_ceq
g dtx_ss_pc_lc  = dtx_labor_pc_lc + dtx_pensions_pc_lc + dtx_capital_SS_pc_lc

foreach v of varlist /*
*/	total_ss_pc_ceq labor_pc_ceq capital_ss_pc_ceq dtr_pc_ceq dtx_ss_pc_ceq pensions_pc_ceq other_pc_ceq /*
*/	total_ss_pc_lc labor_pc_lc capital_ss_pc_lc  dtr_pc_lc dtx_ss_pc_lc pensions_pc_lc other_pc_lc{
sum `v' [fw=weight], d
matrix matrix_`v'=(r(mean),r(p25),r(p75))
}

matrix matrix_ceq = matrix_total_ss_pc_ceq
foreach v of varlist /*total_ss_pc_ceq*/ labor_pc_ceq capital_ss_pc_ceq dtr_pc_ceq dtx_ss_pc_ceq pensions_pc_ceq other_pc_ceq {
matrix matrix_ceq = (matrix_ceq \ matrix_`v')
}

matrix matrix_lc = matrix_total_ss_pc_lc 
foreach v of varlist /*total_ss_pc_lc*/ labor_pc_lc capital_ss_pc_lc dtr_pc_lc dtx_ss_pc_lc pensions_pc_lc other_pc_lc {
matrix matrix_lc = (matrix_lc \ matrix_`v')
}

matrix matrix_income_ss = (matrix_ceq , matrix_lc)
matrix rownames matrix_income_ss = "Total" "Labor" "Capital" "Gov transfers" "Taxes" "Pensions /b" "Other sources"
matrix colnames matrix_income_ss = "Mean" "p25" "p75" "Mean" "p25" "p75"
matrix list matrix_income_ss
// Open a file handle and write the macro
file open myfile using "$results/macros_income_ss_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(matrix_income_ss)
local ncols = colsof(matrix_income_ss)

forval i = 1/`nrows' {
    forval j = 1/`ncols' {

        local val : di %12.1f matrix(matrix_income_ss[`i', `j'])
		file write myfile " \setcellincomess$scenario{`i'}{`j'}{`val'} "
    }
    file write myfile " \ "
}

// Close the file handle
file close myfile

putexcel set "$intermediate/matrix_income_ss" , replace
putexcel A1 = matrix(matrix_income_ss), names
putexcel save

********************************************************************************************
* 2.2. Table \label{table:income_nss}: Households per capita income in the final scenario without pensions 
********************************************************************************************

clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear
*svyset  [iw=weight]
svyset hhid [iw=weight]

g dtx_nss_pc_ceq = dtx_labor_pc_ceq +  dtx_capital_pc_ceq
g dtx_nss_pc_lc  = dtx_labor_pc_lc + dtx_capital_NSS_pc_lc

g empty=0

foreach v of varlist /*
*/	total_nss_pc_ceq labor_pc_ceq capital_nss_pc_ceq dtr_pc_ceq dtx_nss_pc_ceq other_pc_ceq /*
*/	total_nss_pc_lc labor_pc_lc capital_nss_pc_lc dtr_pc_lc dtx_nss_pc_lc empty other_pc_lc{
sum `v'[fw=weight], d
matrix matrix_`v'=(r(mean),r(p25),r(p75))
}

matrix matrix_ceq = matrix_total_nss_pc_ceq
foreach v of varlist /*total_nss_pc_ceq*/ labor_pc_ceq capital_nss_pc_ceq dtr_pc_ceq dtx_nss_pc_ceq  empty other_pc_ceq {
matrix matrix_ceq = (matrix_ceq \ matrix_`v')
}

matrix matrix_lc = matrix_total_nss_pc_lc 
foreach v of varlist /*total_nss_pc_lc*/ labor_pc_lc capital_nss_pc_lc dtr_pc_lc dtx_nss_pc_lc  empty other_pc_lc {
matrix matrix_lc = (matrix_lc \ matrix_`v')
}

matrix matrix_income_nss = (matrix_ceq , matrix_lc)
matrix rownames matrix_income_nss = "Total" "Labor" "Capital" "Gov transfers" "Taxes" "Pensions" "Other sources"
matrix colnames matrix_income_nss = "Mean" "p25" "p75" "Mean" "p25" "p75"
matrix list matrix_income_nss

// Open a file handle and write the macro
file open myfile using "$results/macros_income_nss_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(matrix_income_nss)
local ncols = colsof(matrix_income_nss)

forval i = 1/`nrows' {
    forval j = 1/`ncols' {

        local val : di %12.1f matrix(matrix_income_nss[`i', `j'])
		file write myfile " \setcellincomenss$scenario{`i'}{`j'}{`val'} "
    }
    file write myfile " \ "
}

// Close the file handle
file close myfile

putexcel set "$intermediate/matrix_income_nss" , replace
putexcel A1 = matrix(matrix_income_nss), names
putexcel save

********************************************************************************************
* 3. Table \label{table:ginis_yd}: Gini indexes of household per capita income
********************************************************************************************
/*
We estimate the impact of SS on inequality using DASP package: http://dasp.ecn.ulaval.ca/
Note: ceq_income_concepts.dta contains individuals data so the option hsize() does not apply.
*/

*************************************************************************************************************
*3.1. Life-cycle income
*************************************************************************************************************
* 3.1.1. Life cycle with consumption smoothing
*************************************************************************************************************
clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear
*svyset [iw=weight]
svyset hhid [pweight=weight], vce(linearized) singleunit(missing)
svydes

dineq total_ss_pc_lc total_nss_pc_lc ,  index(gini) // Difference = Ginis without minus with pensions
ereturn list
matrix matrix_lccs = e(mmss)[1,1]\e(mmss)[3,1]\e(mmss)[5,1]\e(mmss)[6,1]

*************************************************************************************************************
* 3.1.2. Life cycle with non-responsive savings
*************************************************************************************************************

g total_nss_cc_pc_lc = total_nss_pc_lc - capital_nss_pc_lc + capital_ss_pc_lc
label var total_nss_cc_pc_lc "Income from savings WITHOUT SS and constant capital, per capita"

dineq total_ss_pc_lc total_nss_cc_pc_lc ,  index(gini) // Difference = Ginis without minus with pensions
ereturn list
matrix matrix_lccc = e(mmss)[1,1]\e(mmss)[3,1]\e(mmss)[5,1]\e(mmss)[6,1]

*************************************************************************************************************
* 3.1.3. Life cycle with lifetime income
*************************************************************************************************************

g lifetime_income_nss_pc = lifetime_income_hh/hsize
label var lifetime_income_nss_pc "Households per capita lifetime income without ss"
g ssw_pc = ssw/hsize
label var ssw_pc "Households per capita social security wealth"
g lifetime_income_ss_pc =  (lifetime_income_hh + ssw)/hsize  
label var lifetime_income_ss_pc "Households per capita lifetime income with ss"

dineq lifetime_income_ss_pc lifetime_income_nss_pc,  index(gini) // Difference = Ginis without minus with pensions
matrix  matrix_lifetime = e(mmss)[1,1]\e(mmss)[3,1]\e(mmss)[5,1]\e(mmss)[6,1] 

*************************************************************************************************************
*3.2. CEQ income
*************************************************************************************************************

dineq total_ss_pc_ceq total_nss_pc_ceq , index(gini)    // Difference = Ginis without minus with pensions
matrix matrix_ceq = e(mmss)[1,1]\e(mmss)[3,1]\e(mmss)[5,1]\e(mmss)[6,1] 

*3.3. Table \label{table:ginis_yd}
*************************************************************************************************************
matrix matrix_dineq_yd = (matrix_ceq , matrix_lccs, matrix_lccc, matrix_lifetime )
matrix rownames matrix_dineq_yd = "Gini with pensions system" "Gini without pensions system" "Difference" "Standard errors" //"t statistic"
matrix colnames matrix_dineq_yd = "CEQ-PGT" "LC CS" "LC constant capital" "LC lifetime"
matrix list matrix_dineq_yd
// Open a file handle and write the macro
file open myfile using "$results/macros_dineq_yd_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(matrix_dineq_yd)
local ncols = colsof(matrix_dineq_yd)

forval i = 1/`nrows' {
    forval j = 1/`ncols' {

        local val : di %12.3f matrix(matrix_dineq_yd[`i', `j'])
		file write myfile " \setcelldineqyd$scenario{`i'}{`j'}{`val'} "
    }
    file write myfile " \ "
}

// Close the file handle
file close myfile

putexcel set "$intermediate/matrix_dineq_yd" , replace
putexcel A1 = matrix(matrix_dineq_yd), names
putexcel save

********************************************************************************************
* 4. Figure \label{figure:income_pensioners}: Income with and without pensions \\ (Pensioners households per capita income) 
********************************************************************************************

* Note: We dropped this graph from the document in march 2024.

clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear

collapse (mean) total_nss_pc_ceq total_ss_pc_ceq total_nss_pc_lc total_ss_pc_lc, by(hhid)

g line45 = total_ss_pc_ceq
label var line45 "45º line"
label var total_nss_pc_ceq "ceq"
label var total_nss_pc_lc "life cycle"

* twoway(scatter total_nss_pc_ceq total_ss_pc_ceq ) (scatter total_nss_pc_lc total_ss_pc_lc ) (line line45 total_ss_pc_ceq)  if gross_pensions_ceq > 0 & total_ss_pc_lc < 1000 & total_ss_pc_ceq < 1000 , ytitle(Income without pensions) xtitle(Income with pensions) // hh earning less than 1000

*twoway(scatter total_nss_pc_ceq total_ss_pc_ceq, msymbol(X)) (scatter total_nss_pc_lc total_ss_pc_lc , msymbol(Oh)) (line line45 total_ss_pc_ceq)  ,  ytitle(Income without pensions) xtitle(Income with pensions)

twoway(scatter total_nss_pc_ceq total_ss_pc_ceq, msymbol(X)) (scatter total_nss_pc_lc total_ss_pc_lc , msymbol(Oh)) (line line45 total_ss_pc_ceq) if total_ss_pc_ceq<10000 ,  ytitle(Income without pensions) xtitle(Income with pensions)
count if total_ss_pc_ceq>=10000
graph export "$results/income_pensioners_$scenario.pdf" , replace

********************************************************************************************
* 5. Figure \label{figure:crowding_out_histogram}: The crowding out of capital income
********************************************************************************************
clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear

merge 1:1 id using "$intermediate/life_cycle_ceq_2017_$scenario.dta", keepusing(weight)
drop if _m!=3
drop _m

g income_pc_volunt_SS_NSS = (income_vsc_SS_pc - income_vsc_NSS_pc)/total_nss_pc_lc
label var income_pc_volunt_SS_NSS "Capital income with minus without SS/total disposable income without SS"

sum income_pc_volunt_SS_NSS [fw=weight], d

capture drop ptile
xtile ptile = income_pc_volunt_SS_NSS [fw=weight],nq(100)
histogram income_pc_volunt_SS_NSS if ptile >1 & ptile <100
graph export "$results/crowding_out_histogram_$scenario.pdf" , replace

********************************************************************************************
* 6. Table \label{table:crowding_out}: Proportion of individuals experiencing crowding out
********************************************************************************************
clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear
g crowding_out = (income_vsc_NSS_pc > income_vsc_SS_pc)
g no_crowding_out = (income_vsc_NSS_pc == income_vsc_SS_pc)
g crowding_in = (income_vsc_NSS_pc < income_vsc_SS_pc)
g partial_crowding_out = (income_vsc_NSS_pc > income_vsc_SS_pc) & (income_vsc_NSS_pc <= income_vsc_SS_pc +  pensions_pc_lc) 

g over_crowding_out = (income_vsc_NSS_pc > income_vsc_SS_pc) & (income_vsc_NSS_pc > income_vsc_SS_pc +  pensions_pc_lc) 

*table [fw=weight], statistic(mean crowding_in no_crowding_out crowding_out  partial_crowding_out over_crowding_out) // {table:crowding out}

foreach var of varlist crowding_in no_crowding_out crowding_out  partial_crowding_out over_crowding_out {
sum `var' [fw=weight], d
matrix matrix_`var' = 100*r(mean)
}

matrix crowding_out = matrix_crowding_in
foreach var of varlist /*crowding_in*/ no_crowding_out crowding_out partial_crowding_out over_crowding_out {
matrix crowding_out=( crowding_out \ matrix_`var' )
}

matrix rownames crowding_out = "Crowding-in" "No crowding-in" "Total crowding-out, of which" "Partial crowding-out" "Over crowding-out"
matrix colnames crowding_out = "Proportion"
matrix list crowding_out  
// Open a file handle and write the macro
file open myfile using "$results/macros_crowding_out_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(crowding_out)
local ncols = colsof(crowding_out)

forval i = 1/`nrows' {
    forval j = 1/`ncols' {

        local val : di %12.1f matrix(crowding_out[`i', `j'])
		file write myfile " \setcellcrowdout$scenario{`i'}{`j'}{`val'} "
    }
    file write myfile " \ "
}

// Close the file handle
file close myfile


*********************************************************************************************************************
* 7. Social security wealth.  Table {table:ssw}: Social security wealth to lifetime income ratios (in percentage)
*********************************************************************************************************************

clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear

g lifetime_income_nss_pc = lifetime_income_hh/hsize
label var lifetime_income_nss_pc "Households per capita lifetime income without ss"

g ssw_income = ssw / lifetime_income_hh
label var ssw_income "Households social security wealth to lifetime income ratio"

xtile pct = lifetime_income_nss_pc [fw=weight], nq(5) 
*table pct [fw=weight], statistic(mean ssw_income) statistic(median ssw_income) statistic(p25 ssw_income) statistic(p75 ssw_income) statistic(sd ssw_income)

forvalues quintiles = 1(1)5 {	
	dis `quintiles'
	preserve
	keep if pct == `quintiles'
	sum ssw_income [fw=weight], d
	restore
	matrix matrix_`quintiles' = r(mean)*100,r(p25)*100,r(p75)*100,r(sd)*100
	}

	matrix matrix_ssw = matrix_1

	forvalues quintiles = 2(1)5 {	
matrix matrix_ssw = matrix_ssw \ matrix_`quintiles'
	}
	
	sum ssw_income [fw=weight], d
	matrix matrix_total = r(mean)*100,r(p25)*100,r(p75)*100,r(sd)*100
	
matrix matrix_ssw = matrix_ssw \ matrix_total 

matrix rownames matrix_ssw = "Poorest quintile" "Quintile 2" "Quintile 3" "Quintile 4" "Richest quintile" "Total"
matrix colnames matrix_ssw = "Mean" "p25" "p75" "sd"
matrix list matrix_ssw

// Open a file handle and write the macro
file open myfile using "$results/macros_ssw_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(matrix_ssw)
local ncols = colsof(matrix_ssw)

forval i = 1/`nrows' {
    forval j = 1/`ncols' {

        local val : di %12.1f matrix(matrix_ssw[`i', `j'])
		file write myfile " \setcellssw$scenario{`i'}{`j'}{`val'} "
    }
    file write myfile " \ "
}

// Close the file handle
file close myfile


putexcel set "$intermediate/matrix_ssw" , replace
putexcel A1 = matrix(matrix_ssw), names
putexcel save

********************************************************************************************
* 8. Poverty 
********************************************************************************************

clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear
g line45 = total_ss_pc_ceq
label var line45 "45º line"
label var total_nss_pc_ceq "ceq"
label var total_nss_pc_lc "life cycle"

merge 1:1 hhid memb_no using "$intermediate/UY CEQ database 2017.dta", keepusing(pline_mod weight)
drop if _merge == 2
drop _m
replace pline_mod = pline_mod/1000


*Poor according to official poverty line in all four income concepts
g poor_nss_ceq = total_nss_pc_ceq < pline_mod
label var poor_nss_ceq "=1 if poor according to CEQ without SS"
g poor_ss_ceq = total_ss_pc_ceq < pline_mod
label var poor_ss_ceq "=1 if poor according to CEQ with SS"
g poor_nss_lc = total_nss_pc_lc < pline_mod
label var poor_nss_lc "=1 if poor according to LC without SS"
g poor_ss_lc = total_ss_pc_lc < pline_mod
label var poor_ss_lc "=1 if poor according to LC with SS"

tab poor_nss_ceq poor_ss_ceq [fw=weight], cell matcell(aux_poor_ceq)
matrix poor_ceq = (100*(aux_poor_ceq[1,2] + aux_poor_ceq[2,2]) / (aux_poor_ceq[1,1]+aux_poor_ceq[1,2]+aux_poor_ceq[2,1]+aux_poor_ceq[2,2])) , (100*(aux_poor_ceq[2,1] + aux_poor_ceq[2,2]) / (aux_poor_ceq[1,1]+aux_poor_ceq[1,2]+aux_poor_ceq[2,1]+aux_poor_ceq[2,2])) , (100*(aux_poor_ceq[2,1] + aux_poor_ceq[2,2] - aux_poor_ceq[1,2] - aux_poor_ceq[2,2]) / (aux_poor_ceq[1,1]+aux_poor_ceq[1,2]+aux_poor_ceq[2,1]+aux_poor_ceq[2,2]))

tab poor_nss_lc poor_ss_lc [fw=weight], cell matcell(aux_poor_lc)
matrix poor_lc = (100*(aux_poor_lc[1,2] + aux_poor_lc[2,2]) / (aux_poor_lc[1,1]+aux_poor_lc[1,2]+aux_poor_lc[2,1]+aux_poor_lc[2,2])) , (100*(aux_poor_lc[2,1] + aux_poor_lc[2,2]) / (aux_poor_lc[1,1]+aux_poor_lc[1,2]+aux_poor_lc[2,1]+aux_poor_lc[2,2])) , (100*(aux_poor_lc[2,1] + aux_poor_lc[2,2] - aux_poor_lc[1,2] - aux_poor_lc[2,2]) / (aux_poor_lc[1,1]+aux_poor_lc[1,2]+aux_poor_lc[2,1]+aux_poor_lc[2,2]))

matrix poor_all = poor_ceq \ poor_lc

matrix rownames poor_all = "CEQ-PGT" "Life-cycle"
matrix colnames poor_all = "With pensions (1)" "Without pensions (2)" "Difference (3)=(2)-(1)" 
matrix list poor_all

// Open a file handle and write the macro
file open myfile using "$results/macros_poverty_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(poor_all)
local ncols = colsof(poor_all)

forval i = 1/`nrows' {
    forval j = 1/`ncols' {

        local val : di %12.1f matrix(poor_all[`i', `j'])
		file write myfile " \setcellpoor$scenario{`i'}{`j'}{`val'} "
    }
    file write myfile " \ "
}

// Close the file handle
file close myfile

tab poor_nss_ceq poor_ss_ceq [fw=weight], cell matcell(aux_transition_ceq)
matrix transition_ceq = (100*(aux_transition_ceq[1,2]) / (aux_transition_ceq[1,1]+aux_transition_ceq[2,1]+aux_transition_ceq[1,2]+aux_transition_ceq[2,2])) , (100*(aux_transition_ceq[2,1]) / (aux_transition_ceq[1,1]+aux_transition_ceq[2,1]+aux_transition_ceq[1,2]+aux_transition_ceq[2,2])) , (100*(aux_transition_ceq[2,1] - aux_transition_ceq[1,2]) / (aux_transition_ceq[1,1]+aux_transition_ceq[2,1]+aux_transition_ceq[1,2]+aux_transition_ceq[2,2])) 

tab poor_nss_lc poor_ss_lc [fw=weight], cell matcell(aux_transition_lc)
matrix transition_lc = (100*(aux_transition_lc[1,2]) / (aux_transition_lc[1,1]+aux_transition_lc[2,1]+aux_transition_lc[1,2]+aux_transition_lc[2,2])) , (100*(aux_transition_lc[2,1]) / (aux_transition_lc[1,1]+aux_transition_lc[2,1]+aux_transition_lc[1,2]+aux_transition_lc[2,2])) , (100*(aux_transition_lc[2,1] - aux_transition_lc[1,2]) / (aux_transition_lc[1,1]+aux_transition_lc[2,1]+aux_transition_lc[1,2]+aux_transition_lc[2,2])) 

matrix transition_all = transition_ceq \ transition_lc

* Column names: (1) Transitions into poverty, (2) Transitions out of poverty, (3) Poverty without minus with pensions.

matrix colnames transition_all = "(1)" "(2)" "(3)=(2)-(1)" 
matrix rownames transition_all = "CEQ-PGT" "Life-cycle" 
matrix list transition_all

// Open a file handle and write the macro
file open myfile using "$results/macros_transition_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(transition_all)
local ncols = colsof(transition_all)

forval i = 1/`nrows' {
    forval j = 1/`ncols' {

        local val : di %12.1f matrix(transition_all[`i', `j'])
		file write myfile " \setcelltransition$scenario{`i'}{`j'}{`val'} "
    }
    file write myfile " \ "
}

// Close the file handle
file close myfile


/*
*False poor: poor according to CEQ without SS but not according to Life Cycle without SS
g false_poor = poor_nss_ceq==1 & poor_nss_lc!=1
label var false_poor "=1 if poor without SS in CEQ and not poor without SS in LC"

tab false_poor [fw=weight]
tab false_poor if poor_nss_ceq==1 [fw=weight]
tab false_poor lifted_by_ss_ceq [fw=weight]
*/


**************************************************************************************************************
* 9. Table \label{table:ginis_yd_ym}: Gini indexes of household per capita 2017 market and disposable income
**************************************************************************************************************
 
* We estimate the impact of SS on inequality using DASP package: http://dasp.ecn.ulaval.ca/

* Note: ceq_income_concepts.dta contains individuals data so the option hsize() does not apply.

*************************************************************************************************************
* 9.1. Life-cycle income
*************************************************************************************************************
* 9.1.1. Disposable income
*************************************************************************************************************
clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear
*svyset [iw=weight]
svyset hhid [pweight=weight], vce(linearized) singleunit(missing)
svydes

dineq total_ss_pc_lc total_nss_pc_lc ,  index(gini) // Difference = Ginis without minus with pensions
ereturn list
matrix matrix_lc = e(mmss)[1,1]\e(mmss)[3,1]\e(mmss)[5,1]\e(mmss)[6,1]

*************************************************************************************************************
* 9.1.2. Market income
*************************************************************************************************************

dineq total_ym_ss_pc_lc total_ym_nss_pc_lc ,  index(gini) // Difference = Ginis without minus with pensions
ereturn list
matrix matrix_ym_lc = e(mmss)[1,1]\e(mmss)[3,1]\e(mmss)[5,1]\e(mmss)[6,1]

*************************************************************************************************************
* 9.2. CEQ income
*************************************************************************************************************
* 9.2.1. Disposable income
*************************************************************************************************************
dineq total_ss_pc_ceq total_nss_pc_ceq , index(gini)    // Difference = Ginis without minus with pensions
matrix matrix_ceq = e(mmss)[1,1]\e(mmss)[3,1]\e(mmss)[5,1]\e(mmss)[6,1] /* \ ACÁ FALTA EL T*/

*************************************************************************************************************
* 9.2.2. Market income
*************************************************************************************************************
dineq total_ym_ss_pc_ceq total_ym_nss_pc_ceq , index(gini)    // Difference = Ginis without minus with pensions
matrix matrix_ym_ceq = e(mmss)[1,1]\e(mmss)[3,1]\e(mmss)[5,1]\e(mmss)[6,1] /* \ ACÁ FALTA EL T*/

*************************************************************************************************************
* 9.3. Table \label{table:ginis_yd_ym}
*************************************************************************************************************
matrix matrix_dineq = (matrix_ym_ceq , matrix_ym_lc , matrix_ceq , matrix_lc )
matrix rownames matrix_dineq = "Gini with pensions system" "Gini without pensions system" "Difference" "Standard errors" //"t statistic"
matrix colnames matrix_dineq = "CEQ-PGT ym" "Life cycle ym" "CEQ-PGT yd" "Life cycle yd"
matrix list matrix_dineq
// Open a file handle and write the macro
file open myfile using "$results/macros_dineq_yd_ym_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(matrix_dineq)
local ncols = colsof(matrix_dineq)

forval i = 1/`nrows' {
    forval j = 1/`ncols' {

        local val : di %12.3f matrix(matrix_dineq[`i', `j'])
		file write myfile " \setcelldineqydym$scenario{`i'}{`j'}{`val'} "
    }
    file write myfile " \ "
}

// Close the file handle
file close myfile

putexcel set "$intermediate/matrix_dineq_yd_ym" , replace
putexcel A1 = matrix(matrix_dineq), names
putexcel save

********************************************************************************************
* 10. Growth incidence curves 
********************************************************************************************
*Income changes comparing CEQ with and without SS

clear all 
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear

quantiles total_ss_pc_ceq [fw=weight], gen(twentiles_ss) n(20) 

collapse total_ss_pc_ceq total_nss_pc_ceq [fw=weight], by(twentiles_ss)
g difference_ceq = 100*(total_nss_pc_ceq - total_ss_pc_ceq )/total_ss_pc_ceq 

sort twentiles_ss
save "$intermediate/gic_ceq.dta", replace

*Income changes comparing LC with and without SS

use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear

quantiles total_ss_pc_lc [fw=weight], gen(twentiles_ss) n(20) 

collapse total_ss_pc_lc total_nss_pc_lc [fw=weight], by(twentiles_ss)
g difference_lc = 100*(total_nss_pc_lc - total_ss_pc_lc )/total_ss_pc_lc 

sort twentiles_ss
merge 1:1 twentiles_ss using "$intermediate/gic_ceq.dta"
drop _merge

label var difference_ceq "CEQ-PGT"
label var difference_lc "LC"

twoway (line difference_ceq twentiles_ss, msymbol(X) yaxis(1)) (line difference_lc twentiles_ss, msymbol(0) yaxis(1)), ytitle(Percentage change) xtitle(Twentiles of per capita household income with SS) yline(0) legend(pos(6) rows(1))

graph export "$results/gic_$scenario.pdf" , replace


*******************************************************************************************
* 11. Accumulated savings
********************************************************************************************
use "$intermediate/life_cycle_ceq_2017_$scenario.dta", clear

keep assets_vsc_NSS_pc assets_vsc_SS_pc assets_vsb_pc weight
g assets_NSS_pc = assets_vsc_NSS_pc + assets_vsb_pc
g assets_SS_pc = assets_vsc_SS_pc + assets_vsb_pc
sum assets_NSS_pc [fw=weight], d
matrix avg_NSS = r(mean)
sum assets_SS_pc [fw=weight], d
matrix avg_SS = r(mean)
    
matrix dif = avg_NSS - avg_SS
matrix list dif

matrix all3 = avg_NSS \ avg_SS \ dif

matrix rownames all3 = "avg_NSS" "avg_SS" "(3)=(1)-(2)"
matrix colnames all3 = "$scenario"
matrix list all3

// Open a file handle and write the macro
file open myfile using "$results/macros_savings_$scenario.tex", write replace

// Write the LaTeX code for the table content with custom labels
local nrows = rowsof(all3)
local ncols = colsof(all3)

forval i = 1/`nrows' {
	forval j = 1/`ncols' {

    	local val : di %12.1f matrix(all3[`i', `j'])
   	 file write myfile " \setcellsavings$scenario{`i'}{`j'}{`val'} "
	}
	file write myfile " \ "
}

// Close the file handle
file close myfile

