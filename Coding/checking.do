/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/
* Checking some results

*********************************************************************************************************************
* 1. Densities of contribution in several databases
*********************************************************************************************************************
* 1.1. The densities of contribution should be the same computed using the binaries contributes_ech and contributes_sim and the continuous prob_contribut_ech. It must be the same in hhs.dta and in panel_active.dta in 2017. 

use "$intermediate/hhs.dta", clear
sum contributes_ech prob_contribut_ech  if  age >=20 & age <=65 & year == 2017
use "$intermediate/panel_active.dta", clear
sum contributes_ech prob_contribut_ech contributes_sim  if  age >=20 & age <=65 & year == 2017

* 1.2. The densities of contribution in panel_active_retired_*.dta should be similar but not necessarily equal to the previous estimations. The variable contributes_sim is modified in panel_active_retired.do due to the imputation of contribution status to individuals who are retired in 2017 according to the HH survey. The imputation may increase densities for some individuals and reduce them for other individuals.

use "$intermediate/panel_active_retired_3000hhs.dta", clear
sum contributes_sim  if  age >=20 & age <=65 & year == 2017

* Conclusion: we have densities in the order of 58% in all these databases in 2017, which is the density we computed in the HH survey. 

*********************************************************************************************************************
* 2. Age earnings and participation profiles
*********************************************************************************************************************
* In our analysis, we computed age profiles using panel social security data. But we can also compute age profiles from cross section household survey data. Here we compare both sources.  

* 2.1. Age earnings profiles

* (i) Using hh survey data.
use "$path/Data/Inputs/ECH/ECH 2017/ph17_v2.dta", clear
rename PT4 wages_ech
rename e27 age
label var age "Age"
keep if wages_ech > 0
table age if age >= 20 & age <=65  , statistic(mean wages_ech)  // STATA 17
collapse wages_ech  , by(age)
twoway (line wages_ech age) if age >= 20 & age <= 65
sort age
save "$intermediate/checking_section 2_1.dta", replace

* (ii) Using SS data.

use "$intermediate/ss_annual_2022.dta", clear
keep if year == 2017
keep if wages_bps > 0
g wages_bps_after_cont = 0.85 * wages_bps   // Assuming wages_bps are nominal (before employee contributions)
table age  if age >=20 & age <=65  , statistic(mean wages_bps_after_cont)  // STATA 17
collapse wages_bps_after_cont  , by(age)
twoway (line wages_bps_after_cont age) if age >= 20 & age <= 65
sort age

merge 1:1 age using "$intermediate/checking_section 2_1.dta"
keep if _merge == 3
drop _merge

twoway (line wages_ech age) (line wages_bps_after_cont age) if age >= 20 & age <= 65

* Results: both curves are quite similar.

* 2.2. Age participation profiles

* (i) Using hh survey data.

use "$intermediate/hhs.dta", clear
table age  if age >=20 & age <=65  , statistic(mean contributes_ech)  // STATA 17
collapse contributes_ech  , by(age)
twoway (line contributes_ech age) if age >= 20 & age <= 65
sort age
save "$intermediate/checking_section 2_2.dta", replace

* (ii) Using SS data.

use "$intermediate/ss_annual_2022.dta", clear
rename cotiza contributes_ss
keep if year == 2017
table age  if age >=20 & age <=65  , statistic(mean contributes_ss)  // STATA 17
collapse contributes_ss  , by(age)
twoway (line contributes_ss age) if age >= 20 & age <= 65
sort age

merge 1:1 age using "$intermediate/checking_section 2_2.dta"
keep if _merge == 3
drop _merge

twoway (line contributes_ech age) (line contributes_ss age) if age >= 20 & age <= 65

* Results: The age participation profiles are quite different. Some potential explanations: (i) ss data contains actual contributions and ech is a report in an interview; (ii) the population is different: ech represents the whole population and ss data comes from the BPS program (accounting for about 80% of the population, but not taken at random).

*********************************************************************************************************************
* 3. Number of members of hh computed in different ways
*********************************************************************************************************************
* 3.1. Comparing mofhh generated using member and memb_no
*********************************************************************************************************************

use "$intermediate/life_cycle.dta", clear
keep if year == 2017

bys hhid: egen mofhh2017_a = sum(member) 
bys hhid: egen mofhh2017_b = max(memb_no)
 sum mofhh2017_a mofhh2017_b
g discrepancies = mofhh2017_a != mofhh2017_b 
sum discrepancies
order id hhid mofhh2017_? 
edit if mofhh2017_a != mofhh2017_b 

* In about 0.1% of cases there are discrepancies due to missing individuals. In these cases, mofhh2017_b > mofhh2017_a because members with memb_no lower than the maximum were missed. I think the discrepancies are not relevant, so I leave it here. 

*********************************************************************************************************************
* 3.2. Comparing mofhh_c and hsize
*********************************************************************************************************************

use "$intermediate/life_cycle_section6_aux.dta", clear
keep if year==2017
merge 1:1 hhid memb_no using "$intermediate/UY CEQ database 2017.dta", keepusing(hsize)

g dif_hsize = mofhh_c - hsize 
sum dif_hsize

*********************************************************************************************************************
* 4. Labor income and pensions in (i) ceq, and (ii) our panel databases 
*********************************************************************************************************************

* We compare labor income variables in the databases hhs.dta and life_cycle_section5.dta, labor_variable and my_labor_in, respectively. These two variables should be similar in 2017. The former is income reported by individuals who were working in 2017. The latter is the sum of simulated labor income of (i) individuals who contributed and (ii) individuals who evaded (worked but did not contribute) in 2017.  The idea of the analysis that follows is to verify this is the case.

use  "$intermediate/life_cycle_section5.dta", clear
keep if year == 2017
merge 1:1 id using "$intermediate/hhs.dta"
keep if _merge == 3 
drop _merge
sum labor_variable if age >= 20
local ceq = r(mean) 
sum my_labor_in if age >= 20
dis `ceq'/r(mean)
* Results: the average labor income en 2017 is about 1.6% larger in ceq than in our panel dataset. 

sum labor_variable my_labor_in if age >= 20, d

* Results: The distribution is very similar, as it should be. Nevertheless, there are some differences I check:
g xx = labor_variable - my_labor_in if age >= 20
sum xx
sum my_labor_in labor_variable pen_ceq_in if xx > 0 &  xx != .
sum my_labor_in labor_variable pen_ceq_in if xx < 0 &  xx != .
/* If the difference is positive then my_labor_in==0 and pen_ceq_in>0, and if negative then labor_variable==0. In the former case, we set my_labor_in to zero because our simulations do not allow that individuals receive pensions and labor income simultaneously. In the latter case, individuals who reported no labor income and no pensions may yet have positive income in our simulations.  */
g dif = (xx > 0 | xx < 0) & xx != .
egen num_dif = total(dif)
sum num_dif
dis r(mean)/r(N). // There are differences in about 3% of the observations.

*********************************************************************************************************************
* 5. Comparing distribution of income in life-cycle and ceq approaches
*********************************************************************************************************************
use "$intermediate/ceq_income_concepts.dta", clear 

dineq yd_income_pc_SS yd_pgt_pc_SS, index(gini) 
dineq yd_income_pc_SS2 yd_pgt_pc_SS, index(gini) 
dineq labor_pc_hh_c yd__liquido_ech, index(gini)
sum labor_pc_hh_c yd__liquido_ech, d

*muy a lo bruto elimino a los dos mayores labor_pc_hh_c

drop if labor_pc_hh_c>4000

sum labor_pc_hh_c yd__liquido_ech, d
g percentage = labor_pc_hh_c / yd__liquido_ech
sum percentage, d

revisar esto último, mirar individuos particulares y tratar de entender si es la comparación correcta
revisar qué es exactamente lo que hay en yd__liquido_ech (qué es lo que dice en teoría el ceq sobre esto)
responder dudas en el documento

ceq qué ingreso del capital usa? solamente el de activos o también debería incluir pasivos?
tomar de ceq los conceptos que nos faltaron a nosotros en life_cycle

*********************************************************************************************************************
* 5. Verifying that pc variables are invariant within households in ceq
*********************************************************************************************************************
use "$intermediate/ceq_income_concepts.dta", clear 
keep hhid memb_no CEQ_NSS_pc CEQ_SS_pc i_lab_pc cap_income_pc net_pension_income_pc other_income_pc
foreach var in CEQ_NSS_pc CEQ_SS_pc i_lab_pc cap_income_pc net_pension_income_pc other_income_pc {
	bys hhid: egen sd_`var' = sd(`var')
	sum sd_`var'
	if r(max) > 10^-6 {
		dis "Non-invariant pc variable `var' within households"
		ABORT
	}
}

