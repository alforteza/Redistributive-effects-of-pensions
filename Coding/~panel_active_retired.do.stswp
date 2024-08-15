/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/

* Imputing labor income that produced pensions declared to ECH

/* We use a reduced form regression model to compute pensions as a function of average labor income in non-retired individuals to ECH.
(We use the pension declared to ECH and overwrite the imputed pension)
With this model, we compute average income of individuals who were already retired in ECH.
Finally, we use the age earnings profiles computed with SS data to generate the labor history of these individuals that is compatible with the declared pension.

Outline
1. Correcting age and year of retirement for individuals who declare receiving a pension to ech
2. Computing average labor income 
3. Imputing contribution status to individuals who are retirees in ECH
4. Imputing labor income using pensions declared to the ECH
	4.1. Imputing my_labor_in_declared
	4.2. Imputing my_labor_in_potential_panel
	4.3. Checking imputation of labor income to retirees in 2017
	4.4. Substituting the simulated variable pen_in for pensions reported to the ECH
5. Computing contributions
6. Evasion
7. Saving the database and merging ids (hhid memb_no)
*/

use "$intermediate/pensions.dta", clear
sort id year
merge m:1 id using "$intermediate/hhs.dta", keepusing(groups pen_ceq_in female)
keep if _merge == 3
drop _merge
sort groups age
merge m:1 groups age using "$intermediate/ss.dta", keepusing(increase_my_labor_in)
drop _merge
sort id age
merge 1:1 id age using "$intermediate/panel_active.dta", keepusing(prob_evade_panel evades_sim)
drop _merge

**************************************************************************
* 0. Checking globals
**************************************************************************

* Thresholds in law 16.713 updated to 2017

if $th5000 == . {
	dis "The threshold in law 16.713 is missing! See master.do"
	ABORT
}

* Comissions of managers of individual accounts (AFAP) + cost of insurance (seguro colectivo de invalidez y muerte)

if $comission == . {
	dis "The comission and cost of insurance is missing! See master.do"
	ABORT
}
**************************************************************************
* 1. Correcting age and year of retirement for individuals who declare receiving a pension to ech. We drop from pen_ceq_in pensions received by individuals aged 25 and less and save these pensions as non contributory pensions (pen_non_cont_in).
**************************************************************************
* 1.1. Age and year of retirement
**************************************************************************
g aux1 = age if year == 2017
bys id: egen age_2017 = min(aux1)
drop aux1

g aux1 = year if age == min(age_2017,age_ret) & pen_ceq_in > 0 & pen_ceq_in!=.
bys id: egen aux2 = min(aux1)
replace date_ret = aux2 if pen_ceq_in > 0 & pen_ceq_in!=.
drop aux?
	
g aux1 = age  if year == date_ret
bys id: egen aux2 = min(aux1)
replace age_ret = aux2
drop aux? 

* Checking: individuals who report receiving a pension to the hhs in 2017 have an age of retirement <= age they had in 2017

sum id  if (age_2017 < age_ret) & pen_ceq_in > 0 & pen_ceq_in !=.
if r(mean) != . {
dis "There is an individual receiving a pension according to ECH who is not retired in 2017"
ABORT
}

	drop age_2017
**************************************************************************
* 1.2. Pensions received by children and young people
**************************************************************************
	
* codebook id if age_ret <= 25	// Relatively few cases: 159 individuals out of 116.334
	
g pen_non_cont_in	= 0
replace pen_non_cont_in = pen_ceq_in if age_ret <=25 & age <= 25
label var pen_non_cont_in "Non-contributory pensions"

replace   pen_ceq_in = 0 if age_ret <=25

**************************************************************************
* 2. Computing (i) average labor income, (ii) # years with positive contributions and (iii) entitlements, IN THE SIMULATION
**************************************************************************

* (i) average labor income
g aux = my_labor_in_declared if my_labor_in_declared >0
bys id: egen ali = mean(aux)
label var ali "Average labor income"
capture drop aux

* (ii) # years with positive contributions in the simulation
bys id: g aux = contributes_cum if age < min(age_ret,65)
bys id: egen contributes_tot= max(aux)

label var contributes_tot "# years with positive contributions"
capture drop aux

* (iii) entitlements
capture drop aux1
g aux1 = (pen_in > 0 & pen_in != . )
bys id: egen entitled = max(aux1)
label var entitled "= 1 if eligible for a pension (based on simulated years of contribution)"
drop aux1

save "$intermediate/panel_active_retired_section2.dta", replace

**************************************************************************
* 3. Imputing contribution status to individuals who are retirees in ECH
**************************************************************************

/* We compute de # of years contributed by individuals entitled to a pension in each group and impute this number to individuals who reported having a pension in the ECH. So we correct the variables contributes_tot, contributes_sim and contributes_cum for individuals who reported having a pension in the ECH. This imputation is incomplete in a few cases when individuals retire very young according to ECH and do not have enough time to accumulate the # of years of contribution of their group (contributes_tot). In these cases, contributes_cum < contributes_tot, and contributes_sim == 1 if age >=20 & age < age_ret. The truncated cases have truncated == 1, where: g truncated =  (age_ret - contributes_tot < 20 ).
*/

use "$intermediate/panel_active_retired_section2.dta", clear

*	sum my_labor_in_potential_panel if year==2017
	
foreach categ in  "female_pu" "male_pu" "female_pr" "male_pr" {
	forvalues x = 1(1)5 {
	
		use	"$intermediate/panel_active_retired_section2.dta", clear
		
/*		
*		Uncomment for computing one group
		keep if groups == "female_puQ5"
		keep if groups == "female_prQ1"
*/		
		keep if groups== "`categ'Q`x'"
		
		tsset id age

		cap drop aux?

		egen aux1 = mean(contributes_tot) if entitled==1  // Average years of contribution of individuals in group xx who fulfill conditions to receive a pension
		egen aux2 = mean(aux1)   // #years contributed in the simulation of individuals in categ'Q`x' who are entitled to a pension 
		replace contributes_tot = floor(aux2) if pen_ceq_in > 0 & pen_ceq_in != .

		/* We correct contributes_sim to match the corrected contributes_tot. We assume the individual contributes aux2 years before age_ret. 
		*/
		g aux3  = 0    		if  pen_ceq_in >0 
		replace aux3 =1     if age >= (age_ret - contributes_tot) & age <= (age_ret - 1)  & pen_ceq_in > 0 
		replace contributes_sim = aux3 if  pen_ceq_in > 0 & age >= 20
		drop aux?
		
		* We recalculate contributes_cum for individuals receiving a pension in 2017 using the imputed contributes_sim.

		bys id (age): replace contributes_cum=sum(contributes_sim) if  pen_ceq_in > 0 
		
		
		* Imputing the probability of contributing for retirees in ECH. Assumption: the probability is 1 in the years previous to retirement in which we imputed that contributes_sim = 1, and 0 in other years. 
		replace prob_contribut_panel = 0 if pen_ceq_in >0 
		replace prob_contribut_panel = 1 if pen_ceq_in >0 & contributes_sim ==1
		
	save "$intermediate/panel_active_retired_section3_`categ'Q`x'.dta", replace
	}
}		
	
	
use "$intermediate/panel_active_retired_section3_female_puQ1.dta", clear
foreach group in  				"female_puQ2"  "female_puQ3"  "female_puQ4"  "female_puQ5" /*
*/       			"male_puQ1"	"male_puQ2"  "male_puQ3"  "male_puQ4"  "male_puQ5" /*
*/					"female_prQ1" "female_prQ2"  "female_prQ3"  "female_prQ4"  "female_prQ5" /*
*/       			"male_prQ1"	"male_prQ2"  "male_prQ3"  "male_prQ4"  "male_prQ5" {

	dis "`group'"
	append using "$intermediate/panel_active_retired_section3_`group'.dta"
}
save "$intermediate/panel_active_retired_section3.dta", replace	
		
**************************************************************************
* 4. Imputing labor income using pensions declared to the ECH
**************************************************************************

foreach categ in  "female_pu" "male_pu" "female_pr" "male_pr" {
	forvalues x = 1(1)5 {
	
		use	"$intermediate/panel_active_retired_section3.dta", clear
		
/*		
*		Uncomment for computing one group
		keep if groups == "female_puQ3"
		keep if groups == "female_prQ1"
*/		
		keep if groups== "`categ'Q`x'"
		
		g iali = .
		label var iali "Imputed average labor income (using pensions in ECH)"

		g ilabincome =.
		label var ilabincome "Imputed labor income (using pensions in ECH)"

		g ailabincome = .
		
		
		tsset id age
		
**************************************************************************
* 4.1. Imputing my_labor_in_declared	
**************************************************************************	
	cap drop aux
	g aux = pen_in/ali if age == 70 & entitled==1 &   pen_in > 0  // see equation {eq:rr}
	sum aux, d
	replace iali = pen_ceq_in/r(p50)   // see equation {eq:iali}
	drop aux
	
	g aux1 = 1 if contributes_sim == 1 & pen_ceq_in > 0  // =1 for each year of contribution, for all people retired in 2017
	bys id: gen aux2= sum(aux1)  // years of contribution so far (partial sum) for each individual retired in 2017
	g aux3 = 1 if aux2== 1  // =1 in the first year of contribution for each individual retired in 2017
	xtset id age
	replace aux3 = l.aux3*(1+increase_my_labor_in) if aux2 > 1 & aux1 == 1  // 
	bys id: egen aux4 = total(aux3) 
	bys id: egen aux5 = total(aux1)  // Total years of contribution
	replace ilabincome = aux5*iali/aux4 if aux2==1  // see equation {eq:liprofile}
	replace ilabincome = l.ilabincome * (1+increase_my_labor_in) if  aux2 > 1 & aux1 == 1  // see equation {eq:liprofile}
	replace my_labor_in_declared = ilabincome if  pen_ceq_in > 0 
	replace my_labor_in_declared = 0 if my_labor_in_declared == . 

**************************************************************************
* 4.2. Imputing my_labor_in_potential_panel	
**************************************************************************
	
	replace my_labor_in_potential_panel = . 	if  pen_ceq_in > 0 
	replace my_labor_in_potential_panel = ilabincome if  pen_ceq_in > 0 & age == age_ret - 1
	
	g neg_age = -age
	sort id neg_age
	xtset id neg_age
	bys id: replace my_labor_in_potential_panel = l.my_labor_in_potential_panel /(l.increase_my_labor_in + 1) if  pen_ceq_in > 0 & age >19 & age < age_ret - 1
	drop neg_age
	
	tsset id age
	sort id age
 *    sum my_labor_in_potential_panel if year==2017
**************************************************************************
* 4.3. Checking imputation of labor income to retirees in 2017
**************************************************************************

	bys id: egen aux_ailabincome = mean(ilabincome) 
	replace ailabincome = aux_ailabincome 
	drop aux_ailabincome 
	g aux6 = (ailabincome-iali)/iali 
	sum aux6
	if abs(r(min))>10^-6 | abs(r(max))>10^-6 {
		dis "Wrong imputed labor income in `categ'Q`x'"
		ABORT
		}
	drop aux?
	
	capture drop aux1
	capture drop aux2
	bys id: egen aux1 = total(my_labor_in_declared)
	g aux2 = aux1 == 0 & pen_ceq_in >0     
	sum aux2
	if abs(r(mean))>10^-3 {
		dis "Some ECH pensioners do not have imputed declared labor income"
		ABORT
		}
	drop aux?

************************************************************************
* 4.4. Substituting pensions reported to the ech for the simulated pensions
************************************************************************

	replace pen_in = pen_ceq_in if age >= age_ret & pen_ceq_in > 0 & pen_ceq_in!=.
	
	replace pen_in = pen_ceq_in if pen_ceq_in == 0 & pen_ceq_in!=. & year <= 2017	
	
**************************************************************************
* 4.5. Saving computations in section 4
**************************************************************************
	
	save "$intermediate/panel_active_retired_section4_`categ'Q`x'.dta", replace
	}
}		
	
use "$intermediate/panel_active_retired_section4_female_puQ1.dta", clear
foreach group in  				"female_puQ2"  "female_puQ3"  "female_puQ4"  "female_puQ5" /*
*/       			"male_puQ1"	"male_puQ2"  "male_puQ3"  "male_puQ4"  "male_puQ5" /*
*/					"female_prQ1" "female_prQ2"  "female_prQ3"  "female_prQ4"  "female_prQ5" /*
*/       			"male_prQ1"	"male_prQ2"  "male_prQ3"  "male_prQ4"  "male_prQ5" {

	dis "`group'"
	append using "$intermediate/panel_active_retired_section4_`group'.dta"
}
save "$intermediate/panel_active_retired_section4.dta", replace		

*			sum my_labor_in_potential_panel if year==2017
	
* Checking: in 2017, non-retired individuals should have the same contributing status in the simulation as in the ECH 

use  "$intermediate/panel_active_retired_section4.dta", clear
		
merge m:1 id using "$intermediate/hhs.dta", keepusing(contributes_ech)
drop _merge
g aux1 = contributes_sim - contributes_ech if year == 2017 & age < age_ret
sum aux1
if abs(r(max))>10^-6 | abs(r(min)) > 10^-6 {
	dis "ERROR: contribution status in 2017 is not equal in the panel and in the ECH!"
	ABORT
}
drop aux? contributes_ech

**************************************************************************
* 5. 	Computing contributions 
**************************************************************************
use "$intermediate/panel_active_retired_section4.dta", clear

local th5000 = $th5000
g threshold1_aux = `th5000'  // This variable is only used for checking. 

matload pat , overwrite p("$intermediate") saving

replace tax_base_payg = 0.5*my_labor_in_declared/(1+pat[1,1]) if pen_ceq_in > 0  & my_labor_in_declared != . //Regime is assumed mixed with art. 8 
replace tax_base_payg = my_labor_in_declared/(1+pat[1,1]) - 0.5*`th5000' if my_labor_in_declared/(1+pat[1,1]) > `th5000' & pen_ceq_in > 0 & my_labor_in_declared != .   
replace tax_base_payg = `th5000' if my_labor_in_declared/(1+pat[1,1]) > 1.5*`th5000'  & pen_ceq_in > 0  & my_labor_in_declared != .

replace tax_base_ia =  0.5*my_labor_in_declared/(1+pat[1,1])   //Regime is assumed mixed with art. 8 
replace tax_base_ia = 0.5*`th5000' if my_labor_in_declared/(1+pat[1,1]) > `th5000' & pen_ceq_in > 0 & my_labor_in_declared != .  
replace tax_base_ia = my_labor_in_declared/(1+pat[1,1]) - `th5000' if my_labor_in_declared/(1+pat[1,1]) > 1.5*`th5000'  & pen_ceq_in > 0 & my_labor_in_declared != .
replace tax_base_ia = 2*`th5000' if my_labor_in_declared/(1+pat[1,1])  > 3*`th5000'   & pen_ceq_in > 0 & my_labor_in_declared != .

replace con_ia = 0 if pen_ceq_in > 0
replace con_ia = 0.15 * tax_base_ia if age < min(age_ret, 65) & pen_ceq_in > 0

replace con_payg = 0 if pen_ceq_in > 0
replace con_payg =  0.15 *tax_base_payg if  age < min(age_ret, 65) & pen_ceq_in > 0

replace tax_base_employer = 0 if pen_ceq_in > 0
replace tax_base_employer = my_labor_in_declared/(1+pat[1,1])  if pen_ceq_in > 0 & age < age_ret & my_labor_in_declared/(1+pat[1,1])  <= 3*`th5000'   & my_labor_in_declared != .

replace con_employer = 0 if pen_ceq_in > 0
replace con_employer = 0.075 * tax_base_employer

replace con_ss_in = con_ia + con_payg + con_employer if pen_ceq_in > 0

save "$intermediate/panel_active_retired_section5.dta", replace

**************************************************************************
* 6. 	Evasion 
**************************************************************************

use "$intermediate/panel_active_retired_section5.dta", clear

* Computing the probability of evading for retirees in 2017
matload coef_evade , overwrite p("$intermediate") saving

capture drop xb
merge m:1 id using "$intermediate/hhs.dta",keepusing(q1_rem q2_rem q3_rem q4_rem)
drop _merge
g const = 1
g age2 = age*age
g xb = coef_evade[1,1]*age +coef_evade[1,2]*age2 +coef_evade[1,3]*female +coef_evade[1,4]*q1_rem +coef_evade[1,5]*q2_rem +coef_evade[1,6]*q3_rem +coef_evade[1,7]*q4_rem +coef_evade[1,8]*const if pen_ceq_in>0

replace prob_evade_panel = exp(xb)/(1+exp(xb)) if age >=20 & age <=65 & pen_ceq_in>0

* Simulating evasion of retirees in 2017

capture drop random
set seed 24106449
g random = runiform()
replace evades_sim = random <= prob_evade_panel if prob_evade_panel != . & contributes_sim == 0 & pen_ceq_in>0
replace evades_sim = 0 if contributes_sim == 1 & pen_ceq_in>0 

/*
* Checking
sum evades_sim prob_evade_panel if prob_evade_panel != . & contributes_sim == 0 // Checking
*/

**************************************************************************
* 7. 	Saving the database and merging ids (hhid memb_no)
**************************************************************************
merge m:1 id using "$intermediate/matching_ids.dta"
drop if _m==2
drop _m

keep id female age year my_labor_in_potential_panel contributes_sim evades_sim age_ret con_ss_in pen_in pen_non_cont_in hhid memb_no

sort hhid
save "$intermediate/panel_active_retired.dta", replace

* Cleaning: erasing some intermediate databases
foreach group in  	"female_puQ1"	"female_puQ2"  "female_puQ3"  "female_puQ4"  "female_puQ5" /*
*/       			"male_puQ1"	"male_puQ2"  "male_puQ3"  "male_puQ4"  "male_puQ5" /*
*/					"female_prQ1" "female_prQ2"  "female_prQ3"  "female_prQ4"  "female_prQ5" /*
*/       			"male_prQ1"	"male_prQ2"  "male_prQ3"  "male_prQ4"  "male_prQ5" {
	erase "$intermediate/panel_active_retired_section3_`group'.dta"
	erase "$intermediate/panel_active_retired_section4_`group'.dta"
}

/*
	erase "$intermediate/panel_active_retired_section2.dta"
	erase "$intermediate/panel_active_retired_section3.dta"
	erase "$intermediate/panel_active_retired_section4.dta"
	erase "$intermediate/panel_active_retired_section5.dta"
*/





