/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/

**************************************************************************
/* Outline
1. Inputs 
2. Households initial year (hh_t0), members of household (member), actuarial discount factor, discounted sum of members of hh (dmofhh) 
	2.1. Household initial year
	2.2. Household members
	2.3. Actuarial discount factor & discounted sum of the expected number of members of the household $j$ across the duration of the household
3. Lifetime income and social security transfers
	3.1. Lifetime income
	3.2. Lifetime social security cash flow
4. Consumption
5. Rate of return from social security
6. Per-period household net wealth and income
	6.1. Mandatory assets and SS income
	6.2. Adding individual variables to compute household variables & collapsing the database to hhid-year.
	6.3. Voluntary assets held by households and the associated income
	6.4. Bequests
	6.5. Household income
7. Saving the household-level database and merging it into the individual-level, and saving	
*/


**************************************************************************
/*
1.Inputs.
	1.1. Load the panel dataset containing the flows of labor income (yit), contributions to SS (τit) and pensions (pit).
	1.2. Merge mortality tables and generate survival function.
*/ 
**************************************************************************
* 1.1. Load the panel dataset containing the flows of labor income (yit), contributions to SS (τit) and pensions (pit).
**************************************************************************

* Creating random samples of the whole database

* 3000 households
use "$intermediate/matching_ids.dta", clear
collapse (max) memb_no, by(hhid)
set seed 24106449
sample 3000, count
keep hhid
sort hhid
save "$intermediate/small_sample.dta", replace

use "$intermediate/panel_active_retired.dta", clear
merge m:1 hhid using "$intermediate/small_sample.dta"
keep if _merge == 3
drop _merge
erase "$intermediate/small_sample.dta"
save  "$intermediate/panel_active_retired_3000hhs.dta", replace


* 500 households
use "$intermediate/matching_ids.dta", clear
collapse (max) memb_no, by(hhid)
set seed 24106449
sample 500, count
keep hhid
sort hhid
save "$intermediate/small_sample.dta", replace

use "$intermediate/panel_active_retired.dta", clear
merge m:1 hhid using "$intermediate/small_sample.dta"
keep if _merge == 3
drop _merge
erase "$intermediate/small_sample.dta"
save  "$intermediate/panel_active_retired_500hhs.dta", replace

* Loading the database. 

* (i) Large database
use  "$intermediate/panel_active_retired.dta", clear

* (ii) Small databases
/*
use  "$intermediate/panel_active_retired_3000hhs.dta", clear
use  "$intermediate/panel_active_retired_500hhs.dta", clear
*/
 
 
use  "$intermediate/panel_active_retired.dta", clear 

**************************************************************************
* 1.2. Merge mortality tables and generate survival function
**************************************************************************

merge m:1 female age using "$intermediate/mortality_cohort2017.dta"
drop _merge

xtset id age
g survival = 1 if age == 0
bys id (age): replace survival = l.survival * (1-l.mortality) if age > 0
sort female id age

**************************************************************************
* 2. Households initial year (hh_t0), members of household (member), actuarial discount factor (adf), discounted sum of members of hh (dmofhh) 
**************************************************************************
* 2.1. Household initial year {eq:hh\_t0}
**************************************************************************
/*
The family begins when its oldest member turns 20, unless he or she is younger in 2017 in which case we assume the household begins in 2017. We will refer to this individual as the "head of household" (hofhh). Notice that this definition departs from the usage of the term in the Uruguayan HH survey 2017 (ECH2017), where the head of household is self reported and may not coincide with the oldest member. The rational for starting the household when the oldest member turns 20 is that we do not want to have adults with no household. Formally, the starting year of household j is hh_t0 = 2017−(age_hofhh_2017 − 20), where age_hofhh_2017 is the age of the head of household j in 2017. In a few cases, the oldest member of a household is below 20 in 2017. In these cases, we impose hh_t0 = 2017 because all individuals in the survey belong to a hh and hence all households begin in 2017 or before.
*/

capture drop aux
g aux = age if year == 2017
bys hhid: egen age_hofhh_2017 = max(aux)
g hh_t0 = 2017 - (age_hofhh_2017 - 20)
replace hh_t0 = 2017 if age_hofhh_2017 < 20
drop aux
label var hh_t0 "Household starting year"
keep if year >= hh_t0

**************************************************************************
* 2.2. Household members (equation {eq:mofhh})
**************************************************************************

/*
we define an indicator variable (member) equal to 1 if the individual is in t with the family he was in 2017 according to the HH survey, provided he is alive in t. Assumptions: (i) adults stay with the family and (ii) children leave at 20. Hence: 
member = 1  if head of household &  t ≥ hh_t0
member = 1  if agei,2017 ≥ 20 & t ≥ hh_t0
member = 1  if agei,2017 < 20 & t ≥ hh_t0 & agei,t ∈ [0, 19]
member = 0  otherwise
*/

g aux = age >=20 & year == 2017
bys id: egen adult_2017 = max(aux)
drop aux
label var adult_2017 "=1 if aged 20 or older in 2017"

g member = 0
replace member = 1 if memb_no == 1 & year >= hh_t0 
replace member = 1 if adult_2017 == 1 & year >= hh_t0
replace member = 1 if adult_2017 == 0 & year >= hh_t0 & age < 20

label var member "= 1 if individual is member of hhid that year"

*keep id age year survival pen_in con_ss_in ym_pc yp_pc weight  hhid memb_no female member hh_t0 adult_2017  my_labor_in_potential_panel contributes_sim  contributes_sim evades_sim 

**************************************************************************
* 2.3. Actuarial discount factor & discounted sum of the expected number of members of the household $j$ across the duration of the household (equation {eq:dmofhh}).
**************************************************************************

g year_of_birth = year - age
label var year_of_birth "Year of birth"
g aux = survival if year == hh_t0 
bys id: egen surv_t0 = mean(aux) // Survival from birth to the formation of household
drop aux
replace surv_t0 = 1 if year_of_birth > hh_t0 // Individuals born after the formation of hh  
g surv_t0_t = survival/surv_t0
label var surv_t0_t "Survival from formation of hh (t0) to t"
replace surv_t0_t = . if year < hh_t0 // the variable is not defined before the formation of hh

*Checking
sum surv_t0_t if year == hh_t0
if r(min) <0.9999 | r(min) > 1.0001 | r(max) <0.9999 | r(max) > 1.0001 {
dis "ABORT! Survival from t0 is not equal to 1 in t0"
Abort
}
sum surv_t0_t if year < hh_t0
if r(min) != . {
dis "ABORT! Survival from t0 MUST be missing if t < t0"
Abort
}
local rr = $rr
g adf = surv_t0_t/((1 + `rr')^ (year - hh_t0))
label var adf "Actuarial discount factor"

g aux = member * adf
bys hhid: egen dmofhh = sum(aux)   // See equation {eq:dmofhh}
label var dmofhh "Discounted sum of mofhh"
drop aux

g aux = member * surv_t0_t
bys hhid year: egen mofhh= sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t} in equation {eq:FBC1hh}
label var mofhh "Expected # of members of hh"
drop aux 

bys hhid year: egen hsize = total(member) 
label var hsize "# of members of hh, conditional on surviving"

**************************************************************************
* 3. Household lifetime income and individual social security transfers and wealth
**************************************************************************
* 3.1. Lifetime income
**************************************************************************
* 3.1.1. Labor income
**************************************************************************

g my_labor_in = .
label var my_labor_in "Effective labor income (in 2017 pesos)"
replace my_labor_in = my_labor_in_potential_panel * (contributes_sim + (1-contributes_sim) * evades_sim)
replace my_labor_in=0 if my_labor_in==.
capture drop aux


* In 2017, effective labor income (my_labor_in) must be equal to reported labor income as computed in CEQ (lab_income_in)
merge 1:1 hhid memb_no year using "$intermediate/hhs.dta", keepusing(lab_income_in)
drop if _merge == 2
replace my_labor_in = lab_income_in if year == 2017 & age >= 20
drop _merge lab_income_in

* sum my_labor_in_potential_panel my_labor_in if year==2017

**************************************************************************
* 3.1.2. "Other" sources of income (= income from transfers = sources different from labor and capital)
**************************************************************************

g income_other_in = 0
label var income_other_in "Other sources of income (in 2017 pesos)"
replace income_other_in = pen_non_cont_in  

**************************************************************************
* 3.1.3 Lifetime income
**************************************************************************
g aux = (my_labor_in + income_other_in) * adf * member
bys hhid: egen lifetime_income_hh = sum(aux)
label var lifetime_income_hh "Lifetime labor income (Actuarial PV, hh)"
drop aux

* order hhid memb_no id age year member survival surv_t0 surv_t0_t hh_t0 adf my_labor_in  lifetime_income_hh 

**************************************************************************
* 3.2. Taxes individuals pay to finance the payg pillar

* We compute the proportion of taxes other than contributions to SS that individuals paid in 2017 to finance the payg pillar. We assume this proportion remains unchanged across time. We computed all pensions and contributions as payg, because in 2017 annuities were still few and small (less than 0.5% of all paid pensions in the country according to ech2017). 
**************************************************************************

egen expenditure_pensions = sum(pen_in) if year == 2017
egen contributions = sum(con_ss_in) if year == 2017
label var expenditure_pensions "Total expenditure in pensions in 2017"
label var contributions "Total contributions made in 2017"

egen income_declared = sum(my_labor_in) if year == 2017
label var income_declared "Total labor income declared in 2017"

g tax = (expenditure_pensions - contributions)/income_declared
sum tax
label var tax "Taxes needed to finance SS deficit as a % of total labor income"
g tax_ss = r(mean) * my_labor_in 
label var tax_ss "Taxes paid to finance SS"

* Checking 1: taxes used to finance SS should be non negative. In practice, the computations above could yield negative values, but we know that pensions are not fully financed by contributions, so a negative value is not credible.

sum tax
if r(mean) < 0 {
	dis "Negative taxes to finance pensions!"
	ABORT
}

* Checking 2: once taxes are computed, payg deficit must be zero.
* This checking must be commented out if tax_ss is set to zero. 

egen taxes_payg = sum(tax_ss) if year == 2017
g deficit_expenditure_with_tax = (expenditure_pensions - contributions - taxes_payg)/expenditure_pensions
sum deficit_expenditure_with_tax if year == 2017
if abs(r(max))>10^-6 | abs(r(min)) > 10^-6 {
dis "ERROR: payg deficit is not zero even when taxes are included!"
ABORT
} 

drop expenditure_pensions income_declared tax taxes_payg deficit_expenditure_with_tax

**************************************************************************
* 3.3. Lifetime social security cash flow and social security wealth
**************************************************************************

* 3.3.1. Social security cash flow (sscf)
replace pen_in=0 if pen_in==.
replace con_ss_in = 0 if con_ss_in == .
g sscf = (pen_in  - con_ss_in - tax_ss) * survival
label var sscf "Expected Social Security cash flow (individual)"

* Checking: There cannot be positive sscf (pensions) without negative sscf (contributions) 

bys id: egen sscf_min = min(sscf)
bys id: egen sscf_max = max(sscf)
g wrong_sscf = (sscf_min >=0 & sscf_max > 0)
sum wrong_sscf
if r(max) == 1  {   
				dis "Wrong social security cash flow"
				ABORT
				}
drop wrong_sscf
				
* 3.3.2. Social security wealth (household)

local rr = $rr
g aux = member*(pen_in - con_ss_in - tax_ss)*surv_t0_t /((1 + `rr')^ (year - hh_t0))
bys hhid: egen ssw = sum(aux)
label var ssw "Social Security Wealth (household)"
drop aux

**************************************************************************
* 4. Consumption
*************************************************************************

* SS = SOCIAL SECURITY, NSS = NO SOCIAL SECURITY

g consumption_SS = (lifetime_income_hh + ssw)/(dmofhh)
label var consumption_SS "Household per-capita consumption WITH SS"
g consumption_NSS = lifetime_income_hh/(dmofhh)
label var consumption_NSS "Household per-capita consumption WITHOUT SS"

* Checking. Lifetime consumption must equal lifetime labor income plus social security wealth.

capture drop aux1
g aux1 = consumption_SS * member * adf
bys hhid: egen lifetime_consumption_SS = sum(aux1)
g aux2 = (lifetime_consumption_SS - lifetime_income - ssw)/lifetime_consumption_SS

g aux3 = consumption_NSS * member * adf
bys hhid: egen lifetime_consumption_NSS = sum(aux3)
g aux4 = (lifetime_consumption_NSS - lifetime_income)/lifetime_consumption_NSS

sum aux2
if abs(r(max))>10^-6 | abs(r(min)) > 10^-6 {
dis "ERROR: Lifetime income and consumption are not equal with SS!"
ABORT
}

sum aux4
if abs(r(max))>10^-6 | abs(r(min)) > 10^-6 {
dis "ERROR: Lifetime income and consumption are not equal without SS!"
ABORT
}

drop aux*

save "$intermediate/life_cycle_ym_section4.dta", replace

**************************************************************************
* 5. Rate of return from social security
**************************************************************************

use "$intermediate/life_cycle_ym_section4.dta", clear
keep id age sscf sscf_min sscf_max
keep if sscf_max > 0 & sscf_max != .  // irrs are computed if there are positive pensions. Otherwise, contributions are pure taxes ($lim_{p_k \to 0} \rho_k = -1$).

xtset id age
bys id (age): g aux1 = 1 if _n == 1
g id_aux = sum(aux1)
drop aux1

g irr = .
label var irr "Internal rate of return in SS cash flows"
g sscf_id = .
g irr_id = .

save "$intermediate/life_cycle_ym_section5_aux.dta", replace

use "$intermediate/life_cycle_ym_section5_aux.dta", clear

* Computing irrs 

local z = 1
sum id_aux
global numb_id = `r(max)'
forvalues x = 1(100)`r(max)' {
	local y = `x' + 99
	local numb_id = $numb_id
	if `y' > `numb_id' {
		local y = `numb_id' 
	}
	use  "$intermediate/life_cycle_ym_section5_aux.dta", clear
	keep if id_aux >= `x'  & id_aux <=`y' 
	forvalues i = `x'/`y' {		
				dis "Individuo `i'"
				replace sscf_id = sscf if id_aux == `i'
				quietly do "$dofiles/irr.do"
				replace irr = irr_id if id_aux == `i'
				replace sscf_id = .
	}
		save "$intermediate/irr_`z'.dta", replace
	global zz = `z'
	local z = `z' + 1
}

use "$intermediate/irr_1.dta", clear
save "$intermediate/life_cycle_ym_irr.dta", replace
local zz = $zz
dis `zz'
forvalues x = 2(1)`zz' {
	append using "$intermediate/irr_`x'.dta"
}

keep id age irr
rename irr irr_bt
label var irr_bt "Internal rate of return in SS cash flows (before taxes)"

sort id age
save "$intermediate/life_cycle_ym_irr.dta", replace

use "$intermediate/life_cycle_ym_irr.dta", clear
merge 1:1 id age using "$intermediate/life_cycle_ym_section4.dta"
drop _merge
save "$intermediate/life_cycle_ym_section5.dta", replace


* Cleaning: erasing intermediate databases

local zz = $zz
dis `zz'
forvalues x = 2(1)`zz' {
	erase "$intermediate/irr_`x'.dta"
}

erase "$intermediate/life_cycle_ym_section5_aux.dta"

**************************************************************************
* 6. Per-period household net wealth and income
**************************************************************************
* Assets

/* 
To compute per period assets, we use equations {FBC1hh} and {eq:FBC4} in "Redistributive-effect-of-Pensions.tex", for scenarios with and without SS, respectively. 

\begin{equation} 	\label{eq:FBC1hh}
\begin{array}{ll}
      S_{i,t+1} a_{i,t+1}^p &= S_{i,t} [(1+ \rho_i ) a_{it}^p - p_{it} +  \tau_{it}]   \\
      a_{jt+1}^v & =\frac{\sum_{i\in J} \mathbbm{1}_{i,t} S_{i,t}}{\sum_{i\in J} \mathbbm{1}_{i,t+1} S_{i,t+1}} [(1+r) a_{jt}^v + y_{it} + p_{it} -  \tau_{it} - c_{j}^p] 
\end{array}
\end{equation}

\begin{equation} 	\label{eq:FBC4}
      a_{jt+1}  = \frac{\sum_{i\in J} \mathbbm{1}_{i,t} S_{i,t}}{\sum_{i\in J} \mathbbm{1}_{i,t+1} S_{i,t+1}} [(1+r) a_{jt} + y_{it}  - c_{j}]
\end{equation}
 
Reorganizing:

\label{eq:FBC1hh}
a_{i,t+1}^p &= S_{i,t} [(1+ \rho_i ) a_{it}^p - p_{it} +  \tau_{it}] / S_{i,t+1}  \\
a_{jt+1}^v  = [mofhh_t((1+r) a_{jt}^v - c_{j}^p) + hh_labor_t + pen_hh_t - con_ss_hh_t ] /mofhh_{t+1}

\label{eq:FBC4}
a_{jt+1}  = [mofhh_t((1+r) a_{jt} - c_{j}) + hh_labor_t] /mofhh_{t+1}

where:
		mofhh_t = \sum_{i\in J} \mathbbm{1}_{i,t} S_{i,t} 
		labor_hh_t = \sum_{i\in J} \mathbbm{1}_{i,t} S_{i,t} y_{it}
		pen_hh_t  =  \sum_{i\in J} \mathbbm{1}_{i,t} S_{i,t} pen_in_{it}
		con_ss_hh_t = \sum_{i\in J} \mathbbm{1}_{i,t} S_{i,t} con_ss_in_{it}
 
In the last period S_{i,T+1}=0, so we cannot divide by S_{i,T+1} to compute a_{j,T+1}^v, but a_{j,T+1}^v == 0 MUST HOLD (in scenarios without bequests). We check this is indeed the case, i.e. we check that 
a_{j,T+1}  = \sum_{i\in J} \mathbbm{1}_{i,T}  [(1+r) a_{j,T} + y_{i,T}  - c_{j}] =0. Otherwise the program aborts. 
 
Some individuals do not receive a pension despite of having contributed because they do not fulfill the vesting period conditions. In their case, contributions to SS are like taxes. They do not accumulate assets in SS.
The variable mandat_assets_SS is missing in their case because irr is missing, and income_ms_SS_in = - con_ss_in - tax_ss ({eq:income_no_access}). 
Notice: these equations differ from those in life_cycle_yd.do in that taxes and government transfers are zero.
*/

use  "$intermediate/life_cycle_ym_section5.dta", clear

**************************************************************************
* 6.1. Mandatory assets and SS income
**************************************************************************

g assets_ms_SS_in = 0 if age == 20 & year >= hh_t0
label var assets_ms_SS_in "Mandatory savings, SS, individual"
tsset id year
replace assets_ms_SS_in = (L.surv_t0_t/surv_t0_t) * ((1+irr)* L.assets_ms_SS_in + (L.con_ss_in - L.pen_in + L.tax_ss)) if age>20 & surv_t0_t >0
replace assets_ms_SS_in =  (1+irr)* L.assets_ms_SS_in + (L.con_ss_in - L.pen_in + L.tax_ss) if age>20  & surv_t0_t == 0


g income_ms_SS_in = irr * assets_ms_SS_in
label var income_ms_SS_in "Income from SS mandatory savings, individual"

* Contributions are taxes if individuals do not access to a contributory pension (in which case irr is missing).
replace income_ms_SS_in = - con_ss_in - tax_ss if irr == .   // See equation {eq:income_no_access}

* Individuals who receive pensions at very low ages tend to have very high irr and computations become unstable. In these cases, income from SS tends to be basically a transfer so we substitute pensions for income_ms_SS_in. Accordingly, assets_ms_SS_in are set to zero and irr to missing.  

replace assets_ms_SS_in = 0						if age_ret < 30
replace income_ms_SS_in = pen_in 				if age_ret < 30
replace irr = .									if age_ret < 30

*Checking
local rr = $rr
g aux1 = assets_ms_SS_in/(lifetime_income_hh * (1 + `rr')^ (year - hh_t0)) if age == 101 & irr < 0.1

sum aux1 
if r(mean) !=. & (abs(r(max))>10^-3 | abs(r(min)) > 10^-3 )  {
dis "ERROR: There are non-zero mandatory assets at the end of the last period (with SS)!"
ABORT
}
drop aux1

**************************************************************************
* 6.2. Adding individual variables to compute household variables & collapsing the database to hhid-year.
**************************************************************************

g aux = member * surv_t0_t * my_labor_in 
bys hhid year: egen labor_hh = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}y_{it} in equation {eq:FBC1hh}
label var labor_hh "Household expected labor income"
drop aux

g aux = member * my_labor_in 
bys hhid year: egen labor_hh_c = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}y_{it} in equation {eq:FBC1hh}, substituting S_{i,t}=1
label var labor_hh_c "Household labor income (conditional on being alive)"
drop aux

g aux = member * surv_t0_t * income_other_in 
bys hhid year: egen income_other_hh = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}y_{it} in equation {eq:FBC1hh}
label var income_other_hh "Household expected other-sources income"
drop aux

g aux = member * income_other_in 
bys hhid year: egen income_other_hh_c = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}y_{it} in equation {eq:FBC1hh}, substituting S_{i,t}=1
label var income_other_hh_c "Household other-sources income (conditional on being alive)"
drop aux

g aux = member * surv_t0_t * pen_in 
bys hhid year: egen pen_hh = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}p_{it} in equation {eq:FBC1hh}
label var pen_hh "Household expected pensions"
drop aux

g aux = member * pen_in 
bys hhid year: egen pen_hh_c = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}p_{it} in equation {eq:FBC1hh}, substituting S_{i,t}=1
label var pen_hh_c "Household pensions (conditional on being alive)"
drop aux

g aux = member * surv_t0_t * con_ss_in
bys hhid year: egen con_ss_hh = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}\tau_{it} in equation {eq:FBC1hh}
label var con_ss_hh "Household expected contributions"
drop aux 

g aux = member * con_ss_in
bys hhid year: egen con_ss_hh_c = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}\tau_{it} in equation {eq:FBC1hh}, substituting S_{i,t}=1
label var con_ss_hh_c "Household expected contributions (conditional on being alive)"
drop aux 


g aux = member * surv_t0_t * tax_ss
bys hhid year: egen tax_ss_hh = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}\tau_{it} in equation {eq:FBC1hh}. Note: in the document notation, \tau_{it} = con_ss_in + tax_ss. 
label var tax_ss_hh "Household expected tax paid to finance payg pillar"
drop aux 

g aux = member  * tax_ss
bys hhid year: egen tax_ss_hh_c = sum(aux) // \sum_{i\in J} \mathbbm{1}_{i,t}S_{i,t}\tau_{it} in equation {eq:FBC1hh}, substituting S_{i,t}=1. Note: in the document notation, \tau_{it} = con_ss_in + tax_ss. 
label var tax_ss_hh_c "Household expected tax paid to finance payg pillar (conditional on being alive)"
drop aux

*g aux = member * surv_t0_t * assets_ms_SS_in
g aux = member * assets_ms_SS_in
bys hhid year: egen mandat_assets_SS_hh = sum(aux) // 
label var mandat_assets_SS_hh "Mandatory assets in SS (household)"
drop aux 

g aux = member * income_ms_SS_in 
bys hhid year: egen income_ms_SS_hh = sum(aux)  // See equation {eq:hh_income}
label var income_ms_SS_hh "Income from SS mandatory savings, household"
drop aux

* Household level

* We preserve the database at the individual level to merge later
save "$intermediate/life_cycle_ym_section6_aux.dta", replace

* We collapse the database to work at the household level

use "$intermediate/life_cycle_ym_section6_aux.dta", clear

keep hhid year mofhh hsize labor_hh labor_hh_c consumption_NSS consumption_SS hh_t0 income_ms_SS_hh pen_hh pen_hh_c con_ss_hh con_ss_hh_c tax_ss_hh tax_ss_hh_c lifetime_income_hh mandat_assets_SS_hh ssw income_other_hh income_other_hh_c

foreach v of var * { 
          local l`v' : variable label `v' 
		   if `"`l`v''"' == "" { 
				  local l`v' "`v'" 
		   } 
} 

collapse (mean)  mofhh hsize labor_hh labor_hh_c consumption_NSS consumption_SS hh_t0 income_ms_SS_hh pen_hh pen_hh_c con_ss_hh con_ss_hh_c tax_ss_hh tax_ss_hh_c lifetime_income_hh mandat_assets_SS_hh ssw income_other_hh income_other_hh_c, by(hhid year)

foreach v of var * { 
     label var `v' "`l`v''" 
} 

**************************************************************************
* 6.3. Voluntary assets held by households and the associated income
**************************************************************************

* Households voluntary assets with SS (see equation {eq:FBC1hh})

g assets_vsc_SS_pc = 0 if year == hh_t0  // Assumption: households start without assets
label var assets_vsc_SS_pc "Voluntary savings held for consumption, WITH SS, per capita"
tsset hhid year
local rr = $rr
bys hhid: replace assets_vsc_SS_pc =  (L.mofhh *((1+`rr')*L.assets_vsc_SS_pc - L.consumption_SS) + L.labor_hh + L.income_other_hh + L.pen_hh - L.con_ss_hh - L.tax_ss_hh) /mofhh  if year > hh_t0 & mofhh >0
bys hhid: replace assets_vsc_SS_pc =   L.mofhh *((1+`rr')*L.assets_vsc_SS_pc - L.consumption_SS) + L.labor_hh + L.income_other_hh + L.pen_hh - L.con_ss_hh - L.tax_ss_hh  if year > hh_t0 & mofhh ==0 

g income_vsc_SS_pc = `rr' * assets_vsc_SS_pc
label var income_vsc_SS_pc "Income from voluntary savings for consumption, WITH SS, per capita"

g income_volunt_assets_SS_hh = income_vsc_SS_pc * hsize if  hsize >0
label var income_volunt_assets_SS_hh "Household aggregate income from voluntary savings WITH SS (if alive)"

* Households voluntary assets without SS ( see equation {eq:FBC4})

g assets_vsc_NSS_pc = 0 if year == hh_t0  // Assumption: households start without assets
label var assets_vsc_NSS_pc "Voluntary savings held for consumption, WITHOUT SS, per capita"
tsset hhid year
bys hhid: replace assets_vsc_NSS_pc =  (L.mofhh *((1+`rr')*L.assets_vsc_NSS_pc - L.consumption_NSS) + L.labor_hh + L.income_other_hh) /mofhh  if year > hh_t0 & mofhh >0  
bys hhid: replace assets_vsc_NSS_pc =   L.mofhh *((1+`rr')*L.assets_vsc_NSS_pc - L.consumption_NSS) + L.labor_hh + L.income_other_hh   if year > hh_t0 & mofhh ==0 

g income_vsc_NSS_pc = `rr' * assets_vsc_NSS_pc
label var income_vsc_NSS_pc "Income from voluntary savings for consumption, WITHOUT SS, per capita"

g income_volunt_assets_NSS_hh = income_vsc_NSS_pc * hsize if hsize >0
label var income_volunt_assets_NSS_hh "Household aggregate income from voluntary savings WITHOUT SS (if alive)"

*Checking

g aux1 = assets_vsc_NSS_pc/(lifetime_income_hh * (1 + `rr')^ (year - hh_t0)) if mofhh == 0
sum aux1
if abs(r(max))>10^-3 | abs(r(min)) > 10^-3 {
dis "ERROR: There are non-zero assets at the end of the last period (without SS)!"
ABORT
}
drop aux1

g aux1 = assets_vsc_SS_pc/(lifetime_income_hh * (1 + `rr')^ (year - hh_t0)) if mofhh == 0
sum aux1
if abs(r(max))>10^-3 | abs(r(min)) > 10^-3 {
dis "ERROR: There are non-zero voluntary assets at the end of the last period (with SS)!"
ABORT
}
drop aux1

 save "$intermediate/life_cycle_ym_section6_3_aux.dta", replace
**************************************************************************
* 6.4. Bequests
**************************************************************************

use "$intermediate/life_cycle_ym_section6_3_aux.dta", clear
merge m:1 hhid year using "$intermediate/hhs_hh.dta", keepusing(cap_income_hh) 
drop if _merge == 2
drop _merge 
 
g income_vsb_hh = cap_income_hh - income_volunt_assets_SS_hh if year == 2017 // See equation {eq:income-bequests}
label var income_vsb_hh "Income from voluntary savings for bequests, household"

**************************************************************************
* 6.5. Household income
**************************************************************************

g income_ym_NSS = labor_hh_c + income_other_hh_c + income_volunt_assets_NSS_hh + income_vsb_hh
label var income_ym_NSS "Household market income without SS"

g income_ym_SS = labor_hh_c + income_other_hh_c + income_volunt_assets_SS_hh + income_ms_SS_hh  + income_vsb_hh
label var income_ym_SS "Household market income with SS"

g income_ym_NSS_pc = income_ym_NSS /hsize if hsize >0
label var income_ym_NSS_pc "Household per capita market income WITHOUT SS"

g income_ym_SS_pc = income_ym_SS /hsize if hsize >0
label var income_ym_SS_pc "Household per capita market income WITH SS"

g income_labor_pc = labor_hh_c/hsize if hsize >0
label var income_labor_pc "Household per capita labor income"

g income_other_pc = income_other_hh_c/hsize if hsize >0
label var income_other_pc "Household per capita other-sources income"

g income_ms_SS_pc = income_ms_SS_hh/hsize if hsize >0
label var income_ms_SS_pc  "Income from SS mandatory savings, per capita"

g income_vsb_pc = income_vsb_hh/hsize if hsize >0
label var income_vsb_pc "Income from voluntary savings for bequests, per capita"

**************************************************************************
* 7. Saving the household-level database and merging it into the individual-level, and saving
**************************************************************************

* We save the household-level database
save "$intermediate/life_cycle_ym_hh_$scenario.dta", replace

use "$intermediate/life_cycle_ym_section6_aux.dta", clear
merge m:1 hhid year using "$intermediate/life_cycle_ym_hh_$scenario.dta", keepusing(assets_vsc_SS_pc income_volunt_assets_SS_hh assets_vsc_NSS_pc income_volunt_assets_NSS_hh income_ym_NSS income_ym_SS income_ym_NSS_pc income_ym_SS_pc income_labor_pc income_other_pc income_vsc_NSS_pc income_vsc_SS_pc income_ms_SS_pc income_vsb_hh income_vsb_pc)

* Checking
count if assets_vsc_SS_pc == .  & age >= 20 
if r(N) != 0   {
dis "ERROR: There are missing voluntary assets (with SS)!"
ABORT
}

count if assets_vsc_NSS_pc == .  & age >= 20
if r(N) != 0   {
dis "ERROR: There are missing voluntary assets (without SS)!"
ABORT
}


drop if _m==1 // We drop not matched observations from master (life_cycle_section6_aux.dta). We checked these observations are useless: (i) year < 2017 (survey year) and (ii) year < hh_t0.  
drop _m

keep  hhid memb_no id year age income_ym_NSS_pc income_ym_SS_pc

save "$intermediate/life_cycle_ym_$scenario.dta", replace




