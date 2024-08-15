/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/


***************************************************************************************
/*We build a panel for the desired age spells of each individual.
Merge with estimated wage increases by group and age, to simulate wage histories starting with observed wages in 2017.
Impute probabilities of contributing according to age and group.
Simulate contributory status. People contributing are working.
Using the calculated probability (working | not contributing), simulate evasion
*/
***************************************************************************************
/*  
1. Data preparation: Logging the cross-section database and expanding to build a panel database 
	1.1. Data import and checking: Import the cross-section database and check relevant variables
	1.2. Creation of the panel: expanding to build a panel database of years 1919-2118 and calculating ages
2. Computing potential labor income for years different to the year the HH survey was gathered 
*/

***************************************************************************************
* 1. Loading the cross-section database and expanding to build a panel database 
***************************************************************************************

***************************************************************************************
*1.1. Load the cross-section database and check relevant variables
***************************************************************************************

use "$intermediate/hhs.dta", clear
keep id age groups my_labor_in_potential_ech

***************************************************************************************
*1.2. Creation of the panel: expanding to build a panel database of years 1919-2118 and calculating ages
***************************************************************************************

rename age age_ech

expand 102

bys id : gen age=_n - 1

g year= 2017 + age - age_ech
g age2= age^2/10^3

drop age_ech

***************************************************************************************
* 2. Computing potential labor income for years different to the year the HH survey was gathered 
***************************************************************************************
/* Four cases: 
(i) Individuals aged \in [20,65] in 2017: We have my_labor_in_potential_ech at an age for which we do have an age labor income profile. 
(ii) Individuals aged < 20 in 2017: In hhs.do ("* Aged less than 20"), we imputed labor income for age 20 and year > 2017.
(iii) Other individuals (including aged>65 in 2017): We impute labor income using my_labor_in_potential_ech and assuming a flat age earnings profile.
(iv) Individuals aged > 65 in 2017: We will replace labor income in panel_active_retired.do, if they have contributory pensions.
*/

*** Merge with BPS estimated wages

merge m:1 groups age using "$intermediate/ss.dta"
* Checking: 
/*
tab _merge if  year == 2017 & age >= 20 & age <=65 // all individuals aged 20 to 65 in the year of the HH survey are matched
tab _m if age <20 // No individuals under 20 are matched
tab age if _m!=3 // All unmatched individuals are aged 0-19 and 71-101
*/
drop _m

sort id age

xtset id age

count if my_labor_income_profile == . & age >=20 & age <= 65
if r(N) >0 {
	dis "Individuals without income profiles between 20 and 65"
	ABORT
}

if "$scenario" == "SSflat" replace increase_my_labor_in = 0 //For scenario assuming no real wage growth
if "$scenario" == "SSone" replace increase_my_labor_in = 0.01 //For scenario assuming 1% real wage growth
if "$scenario" == "SStwo" replace increase_my_labor_in = 0.02 //For scenario assuming 2% real wage growth
if "$scenario" == "SSthree" replace increase_my_labor_in = 0.03 //For scenario assuming 3% real wage growth

replace increase_my_labor_in = 0 if increase_my_labor_in == . // (iii) Assume a flat profile in case of no data on age earnings profile

g my_labor_in_potential_panel = my_labor_in_potential_ech if year == 2017 & age >=20 // Individuals aged \in[20,65] in 2017
* sum my_labor_in_potential_ech my_labor_in_potential_panel if year==2017

label var my_labor_in_potential_panel "Potential labor income (at 2017 values)"
replace  my_labor_in_potential_panel = my_labor_in_potential_ech if year > 2017 & age ==20 // Individuals aged <20 in 2017
bys id: replace my_labor_in_potential_panel = (increase_my_labor_in + 1)*l.my_labor_in_potential_panel if year>2017 & age >20

g neg_age = -age
sort id neg_age
xtset id neg_age
bys id: replace my_labor_in_potential_panel = l.my_labor_in_potential_panel /(l.increase_my_labor_in + 1) if year<2017 & age >= 20 
drop neg_age

sort id age

save "$intermediate/panel_active_section2.dta", replace
 
***************************************************************************************
* 3. Computing the probabilites of contributing and evading and simulating contribution and evasion status  
***************************************************************************************
use "$intermediate/panel_active_section2.dta", clear
merge m:1 id using "$intermediate/hhs.dta" , keepusing(prob_contribut_ech prob_evade_ech contributes_ech evades_ech female q?_rem )
keep if _merge == 3
drop _merge 

* Probability of contributing

g prob_contribut_panel = .
label var prob_contribut_panel "Prob of contributing (panel)"

quietly {
	foreach categ in  "female_pu" "male_pu" "female_pr" "male_pr"{
	*foreach categ in  "female_pu"  { 
			forvalues x = 1(1)5 {
				matload coef`categ'q`x' , overwrite p("$intermediate") saving
				g aux1 = ln(prob_contribut_ech/(1-prob_contribut_ech))- coef`categ'q`x'[1,1]*age -	coef`categ'q`x'[1,2]*age2  if groups == "`categ'Q`x'" & year == 2017 // see equation {eq:piHH}
				bys id: egen aux2 = min(aux1)  
				replace prob_contribut_panel = exp(aux2 + coef`categ'q`x'[1,1]*age + coef`categ'q`x'[1,2]*age2)/(1+exp(aux2 + coef`categ'q`x'[1,1]*age + coef`categ'q`x'[1,2]*age2)) if age >= 20 & age <= 65 & groups == "`categ'Q`x'" // see equation {eq:pie}
				drop aux1 aux2
			}
	}
}

* Checking.   
count if prob_contribut_panel == . &  age >=20 & age <=65 
if r(N) > 0. {
	dis "Some individuals have no imputed probability of contributing"
	ABORT
} 

* Simulating the contributive status 

g random = runiform()
global state = c(seed)

g contributes_sim = random <= prob_contribut_panel if prob_contribut_panel != .
replace contributes_sim = contributes_ech if year ==2017  // In the year of the survey, simulated and "observed" contributive status must be equal.
label var contributes_sim "Simulated contribution status"

* Probability that someone who is not contributing is working = probability of evading conditional on not contributing.

g prob_evade_panel = .
label var prob_evade_panel "Prob someone not contributing is working (panel)"

matload coef_evade , overwrite p("$intermediate") saving

g const = 1

g xb = coef_evade[1,1]*age +coef_evade[1,2]*age2 +coef_evade[1,3]*female +coef_evade[1,4]*q1_rem +coef_evade[1,5]*q2_rem +coef_evade[1,6]*q3_rem +coef_evade[1,7]*q4_rem +coef_evade[1,8]*const

replace prob_evade_panel = exp(xb)/(1+exp(xb)) if age >=20 & age <=65

*Checking: probabilites of contributing and evading computed in this file (prob_contribut_panel, prob_evade_panel) for the year of the survey must be the same as the probabilities computed in the cross-section database (prob_contribut_ech, prob_evade_ech).

g aux1 = abs(prob_contribut_ech - prob_contribut_panel) if year == 2017 
sum aux1
if abs(r(mean))>10^-4 {
dis "Wrong prob_contribut_panel in 2017"
ABORT
}
drop aux?

g aux1 = abs(prob_evade_ech - prob_evade_panel) if year == 2017
sum aux1
if abs(r(mean))>10^-4 {
dis "Wrong prob_evade_panel in 2017"
ABORT
}
drop aux?

* Simulating evasion 

local state = "$state"
* dis "`state'"
set seed `state'
g random2 = runiform()
g evades_sim = random2 <= prob_evade_panel if prob_evade_panel != . & contributes_sim == 0
replace evades_sim = 0 if contributes_sim == 1
replace evades_sim = evades_ech if year ==2017  // In the year of the survey, simulated and "observed" evasion must be equal. 
label var evades_sim "=1 if contributes_sim=0 & the individual is working"
*sum evades_sim prob_evade_panel if prob_evade_panel != . & contributes_sim == 0 // Checking

keep id year age contributes_sim my_labor_in_potential_panel prob_contribut_panel prob_evade_panel evades_sim
sort id age
save "$intermediate/panel_active.dta", replace
