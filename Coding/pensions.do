/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/

* Simulation of pension rights in Uruguay assuming individuals retire as soon as they are eligible

/*
* Outline
	1. Average wage index and annuity factors
	2. Eligibility, age of retirement 
	3. Declared labor income, tax bases, contributions
	4. Pension rights (law 16.713)
		4.1. Pension rights: first pillar (payg-db)
		4.2. Pension rights: second pillar (individual-accounts-defined-contributions pensions)
		4.3. Total pension (payg + ia)
*/

use "$intermediate/panel_active.dta", clear
keep id age year my_labor_in_potential_panel prob_contribut_panel contributes_sim 

**************************************************************************
* 1. Average wage index, annuity factors, comissions from managers of individual accounts and cost of insurance. 
**************************************************************************

g edad=age

* IMS
merge m:1 year using "$intermediate/ims_annual.dta"
drop _m

* Thresholds in law 16.713 updated to 2017

if $th5000 == . {
	dis "The threshold in law 16.713 is missing! See master.do"
	ABORT
}

* PRIMA BCU
merge m:1 edad using "$path/Data/Inputs/prima_bcu.dta"
replace prima_bcu = . if prima_bcu==0
drop _m

drop edad

* Comissions of managers of individual accounts (AFAP) + insurance (seguro colectivo de invalidez y muerte)

if $comission == . {
	dis "The comission and cost of insurance is missing! See master.do"
	ABORT
}


xtset id age

**********************************************************************************************************************
* 2. Eligibility, age of retirement 
* We assume individuals retire as soon as they become eligible. Mixed regime (Law 16713), includes 2008 changes.
***********************************************************************************************************************

sort id year
bys id (age): g contributes_cum=sum(contributes_sim)
label var contributes_cum "Cumulative years of contribution"

g age_min = 60 

capture g eligible_op=.
replace eligible_op = (age>=age_min & contributes_cum>=30)
label var eligible_op "=1 if fulfills eligibility conditions for ordinary pension"

g eligible_aa =  age>=65 & 2*age + contributes_cum >= 155 & contributes_cum >= 15
label var eligible_aa "=1 if fulfills eligibility conditions for advanced age pension"

g retired=(eligible_op==1 | eligible_aa==1) & age >= 60
label var retired "=1 if retired"
g date_ret_aux=year if retired==1
bys id  (year): egen date_ret=min(date_ret_aux)
label var date_ret "Year of retirement"
drop date_ret_aux

g age_ret_aux = age if year == date_ret 
bys id: egen age_ret = min(age_ret_aux) 
label var age_ret "Age of retirement"
drop age_ret_aux

**************************************************************************
* 3. Declared labor income, tax bases, contributions
**************************************************************************
* We assume that all individuals used the options in (i) article 8 of law 16.713, and (ii) article 3 of law 17.445 (the right to collect the annuity at 65 even without being eligible for ordinary pension or advanced age pension). Tax bases for employee contributions are named "asignaciones computables" in law 16.713. tax_base_payg and tax_base_ia correspond to the payg and individual account pillars, respectively. Employers contribute 7.5% over declared labor income below 15.000 pesos of May 1995 (3*`th5000', in 2017 values). Employer contributions go to the payg pillar. 

g my_labor_in_declared = my_labor_in_potential_panel*contributes_sim  
label var my_labor_in_declared "Labor income, declared to SS, in 2017 wages"

local th5000 = $th5000
g threshold1_aux = `th5000'  // This variable is only used for checking. 

matload pat , overwrite p("$intermediate") saving

g tax_base_payg = 0.5*my_labor_in_declared/(1+pat[1,1])   //Regime is assumed mixed with art. 8 
replace tax_base_payg = my_labor_in_declared/(1+pat[1,1]) - 0.5*`th5000' if my_labor_in_declared/(1+pat[1,1]) > `th5000'  & my_labor_in_declared != .   
replace tax_base_payg = `th5000' if my_labor_in_declared/(1+pat[1,1]) > 1.5*`th5000'  & my_labor_in_declared != .  
label var tax_base_payg "Tax base for pay-as-you-go"


g tax_base_ia =  0.5*my_labor_in_declared/(1+pat[1,1])    //Regime is assumed mixed with art. 8 
replace tax_base_ia = 0.5*`th5000' if my_labor_in_declared/(1+pat[1,1]) > `th5000' & my_labor_in_declared != .   
replace tax_base_ia = my_labor_in_declared/(1+pat[1,1]) - `th5000' if my_labor_in_declared/(1+pat[1,1]) > 1.5*`th5000'  & my_labor_in_declared != .
replace tax_base_ia = 2*`th5000' if my_labor_in_declared > 3*`th5000'   & my_labor_in_declared/(1+pat[1,1]) != .
label var tax_base_ia "Tax base for individual savings"

g con_ia = 0
replace con_ia = 0.15 * tax_base_ia if  age < min(age_ret,65) // Individuals stop contributing to IA if (i) they retire or (ii) start collecting an annuity based on law 17445. We assume everybody claim the annuity when they turn 65, if they are not retired.
label var con_ia "Employee contributions to individual account (gross of comissions)"

g con_payg = 0
replace con_payg = 0.15 * tax_base_payg if age < age_ret 
label var con_payg "Employee contributions to pay-as-you-go pillar"

g tax_base_employer = 0
replace tax_base_employer = my_labor_in_declared/(1+pat[1,1])  if  age < age_ret & my_labor_in_declared != .
replace tax_base_employer = 3*`th5000' if my_labor_in_declared/(1+pat[1,1])  > 3*`th5000' & my_labor_in_declared != .
label var tax_base_employer "Tax base for employers"

g 		con_employer = 0
replace con_employer = 0.075 * tax_base_employer
label var con_employer "Employer contributions to pay-as-you-go pillar"

g con_ss_in = con_ia + con_payg + con_employer
label var con_ss_in "Contributions to SS - Effectively paid"

**************************************************************************
* 4. Pension rights (law 16.713)
**************************************************************************

**************************************************************************
* 4.1. Pension rights: first pillar (payg-db)
**************************************************************************

**************************************************************************
* 4.1.1. Average Indexed Monthly Earnings (aime)(Sueldo Básico Jubilatorio - sbj)
**************************************************************************

*	a) Last 10 years

g aux = 1 if contributes_sim==1 & retired ==0 
gsort + id - year
bys id: gen aux10a = sum(aux)      
replace aux10a = . if aux==.
sort id year
drop aux

capture drop aime*
bys id: egen aime_aux = mean(tax_base_payg) if contributes_sim ==1 & aux10a<=10
bys id: egen aime_aux2=mean(aime_aux) 
g aime_last10 = aime_aux2 
label var aime_last10 "AIME based on last 10 years of contributions"
drop aime_aux aime_aux2


*	b) Best 20 years

g aux = 1 if contributes_sim==1 & retired ==0 
gsort + id - tax_base_payg
bys id: gen aux20a = sum(aux)  
replace aux20a= . if aux==.
sort id year
drop aux

bys id: egen aime_aux = mean(tax_base_payg) if contributes_sim ==1 & aux20a <= 20
bys id: egen aime_aux2=mean(aime_aux) 
g aime_best20 = aime_aux2 
label var aime_best20 "AIME based on best 20 years of contributions"
drop aime_aux aime_aux2 

* 	c) AIME

g aime = max(aime_best20, min(aime_last10, aime_best20*1.05))
label var aime "Average Indexed Monthly Earnings"

**************************************************************************
* 4.1.2. Replacement rates
**************************************************************************

* Loading  matrixes containing replacement rates for ordinary and advanced age pensions.

matload TR_comun,  path( "$inputs") saving overwrite

matload TR_avanzada,  path("$inputs") saving overwrite

g aux1 = contributes_cum if age == age_ret  // Years of contribution when the individual retires
bys id: egen aux2 = min(aux1) 
 
g reprate_op = TR_comun[aux2-29,age_ret-58]	
g reprate_aa = TR_avanzada[aux2-14,age_ret-63]
label var reprate_op "Replacement rate for ordinary pensions"
label var reprate_aa "Replacement rate for advanced age pensions"
drop aux?

g reprate=0
replace reprate=reprate_op if age == age_ret & eligible_op==1
replace reprate=reprate_aa if age == age_ret & eligible_aa==1 & eligible_op==0
label var reprate "Replacement rate"

/*
* Reviewing replacement rates
edit id age contributes_cum age_ret reprate_op reprate_aa reprate eligible_op eligible_aa if id == 1  &  age >59 & age < 70
*/

**************************************************************************
* 4.1.4. Minimum and maximum payg pensions
**************************************************************************

* Minimum pension
*Articles 40 and 75 law 16.713: (i) Minimum pension is increased by X% per year after 60, with a maximum of 10*X.
*If retirement is between 1/1/99 and 1/1/2001, X = 0.04. If it is between 1/1/2001 y 1/1/2003, X = 0.08.
*If it is after, X=0.12. (ii) Minimum pension for takers of Art. 8 option is 0.75 of those without the option. Everyone chooses art. 8 in the current version, but we also compute minimum pensions without art 8 for future use.  

local th5000 = $th5000
capture g pen_min_with_art8=0

* Minimum pension according to law 16.713
replace pen_min_with_art8 = 550*(1+ min(0.12*(age_ret - 60),1.2))*0.75*`th5000'/5000 if age == age_ret & eligible_aa==0 & eligible_op==1
replace pen_min_with_art8 = 950*0.75*`th5000'/5000 if age == age_ret & eligible_aa==1 & eligible_op==0
xtset id age
replace pen_min_with_art8 = l.pen_min_with_art8 if age > age_ret
label var pen_min_with_art8 "Minimum legal pension (with art 8)"

g pen_min_wo_art8 = pen_min_with_art8/0.75
label var pen_min_wo_art8 "Minimum legal pension (without art 8)"

* Minimum pension according to several decrees (deflated to 2017 values using the average wage index). 
sum ims if year == 2017
g min_decrees = 16222 * (12/1000) * r(mean)/ims if year == 2022 // Updated for yearly values.

replace min_decrees = 2.5 *(12/1000)* 2819* r(mean)/ims if year == 2014 // 2819 es el valor de la bpc fijado en enero 2014
replace min_decrees = 1.25 *(12/1000)* 1775* r(mean)/ims if year == 2008 // 1775 es el valor de la bpc fijado en enero 2008
* Interpolación mínimos por decrees
replace min_decrees = 5043*(12/1000) + ((9283*(12/1000)-5043*(12/1000))/(2014-2008)) * (year - 2008) if year > 2008 & year < 2014
replace min_decrees = 9283*(12/1000) + ((10248*(12/1000)-9283*(12/1000))/(2022-2014)) * (year - 2014) if year > 2014 & year < 2022
replace min_decrees = 10248*(12/1000) if year >2022

replace pen_min_wo_art8 = min_decrees 			if min_decrees > pen_min_wo_art8 & pen_min_wo_art8 != . & min_decrees !=. 
replace pen_min_with_art8 = pen_min_wo_art8 * 0.75 	 

* Maximum pension (art. 41, law 16.713)

g pen_max = (4125/5000)*`th5000'  if eligible_aa==1 | eligible_op==1
label var pen_max "Maximum legal pension"

**************************************************************************
* 4.1.5. PAYG pension
**************************************************************************

g aux1 = reprate*aime if year==date_ret 
bys id: egen pen_in_payg = mean(aux1)
replace pen_in_payg = 0 if year < date_ret
label var pen_in_payg "Pension from pay-as-you-go pillar"
 
replace  pen_in_payg = pen_min_with_art8  if (pen_in_payg<pen_min_with_art8 & pen_min_with_art8 !=. & year>=date_ret )  
replace  pen_in_payg = pen_max  if (pen_in_payg>pen_max & pen_in_payg !=. & year>=date_ret )
drop aux*

**************************************************************************
* 4.2. Pension rights: second pillar (individual-accounts-defined-contributions pensions)
**************************************************************************

*  Cumulative savings in individual account

local rr = $rr

sort id year
g aux1 =  log(1+`rr') if age < min(age_ret,65)
gsort + id - year
bys id: gen aux2 = sum(aux1)
sort id year
bys id: gen disc_fac= exp(aux2)
label var disc_fac "Discount factor"
*drop aux1 aux2
g con_ia_cap = con_ia * (1- $comission / 0.15) *  disc_fac if contributes_sim==1
bys id (year): g individual_savings_account = sum(con_ia_cap) if age <= min(age_ret, 65)
label var individual_savings_account "Individual savings account"

* 	Annuity

g pen_in_ia = 0
bys id (year): replace pen_in_ia = (individual_savings_account/prima_bcu)*12 if age ==min(age_ret,65)
xtset id year
replace pen_in_ia = l.pen_in_ia  if age > min(age_ret,65)
label var pen_in_ia "Pension from individual pillar"

**************************************************************************
* 4.3. Total pension (payg + ia)
**************************************************************************
g pen_in = 0							
replace pen_in = pen_in_payg + pen_in_ia 	if year  >= date_ret
label var pen_in "Total pension, both pillars"	

**************************************************************************
* 5. Saving
**************************************************************************
save "$intermediate/pensions_aux.dta", replace

use "$intermediate/pensions_aux.dta", clear
keep  id age year age_ret date_ret my_labor_in_declared contributes_sim contributes_cum pen_in prob_contribut_panel my_labor_in_potential_panel tax_base_payg tax_base_ia con_ia con_payg tax_base_employer con_employer con_ss_in
save "$intermediate/pensions.dta", replace

/*
erase "$intermediate/pensions_aux.dta"
*/





