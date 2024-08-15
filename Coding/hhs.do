/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/

* We process the 2017 CEQ database (constructed from the 2017 household survey) to produce a cross-section database containing the income concepts we later use to estimate the redistributive impact of pensions. We also classify individuals into 20 groups (gender, public-private, income quintiles) to be used in the matching with social security data. We estimate probabilities of contributing and of evading social security contributions.

/*******************************************************************************************************
1. Data preparation: creation of variables needed for the analysis
2. Labor income: we estimate the potential labor income for individuals in 2017 (if they worked, what their labor income would be)
  2.1: Individuals who declared positive labor income in the hh survey: that income is assigned as the potential
  2.2: Individuals who did not declare positive labor income in the hh survey: we impute potential labor income using the predicted values after regressing observed labor income on some covariates.
3. Social Security: we compute pensions and evasion in ECH, and generate logit models for (i) the probability of contributing and (ii) the probability of evading, in the ECH database.
  3.1: Pensions: Individuals who declared pensions in ECH
  3.2: Contribution model: Logit model for contribution  
  3.3: Evasion: Individuals who declared any labor income and no contribution to SS
		3.3.1. Evades SS contributions
		3.3.2. Evasion model. We estimate the probability that individuals who are not contributing work.
4. Capital income
5. Classification of individuals into groups for matching with SS data. We create 20 groups: male/female; public/private; quintiles of labor income.
*******************************************************************************************************/

******************************************************************************************************************
* 1. Data preparation
******************************************************************************************************************

*Database - ECH 2017
use "$inputs/ECH/ECH 2017/ph17.dta", clear

*Variable cleanup
rename anio year
rename pesoano weight
rename numero hhid
rename nper memb_no
destring year, replace
destring pt4, dpcomma replace

drop _m

*Merging with variables of interest from CEQ 2017

global all_vars i_lab_in con_patfrl_in con_paths_in con_patss_in i_tpri_in i_cap_in i_vloc_in i_cppriv_in2 i_cppriv_in3 i_cppub_in2 i_cppub_in3 con_bankprof_in con_afap_in con_ss_in dtr_ncpx_in dtr_optx_in dtr_food_in i_cpen_in2 i_cpen_in3 dtx_pitc_in dtx_pitlab_in dtx_pitcp_in con_hslabsnis_in con_hslabother_in con_hscp_in con_paths_in con_patfrl_in con_frl_in	dtr_afam_hh dtr_ncpx_hh dtr_optx_hh dtr_food_hh
		
merge 1:1 hhid memb_no using "$intermediate/UY CEQ database 2017.dta", keepusing($all_vars years_school)
drop if _m!=3
drop _m
destring hhid, replace

g one=1
bys hhid: egen hsize = sum(one)   // See equation {eq:dmofhh}
drop one

label var i_cpen_in2 "Contributory pensions before taxes Public & Private(individual) - Only retirement"
label var i_cpen_in3 "Contributory pensions before taxes Public & Private(individual) - Only old age"

label var i_cppub_in2 "Contributory pensions before taxes - Public (individual) - Only retirement"
label var i_cppub_in3 "Contributory pensions before taxes - Public (individual) - Only old age"


foreach var of varlist $all_vars {
replace `var' = `var'/1000
}

* Generate id and save the codebook for later use
sort hhid memb_no
g id = _n
 preserve
 keep hhid memb_no id 
 save "$intermediate/matching_ids.dta", replace
 restore

replace con_hscp_in = 0 if i_cppriv_in2 + i_cppriv_in3 + i_cppub_in2 + i_cppub_in3 == 0
 
* Individual income
g lab_income_in = i_lab_in + con_patfrl_in + con_paths_in + con_patss_in 
g cap_income_in = i_cap_in + i_vloc_in
g net_pension_income_in = i_cppriv_in2 + i_cppriv_in3 + i_cppub_in2 + i_cppub_in3 - con_bankprof_in - con_afap_in - con_ss_in- con_patss_in 
g other_income_in = 0 /*i_tpri_in*/

g dtx_labor_in = dtx_pitlab_in  + con_hslabsnis_in +  con_hslabother_in + con_paths_in  + con_patfrl_in + con_frl_in
g dtx_pensions_in = dtx_pitcp_in + con_hscp_in
g dtx_capital_in = dtx_pitc_in

* Household and per capita income

foreach var in lab_income cap_income net_pension_income other_income dtx_labor dtx_pensions dtx_capital i_tpri {
	egen `var'_hh = total(`var'_in), by(hhid)
	g `var'_pc = `var'_hh/hsize
	
}

g dtr_all_hh = dtr_afam_hh + dtr_ncpx_hh + dtr_optx_hh + dtr_food_hh
g dtr_all_pc = dtr_all_hh/hsize

* Total income (household and per capita)

g aux = lab_income_in + other_income_in + cap_income_in 
egen CEQ_NSS_ym_hh = total(aux), by(hhid)
g CEQ_NSS_ym_pc = CEQ_NSS_ym_hh/hsize
drop aux

g aux = lab_income_in + other_income_in + cap_income_in + net_pension_income_in 
egen CEQ_SS_ym_hh = total(aux), by(hhid)
g CEQ_SS_ym_pc = CEQ_SS_ym_hh/hsize
drop aux

g CEQ_NSS_yd_hh = CEQ_NSS_ym_hh + dtr_all_hh - dtx_labor_hh - dtx_capital_hh
g CEQ_NSS_yd_pc = CEQ_NSS_yd_hh / hsize
g CEQ_SS_yd_hh  = CEQ_SS_ym_hh + dtr_all_hh - dtx_labor_hh - dtx_pensions_hh - dtx_capital_hh
g CEQ_SS_yd_pc  = CEQ_SS_yd_hh / hsize

label var CEQ_SS_yd_hh "CEQ disposable income with pensions - Household aggregate"
label var CEQ_NSS_yd_hh "CEQ disposable income without pensions - Household aggregate"
label var CEQ_SS_yd_pc "CEQ disposable income with pensions - Per capita"
label var CEQ_NSS_yd_pc "CEQ disposable income without pensions - Per capita"

label var dtr_all_hh "Direct public transfers - Household aggregate"
label var dtr_all_pc "Direct public transfers - Per capita"

label var dtx_labor_in "Direct taxes on labor - Individual"
label var dtx_labor_hh "Direct taxes on labor - Household aggregate"
label var dtx_labor_pc "Direct taxes on labor - Per capita"

label var dtx_pensions_in "Direct taxes on pensions - Individual"
label var dtx_pensions_hh "Direct taxes on pensions - Household aggregate"
label var dtx_pensions_pc "Direct taxes on pensions - Per capita"

label var dtx_capital_in "Direct taxes on capital - Individual"
label var dtx_capital_hh "Direct taxes on capital - Household aggregate"
label var dtx_capital_pc "Direct taxes on capital - Per capita"

label var CEQ_NSS_ym_hh "CEQ market income without pensions - Household aggregate"
label var CEQ_SS_ym_hh "CEQ market income with pensions - Household aggregate"
label var CEQ_NSS_ym_pc "CEQ market income without pensions - Per capita"
label var CEQ_SS_ym_pc "CEQ market income with pensions - Per capita"

label var cap_income_in "CEQ capital income - Individual"
label var cap_income_hh "CEQ capital income - Household aggregate"
label var cap_income_pc "CEQ capital income - Per capita"

label var other_income_in "CEQ other sources of income - Individual"
label var other_income_hh "CEQ other sources of income - Household aggregate"
label var other_income_pc "CEQ other sources of income - Per capita"

label var lab_income_in "CEQ labor income - Individual"
label var lab_income_hh "CEQ labor income - Household aggregate"
label var lab_income_pc "CEQ labor income - Per capita"

label var net_pension_income_in "CEQ net pension income - Individual"
label var net_pension_income_hh "CEQ net pension income - Household aggregate"
label var net_pension_income_pc "CEQ net pension income - Per capita"

label var i_tpri_in "CEQ private transfers - Individual"
label var i_tpri_hh "CEQ private transfers - Household aggregate"
label var i_tpri_pc "CEQ private transfers - Per capita"

*Variables for regressions
g age = (e27)
label var age "Age in years according to ECH"

g years_school2 = (years_school^2)/10^3
label var years_school2 "Years of formal education, squared"

g female = (e26==2)
label var female "=1 if the person is female"

g age2 = (age^2)/10^3
label var age2 " = age^2/10^3"

g contributes_ech = f82==1 | f96==1
label var contributes_ech "Contributing to SS according to ECH"

g contributes_contributed = contributes_ech==1 | f123==1
label var contributes_contributed "Contributing to SS now or in last job held according to ECH"
 
g public_worker = (f73==2 | f121==2)
label var public_worker "Public worker (main occupation is public, current if employed and previous if not working)"

replace public_worker = 1 if g148_1_2 > 0 | g148_1_5 > 0 | g148_1_6 > 0 // Pensioneers from the public sector

g aux1 = public_worker if memb_no == 1
bys hhid: egen aux2 = mean(aux1) 
replace public_worker = 1 if aux2 == 1 & age <=19 // Children of public employees
drop aux? 

*Classification into four groups (by sex and public/private). Warning: we are classifying as private all individuals who are not classified as public workers.
g classif = 1 			if female==0 & public_worker==1
replace classif = 2 	if female==1 & public_worker==1
replace classif = 3 	if female==0 & public_worker==0
replace classif = 4 	if female==1 & public_worker==0 
label define classif1	1 "male_pu" 2 "female_pu" 3 "male_pr" 4 "female_pr"
label values classif classif1
label var classif "Classification into male/female public/private"

*tab classif, m // All individuals are classified into a category

* Employer contribution rate
g aux1 = (con_patfrl_in + con_paths_in + con_patss_in)/i_lab_in 
sum aux1
matrix pat = r(mean)
matsave pat , replace p("$intermediate") saving
drop aux1
 
******************************************************************************************************************
* 2. Labor income
*******************************************************************************************************************
/* We generate the variable my_labor_in_potential_ech. It is an estimation of the labor income the individual would have obtained in the year of the survey if he had worked (hence the word "potential"). We are using labor income before taxes. If the individual declared a positive labor income, as registered in lab_income_in, then variable my_labor_in_potential_ech = lab_income_in. Otherwise, we impute a potential labor income based on attributes like age, years of schooling and the contribution status.  
*/
***************************************************************************************
* 2.1. Individuals who declared positive labor income in the hh survey.
***************************************************************************************
g my_labor_in_potential_ech = lab_income_in
label var my_labor_in_potential_ech "Potential labor income 2017 (imputed if labor income ceq=0), annual, thousand pesos"

sum my_labor_in_potential_ech lab_income_in [fw=weight]
sum my_labor_in_potential_ech if my_labor_in_potential_ech >0 [fw=weight], d
sum lab_income_in if lab_income_in >0 [fw=weight], d

***************************************************************************************
* 2.2. Individuals who did not declare positive labor income in the hh survey. 
***************************************************************************************
* Imputing potential labor income to individuals who did not declare positive labor income in the hh survey
* (We use 1800 as the max income (monthly 150000*12/1000), less than top 1%)

* Individuals aged 66 and +

forval categ = 1/4{
reg my_labor_in_potential_ech years_school years_school2 age age2 contributes_contributed if age >=66 & my_labor_in_potential_ech>0 & my_labor_in_potential_ech<1800 & classif == `categ'
predict fitted if age >=66 & classif == `categ'
replace fitted= 0 if fitted < 0 & age >=66 & classif == `categ'
replace my_labor_in_potential_ech = fitted if my_labor_in_potential_ech==0 & age >=66 & classif == `categ'
drop fitted
sum lab_income_in if lab_income_in>0 & age >=66 & classif == `categ', d
replace my_labor_in_potential_ech = r(p1) if my_labor_in_potential_ech <=r(p1) & age >=66 & classif == `categ'
}
sum my_labor_in_potential_ech lab_income_in

* Individuals aged 26-65
forval categ = 1/4{
reg my_labor_in_potential_ech years_school years_school2 age age2 contributes_contributed if age >=26 & age <=65 & my_labor_in_potential_ech>0 & my_labor_in_potential_ech<1800 & classif == `categ'
predict fitted if age >=26 & age <=65 & classif == `categ'
replace fitted= 0 if fitted < 0 & age >=26 & age <=65 & classif == `categ'
replace my_labor_in_potential_ech = fitted if my_labor_in_potential_ech==0 & age >=26 & age <=65 & classif == `categ'
drop fitted
sum lab_income_in if lab_income_in>0 & age >=26 & age <=65 & classif == `categ', d
replace my_labor_in_potential_ech = r(p1) if my_labor_in_potential_ech <=r(p1) & age >=26 & age <=65 & classif == `categ'
}
sum my_labor_in_potential_ech lab_income_in

* Individuals aged (i) 20-25 and (ii) less than 20
forval categ = 1/4{
reg my_labor_in_potential_ech years_school years_school2 age age2 contributes_contributed if age >=20 & age <=25 & my_labor_in_potential_ech>0 & my_labor_in_potential_ech<1800 & classif == `categ'

* Aged 20-25
predict fitted if ((age >=20 & age <=25) | (age ==26 & my_labor_in_potential_ech==0)) & classif == `categ' 
replace fitted= 0 if fitted < 0 & age >=20 & age <=26 & classif == `categ'
replace my_labor_in_potential_ech = fitted if my_labor_in_potential_ech==0 & age >=20 & age <=26 & classif == `categ'
drop fitted
sum lab_income_in if lab_income_in>0 & age >=20 & age <=26 & classif == `categ', d
replace my_labor_in_potential_ech = r(p1) if my_labor_in_potential_ech <=r(p1) & age >=20 & age <=26 & classif == `categ'

* Aged less than 20
g minor = age < 20
g aux1 = age
g aux2 = age2
replace age = 20 if minor ==1
replace age2 = (age^2)/10^3 if minor == 1
predict fitted if minor == 1 & my_labor_in_potential_ech==0 & classif == `categ' 
replace fitted= 0 if fitted < 0 & minor == 1 & classif == `categ'
replace my_labor_in_potential_ech = fitted if my_labor_in_potential_ech==0 & minor == 1 & classif == `categ'
drop fitted
sum lab_income_in if lab_income_in>0 & minor == 1 & classif == `categ', d
replace my_labor_in_potential_ech = r(p1) if my_labor_in_potential_ech <=r(p1) & minor == 1 & classif == `categ'
replace age = aux1
replace age2 = aux2
drop aux? minor
}

sum my_labor_in_potential_ech lab_income_in

***************************
* 3. Social Security
****************************
***************************************************************************************
* 3.1. Pensions. Individuals who declared pensions in ECH.
******************************************************************************************************************
* 3.1.1. Old age pensions. 
***************************

*We are using public and private pensions before taxes (excluding survivors pensions)

g pen_oa_ceq_in = i_cpen_in2
label var pen_oa_ceq_in "Old age pensions CEQ (individual), annual, thousands"

***************************************************************************************
* 3.1.2. Survivors and disability pensions. 
***************************************************************************************

g pen_sdi_ceq_in = i_cpen_in3
label var pen_sdi_ceq_in "Survivors and disability pensions CEQ,(individual), annual, thousands"

***************************************************************************************
* 3.1.3. Sum of old-age, survivors and disability pensions (CEQ). 
***************************************************************************************

g pen_ceq_in = pen_oa_ceq_in + pen_sdi_ceq_in
label var pen_ceq_in "Old-age, survivors and disability pensions CEQ,(individual), annual, thousands"

save "$intermediate/hhs_section3_1.dta", replace

***************************************************************************************
* 3.2. Contribution model
* We compute the probability that individuals in the ECH contribute to SS.
***************************************************************************************
quantiles my_labor_in_potential_ech [w=weight], gen(quint_rem) n(5)
g q1_rem = quint_rem==1
g q2_rem = quint_rem==2
g q3_rem = quint_rem==3
g q4_rem = quint_rem==4
g q5_rem = quint_rem==5

* Estimating the probability of contributing to SS
logit contributes_ech age age2 female q1_rem q2_rem q3_rem q4_rem if age >=20 & age <= 65 //

predict prob_contribut_ech if age >=20 & age <= 65, pr
label var prob_contribut_ech "Prob of contributing in the ECH year"

* Correcting prob_contribut_ech
count if prob_contribut_ech==. & age >=20 & age <= 65
count if prob_contribut_ech==. 

/* Many individuals have a missing because they are younger than 20 or older than 65 in 2017 (there are 3 cases with missing in working age). This causes that prob_contribut_panel (generated in another do file) has many missing values. We impute the average probability of contributing at 20 if the individual is younger than 20 and at 65 if the individual is older than 65. Warning: we are not considering income quintiles for this correction.
*/

sum prob_contribut_ech if age ==20
replace prob_contribut_ech = r(mean) if age <20
sum prob_contribut_ech if age ==65
replace prob_contribut_ech = r(mean) if age >65

***************************
* 3.3. Evasion
*********************************
* 3.3.1. Evades SS contributions
*********************************

 g evades_ech = (contributes_ech==0 & lab_income_in>0)
 label var evades_ech "Evading SS contributions"

***************************************************************************************
* 3.3.2. Evasion model. We estimate the probability that individuals who are not contributing work.
***************************************************************************************
g works = lab_income_in>0
lab var works "Working (labor income > 0)"

logit works age age2 female q1_rem q2_rem q3_rem q4_rem if age >=20 & age <= 65 & contributes_ech==0

matrix A = e(b) //e(b) = coefficient vector from model
matrix coef_evade = (A[1,1], A[1,2], A[1,3], A[1,4],A[1,5], A[1,6], A[1,7], A[1,8])
matrix colnames coef_evade = age age2 male q1_rem q2_rem q3_rem q4_rem const 
matsave coef_evade , replace p("$intermediate") saving

predict prob_evade_ech if age >=20 & age <= 65, pr
label var prob_evade_ech "Prob someone not contributing is working in the ECH year"

***************************************************************************************
* 4. Classification into groups for matching with SS data
***************************************************************************************
/* 
We classify individuals in the HH survey in 20 groups. Within each of the four categories generated above (variable classif coding for male/female and public/private), we compute income quintiles controling by age. There are no missing values in the variables used for this classification, so all individuals in the ECH are assigned to one of the 20 groups:

codebook classif my_labor_in_potential_ech age 
*/

g individual_effect=0

g quintiles_ech =.

forval categ = 1/4{
	reg my_labor_in_potential_ech age age2 if age >=20 & age <= 65 & classif == `categ'
	predict fitted 
	replace individual_effect = my_labor_in_potential_ech - fitted 
	drop fitted
	xtile aux = individual_effect if classif == `categ', nq(5) 
	replace quintiles_ech = aux if classif == `categ'
	drop aux
}

g groups = ""
forval quint = 1/5{
replace groups = "female_prQ`quint'" if classif==4 & quintiles_ech==`quint'
replace groups = "male_prQ`quint'" if classif==3 & quintiles_ech==`quint'
replace groups = "female_puQ`quint'" if classif==2 & quintiles_ech==`quint'
replace groups = "male_puQ`quint'" if classif==1 & quintiles_ech==`quint'
}

label var groups "Groups by gender, pub-priv, quintile"


* Checking pen_oa_ceq_in
tab pobpcoac if pen_oa_ceq_in>0 // El 86% se declara jubilado y el 14% también tiene ingresos laborales
tab works pobpcoac if pen_oa_ceq_in>0 // 
tab g_it_1 g_it_2 if pen_oa_ceq_in>0 // Un 98% de quienes tienen pen_oa_ceq_in>0 percibieron ingresos por jubilaciones. Un 2% percibieron pensiones (aunque igualmente hay solapamiento casi total)
tab f125 if pen_oa_ceq_in>0 & g_it_2==1 // 99% de quienes percibieron pensiones, son pensionistas por sobrevivencia

save "$intermediate/hhs_section4.dta", replace

***************************************************************************************
* 5. Computing apparent tax rates (to be used in life cycle model) 
***************************************************************************************

use "$intermediate/hhs_section4.dta", clear

g dtxr_labor_in = dtx_labor_in/lab_income_in

g pension_income = i_cppriv_in2 + i_cppriv_in3 + i_cppub_in2 + i_cppub_in3 
g dtxr_pensions_in = dtx_pensions_in/pension_income
 
g dtxr_capital_in = dtx_capital_in/cap_income_in 

sum dtxr_labor_in, d
matrix dtxr_labor = r(p50) 
sum dtxr_pensions_in, d
matrix dtxr_pensions = r(p50) 
sum dtxr_capital_in, d
matrix dtxr_capital = r(mean) 

matrix apparent_tax_rates = (dtxr_labor \ dtxr_pensions \ dtxr_capital)
matrix rownames apparent_tax_rates = "Labor" "Pensions" "Capital" 
matsave apparent_tax_rates , replace p("$intermediate") saving

* Saving  

keep hhid memb_no year weight id age age2 groups my_labor_in_potential_ech prob_contribut_ech female q1_rem q2_rem q3_rem q4_rem prob_evade_ech contributes_ech evades_ech pen_oa_ceq_in pen_sdi_ceq_in pen_ceq_in lab_income_in i_cap_in i_cppub_in2 cap_income* lab_income*  net_pension*  i_tpri* i_lab_in i_cppub_in3 other_income* CEQ_NSS* CEQ_SS* hsize con_paths_in con_ss_in con_patfrl* i_vloc_in con_patss* con_bankprof* i_cppriv_in2 i_cppriv_in3 dtr_all_pc dtx_labor* dtx_pensions* dtx_capital*

save "$intermediate/hhs.dta", replace

use "$intermediate/hhs.dta", clear
collapse year lab_income_hh other_income_hh cap_income_hh net_pension_income_hh CEQ_NSS* CEQ_SS* i_tpri_hh dtx_labor_hh  dtx_pensions_hh dtx_capital_hh , by(hhid) 

save "$intermediate/hhs_hh.dta", replace

