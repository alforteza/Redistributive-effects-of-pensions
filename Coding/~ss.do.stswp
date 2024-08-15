/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/
 
/* We compute age earnings and contributing-status profiles using data from SS. The input is a database generated with ss_database.do.
*/

use "$intermediate/ss_annual_2022.dta", clear

g categ = "female_pu" if female == 1 & public == 1
replace categ = "female_pr" if female == 1 & public == 0
replace categ = "male_pu" if female == 0 & public == 1
replace categ = "male_pr" if female == 0 & public == 0

global per = 5

***************************************************************************************************************************
* 1. Classify individuals in income quantiles
***************************************************************************************************************************
	/* Since we are observing different ages, we control by age.
	We use the individual effect to estimate quantiles.
	*/
	
	xtset id age
	g age2 = (age^2)/10^3
	g alive_and_not_retired = year < year(death_date) & year < year(retirement_date)
	
foreach categ in female_pu female_pr male_pu male_pr {
	xtreg wages_bps age age2 if age >=20 & age <= 65 & categ == "`categ'" &  alive_and_not_retired == 1, fe  // Using re, the estimation of u can be zero due to corner solutions: https://www.stata.com/statalist/archive/2011-08/msg01395.html  
		
	predict aux if wages_bps >0  &  categ == "`categ'" , u
	bys id: egen nu_`categ' = mean(aux) if  categ == "`categ'"
	*count if nu==. //Checking
*	drop if nu_female_pu == .   // There are missing nu for individuals without wages_bps data at relevant ages.
	drop aux
	local lper = $per
	xtile quantile_bps_`categ' = nu_`categ' , nq(`lper')
}

*  Classification into groups

g groups = "."
foreach categ in female_pu female_pr male_pu male_pr {			
	forvalues x = 1(1)$per {
		replace groups = "`categ'Q`x'" if quantile_bps_`categ' == `x'
	}	
}
g aux = groups == "."
sum aux
drop if groups == "." // Warning: We are dropping individuals we could not classify into groups.Check the number is not too large... 
drop aux

save "$intermediate/ss_section_1.dta", replace
	
****************************************************************************************************************	
* 2. Estimating age labor-income profiles
****************************************************************************************************************	
* 2.1. Estimation
****************************************************************************************************************
	use  "$intermediate/ss_section_1.dta", clear

* 2.1.1. Estimating  my_labor_income_profile
	
xtset id age
g my_labor_income_profile = .
label var my_labor_income_profile "SS labor income profiles"

local lper = $per

foreach categ in female_pu female_pr male_pu male_pr {	
	forvalues x = 1(1) `lper' {
*		xtreg wages_bps i.age   if  quantile_bps_`categ' == `x' &  age >=20 & age <= 65 & alive_and_not_retired == 1, 
		xtreg wages_bps age age2  if  quantile_bps_`categ' == `x' &  age >=20 & age <= 65 & alive_and_not_retired == 1, fe		
		predict aux1 if quantile_bps_`categ' == `x' & age >=20 & age <= 65 & alive_and_not_retired == 1  , xb
		replace my_labor_income_profile = aux1  if quantile_bps_`categ' == `x' &  age >=20 & age <= 65 & alive_and_not_retired == 1
		drop aux1	
	}
}

* Checking and replacing.
* The variable my_labor_income_profile may have a few non-positive values because of the use a linear model to impute values the prediction. We do two things: (i) We check wether we do have cases like this and explore their incidence, and (ii) we impute a missing in those cases, if we are convinced that this is not a relevant phenomenon.

	sum my_labor_income_profile
	if r(min) <= 0 {
	dis "ABORT: predicted labor income includes some non-positive values!"
	ABORT
	}

*	replace my_labor_income_profile = . if my_labor_income_profile <= 0 // To be used (with caution!) if the ABORT is activated.

* 2.1.2. Collapsing to one observation per "groups-age"

bys groups age: egen sd = sd(my_labor_income_profile) // We verify that my_labor_income_profile is invariant within "groups-age"
sum sd
if r(mean) > 10-6 {
	dis "Profiles not invariant within groups-age"
	ABORT
}

collapse my_labor_income_profile quantile_bps_* , by(groups age)

* 2.1.3. Estimating  increase_my_labor_in

g increase_my_labor_in = 0
label var increase_my_labor_in "Percent increase in remunerations according to group and age"
encode groups, g(groups_code)
xtset groups_code age
replace increase_my_labor_in = (my_labor_income_profile / l.my_labor_income_profile)-1 if  my_labor_income_profile!=.			
sum increase_my_labor_in , d

keep groups groups_code age my_labor_income_profile increase_my_labor_in quantile_bps_*
sort groups age
save "$intermediate/ss_section_21.dta", replace

****************************************************************************************************************
* 2.2. Checking 
****************************************************************************************************************
/*
use "$intermediate/ss_section_21.dta", clear

local lper = $per
foreach categ in female_pu female_pr male_pu male_pr {	
	forvalues x = 1(1) `lper' {
		capture drop `categ'_`x'
		g `categ'_`x' = my_labor_income_profile if quantile_bps_`categ' == `x'
	}
twoway 	(scatter `categ'_1  age if age<=65, yaxis(1)) (scatter `categ'_2  age if age<=65, yaxis(1))	(scatter `categ'_3  age if age<=65, yaxis(1)) (scatter `categ'_4  age if age<=65, yaxis(1)) (scatter `categ'_5  age if age<=65, yaxis(1)) , title(`categ') legend(off) saving("$intermediate/age_labor_income_profile_`categ'.gph", replace)
capture drop `categ'_?
}

graph combine "$intermediate/age_labor_income_profile_female_pu.gph" "$intermediate/age_labor_income_profile_female_pr.gph" "$intermediate/age_labor_income_profile_male_pu.gph" "$intermediate/age_labor_income_profile_male_pr.gph", ycommon title(Age labor income profiles) saving("$intermediate/age_labor_income_profiles.gph", replace)

* graph use "$intermediate/age_labor_income_profile_female_pu.gph"
* graph use "$intermediate/age_labor_income_profile_female_pr.gph"
* graph use "$intermediate/age_labor_income_profile_male_pu.gph"
* graph use "$intermediate/age_labor_income_profile_male_pr.gph"
* graph use "$intermediate/age_labor_income_profiles.gph"
*/

***************************************************************************************
* 2.3. Saving a database with age labor income profiles  
***************************************************************************************

use "$intermediate/ss_section_21.dta", clear
keep groups groups_code age my_labor_income_profile increase_my_labor_in
sort groups age
save "$intermediate/ss.dta", replace

***************************************************************************************
* 3. Estimating age contributing-status profiles
***************************************************************************************
* 3.1. Estimation
***************************************************************************************

* Logit model for the contributing status.
* Saving coefficients to compute probability of contributing in ECH individuals.

	use  "$intermediate/ss_section_1.dta", clear
	xtset id age
	
	* Making "contributes_bps" a binary variable to estimate a Logit model.
	
	g contributes_bps = .
	replace contributes_bps = 0 if cotiza <0.5
	replace contributes_bps = 1 if cotiza >=0.5 & cotiza != .
	
*  Logit model

g contributes_pr = .
label var contributes_pr "Probability of contributing"
local lper = $per 
foreach categ in female_pu female_pr male_pu male_pr {	
	forvalues x = 1(1) `lper' {
		xtlogit contributes_bps age age2 if quantile_bps_`categ' == `x' & age >=20 & age <= 65 & alive_and_not_retired == 1
*		xtlogit contributes_bps age age2 if quantile_bps_`categ' == `x' & age >=20 & age <= 65 
		matrix A = e(b) // e(b) = coefficient vector from model
		matrix coef`categ'q`x' = (A[1,1], A[1,2], A[1,3], A[1,4])
		matrix colnames coef`categ'q`x' = age age2 cons lnsig2u  
		matsave coef`categ'q`x' , replace p("$intermediate") saving	
		predict aux1 if quantile_bps_`categ' == `x' & age >=20 & age <= 65 & alive_and_not_retired == 1  , pr
*		predict aux1 if quantile_bps_`categ' == `x' & age >=20 & age <= 65  , pr
		replace contributes_pr = aux1 if quantile_bps_`categ' == `x' & age >=20 & age <= 65 & alive_and_not_retired == 1
*		replace contributes_pr = aux1 if quantile_bps_`categ' == `x' & age >=20 & age <= 65 		
		drop aux1
	}
}

save "$intermediate/ss_section3_1.dta", replace

****************************************************************************************************************
* 3.2. Checking
****************************************************************************************************************

/*
use "$intermediate/ss_section3_1.dta", clear

global per = 5
local lper = $per
foreach categ in female_pu female_pr male_pu male_pr {			
	forvalues x = 1(1) `lper' {
		capture drop `categ'_`x'
		g `categ'_`x' = contributes_pr if quantile_bps_`categ' == `x'
	}

sort id age	
twoway 	(scatter `categ'_1  age if age<=65, yaxis(1)) (scatter `categ'_2  age if age<=65, yaxis(1))	(scatter `categ'_3  age if age<=65, yaxis(1)) (scatter `categ'_4  age if age<=65, yaxis(1)) (scatter `categ'_5  age if age<=65, yaxis(1)) , title(`categ') legend(off) saving("$intermediate/age_contributions_profile_`categ'.gph", replace)
capture drop `categ'_?
}

graph combine "$intermediate/age_contributions_profile_female_pu.gph" "$intermediate/age_contributions_profile_female_pr.gph" "$intermediate/age_contributions_profile_male_pu.gph" "$intermediate/age_contributions_profile_male_pr.gph", ycommon title(Age contributions profiles) saving("$intermediate/age_contributions_profiles.gph", replace)
/*
graph use "$intermediate/age_contributions_profile_female_pu.gph"
graph use "$intermediate/age_contributions_profile_female_pr.gph"
graph use "$intermediate/age_contributions_profile_male_pu.gph"
graph use "$intermediate/age_contributions_profile_male_pr.gph"
graph use "$intermediate/age_contributions_profiles.gph"
 */
*/
