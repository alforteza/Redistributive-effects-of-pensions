* Computing mortality rates using information from BCU

*Mortality rates male

import excel "$inputs/Tablas de mortalidad/Tablas dinámicas2.xlsx", sheet("aux hombres") cellrange(H3:GI104) firstrow clear
local year = 2017
foreach var of varlist H-GI {
	rename `var' mortality`year'
	local year = `year' +1
}

g age = _n -1
reshape long mortality, i(age) j(year)
g female = 0

save "$intermediate/male_aux.dta", replace


*Mortality rates female

import excel "$inputs/Tablas de mortalidad/Tablas dinámicas2.xlsx", sheet("aux mujeres") cellrange(H3:DF104) firstrow clear
local year = 2017
foreach var of varlist H-DF {
	rename `var' mortality`year'
	local year = `year' +1
}

g age = _n -1
reshape long mortality, i(age) j(year)
g female = 1

save "$intermediate/female_aux.dta", replace

* Merging
append using "$intermediate/male_aux.dta"

sort age year female
save "$intermediate/mortality.dta", replace

* Mortality 2017 cohort

use "$intermediate/mortality.dta", clear

keep  if year - age == 2017 
keep age mortality female
sort female age

save "$intermediate/mortality_cohort2017.dta", replace
