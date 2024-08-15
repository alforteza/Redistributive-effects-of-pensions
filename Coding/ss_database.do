/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/


*******************************************************************************************************
* Social security data (BPS).
*******************************************************************************************************
 
/* We build a SS database. The input is a stata database built using "RDS to DTA.R" on three files sent by BPS in 2022: (i) muestra300.RDS, (ii) muestra_HL.RDS y (iii) art_8.RDS.
*/


/*
use "$intermediate/muestra_HL.dta" , clear

keep ID_PERS fecha_nacimiento cod_sexo ANIO_MES TA ACUMULACION_LABORAL DIAS_TRABAJADOS VINCULO_FUNCIONAL TIPO_REMUNERACION CATEGORIA CONCEPTO_1 CONCEPTO_2 CONCEPTO_3
save "$intermediate/muestra_HL_after_dropping.dta" , replace

preserve
keep ID_PERS
duplicates drop
set seed 1234
sample 10
save "$intermediate/aux_individuals.dta" , replace
restore

merge m:1 ID_PERS using "$intermediate/aux_individuals.dta"
keep if _m==3

save "$intermediate/muestra_HL_10percent.dta" , replace
*/

/*
* Choose a database (decreasing size)

use "$intermediate/muestra_HL.dta" , clear
use "$intermediate/muestra_HL_after_dropping.dta" , clear
use "$intermediate/muestra_HL_10percent_all_vars.dta", clear
use "$intermediate/muestra_HL_10percent.dta" , clear
*/

use "$intermediate/muestra_HL.dta", clear

drop if DIAS_TRABAJADOS < 0 
*sum _merge
cap drop _merge

**************************************************************************************************************************
* 0. Labeling variables, redefining time variables, etc.
**************************************************************************************************************************

* 0.1. Labels

* We label variables according to: (i) Documentacion_v1.docx (from BPS), and (ii) Sistema Unificado BPS-MTSS, Documento de registro de información. Réplica base de datos. (file: Diccionario réplica PTU.docx) 


label var DIAS_TRABAJADOS "Number of days worked by the worker in the year and month of reference"
label var ANIO_MES "Year and month when the payroll was submitted"
*label var HORAS_SEMANALES "Number of hours worked in a week by the worker in the year and month of reference"
label var ID_PERS "Identificator, unique for each worker"
label var fecha_nacimiento "Date of birth"
label var cod_sexo "Sex (1=male; 2=female)"
label var TA "Tipo de aportacion - Codes for different kinds of contributions"
label var ACUMULACION_LABORAL "Indicates to which of the person's jobs it corresponds (in case of working more than one job in a given month)"
label var VINCULO_FUNCIONAL "Type of contract with employer"
label var TIPO_REMUNERACION "Type of worker (monthly, daily, on commission, by product"
label var CATEGORIA "Category of worker" //???
label var CONCEPTO_1 "Monthly remuneration"
label var CONCEPTO_2 "Amount perceived as Christmas bonus (aguinaldo)"
label var CONCEPTO_3 "Retroactive payments"
label var FECHA_EGRESO "Date the individual leaves the job"
label var COD_CAUSAL_EGRESO "Motive for leaving the job"

label define aportacion 1 "Ind y Com" 2 "Civil" 3 "Rural" 4 "Construcción" 48 "Serv Doméstico" 5 "Notarial" 6 "Bancaria" 7 "Trabajo a Domicilio" 8 "Escolar" 11 "Profesional" 12 "Militar" 13 "Policial" 14 "Unión Postal" 
label values TA aportacion

*tab TIPO_REMUNERACION

* 0.1. Sex 

g female = 0
replace female = 1 if cod_sexo == 2

* 0.2. Dates

tostring ANIO_MES, generate(str_ANIO_MES)
g date = date(str_ANIO_MES,"YM")
format date %td
drop ANIO_MES str_ANIO_MES

g year = year(date)
g month = month(date)

rename fecha_nacimiento birth_date
format birth_date %td
g year_birth = year(birth_date)
g month_birth = month(birth_date)

g age = year - year_birth
replace age = age + 1 if month>month_birth

format FECHA_INGRESO %td
format FECHA_EGRESO %td

g aux1 = FECHA_EGRESO if COD_CAUSAL_EGRESO == 3
bys ID_PERS: egen death_date = min(aux1)
format death_date %td
capture drop aux1

g aux1 = FECHA_EGRESO if COD_CAUSAL_EGRESO == 5
bys ID_PERS: egen retirement_date = min(aux1)
format retirement_date %td
capture drop aux1

* Correcting imputed death and retirement dates. Some individuals register worked days after we imputed death or retirement. We replace death_date and retirement_date by missing in those cases.

g dead_contributing = DIAS_TRABAJADOS > 0 &  date > death_date & (DIAS_TRABAJADOS != . & date != .)
sum dead_contributing
bys ID_PERS: egen aux1 = max(dead_contributing) 
* order ID_PERS date death_date dead_contributing aux1 DIAS_TRABAJADOS
replace death_date = . if aux1 == 1
drop aux1

g retired_contributing = DIAS_TRABAJADOS > 0 &  date > retirement_date & (DIAS_TRABAJADOS != . & date != .)
sum retired_contributing
bys ID_PERS: egen aux1 = max(retired_contributing) 
* order ID_PERS date retirement_date retired_contributing aux1 DIAS_TRABAJADOS
replace retirement_date = . if aux1 == 1
drop aux1

* Checking
count if death_date < retirement_date & retirement_date != . 
if r(N) > 0 {
	dis "Some individuals die before retiring!"
	ABORT
} 

bys ID_PERS: egen min_date = min(date)
format min_date %td

**************************************************************************************************************************
* 1. Computing the number of days worked each month.
**************************************************************************************************************************
* We compute the number of days worked in the month considering that many individuals have more than one job in the same month.
 
* The equivalent worked days (DIAS_TRABAJADOS_EQUIV) are adjusted by the factor (30*11/150) in the case of "jornalero" (TIPO_REMUNERACION == 2). This adjusment factor stems from the SS administration (BPS) registering a year of service if the worker completed 150 days of work (R.D. N° 12-37/2005). In other types (TIPO_REMUNERACION), the year of service are computed as 30 days times 11 months.
 
g DIAS_TRABAJADOS_EQUIV = DIAS_TRABAJADOS
replace DIAS_TRABAJADOS_EQUIV = DIAS_TRABAJADOS * (30*11 / 150) if TIPO_REMUNERACION == 2
replace DIAS_TRABAJADOS_EQUIV = 30 if DIAS_TRABAJADOS_EQUIV > 30 & DIAS_TRABAJADOS_EQUIV != .

bys ID_PERS year month: egen days_max = max(DIAS_TRABAJADOS_EQUIV)
*order ID_PERS year month days_max DIAS_TRABAJADOS_EQUIV DIAS_TRABAJADOS TIPO_REMUNERACION
sort ID_PERS year month

bys ID_PERS year month: g aux1 = _N
g several_occupations = aux1 > 1
label var several_occupations "The individual has more than 1 job in the month"
sum several_occupations
*order ID_PERS year month aux1 several_occupations days_max DIAS_TRABAJADOS_EQUIV DIAS_TRABAJADOS

g problem =  several_occupations == 1 &  DIAS_TRABAJADOS_EQUIV < 30
sum problem

* Generating worked_days

g worked_days = 0
replace worked_days = DIAS_TRABAJADOS_EQUIV if several_occupations == 0
replace worked_days = days_max if several_occupations == 1 & days_max == 30
order ID_PERS year month several_occupations days_max DIAS_TRABAJADOS_EQUIV worked_days

bys ID_PERS year month: egen worked_days_adding = total(DIAS_TRABAJADOS_EQUIV) if days_max < 30 & several_occupations == 1
replace worked_days_adding = 30 if worked_days_adding > 30 & worked_days_adding != .
order ID_PERS year month FECHA_INGRESO FECHA_EGRESO days_max worked_days_adding DIAS_TRABAJADOS_EQUIV  DIAS_TRABAJADOS
sort ID_PERS year month FECHA_INGRESO

gen aux2 = worked_days_adding > 30 & worked_days_adding != .
sum aux2
drop aux*

replace worked_days = worked_days_adding if days_max < 30 & several_occupations == 1

* worked_days = 0 if there is no report before retirement and death
replace worked_days = 0 if worked_days == . & date < retirement_date & date < retirement_date

* worked_days = . after retirement or death
replace worked_days = . if  (date >= death_date | date >= retirement_date ) & date != .

**************************************************************************************************************************
* 2. Sector of activity: public or private
************************************************************************************************************************** 
 
* Defined as public worker when at least half of the time in the base belongs to "caja civil".

*These calculations are done by job-month-individual
bys ID_PERS: egen cotiza_pub_aux = total(DIAS_TRABAJADOS) if TA == 2
bys ID_PERS: egen cotiza_pub = mean(cotiza_pub_aux)
replace cotiza_pub = 0 if cotiza_pub == . 

bys ID_PERS: egen cotiza_total = total(DIAS_TRABAJADOS)
g public_prop = cotiza_pub/cotiza_total

g public = public_prop > 0.5
sum public

save "$intermediate/ss_database_section2.dta" , replace

**************************************************************************************************************************
* 3. Computing labor income
**************************************************************************************************************************
use "$intermediate/ss_database_section2.dta" , clear
   
replace CONCEPTO_1 =0 if CONCEPTO_1==.
replace CONCEPTO_2 =0 if CONCEPTO_2==.
replace CONCEPTO_3 =0 if CONCEPTO_3==.

g aux = CONCEPTO_1 + CONCEPTO_2 + CONCEPTO_3
drop CONCEPTO_1 CONCEPTO_2 CONCEPTO_3

merge m:m year month using "$intermediate/ims_monthly.dta"
drop _merge
sum ims if year == 2017
g wages_bps = aux/(ims/r(mean))
label var wages_bps "Labor income from BPS deflated by IMS"

**************************************************************************************************************************
* 4. Computing the proportion of the month the individual contributed
**************************************************************************************************************************

* 4.1. Collapsing to individual-year-month. (a) We add wages in all the jobs in each month. (b) We verify that (i) worked_days and (ii) date contain only one value per individual-year-month (other variables do)

bys ID_PERS year month: egen sd = sd(worked_days)
sum sd
drop sd

bys ID_PERS year month: egen sd = sd(date)
sum sd
drop sd

collapse (mean) worked_days date female death_date birth_date public retirement_date (sum) wages_bps, by(ID_PERS year month)

* 4.2. Computing cotiza as the proportion of the month the individual contributes. We assume a dependent workers is expected to work 5 days per week and there are 4.4 weeks per month, so we say an individual contributed 100% of the time if he contributed 22 (=5*4.4) or more days in the month.   

g cotiza = worked_days/22 
replace cotiza = 1 if cotiza > 1 & cotiza != . 
replace cotiza = 0 if cotiza == . & date < death_date & date < retirement_date 

save "$intermediate/ss_database_section4.dta" , replace
 

**************************************************************************************************************************
* 5. Building a "balanced" panel 
**************************************************************************************************************************

use "$intermediate/muestra_HL_10percent_all_vars.dta", clear

keep ID_PERS
duplicates drop
merge 1:m ID_PERS using "$intermediate/ss_database_section2.dta", keepusing(birth_date retirement_date death_date )
drop _merge
duplicates drop

expand 25 // 25 years spanning 1996 to 2020.
bys ID_PERS: g year = 1995 + _n
sort ID_PERS year

expand 12 // per month
bys ID_PERS year: g month = _n
sort ID_PERS year month

merge 1:1 ID_PERS year month using "$intermediate/ss_database_section4.dta"
drop _merge


* Time invariant attributes of individuals

rename female female2
bys ID_PERS: egen female = mean(female2)
drop female2

* We take the binary public as an attribute of the individual
rename public public2
bys ID_PERS: egen public = mean(public2)
drop public2

g birth_year = year(birth_date)
g age = year - birth_year
keep if age >= 20 & age <= 70

* Zero wages reset to missing
replace wages_bps = . if wages_bps == 0

* An individual who does not register worked days and contributions in a period should have zero, not missing, in these variables.
replace worked_days = 0 if worked_days == . 
replace worked_days = . if (date > death_date | date > retirement_date) & date !=.
replace cotiza = 0 if cotiza == .
replace cotiza = . if (date > death_date | date > retirement_date) & date !=.
replace cotiza = . if year == 1996 & month < 4

* Checking: 
count if worked_days == . &  date < retirement_date & retirement_date!=. 
count if worked_days != . & date >= retirement_date & retirement_date!=.

count if cotiza == . &  date < retirement_date & retirement_date!=. 
if r(N) >0 {
	dis "Variable "cotiza" cannot be missing before retirement"
	ABORT
}
count if cotiza != . & date > retirement_date & date !=.
if r(N) >0 {
	dis "Variable cotiza must be missing after retirement"
	ABORT
}

rename ID_PERS id
save "$intermediate/ss_monthly_2022.dta", replace

use "$intermediate/ss_monthly_2022.dta", clear
collapse (mean) birth_date death_date retirement_date wages_bps cotiza female public age    , by(id year)
save "$intermediate/ss_annual_2022.dta", replace

