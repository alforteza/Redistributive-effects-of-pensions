/* 
This do file is freely available and may be used under proper citation. Please cite as follows:
Forteza, Alvaro and Diego Tuzman (2024): The Redistributive Effect of Pensions. The Case of Uruguay. Departamento de Economía, FCS-UDELAR. 
*/


* Version 6.4 - (ADJUST THE GLOBAL $vers) 

/* OUTLINE

1. Preamble.

2. Household survey.

3. Social security data (BPS): computing age earning and contribution profiles using SS data.

4. Panel active: expands HHS database to generate work histories for active workers in 2017 using SS data. 

5. Pensions: simulates pensions for active workers in 2017.

6. Work histories for people retired in 2017.

7. Life-cycle approach: computing income concepts.

8. Tables and figures

9. Description and checking of databases

*/


/*******************************************************************************************************
1. Preamble.
******************************************************************************************************
Set version
Generate paths
Input parameters
Defines scenarios
*/

 ******************IMPORTANT*****************
 *VERSION BELOW MUST BE CHANGED WHEN APPLICABLE*
 global vers = "6_4"
 ******************IMPORTANT*****************

 
if ("`c(hostname)'"=="Diego-Tuzman0") global path "C:\Users/Usuario/OneDrive/Documentos/Doctorado/Pensions" 
else if ("`c(hostname)'"=="MacBook-Pro-de-Alvaro.local") global path "/Users/alvaroforteza/Documents/Pensions"  
else global path "/Users/alvaroforteza/Documents/Pensions"  

global intermediate "$path/Data/Intermediate"
global inputs "$path/Data/Inputs"
global dofiles "$path/Coding/Version $vers"
global results "$path/Data/Results"

* Parameters to be used in several do files
global rr = 0.02 // Real interest rate (measured in wage index)
global comission = 0.03 // Managing and insurance costs (comisiones afap + costo seguro colectivo invalidez y fallecimiento)
global th5000 = 48953*12/1000  // Thresholds in law 16.713 updated to 2017, in annual terms, thousand  pesos. https://www.bps.gub.uy/bps/valoreshistoricos.jsp?idVariable=29&contentid=5479, consult 10/08/2022. 

* Setting a seed
set seed 24106449

* Defines scenarios
global scenario = "Baseline" 
*global scenario = "SSflat" //For scenario assuming no real wage growth
*global scenario = "SSone" //For scenario assuming 1% real wage growth
*global scenario = "SStwo" //For scenario assuming 2% real wage growth
*global scenario = "SSthree" //For scenario assuming 3% real wage growth
*global scenario = "RRzero" //For scenario assuming a zero percent real interest rate (measured in wage index)
*global scenario = "RRone" //For scenario assuming a 1% real interest rate (measured in wage index)
*global scenario = "RRthree" //For scenario assuming a 3% real interest rate (measured in wage index)
*global scenario = "RRfour" //For scenario assuming a 4% real interest rate (measured in wage index)


if "$scenario" == "RRzero" global rr = 0 //For scenario assuming a zero percent real interest rate (measured in wage index)
if "$scenario" == "RRone" global rr = 0.01 //For scenario assuming a 1% real interest rate (measured in wage index)
if "$scenario" == "RRthree" global rr = 0.03 //For scenario assuming a 4% real interest rate (measured in wage index)
if "$scenario" == "RRfour" global rr = 0.04 //For scenario assuming a 4% real interest rate (measured in wage index)

/*******************************************************************************************************
2. Household survey.
*******************************************************************************************************
*/
* We process the 2017 household survey to produce a cross-section database containing the income concepts we later use to estimate the redistributive impact of pensions. We also classify individuals into 20 groups (gender, public-private, income quintiles) to be used in the matching with social security data. We estimate probabilities of contributing and of evading social security contributions. 

do "$dofiles/hhs.do"

*******************************************************************************************************
* 3. Social security data (BPS).
*******************************************************************************************************
  
* We use data from SS work histories to compute age labor income and contribution profiles. BPS 2022 sample
/*
RDS to DTA.R
do "$dofiles/ims.do"
do "$dofiles/ss_database.do"
do "$dofiles/ss.do"  
*/

 
*******************************************************************************************************
* 4. Panel creation, wages, densities
*******************************************************************************************************  

/* Starting with ECH 2017 (cross-section), we construct a panel using inputs from SS data (BPS). We build a panel for the desired age spells of each individual. Merge with estimated wage increases by group and age to simulate wage histories starting with observed wages in 2017. Impute probability of contributing according to age and group. Simulate contributory status. Using the calculated probability (working | not contributing), simulate evasion. Output variables: my_labor_in_potential (now as a panel variable), contributes_sim, evades_sim.
*/
do "$dofiles/panel_active.do"

*******************************************************************************************************
* 5. Pensions 
*******************************************************************************************************
* We compute pension rights of individuals who were not retired in 2017 according to the HH survey (using pension rules in law 16.713).
  
do "$dofiles/pensions.do"

*******************************************************************************************************
* 6. Labor income for people retired in 2017
*******************************************************************************************************
* We impute labor income of individuals who were retired in 2017 according to the HH survey. We replace pensions computed in section 5 with pensions in the hh survey.
  
do "$dofiles/panel_active_retired.do"

*******************************************************************************************************
* 7. Life-cycle approach: computing income concepts.
*******************************************************************************************************  

* Calculating income concepts for the life-cycle approach

*do "$dofiles/mortality.do"  // Computing mortality rates using data from BCU

do "$dofiles/life_cycle_ym.do" // Market income (this do file calls irr.do)
do "$dofiles/life_cycle_yd.do" // Disposable income (this do file calls irr.do)

*******************************************************************************************************
* 8. Tables and figures
*******************************************************************************************************

do "$dofiles/results.do"

*******************************************************************************************************
* 9. Description and checking of databases
*******************************************************************************************************
stop
do "$dofiles/descriptive_statistics.do"

do "$dofiles/describe_databases.do"

do "$dofiles/checking.do"
