*Results
***ELIMINAR ESTAS LINEAS LUEGO
clear all
global path "C:\Users/Usuario/OneDrive/Documentos/Doctorado/Paper 2" 
global intermediate "$path/Data/Intermediate"
global inputs "$path/Data/Inputs"
global scenario "baseline"
***ELIMINAR ESTAS LINEAS LUEGO



use "$intermediate/life_cycle_yd_section6_aux.dta", clear
merge m:1 hhid year using "$intermediate/life_cycle_yd_hh_$scenario.dta", keepusing(assets_vsc_SS_pc assets_vsb_pc income_volunt_assets_SS_hh assets_vsc_NSS_pc income_volunt_assets_NSS_hh income_NSS income_SS income_NSS_pc income_SS_pc income_labor_pc income_other_pc income_vsc_NSS_pc income_vsc_SS_pc income_ms_SS_pc income_vsb_hh income_vsb_pc income_dtr_pc dtx_labor_pc dtx_pensions_pc dtx_volunt_assets_NSS_pc dtx_volunt_assets_SS_pc pline_mod)
drop if _m!=3
drop _m

keep if year==2017 

merge m:1 hhid memb_no using "C:\Users\Usuario\OneDrive\Documentos\Doctorado\Pensions\Data\Intermediate/UY CEQ database 2017.dta", keepusing(i_vloc weight)
drop if _m!=3
drop _m

replace i_vloc = i_vloc/1000

egen i_vloc_hh = max(i_vloc), by(hhid)
g prop = i_vloc_hh / income_vsb_hh

sum prop , d 

quantiles income_SS_pc [w=weight], gen(quint_income_SS) n(5)

bys quint_income_SS: sum prop if prop!=.

quantiles income_NSS_pc [w=weight], gen(quint_income_NSS) n(5)

bys quint_income_NSS: sum prop if prop!=.