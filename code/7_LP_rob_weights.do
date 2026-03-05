
* 7_LP_rob_weights.do — Instrument Robustness: Bartik Weight Variants
*
* Three weighting specifications for the Bartik instrument:
*   Spec 1 (baseline): Z_bartik_wt  = w_trad        x bh_oil_price_exp_shock
*   Spec 2 (robust):   Z_bartik_wi  = wi_imp_gdp/100 x bh_oil_price_exp_shock
*   Spec 3 (robust):   Z_direct     = bh_oil_price_exp_shock (no scaling)
*
* All specs: Country FE, 12 lags, full sample

cap cd code
do _setup.do

*===============================================================================
* Panel Setup
*===============================================================================
keep if emu == 1

egen id = group(code)
sort code year month
gen time = ym(year, month)
format time %tm
xtset id time

*===============================================================================
* Weight Variants
*===============================================================================
gen Z_bartik_wt = Z_bartik
gen Z_bartik_wi = (wi_imp_gdp / 100) * bh_oil_price_exp_shock
gen Z_direct    = bh_oil_price_exp_shock

label var Z_bartik_wt "Bartik IV: Tradables Weight x Market Oil Price Shock"
label var Z_bartik_wi "Bartik IV: Import/GDP Weight x Market Oil Price Shock"
label var Z_direct    "Direct IV: Market Oil Price Shock (no scaling)"

*===============================================================================
* LP Parameters
*===============================================================================
global hmax = 13
global lags  = 12

forv h = 0/$hmax {
    gen pi_nt_h`h' = F`h'.pi_nontrad
    label var pi_nt_h`h' "Non-Tradable Inflation, horizon h=`h'"
}

*===============================================================================
* IRF Storage
*===============================================================================
cap drop Months Zero
cap drop b_wt u_wt d_wt Fstat_wt
cap drop b_wi u_wi d_wi Fstat_wi
cap drop b_dir u_dir d_dir Fstat_dir

gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1

foreach stub in wt wi dir {
    gen b_`stub'     = .
    gen u_`stub'     = .
    gen d_`stub'     = .
    gen Fstat_`stub' = .
}

*===============================================================================
* LP-IV Loop: Spec 1 — w_trad (baseline)
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik_wt)                  ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        , absorb(id) vce(robust)

    replace b_wt     = _b[pi_trad]                       if _n == `h' + 1
    replace u_wt     = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h' + 1
    replace d_wt     = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h' + 1
    replace Fstat_wt = e(widstat)                         if _n == `h' + 1
}

*===============================================================================
* LP-IV Loop: Spec 2 — wi_imp_gdp/100
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik_wi)                  ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        , absorb(id) vce(robust)
    replace b_wi     = _b[pi_trad]                       if _n == `h' + 1
    replace u_wi     = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h' + 1
    replace d_wi     = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h' + 1
    replace Fstat_wi = e(widstat)                         if _n == `h' + 1
}

*===============================================================================
* LP-IV Loop: Spec 3 — direct, no scaling
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_direct)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        , absorb(id) vce(robust)
    replace b_dir     = _b[pi_trad]                       if _n == `h' + 1
    replace u_dir     = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h' + 1
    replace d_dir     = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h' + 1
    replace Fstat_dir = e(widstat)                         if _n == `h' + 1
}

*===============================================================================
* Diagnostics
*===============================================================================
di "KP F-statistics — Spec 1 (w_trad Bartik):"
list Months Fstat_wt if Months != .

di "KP F-statistics — Spec 2 (wi/GDP Bartik):"
list Months Fstat_wi if Months != .

di "KP F-statistics — Spec 3 (direct):"
list Months Fstat_dir if Months != .

*===============================================================================
* Overlay Figure
*===============================================================================
twoway ///
    (rarea u_wt d_wt Months,                                    ///
        fcolor(blue%15) lcolor(blue%15) lw(none))               ///
    (line b_wt Months,                                          ///
        lcolor(blue) lpattern(solid) lwidth(thick))             ///
    (line b_wi Months,                                          ///
        lcolor(red) lpattern(dash) lwidth(medthick))            ///
    (line b_dir Months,                                         ///
        lcolor(gs8) lpattern(shortdash) lwidth(medthick))       ///
    (line Zero Months,                                          ///
        lcolor(black) lpattern(dash) lwidth(thin)),             ///
    legend(order(                                               ///
        2 "Baseline: {it:w}{subscript:trad} x {it:Z}{subscript:t}"  ///
        3 "Robustness 1: {it:w_}{subscript:IMPORT/GDP}  x {it:Z}{subscript:t}"               ///
        4 "Robustness 2: {it:Z}{subscript:t} (no scaling)")                       ///
        size(small) rows(3) pos(5) ring(0))                     ///
    title("Second-Round Pass-Through: Instrument Robustness",   ///
          color(black) size(medsmall))                          ///
    ytitle("{&theta}{subscript:h}", size(medsmall))             ///
    xtitle("Months after shock", size(medsmall))                ///
    xlabel(0(2)12) xscale(range(0 12))                          ///
    ylabel(, labsize(small) format(%5.2f))                      ///
    note("90% CI shown for baseline only. EMU20. 12 lags. Country FE. vce(robust).", ///
         size(vsmall))                                          ///
    graphregion(color(white))

gr rename g_lp_base_rob_w, replace
graph export "../output/figures/g_lp_base_rob_w.pdf", replace

di ""
di "Figure saved: output/figures/g_lp_base_rob_w.pdf"
