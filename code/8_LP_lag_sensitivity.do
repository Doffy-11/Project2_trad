
* 8_LP_lag_sensitivity.do — Lag Sensitivity
*
* Runs the baseline LP-IV (EMU20, BH instrument) at four lag structures:
*   L = 4, 8, 12 (baseline), 16
* H = 13 horizons throughout. All other choices identical to baseline.

cap cd code
do _setup.do

*===============================================================================
* Panel Setup — EMU20 only
*===============================================================================
keep if emu == 1

egen id = group(code)
sort code year month
gen time = ym(year, month)
format time %tm
xtset id time

*===============================================================================
* Forward LHS Variables (generated once, valid for all lag lengths)
*===============================================================================
global hmax = 13

forv h = 0/$hmax {
    gen pi_nt_h`h' = F`h'.pi_nontrad
    label var pi_nt_h`h' "Non-Tradable Inflation, h=`h'"
}

*===============================================================================
* IRF Storage
*===============================================================================
cap drop Months Zero
gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1

foreach L in 4 8 12 16 {
    foreach v in b u90 d90 u68 d68 Fstat {
        cap drop `v'_L`L'
        gen `v'_L`L' = .
    }
}

*===============================================================================
* LP-IV Loop Over Lag Lengths
*===============================================================================
foreach L in 4 8 12 16 {

    di ""
    di "=== Running LP-IV with L = `L' lags ==="

    qui forv h = 0/$hmax {

        ivreghdfe pi_nt_h`h'                    ///
            (pi_trad = Z_bartik)                 ///
            l(1/`L').pi_nontrad                  ///
            l(1/`L').pi_trad                     ///
            l(1/`L').ur                          ///
            l(1/`L').dln_neer                    ///
            , absorb(id) vce(robust)

        replace b_L`L'     = _b[pi_trad]                       if _n == `h' + 1
        replace u90_L`L'   = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h' + 1
        replace d90_L`L'   = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h' + 1
        replace u68_L`L'   = _b[pi_trad] + 1.000*_se[pi_trad] if _n == `h' + 1
        replace d68_L`L'   = _b[pi_trad] - 1.000*_se[pi_trad] if _n == `h' + 1
        replace Fstat_L`L' = e(widstat)                        if _n == `h' + 1
    }
}

*===============================================================================
* Diagnostics
*===============================================================================
di ""
di "KP F-statistics by lag length:"
list Months Fstat_L4 Fstat_L8 Fstat_L12 Fstat_L16 if Months != .

di ""
di "Point estimates by lag length:"
list Months b_L4 b_L8 b_L12 b_L16 if Months != .

*===============================================================================
* Save IRF data
*===============================================================================
preserve
keep if Months != .
keep Months b_L4 u90_L4 d90_L4 Fstat_L4 ///
            b_L8 u90_L8 d90_L8 Fstat_L8 ///
            b_L12 u90_L12 d90_L12 Fstat_L12 ///
            b_L16 u90_L16 d90_L16 Fstat_L16
export delimited "../output/tables/irf_lag_sensitivity.csv", replace
restore

*===============================================================================
* Overlay Figure
*===============================================================================
twoway ///
    (rarea u90_L12 d90_L12 Months,                                ///
        fcolor(blue%15) lcolor(blue%15) lw(none))                 ///
    (line b_L4  Months, lcolor(cranberry)    lpattern(shortdash) lwidth(medthick)) ///
    (line b_L8  Months, lcolor(forest_green) lpattern(dash)      lwidth(medthick)) ///
    (line b_L12 Months, lcolor(blue)         lpattern(solid)     lwidth(thick))    ///
    (line b_L16 Months, lcolor(gs8)          lpattern(longdash)  lwidth(medthick)) ///
    (line Zero  Months, lcolor(black) lpattern(dash) lwidth(thin)),               ///
    legend(order(                                                  ///
        4 "L=12 (baseline)"                                       ///
        2 "L=4"                                                   ///
        3 "L=8"                                                   ///
        5 "L=16"                                                  ///
        1 "90% CI (L=12)")                                        ///
        size(small) rows(5) pos(5) ring(0))                       ///
    title("Lag Sensitivity: L = 4, 8, 12, 16",                   ///
          color(black) size(medsmall))                            ///
    ytitle("{&theta}{subscript:h}", size(medsmall))               ///
    xtitle("Months after shock", size(medsmall))                  ///
    xlabel(0(2)12) xscale(range(0 12))                            ///
    ylabel(, labsize(small) format(%5.2f))                        ///
    note("EMU20. Bartik IV: w{subscript:trad} {&times} BH oil shock. Country FE. vce(robust)." ///
         "90% CI shown for baseline (L=12) only.",                ///
         size(vsmall))                                            ///
    graphregion(color(white))

gr rename g_lag_sensitivity, replace
graph export "../output/figures/g_lag_sensitivity.pdf", replace

di ""
di "Figure saved: output/figures/g_lag_sensitivity.pdf"
di "IRF data saved: output/tables/irf_lag_sensitivity.csv"
