
* 1_LP_baseline.do — Baseline LP-IV on EMU20
*
* Specification:
*   Dependent:  pi_nt_h`h'  = F`h'.pi_nontrad
*   Endogenous: pi_trad
*   Instrument: Z_bartik = w_trad x bh_oil_price_exp_shock
*   Controls:   l(1/12).pi_nontrad, l(1/12).pi_trad, l(1/12).ur, l(1/12).dln_neer
*   FE:         Country (id)
*   SE:         vce(robust)
*   Sample:     EMU20 (emu == 1), full sample
*   H:          0 to 13 | Lags: 12

*cap cd code
do "code/_setup.do"

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
cap drop b u90 d90 u68 d68 Months Zero Fstat
gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1
gen b    = .
gen u90  = .    // upper 90% CI  (z = 1.645)
gen d90  = .    // lower 90% CI
gen u68  = .    // upper 68% CI  (z = 1.000)
gen d68  = .    // lower 68% CI
gen Fstat = .

*===============================================================================
* LP-IV Loop
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        , absorb(id) vce(robust)

    replace b     = _b[pi_trad]                           if _n == `h' + 1
    replace u90   = _b[pi_trad] + 1.645*_se[pi_trad]     if _n == `h' + 1
    replace d90   = _b[pi_trad] - 1.645*_se[pi_trad]     if _n == `h' + 1
    replace u68   = _b[pi_trad] + 1.000*_se[pi_trad]     if _n == `h' + 1
    replace d68   = _b[pi_trad] - 1.000*_se[pi_trad]     if _n == `h' + 1
    replace Fstat = e(widstat)                            if _n == `h' + 1
}

*===============================================================================
* Diagnostics
*===============================================================================
di ""
di "Kleibergen-Paap F-statistics by horizon:"
list Months Fstat if Months != .

di ""
di "Point estimates:"
list Months b u90 d90 if Months != .

*===============================================================================
* Save IRF to CSV for table generation
*===============================================================================
preserve
keep if Months != .
keep Months b u90 d90 u68 d68 Fstat
gen sample = "EMU20"
export delimited using "../output/tables/irf_emu20.csv", replace
restore

*===============================================================================
* Publication-Quality IRF Figure
*===============================================================================
twoway ///
    (rarea u90 d90 Months,                                  ///
        fcolor(blue%15) lcolor(blue%15) lw(none))           ///
    (rarea u68 d68 Months,                                  ///
        fcolor(blue%30) lcolor(blue%30) lw(none))           ///
    (line b Months,                                         ///
        lcolor(blue) lpattern(solid) lwidth(thick))         ///
    (line Zero Months,                                      ///
        lcolor(black) lpattern(dash) lwidth(thin)),         ///
    legend(order(                                           ///
        3 "Point estimate"                                  ///
        2 "68% CI"                                          ///
        1 "90% CI")                                         ///
        size(small) rows(3) pos(5) ring(0))                 ///
    title("Second-Round Pass-Through: EMU20 Baseline",      ///
          color(black) size(medsmall))                      ///
    ytitle("{&theta}{subscript:h}", size(medsmall))         ///
    xtitle("Months after shock", size(medsmall))            ///
    xlabel(0(2)12) xscale(range(0 12))                      ///
    ylabel(, labsize(small) format(%5.2f))                  ///
    note("90% and 68% CI. Bartik IV: w{subscript:trad} {&times} BH oil price shock." ///
         "12 lags. Country FE. vce(robust). EMU20, full sample.", ///
         size(vsmall))                                      ///
    graphregion(color(white))

gr rename g_lp_emu20_replication, replace
graph export "output/figures/g_lp_emu20_replication.pdf", replace

di ""
di "Figure saved: output/figures/g_lp_emu20_replication.pdf"
di "IRF data saved: output/tables/irf_emu20.csv"
