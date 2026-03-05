
* 2_LP_samples_EU.do — EU Sample Comparison (EMU20, EU27, EU7 Placebo)
*
* Loads the panel once, constructs variables once, then runs three LP-IV
* specifications:
*   (1) EMU20 only (if emu == 1)
*   (2) Full EU27
*   (3) EU7 non-EMU only (if emu == 0) — placebo test
*
* Produces:
*   Figures: g_lp_eu27_comparison.pdf, g_lp_eu7_placebo.pdf,
*            g_lp_emu20_vs_eu7.pdf
*   CSVs:   irf_eu27_comparison.csv, irf_eu7_placebo.csv
*
* NEER variable:
*   EMU20 countries — log change in Euro EER-40 (common, from ECB)
*   EU7 countries   — log change in bilateral EUR spot rate (country-specific)


do "code/_setup.do"

*===============================================================================
* Panel Setup — Full EU27
*===============================================================================
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
cap drop Months Zero
foreach stub in emu20 eu27 eu7 {
    cap drop b_`stub' u90_`stub' d90_`stub' u68_`stub' d68_`stub' Fstat_`stub'
    gen b_`stub'     = .
    gen u90_`stub'   = .
    gen d90_`stub'   = .
    gen u68_`stub'   = .
    gen d68_`stub'   = .
    gen Fstat_`stub' = .
}
gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1

*===============================================================================
* LP-IV Loop: EMU20 only
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        if emu == 1                              ///
        , absorb(id) vce(robust)

    replace b_emu20     = _b[pi_trad]                         if _n == `h' + 1
    replace u90_emu20   = _b[pi_trad] + 1.645*_se[pi_trad]   if _n == `h' + 1
    replace d90_emu20   = _b[pi_trad] - 1.645*_se[pi_trad]   if _n == `h' + 1
    replace u68_emu20   = _b[pi_trad] + 1.000*_se[pi_trad]   if _n == `h' + 1
    replace d68_emu20   = _b[pi_trad] - 1.000*_se[pi_trad]   if _n == `h' + 1
    replace Fstat_emu20 = e(widstat)                          if _n == `h' + 1
}
di "KP F (EMU20): " Fstat_emu20[1]

*===============================================================================
* LP-IV Loop: Full EU27
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        , absorb(id) vce(robust)

    replace b_eu27     = _b[pi_trad]                         if _n == `h' + 1
    replace u90_eu27   = _b[pi_trad] + 1.645*_se[pi_trad]   if _n == `h' + 1
    replace d90_eu27   = _b[pi_trad] - 1.645*_se[pi_trad]   if _n == `h' + 1
    replace u68_eu27   = _b[pi_trad] + 1.000*_se[pi_trad]   if _n == `h' + 1
    replace d68_eu27   = _b[pi_trad] - 1.000*_se[pi_trad]   if _n == `h' + 1
    replace Fstat_eu27 = e(widstat)                          if _n == `h' + 1
}
di "KP F (EU27):  " Fstat_eu27[1]

*===============================================================================
* LP-IV Loop: EU7 only (placebo)
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        if emu == 0                              ///
        , absorb(id) vce(robust)

    replace b_eu7     = _b[pi_trad]                         if _n == `h' + 1
    replace u90_eu7   = _b[pi_trad] + 1.645*_se[pi_trad]   if _n == `h' + 1
    replace d90_eu7   = _b[pi_trad] - 1.645*_se[pi_trad]   if _n == `h' + 1
    replace u68_eu7   = _b[pi_trad] + 1.000*_se[pi_trad]   if _n == `h' + 1
    replace d68_eu7   = _b[pi_trad] - 1.000*_se[pi_trad]   if _n == `h' + 1
    replace Fstat_eu7 = e(widstat)                          if _n == `h' + 1
}
di "KP F (EU7):   " Fstat_eu7[1]

*===============================================================================
* Diagnostics
*===============================================================================
di ""
di "Point estimates comparison:"
list Months b_emu20 b_eu27 b_eu7 if Months != .

*===============================================================================
* Save IRF CSVs
*===============================================================================

* EU27 comparison (EMU20 + EU27)
preserve
keep if Months != .
keep Months b_emu20 u90_emu20 d90_emu20 u68_emu20 d68_emu20 Fstat_emu20 ///
           b_eu27  u90_eu27  d90_eu27  u68_eu27  d68_eu27  Fstat_eu27
export delimited using "output/tables/irf_eu27_comparison.csv", replace
restore

* EU7 placebo
preserve
keep if Months != .
keep Months b_eu7 u90_eu7 d90_eu7 u68_eu7 d68_eu7 Fstat_eu7
rename (b_eu7 u90_eu7 d90_eu7 u68_eu7 d68_eu7 Fstat_eu7) (b u90 d90 u68 d68 Fstat)
gen sample = "EU7_placebo"
export delimited using "output/tables/irf_eu7_placebo.csv", replace
restore

*===============================================================================
* Figure 1: EMU20 vs EU27 comparison
*===============================================================================
twoway ///
    (rarea u90_emu20 d90_emu20 Months,                           ///
        fcolor(blue%15) lcolor(blue%15) lw(none))                ///
    (rarea u68_emu20 d68_emu20 Months,                           ///
        fcolor(blue%30) lcolor(blue%30) lw(none))                ///
    (line b_emu20 Months,                                        ///
        lcolor(blue) lpattern(solid) lwidth(thick))              ///
    (line b_eu27 Months,                                         ///
        lcolor(red) lpattern(dash) lwidth(medthick))             ///
    (line Zero Months,                                           ///
        lcolor(black) lpattern(dash) lwidth(thin)),              ///
    legend(order(                                                ///
        3 "EMU20"                                                ///
        4 "EU27"                                                 ///
        2 "68% CI (EMU20)"                                       ///
        1 "90% CI (EMU20)")                                      ///
        size(small) rows(4) pos(5) ring(0))                      ///
    title("Second-Round Pass-Through: EMU20 vs EU27",            ///
          color(black) size(medsmall))                           ///
    ytitle("{&theta}{subscript:h}", size(medsmall))              ///
    xtitle("Months after shock", size(medsmall))                 ///
    xlabel(0(2)12) xscale(range(0 12))                           ///
    ylabel(, labsize(small) format(%5.2f))                       ///
    note("90% and 68% CI shown for EMU20. Bartik IV: w{subscript:trad} {&times} BH oil shock." ///
         "12 lags. Country FE. vce(robust). Full sample.",       ///
         size(vsmall))                                           ///
    graphregion(color(white))

gr rename g_lp_eu27_comparison, replace
graph export "output/figures/g_lp_eu27_comparison.pdf", replace

*===============================================================================
* Figure 2: EU7 placebo
*===============================================================================
twoway ///
    (rarea u90_eu7 d90_eu7 Months,                               ///
        fcolor(red%15) lcolor(red%15) lw(none))                  ///
    (rarea u68_eu7 d68_eu7 Months,                               ///
        fcolor(red%30) lcolor(red%30) lw(none))                  ///
    (line b_eu7 Months,                                          ///
        lcolor(red) lpattern(solid) lwidth(thick))               ///
    (line Zero Months,                                           ///
        lcolor(black) lpattern(dash) lwidth(thin)),              ///
    legend(order(                                                ///
        3 "Point estimate (EU7)"                                 ///
        2 "68% CI"                                               ///
        1 "90% CI")                                              ///
        size(small) rows(3) pos(5) ring(0))                      ///
    title("Placebo: Pass-Through in Non-EMU EU Countries",       ///
          color(black) size(medsmall))                           ///
    ytitle("{&theta}{subscript:h}", size(medsmall))              ///
    xtitle("Months after shock", size(medsmall))                 ///
    xlabel(0(2)12) xscale(range(0 12))                           ///
    ylabel(, labsize(small) format(%5.2f))                       ///
    note("90% and 68% CI. Bartik IV: w{subscript:trad} {&times} BH oil shock." ///
         "12 lags. Country FE. vce(robust). EU7 non-EMU countries only.",      ///
         size(vsmall))                                           ///
    graphregion(color(white))

gr rename g_lp_eu7_placebo, replace
graph export "output/figures/g_lp_eu7_placebo.pdf", replace

*===============================================================================
* Figure 3: EMU20 vs EU7 overlay
*===============================================================================
twoway ///
    (rarea u90_emu20 d90_emu20 Months,                           ///
        fcolor(blue%15) lcolor(blue%15) lw(none))                ///
    (rarea u68_emu20 d68_emu20 Months,                           ///
        fcolor(blue%30) lcolor(blue%30) lw(none))                ///
    (rarea u90_eu7 d90_eu7 Months,                               ///
        fcolor(red%12) lcolor(red%12) lw(none))                  ///
    (rarea u68_eu7 d68_eu7 Months,                               ///
        fcolor(red%25) lcolor(red%25) lw(none))                  ///
    (line b_emu20 Months,                                        ///
        lcolor(blue) lpattern(solid) lwidth(thick))              ///
    (line b_eu7 Months,                                          ///
        lcolor(red) lpattern(dash) lwidth(thick))                ///
    (line Zero Months,                                           ///
        lcolor(black) lpattern(dash) lwidth(thin)),              ///
    legend(order(                                                ///
        5 "EMU20"                                                ///
        6 "EU7 (Non-EMU)"                                        ///
        2 "68% CI"                                               ///
        1 "90% CI")                                              ///
        size(small) rows(4) pos(5) ring(0))                      ///
    title("Second-Round Pass-Through: EMU20 vs Non-EMU EU",      ///
          color(black) size(medsmall))                           ///
    ytitle("{&theta}{subscript:h}", size(medsmall))              ///
    xtitle("Months after shock", size(medsmall))                 ///
    xlabel(0(2)12) xscale(range(0 12))                           ///
    ylabel(, labsize(small) format(%5.2f))                       ///
    note("90% and 68% CI shown for both samples. Bartik IV: w{subscript:trad} {&times} BH oil shock." ///
         "12 lags. Country FE. vce(robust). EU7 NEER: bilateral EUR rate (ECB).", ///
         size(vsmall))                                           ///
    graphregion(color(white))

gr rename g_lp_emu20_vs_eu7, replace
graph export "output/figures/g_lp_emu20_vs_eu7.pdf", replace

di ""
di "Figures saved: g_lp_eu27_comparison.pdf, g_lp_eu7_placebo.pdf, g_lp_emu20_vs_eu7.pdf"
di "IRF data saved: irf_eu27_comparison.csv, irf_eu7_placebo.csv"
