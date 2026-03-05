
* 4_LP_sample_splits.do — Pre vs Post COVID Sample Split (EMU20)
*
*   Full sample:   1998m1–2025m12 (N=20, T=336)
*   Pre-COVID:     1998m1–2019m12 (T=264)
*   Post-COVID:    2020m1–2025m12 (T=72)
*
* Restricted to EMU20 (emu==1) throughout.

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
* LP Parameters
*===============================================================================
global hmax = 13
global lags  = 12

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

foreach stub in full pre post {
    foreach v in b u90 d90 u68 d68 Fstat {
        cap drop `v'_`stub'
        gen `v'_`stub' = .
    }
}

*===============================================================================
* LP-IV: Full Sample
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)              ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad            ///
        l(1/$lags).ur l(1/$lags).dln_neer                   ///
        , absorb(id) vce(robust)
    replace b_full     = _b[pi_trad]                       if _n == `h' + 1
    replace u90_full   = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h' + 1
    replace d90_full   = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h' + 1
    replace u68_full   = _b[pi_trad] + 1.000*_se[pi_trad] if _n == `h' + 1
    replace d68_full   = _b[pi_trad] - 1.000*_se[pi_trad] if _n == `h' + 1
    replace Fstat_full = e(widstat)                        if _n == `h' + 1
}
di "KP F (Full sample): " Fstat_full[1]

*===============================================================================
* LP-IV: Pre-COVID (year < 2020)
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)              ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad            ///
        l(1/$lags).ur l(1/$lags).dln_neer                   ///
        if year < 2020, absorb(id) vce(robust)
    replace b_pre     = _b[pi_trad]                       if _n == `h' + 1
    replace u90_pre   = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h' + 1
    replace d90_pre   = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h' + 1
    replace u68_pre   = _b[pi_trad] + 1.000*_se[pi_trad] if _n == `h' + 1
    replace d68_pre   = _b[pi_trad] - 1.000*_se[pi_trad] if _n == `h' + 1
    replace Fstat_pre = e(widstat)                        if _n == `h' + 1
}
di "KP F (Pre-2020):     " Fstat_pre[1]

*===============================================================================
* LP-IV: Post-COVID (year >= 2020)
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)              ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad            ///
        l(1/$lags).ur l(1/$lags).dln_neer                   ///
        if year >= 2020, absorb(id) vce(robust)
    replace b_post     = _b[pi_trad]                       if _n == `h' + 1
    replace u90_post   = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h' + 1
    replace d90_post   = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h' + 1
    replace u68_post   = _b[pi_trad] + 1.000*_se[pi_trad] if _n == `h' + 1
    replace d68_post   = _b[pi_trad] - 1.000*_se[pi_trad] if _n == `h' + 1
    replace Fstat_post = e(widstat)                        if _n == `h' + 1
}
di "KP F (Post-2020):    " Fstat_post[1]

*===============================================================================
* Diagnostics
*===============================================================================
di ""
di "KP F-statistics by sample:"
list Months Fstat_full Fstat_pre Fstat_post if Months != .

di ""
di "Point estimates:"
list Months b_full b_pre b_post if Months != .

*===============================================================================
* Save IRF data
*===============================================================================
preserve
keep if Months != .
keep Months ///
     b_full u90_full d90_full u68_full d68_full Fstat_full ///
     b_pre  u90_pre  d90_pre  u68_pre  d68_pre  Fstat_pre  ///
     b_post u90_post d90_post u68_post d68_post Fstat_post
export delimited "../output/tables/irf_sample_splits.csv", replace
restore

*===============================================================================
* Overlay Figure
*===============================================================================
twoway ///
    (rarea u90_full d90_full Months,                             ///
        fcolor(blue%15) lcolor(blue%15) lw(none))                ///
    (rarea u68_full d68_full Months,                             ///
        fcolor(blue%30) lcolor(blue%30) lw(none))                ///
    (line b_full Months,                                         ///
        lcolor(blue) lpattern(solid) lwidth(thick))              ///
    (line b_pre  Months,                                         ///
        lcolor(gs6) lpattern(shortdash) lwidth(medthick))        ///
    (line b_post Months,                                         ///
        lcolor(cranberry) lpattern(dash) lwidth(medthick))       ///
    (line Zero Months,                                           ///
        lcolor(black) lpattern(dash) lwidth(thin)),              ///
    legend(order(                                                 ///
        3 "Full sample (1998–2025)"                              ///
        4 "Pre-COVID (1998–2019)"                                ///
        5 "Post-COVID (2020–2025)"                               ///
        2 "68% CI (full)"                                        ///
        1 "90% CI (full)")                                       ///
        size(small) rows(5) pos(5) ring(0))                      ///
    title("Sample Stability: Pre vs Post COVID",                  ///
          color(black) size(medsmall))                           ///
    ytitle("{&theta}{subscript:h}", size(medsmall))              ///
    xtitle("Months after shock", size(medsmall))                 ///
    xlabel(0(2)12) xscale(range(0 12))                           ///
    ylabel(, labsize(small) format(%5.2f))                       ///
    note("EMU20. Bartik IV: w{subscript:trad} {&times} BH oil shock. 12 lags. Country FE. vce(robust)." ///
         "CI bands shown for full sample only.", size(vsmall))   ///
    graphregion(color(white))

gr rename g_sample_splits, replace
graph export "../output/figures/g_sample_splits.pdf", replace

di ""
di "Figure saved: output/figures/g_sample_splits.pdf"
di "IRF data saved: output/tables/irf_sample_splits.csv"
