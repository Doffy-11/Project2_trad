* 4_LP_sample_splits.do — Pre vs Post COVID Sample Split (EMU20)
*
*   Full sample:   1998m1–2025m12 (N=20, T=336)
*   Pre-COVID:     1998m1–2019m12 (T=264)
*   Post-COVID:    2020m1–2025m12 (T=72)
*
* Restricted to EMU20 (emu==1) throughout.

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
    label var pi_nt_h`h' "Non-Tradable Inflation, h=`h'"
}

*===============================================================================
* IRF Storage
*===============================================================================
cap drop Months Zero
gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1

foreach stub in full pre post {
    foreach v in b u90 d90 Fstat {
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
     b_full u90_full d90_full Fstat_full ///
     b_pre  u90_pre  d90_pre  Fstat_pre  ///
     b_post u90_post d90_post Fstat_post
export delimited "output/tables/irf_sample_splits.csv", replace
restore

*===============================================================================
* Capture first-stage F-stats for graph notes
*===============================================================================
quietly {
    summarize Fstat_full if Months == 0
    local fs_full : display %5.2f r(mean)
    summarize Fstat_pre if Months == 0
    local fs_pre  : display %5.2f r(mean)
    summarize Fstat_post if Months == 0
    local fs_post : display %5.2f r(mean)
}

*===============================================================================
* Graph — Full Sample (1998–2025)
*===============================================================================
twoway ///
    (rarea u90_full d90_full Months,                             ///
        fcolor(blue%15) lcolor(blue%15) lw(none))                ///
    (line b_full Months,                                         ///
        lcolor(blue) lpattern(solid) lwidth(thick))              ///
    (line Zero Months,                                           ///
        lcolor(black) lpattern(dash) lwidth(thin)),              ///
    legend(order(2 "Full sample (1998-2025)" 1 "90% CI")         ///
        size(small) rows(2) pos(5) ring(0))                      ///
    title("A. Full Sample (1998-2025)",                             ///
          color(black) size(medsmall))                           ///
    ytitle("{&theta}{subscript:h}", size(medsmall))              ///
    xtitle("Months after shock", size(medsmall))                 ///
    xlabel(0(2)12) xscale(range(0 12))                           ///
    ylabel(, labsize(small) format(%5.2f))                       ///
    note("First-stage KP F-statistic: `fs_full'", size(vsmall)) ///
    graphregion(color(white))

gr rename g_full, replace
graph export "output/figures/g_split_full.pdf", replace

*===============================================================================
* Graph — Pre-COVID (1998–2019)
*===============================================================================
twoway ///
    (rarea u90_pre d90_pre Months,                               ///
        fcolor(gs6%15) lcolor(gs6%15) lw(none))                  ///
    (line b_pre Months,                                          ///
        lcolor(gs6) lpattern(solid) lwidth(thick))        ///
    (line Zero Months,                                           ///
        lcolor(black) lpattern(dash) lwidth(thin)),              ///
    legend(order(2 "Pre-COVID (1998-2019)" 1 "90% CI")           ///
        size(small) rows(2) pos(5) ring(0))                      ///
    title("B. Pre-COVID (1998-2019)",                               ///
          color(black) size(medsmall))                           ///
    ytitle("{&theta}{subscript:h}", size(medsmall))              ///
    xtitle("Months after shock", size(medsmall))                 ///
    xlabel(0(2)12) xscale(range(0 12))                           ///
    ylabel(, labsize(small) format(%5.2f))                       ///
    note("First-stage KP F-statistic: `fs_pre'", size(vsmall))  ///
    graphregion(color(white))

gr rename g_pre, replace
graph export "output/figures/g_split_pre.pdf", replace

*===============================================================================
* Graph — Post-COVID (2020–2025)
*===============================================================================
twoway ///
    (rarea u90_post d90_post Months,                             ///
        fcolor(cranberry%15) lcolor(cranberry%15) lw(none))      ///
    (line b_post Months,                                         ///
        lcolor(cranberry) lpattern(solid) lwidth(thick))       ///
    (line Zero Months,                                           ///
        lcolor(black) lpattern(dash) lwidth(thin)),              ///
    legend(order(2 "Post-COVID (2020-2025)" 1 "90% CI")          ///
        size(small) rows(2) pos(5) ring(0))                      ///
    title("C. Post-COVID (2020-2025)",                              ///
          color(black) size(medsmall))                           ///
    ytitle("{&theta}{subscript:h}", size(medsmall))              ///
    xtitle("Months after shock", size(medsmall))                 ///
    xlabel(0(2)12) xscale(range(0 12))                           ///
    ylabel(, labsize(small) format(%5.2f))                       ///
    note("First-stage KP F-statistic: `fs_post'", size(vsmall)) ///
    graphregion(color(white))

gr rename g_post, replace
graph export "output/figures/g_split_post.pdf", replace

*===============================================================================
* Combined panel
*===============================================================================
graph combine g_full g_pre g_post,                               ///
    rows(3) cols(1) iscale(1)                                  ///
    title("Sample Stability: Pre vs Post COVID",                   ///
        color(black) size(medsmall))                              ///
    graphregion(color(white)) imargin(small) ycommon             ///
    xsize(4) ysize(9)


	
gr rename g_sample_splits, replace
graph export "output/figures/g_sample_splits.pdf", replace

di ""
di "Figures saved:"
di "  output/figures/g_split_full.pdf"
di "  output/figures/g_split_pre.pdf"
di "  output/figures/g_split_post.pdf"
di "  output/figures/g_sample_splits.pdf"
di "IRF data saved: output/tables/irf_sample_splits.csv"
