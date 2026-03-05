
* 6_LP_rob_instruments.do — Instrument Robustness
*
* Compares baseline Baumeister-Hamilton instrument against two alternatives:
*   (1) GSCPI shocks: AR innovations on NY Fed Global Supply Chain Pressure Index
*   (2) IMF non-fuel commodity shocks: AR innovations on dln(IMF non-fuel index)
* All in Bartik form: Z = w_trad x shock
* Sample: EMU20 only (if emu==1). Controls, FE, and lags identical to baseline.
* GSCPI retained for diagnostic completeness only (KP F ≈ 2, weak instrument).

do "code/_setup.do"

*===============================================================================
* Panel Setup — Full panel (filter in regression with if emu==1)
*===============================================================================
egen id = group(code)
sort code year month
gen time = ym(year, month)
format time %tm
xtset id time

*===============================================================================
* Instruments: rename Z_bartik for consistency, add GSCPI and IMF
*===============================================================================
rename Z_bartik Z_bh
label var Z_bh "Bartik IV: w_trad x BH oil price shock"

* Merge GSCPI shocks
preserve
import delimited "data/clean/gscpi_shocks.csv", clear varnames(1)
tempfile gscpi_tmp
save `gscpi_tmp'
restore
merge m:1 year month using `gscpi_tmp', keep(match master) nogen

gen Z_gscpi = w_trad * gscpi_shock
label var Z_gscpi "Bartik IV: w_trad x GSCPI AR shock"

gen Z_gscpi_direct = gscpi_shock
label var Z_gscpi_direct "Direct IV: GSCPI AR shock (no Bartik weight)"

* Merge IMF non-fuel commodity shocks
preserve
import delimited "data/clean/imf_shocks.csv", clear varnames(1)
tempfile imf_tmp
save `imf_tmp'
restore
merge m:1 year month using `imf_tmp', keep(match master) nogen

gen Z_imf = w_trad * imf_shock
label var Z_imf "Bartik IV: w_trad x IMF non-fuel AR shock"

* Re-sort and re-declare panel after merges
sort id time
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
foreach stub in bh gscpi gscpi_d imf {
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
* LP-IV: Baseline (BH oil shock)
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'              ///
        (pi_trad = Z_bh)               ///
        l(1/$lags).pi_nontrad          ///
        l(1/$lags).pi_trad             ///
        l(1/$lags).ur                  ///
        l(1/$lags).dln_neer            ///
        if emu == 1                    ///
        , absorb(id) vce(robust)

    replace b_bh     = _b[pi_trad]                       if _n == `h' + 1
    replace u90_bh   = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h' + 1
    replace d90_bh   = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h' + 1
    replace u68_bh   = _b[pi_trad] + 1.000*_se[pi_trad] if _n == `h' + 1
    replace d68_bh   = _b[pi_trad] - 1.000*_se[pi_trad] if _n == `h' + 1
    replace Fstat_bh = e(widstat)                        if _n == `h' + 1
}

*===============================================================================
* LP-IV: GSCPI shocks (diagnostic — weak instrument, KP F ≈ 2)
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'              ///
        (pi_trad = Z_gscpi)            ///
        l(1/$lags).pi_nontrad          ///
        l(1/$lags).pi_trad             ///
        l(1/$lags).ur                  ///
        l(1/$lags).dln_neer            ///
        if emu == 1                    ///
        , absorb(id) vce(robust)

    replace b_gscpi     = _b[pi_trad]                       if _n == `h' + 1
    replace u90_gscpi   = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h' + 1
    replace d90_gscpi   = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h' + 1
    replace u68_gscpi   = _b[pi_trad] + 1.000*_se[pi_trad] if _n == `h' + 1
    replace d68_gscpi   = _b[pi_trad] - 1.000*_se[pi_trad] if _n == `h' + 1
    replace Fstat_gscpi = e(widstat)                        if _n == `h' + 1
}

*===============================================================================
* LP-IV: GSCPI direct (no Bartik weight) — diagnostic
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'              ///
        (pi_trad = Z_gscpi_direct)     ///
        l(1/$lags).pi_nontrad          ///
        l(1/$lags).pi_trad             ///
        l(1/$lags).ur                  ///
        l(1/$lags).dln_neer            ///
        if emu == 1                    ///
        , absorb(id) vce(robust)

    replace b_gscpi_d     = _b[pi_trad]                       if _n == `h' + 1
    replace u90_gscpi_d   = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h' + 1
    replace d90_gscpi_d   = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h' + 1
    replace u68_gscpi_d   = _b[pi_trad] + 1.000*_se[pi_trad] if _n == `h' + 1
    replace d68_gscpi_d   = _b[pi_trad] - 1.000*_se[pi_trad] if _n == `h' + 1
    replace Fstat_gscpi_d = e(widstat)                        if _n == `h' + 1
}

*===============================================================================
* LP-IV: IMF non-fuel commodity shocks
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'              ///
        (pi_trad = Z_imf)              ///
        l(1/$lags).pi_nontrad          ///
        l(1/$lags).pi_trad             ///
        l(1/$lags).ur                  ///
        l(1/$lags).dln_neer            ///
        if emu == 1                    ///
        , absorb(id) vce(robust)

    replace b_imf     = _b[pi_trad]                       if _n == `h' + 1
    replace u90_imf   = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h' + 1
    replace d90_imf   = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h' + 1
    replace u68_imf   = _b[pi_trad] + 1.000*_se[pi_trad] if _n == `h' + 1
    replace d68_imf   = _b[pi_trad] - 1.000*_se[pi_trad] if _n == `h' + 1
    replace Fstat_imf = e(widstat)                        if _n == `h' + 1
}

*===============================================================================
* Diagnostics
*===============================================================================
di ""
di "KP F-statistics:"
di ""
list Months Fstat_bh Fstat_gscpi Fstat_gscpi_d Fstat_imf if Months != .

di ""
di "Point estimates — b_bh vs b_gscpi vs b_gscpi_d vs b_imf:"
list Months b_bh b_gscpi b_gscpi_d b_imf if Months != .

*===============================================================================
* Save IRF data for table
*===============================================================================
preserve
keep if Months != .
keep Months b_bh u90_bh d90_bh u68_bh d68_bh Fstat_bh ///
            b_gscpi u90_gscpi d90_gscpi u68_gscpi d68_gscpi Fstat_gscpi ///
            b_gscpi_d u90_gscpi_d d90_gscpi_d u68_gscpi_d d68_gscpi_d Fstat_gscpi_d ///
            b_imf u90_imf d90_imf u68_imf d68_imf Fstat_imf
export delimited "output/tables/irf_rob_instruments.csv", replace
restore

di ""
di "IRF data saved: output/tables/irf_rob_instruments.csv"

*===============================================================================
* Overlay Figure: Three instruments comparison
*===============================================================================
twoway ///
    (rarea u90_bh d90_bh Months,                                  ///
        fcolor(blue%15) lcolor(blue%15) lw(none))                 ///
    (rarea u68_bh d68_bh Months,                                  ///
        fcolor(blue%30) lcolor(blue%30) lw(none))                 ///
    (rarea u90_gscpi d90_gscpi Months,                            ///
        fcolor(red%12) lcolor(red%12) lw(none))                   ///
    (rarea u68_gscpi d68_gscpi Months,                            ///
        fcolor(red%25) lcolor(red%25) lw(none))                   ///
    (rarea u90_imf d90_imf Months,                                ///
        fcolor(green%12) lcolor(green%12) lw(none))               ///
    (rarea u68_imf d68_imf Months,                                ///
        fcolor(green%25) lcolor(green%25) lw(none))               ///
    (line b_bh Months,                                            ///
        lcolor(blue) lpattern(solid) lwidth(thick))               ///
    (line b_gscpi Months,                                         ///
        lcolor(red) lpattern(dash) lwidth(thick))                 ///
    (line b_imf Months,                                           ///
        lcolor(green) lpattern(longdash) lwidth(thick))           ///
    (line Zero Months,                                            ///
        lcolor(black) lpattern(dash) lwidth(thin)),               ///
    legend(order(                                                  ///
        7 "BH oil shock (baseline)"                               ///
        8 "GSCPI AR shocks"                                       ///
        9 "IMF non-fuel AR shocks"                                ///
        2 "68% CI"                                                ///
        1 "90% CI")                                               ///
        size(small) rows(5) pos(5) ring(0))                       ///
    title("Instrument Robustness: Alternative IV Specifications", ///
          color(black) size(medsmall))                            ///
    ytitle("{&theta}{subscript:h}", size(medsmall))               ///
    xtitle("Months after shock", size(medsmall))                  ///
    xlabel(0(2)12) xscale(range(0 12))                            ///
    ylabel(, labsize(small) format(%5.2f))                        ///
    note("EMU20 sample. Bartik IV: w{subscript:trad} {&times} instrument shock."  ///
         "Baseline: BH oil price expectation shock. GSCPI: AR residuals (NY Fed)." ///
         "IMF: AR residuals on dln(non-fuel commodity index, PALLFNFINDEXM via FRED)." ///
         "90% and 68% CI shown. 12 lags. Country FE. vce(robust).",             ///
         size(vsmall))                                            ///
    graphregion(color(white))

gr rename g_lp_rob_instruments, replace
graph export "output/figures/g_lp_rob_instruments.pdf", replace

di ""
di "Figure saved: output/figures/g_lp_rob_instruments.pdf"
