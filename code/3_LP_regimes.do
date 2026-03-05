
* 3_LP_regimes.do — State Dependence (EMU20)
*
* Three state-dependence specifications:
*   (1) GSCPI: high vs low supply chain stress (panel median split)
*   (2) NEER:  appreciation (neer_ap) vs depreciation (neer_dep)
*   (3) Inflation volatility: high vs low (36m rolling std, within-country median)
* All regressions restricted to EMU20 (emu==1). Baseline spec: L=12.

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
* Regime Definitions
*===============================================================================

* ── 1. GSCPI: above vs below panel median ─────────────────────────────────────
summarize gscpi, detail
local gscpi_med = r(p50)
gen high_gscpi = (gscpi > `gscpi_med') if gscpi != .
label var high_gscpi "High GSCPI (above panel median)"
di "GSCPI panel median: `gscpi_med'"

* ── 2. NEER: appreciation (dln_neer > 0) vs depreciation ─────────────────────
gen apprec = (dln_neer > 0) if dln_neer != .
label var apprec "NEER Appreciation (dln_neer > 0)"

* ── 3. Inflation volatility: 36m rolling std, within-country median split ──────
rangestat (sd) roll_std=pi_headline, interval(time -35 0) by(id)
label var roll_std "36m rolling SD of headline inflation"
bysort id: egen vol_median = median(roll_std)
gen high_vol = (roll_std > vol_median) if roll_std != .
label var high_vol "High inflation volatility (above within-country median)"
drop vol_median

di ""
di "Regime observation counts (EMU20):"
tab high_gscpi
tab apprec
tab high_vol

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

foreach stub in gscpi_lo gscpi_hi neer_ap neer_dep vol_lo vol_hi {
    foreach v in b u90 d90 Fstat {
        cap drop `v'_`stub'
        gen `v'_`stub' = .
    }
}

*===============================================================================
* LP-IV: GSCPI regimes
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)         ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad       ///
        l(1/$lags).ur l(1/$lags).dln_neer              ///
        if high_gscpi == 0, absorb(id) vce(robust)
    replace b_gscpi_lo    = _b[pi_trad]                       if _n == `h'+1
    replace u90_gscpi_lo  = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h'+1
    replace d90_gscpi_lo  = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h'+1
    replace Fstat_gscpi_lo = e(widstat)                       if _n == `h'+1
}
di "KP F (Low GSCPI):  " Fstat_gscpi_lo[1]

qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)         ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad       ///
        l(1/$lags).ur l(1/$lags).dln_neer              ///
        if high_gscpi == 1, absorb(id) vce(robust)
    replace b_gscpi_hi    = _b[pi_trad]                       if _n == `h'+1
    replace u90_gscpi_hi  = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h'+1
    replace d90_gscpi_hi  = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h'+1
    replace Fstat_gscpi_hi = e(widstat)                       if _n == `h'+1
}
di "KP F (High GSCPI): " Fstat_gscpi_hi[1]

*===============================================================================
* LP-IV: NEER regimes
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)         ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad       ///
        l(1/$lags).ur l(1/$lags).dln_neer              ///
        if apprec == 1, absorb(id) vce(robust)
    replace b_neer_ap    = _b[pi_trad]                       if _n == `h'+1
    replace u90_neer_ap  = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h'+1
    replace d90_neer_ap  = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h'+1
    replace Fstat_neer_ap = e(widstat)                       if _n == `h'+1
}
di "KP F (Appreciation): " Fstat_neer_ap[1]

qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)         ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad       ///
        l(1/$lags).ur l(1/$lags).dln_neer              ///
        if apprec == 0, absorb(id) vce(robust)
    replace b_neer_dep    = _b[pi_trad]                       if _n == `h'+1
    replace u90_neer_dep  = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h'+1
    replace d90_neer_dep  = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h'+1
    replace Fstat_neer_dep = e(widstat)                       if _n == `h'+1
}
di "KP F (Depreciation): " Fstat_neer_dep[1]

*===============================================================================
* LP-IV: Volatility regimes
*===============================================================================
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)         ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad       ///
        l(1/$lags).ur l(1/$lags).dln_neer              ///
        if high_vol == 0, absorb(id) vce(robust)
    replace b_vol_lo    = _b[pi_trad]                       if _n == `h'+1
    replace u90_vol_lo  = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h'+1
    replace d90_vol_lo  = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h'+1
    replace Fstat_vol_lo = e(widstat)                       if _n == `h'+1
}
di "KP F (Low volatility):  " Fstat_vol_lo[1]

qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)         ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad       ///
        l(1/$lags).ur l(1/$lags).dln_neer              ///
        if high_vol == 1, absorb(id) vce(robust)
    replace b_vol_hi    = _b[pi_trad]                       if _n == `h'+1
    replace u90_vol_hi  = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h'+1
    replace d90_vol_hi  = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h'+1
    replace Fstat_vol_hi = e(widstat)                       if _n == `h'+1
}
di "KP F (High volatility): " Fstat_vol_hi[1]

*===============================================================================
* Save diagnostics
*===============================================================================
di ""
di "Summary of KP F-statistics (all regimes):"
list Months Fstat_gscpi_lo Fstat_gscpi_hi Fstat_neer_ap Fstat_neer_dep ///
            Fstat_vol_lo Fstat_vol_hi if Months == 0

di ""
di "Point estimates at h=0, 6, 12:"
list Months b_gscpi_lo b_gscpi_hi b_neer_ap b_neer_dep b_vol_lo b_vol_hi ///
    if inlist(Months, 0, 6, 12)

*===============================================================================
* Save IRF data
*===============================================================================
preserve
keep if Months != .
keep Months ///
     b_gscpi_lo u90_gscpi_lo d90_gscpi_lo Fstat_gscpi_lo   ///
     b_gscpi_hi u90_gscpi_hi d90_gscpi_hi Fstat_gscpi_hi   ///
     b_neer_ap u90_neer_ap d90_neer_ap Fstat_neer_ap        ///
     b_neer_dep u90_neer_dep d90_neer_dep Fstat_neer_dep    ///
     b_vol_lo u90_vol_lo d90_vol_lo Fstat_vol_lo            ///
     b_vol_hi u90_vol_hi d90_vol_hi Fstat_vol_hi
export delimited "../output/tables/irf_state_dependence.csv", replace
restore

*===============================================================================
* Build F-stat locals for graph notes
*===============================================================================
local f_gscpi_lo = string(round(Fstat_gscpi_lo[1],  0.1))
local f_gscpi_hi = string(round(Fstat_gscpi_hi[1],  0.1))
local f_neer_ap  = string(round(Fstat_neer_ap[1],   0.1))
local f_neer_dep = string(round(Fstat_neer_dep[1],  0.1))
local f_vol_lo   = string(round(Fstat_vol_lo[1],    0.1))
local f_vol_hi   = string(round(Fstat_vol_hi[1],    0.1))

*===============================================================================
* Individual sub-graphs (to be combined)
*===============================================================================

* ── GSCPI: Low ────────────────────────────────────────────────────────────────
twoway ///
    (rarea u90_gscpi_lo d90_gscpi_lo Months, fcolor(green%20) lcolor(green%20) lw(none)) ///
    (line b_gscpi_lo Months, lcolor(green) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("Low Supply Chain Stress", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_gscpi_lo'", size(vsmall)) graphregion(color(white))
gr rename g_gscpi_lo, replace

* ── GSCPI: High ───────────────────────────────────────────────────────────────
twoway ///
    (rarea u90_gscpi_hi d90_gscpi_hi Months, fcolor(red%20) lcolor(red%20) lw(none)) ///
    (line b_gscpi_hi Months, lcolor(red) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("High Supply Chain Stress", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_gscpi_hi'", size(vsmall)) graphregion(color(white))
gr rename g_gscpi_hi, replace

* ── NEER: Appreciation ────────────────────────────────────────────────────────
twoway ///
    (rarea u90_neer_ap d90_neer_ap Months, fcolor(green%20) lcolor(green%20) lw(none)) ///
    (line b_neer_ap Months, lcolor(green) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("NEER Appreciation", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_neer_ap'", size(vsmall)) graphregion(color(white))
gr rename g_neer_ap, replace

* ── NEER: Depreciation ────────────────────────────────────────────────────────
twoway ///
    (rarea u90_neer_dep d90_neer_dep Months, fcolor(red%20) lcolor(red%20) lw(none)) ///
    (line b_neer_dep Months, lcolor(red) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("NEER Depreciation", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_neer_dep'", size(vsmall)) graphregion(color(white))
gr rename g_neer_dep, replace

* ── Volatility: Low ───────────────────────────────────────────────────────────
twoway ///
    (rarea u90_vol_lo d90_vol_lo Months, fcolor(green%20) lcolor(green%20) lw(none)) ///
    (line b_vol_lo Months, lcolor(green) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("Low Inflation Volatility", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("Months after shock", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_vol_lo'", size(vsmall)) graphregion(color(white))
gr rename g_vol_lo, replace

* ── Volatility: High ──────────────────────────────────────────────────────────
twoway ///
    (rarea u90_vol_hi d90_vol_hi Months, fcolor(red%20) lcolor(red%20) lw(none)) ///
    (line b_vol_hi Months, lcolor(red) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("High Inflation Volatility", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("Months after shock", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_vol_hi'", size(vsmall)) graphregion(color(white))
gr rename g_vol_hi, replace

*===============================================================================
* Combined Grid: 3 rows x 2 columns
*===============================================================================
graph combine ///
    g_gscpi_lo  g_gscpi_hi  ///
    g_neer_ap   g_neer_dep  ///
    g_vol_lo    g_vol_hi    ///
    , cols(2) iscale(0.55) imargin(2 4 2 4)  ///
    title("State Dependence: EMU20 (Extended Dataset)", ///
          size(medsmall) color(black))        ///
    note("LP-IV. Bartik IV: w{subscript:trad} {&times} BH oil shock. 12 lags. EMU20. Country FE. vce(robust). 90% CI.", ///
         size(vsmall))                        ///
    graphregion(color(white)) xsize(8) ysize(10)

gr rename g_state_dependence, replace
graph export "../output/figures/g_state_dependence.pdf", replace

di ""
di "Figure saved: output/figures/g_state_dependence.pdf"
di "IRF data saved: output/tables/irf_state_dependence.csv"
