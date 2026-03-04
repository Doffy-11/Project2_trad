
* Phase 6 — State Dependence (Extended Dataset)
* Confirms the three state-dependence specifications from 3.0_LP_regimes.do
* are stable on the extended EMU20 dataset (1998m1–2025m12, N=20 countries).
* Regimes:
*   (1) GSCPI: high vs low supply chain stress (panel median split)
*   (2) NEER:  appreciation vs depreciation (dln_neer > 0)
*   (3) Inflation volatility: high vs low (36m rolling std, within-country median)
* All regressions restricted to EMU20 (emu==1). Baseline spec: L=12.
*
* Comparison with 3.0_LP_regimes.do:
*   - Old: ran on full panel before EU27 extension (EMU19 only, up to ~2024)
*   - New: EMU20 (includes Greece), full 1998m1–2025m12 range

cap cd code

clear all
cap drop _all
cap graph drop _all

*===============================================================================
* Graph Settings
*===============================================================================
grstyle clear
set scheme s2color
grstyle init
grstyle set plain, horizontal grid
grstyle set symbol
grstyle set legend 10, inside nobox

*===============================================================================
* Load panel — restrict to EMU20
*===============================================================================
use "../data/clean/panel.dta"
keep if emu == 1

egen id = group(code)
sort code year month
gen time = ym(year, month)
format time %tm
xtset id time

*===============================================================================
* Variable Construction
*===============================================================================
gen w_trad = w_food + w_clothing + w_furnishing + w_transport + w_alcohol

gen pi_trad = (pi_food        * w_food        ///
             + pi_clothing    * w_clothing     ///
             + pi_furnishing  * w_furnishing   ///
             + pi_transport   * w_transport    ///
             + pi_alcohol     * w_alcohol)     ///
             / w_trad

gen w_nontrad = w_housing + w_health + w_education ///
              + w_restaurants + w_other             ///
              + w_communication + w_recreation

gen pi_nontrad = (pi_housing       * w_housing       ///
                + pi_health        * w_health        ///
                + pi_education     * w_education     ///
                + pi_restaurants   * w_restaurants   ///
                + pi_other         * w_other         ///
                + pi_communication * w_communication ///
                + pi_recreation    * w_recreation)   ///
                / w_nontrad

gen Z_bartik = w_trad * bh_oil_price_exp_shock
label var Z_bartik "Bartik IV: w_trad x BH oil shock"

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
* 36m rolling std of headline inflation, within-country median
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

* ── Forward LHS Variables ─────────────────────────────────────────────────────
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

foreach stub in gscpi_lo gscpi_hi apprec_hi apprec_lo vol_lo vol_hi {
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
    replace b_apprec_hi    = _b[pi_trad]                       if _n == `h'+1
    replace u90_apprec_hi  = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h'+1
    replace d90_apprec_hi  = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h'+1
    replace Fstat_apprec_hi = e(widstat)                       if _n == `h'+1
}
di "KP F (Appreciation): " Fstat_apprec_hi[1]

qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)         ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad       ///
        l(1/$lags).ur l(1/$lags).dln_neer              ///
        if apprec == 0, absorb(id) vce(robust)
    replace b_apprec_lo    = _b[pi_trad]                       if _n == `h'+1
    replace u90_apprec_lo  = _b[pi_trad] + 1.645*_se[pi_trad] if _n == `h'+1
    replace d90_apprec_lo  = _b[pi_trad] - 1.645*_se[pi_trad] if _n == `h'+1
    replace Fstat_apprec_lo = e(widstat)                       if _n == `h'+1
}
di "KP F (Depreciation): " Fstat_apprec_lo[1]

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
list Months Fstat_gscpi_lo Fstat_gscpi_hi Fstat_apprec_hi Fstat_apprec_lo ///
            Fstat_vol_lo Fstat_vol_hi if Months == 0

di ""
di "Point estimates at h=0, 6, 12:"
list Months b_gscpi_lo b_gscpi_hi b_apprec_hi b_apprec_lo b_vol_lo b_vol_hi ///
    if inlist(Months, 0, 6, 12)

*===============================================================================
* Save IRF data
*===============================================================================
preserve
keep if Months != .
keep Months ///
     b_gscpi_lo u90_gscpi_lo d90_gscpi_lo Fstat_gscpi_lo   ///
     b_gscpi_hi u90_gscpi_hi d90_gscpi_hi Fstat_gscpi_hi   ///
     b_apprec_hi u90_apprec_hi d90_apprec_hi Fstat_apprec_hi ///
     b_apprec_lo u90_apprec_lo d90_apprec_lo Fstat_apprec_lo ///
     b_vol_lo u90_vol_lo d90_vol_lo Fstat_vol_lo             ///
     b_vol_hi u90_vol_hi d90_vol_hi Fstat_vol_hi
export delimited "../output/tables/irf_state_dependence.csv", replace
restore

*===============================================================================
* Build F-stat locals for graph notes
*===============================================================================
local f_gscpi_lo = string(round(Fstat_gscpi_lo[1],  0.1))
local f_gscpi_hi = string(round(Fstat_gscpi_hi[1],  0.1))
local f_apprec   = string(round(Fstat_apprec_hi[1], 0.1))
local f_deprec   = string(round(Fstat_apprec_lo[1], 0.1))
local f_vol_lo   = string(round(Fstat_vol_lo[1],    0.1))
local f_vol_hi   = string(round(Fstat_vol_hi[1],    0.1))

*===============================================================================
* Individual sub-graphs (to be combined)
*===============================================================================

* ── GSCPI: Low ────────────────────────────────────────────────────────────────
twoway ///
    (rarea u90_gscpi_lo d90_gscpi_lo Months, fcolor(forest_green%20) lcolor(forest_green%20) lw(none)) ///
    (line b_gscpi_lo Months, lcolor(forest_green) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("Low Supply Chain Stress", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_gscpi_lo'", size(vsmall)) graphregion(color(white))
gr rename g_gscpi_lo, replace

* ── GSCPI: High ───────────────────────────────────────────────────────────────
twoway ///
    (rarea u90_gscpi_hi d90_gscpi_hi Months, fcolor(cranberry%20) lcolor(cranberry%20) lw(none)) ///
    (line b_gscpi_hi Months, lcolor(cranberry) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("High Supply Chain Stress", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_gscpi_hi'", size(vsmall)) graphregion(color(white))
gr rename g_gscpi_hi, replace

* ── NEER: Appreciation ────────────────────────────────────────────────────────
twoway ///
    (rarea u90_apprec_hi d90_apprec_hi Months, fcolor(forest_green%20) lcolor(forest_green%20) lw(none)) ///
    (line b_apprec_hi Months, lcolor(forest_green) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("NEER Appreciation", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_apprec'", size(vsmall)) graphregion(color(white))
gr rename g_apprec, replace

* ── NEER: Depreciation ────────────────────────────────────────────────────────
twoway ///
    (rarea u90_apprec_lo d90_apprec_lo Months, fcolor(cranberry%20) lcolor(cranberry%20) lw(none)) ///
    (line b_apprec_lo Months, lcolor(cranberry) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("NEER Depreciation", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_deprec'", size(vsmall)) graphregion(color(white))
gr rename g_deprec, replace

* ── Volatility: Low ───────────────────────────────────────────────────────────
twoway ///
    (rarea u90_vol_lo d90_vol_lo Months, fcolor(forest_green%20) lcolor(forest_green%20) lw(none)) ///
    (line b_vol_lo Months, lcolor(forest_green) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("Low Inflation Volatility", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("Months after shock", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_vol_lo'", size(vsmall)) graphregion(color(white))
gr rename g_vol_lo, replace

* ── Volatility: High ──────────────────────────────────────────────────────────
twoway ///
    (rarea u90_vol_hi d90_vol_hi Months, fcolor(cranberry%20) lcolor(cranberry%20) lw(none)) ///
    (line b_vol_hi Months, lcolor(cranberry) lpattern(solid) lwidth(thick)) ///
    (line Zero Months, lcolor(black) lpattern(dash) lwidth(thin)), ///
    legend(off) title("High Inflation Volatility", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small)) xtitle("Months after shock", size(small)) ///
    xlabel(0(2)12) xscale(range(0 12)) ylabel(, labsize(small) format(%5.2f)) ///
    note("KP F = `f_vol_hi'", size(vsmall)) graphregion(color(white))
gr rename g_vol_hi, replace

*===============================================================================
* Combined Grid: 3 rows × 2 columns
*===============================================================================
graph combine ///
    g_gscpi_lo  g_gscpi_hi  ///
    g_apprec    g_deprec    ///
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
