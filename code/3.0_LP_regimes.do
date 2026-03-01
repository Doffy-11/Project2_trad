
* Run this do-file from the code/ directory.
* From the project root in Stata: cd code
* Then: do 3.0_LP_regimes.do
cap cd code

clear all
cap drop _all
cap graph drop _all
cls

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
* Load & Panel Setup
*===============================================================================
use "../data/clean/panel.dta"

egen id = group(code)
sort code year month
gen time = ym(year, month)
format time %tm
xtset id time

*===============================================================================
* Tradable & Non-Tradable Inflation
*===============================================================================

* ── Tradable ──────────────────────────────────────────────────────────────────
gen w_trad = w_food + w_clothing + w_furnishing + w_transport + w_alcohol

gen pi_trad = (pi_food        * w_food        ///
             + pi_clothing    * w_clothing     ///
             + pi_furnishing  * w_furnishing   ///
             + pi_transport   * w_transport    ///
             + pi_alcohol     * w_alcohol)     ///
             / w_trad

label var pi_trad "Tradable Inflation"

* ── Non-Tradable ──────────────────────────────────────────────────────────────
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

label var pi_nontrad "Non-Tradable Inflation"

*===============================================================================
* Regime Definitions
*===============================================================================

* ── 1. GSCPI: above vs below panel median ─────────────────────────────────────
summarize gscpi, detail
local gscpi_med = r(p50)
gen high_gscpi = (gscpi > `gscpi_med') if gscpi != .
label var high_gscpi "High GSCPI (above median)"
di "GSCPI median threshold: `gscpi_med'"

* ── 2. NEER: appreciation vs depreciation ─────────────────────────────────────
gen apprec = (dln_neer > 0) if dln_neer != .
label var apprec "Appreciation (dln_neer > 0)"

* ── 3. Inflation volatility: 36m rolling std, within-country median split ─────
* Step 1: compute rolling 36-month std of headline inflation within country
rangestat (sd) roll_std=pi_headline, interval(time -35 0) by(id)
label var roll_std "36m rolling std of headline inflation"

* Step 2: within-country median of roll_std
bysort id: egen vol_median = median(roll_std)
gen high_vol = (roll_std > vol_median) if roll_std != .
label var high_vol "High Inflation Volatility (above within-country median)"
drop vol_median

di "Regime obs counts:"
tab high_gscpi
tab apprec
tab high_vol

*===============================================================================
* LP-IV Settings — Baseline Spec
*   Instrument:  Z_bartik = w_trad x bh_oil_price_exp_shock
*   Controls:    12 lags of pi_nontrad, pi_trad, ur, dln_neer
*   FE:          Country only
*   SE:          vce(robust)
*   Sample:      Full
*===============================================================================

global hmax = 13
global lags  = 12

* ── Bartik Instrument ─────────────────────────────────────────────────────────
gen Z_bartik = w_trad * bh_oil_price_exp_shock
label var Z_bartik "Bartik IV: Tradables Weight x Market Oil Price Shock"

* ── Forward LHS Variables ─────────────────────────────────────────────────────
forv h = 0/$hmax {
    gen pi_nt_h`h' = F`h'.pi_nontrad
    label var pi_nt_h`h' "Non-Tradable Inflation, horizon h=`h'"
}

* ── IRF Storage: one set per specification ────────────────────────────────────
cap drop Months Zero
gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1

foreach stub in base gscpi_lo gscpi_hi neer_ap neer_dep vol_lo vol_hi {
    cap drop b_`stub' u_`stub' d_`stub' F_`stub'
    gen b_`stub' = .
    gen u_`stub' = .
    gen d_`stub' = .
    gen F_`stub' = .
}

*===============================================================================
* LP-IV Loops
*===============================================================================

* ── Baseline ──────────────────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)      ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad    ///
        l(1/$lags).ur l(1/$lags).dln_neer           ///
        , absorb(id) vce(robust)
    replace b_base = _b[pi_trad]                       if _n == `h'+1
    replace u_base = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h'+1
    replace d_base = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h'+1
    replace F_base = e(widstat)                         if _n == `h'+1
}
di "KP F-stat (baseline): " F_base[1]

* ── GSCPI: Low ────────────────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)      ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad    ///
        l(1/$lags).ur l(1/$lags).dln_neer           ///
        if high_gscpi == 0                          ///
        , absorb(id) vce(robust)
    replace b_gscpi_lo = _b[pi_trad]                       if _n == `h'+1
    replace u_gscpi_lo = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h'+1
    replace d_gscpi_lo = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h'+1
    replace F_gscpi_lo = e(widstat)                         if _n == `h'+1
}
di "KP F-stat (Low GSCPI): " F_gscpi_lo[1]

* ── GSCPI: High ───────────────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)      ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad    ///
        l(1/$lags).ur l(1/$lags).dln_neer           ///
        if high_gscpi == 1                          ///
        , absorb(id) vce(robust)
    replace b_gscpi_hi = _b[pi_trad]                       if _n == `h'+1
    replace u_gscpi_hi = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h'+1
    replace d_gscpi_hi = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h'+1
    replace F_gscpi_hi = e(widstat)                         if _n == `h'+1
}
di "KP F-stat (High GSCPI): " F_gscpi_hi[1]

* ── NEER: Appreciation ────────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)      ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad    ///
        l(1/$lags).ur l(1/$lags).dln_neer           ///
        if apprec == 1                              ///
        , absorb(id) vce(robust)
    replace b_neer_ap = _b[pi_trad]                       if _n == `h'+1
    replace u_neer_ap = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h'+1
    replace d_neer_ap = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h'+1
    replace F_neer_ap = e(widstat)                         if _n == `h'+1
}
di "KP F-stat (Appreciation): " F_neer_ap[1]

* ── NEER: Depreciation ────────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)      ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad    ///
        l(1/$lags).ur l(1/$lags).dln_neer           ///
        if apprec == 0                              ///
        , absorb(id) vce(robust)
    replace b_neer_dep = _b[pi_trad]                       if _n == `h'+1
    replace u_neer_dep = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h'+1
    replace d_neer_dep = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h'+1
    replace F_neer_dep = e(widstat)                         if _n == `h'+1
}
di "KP F-stat (Depreciation): " F_neer_dep[1]

* ── Volatility: Low ───────────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)      ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad    ///
        l(1/$lags).ur l(1/$lags).dln_neer           ///
        if high_vol == 0                            ///
        , absorb(id) vce(robust)
    replace b_vol_lo = _b[pi_trad]                       if _n == `h'+1
    replace u_vol_lo = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h'+1
    replace d_vol_lo = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h'+1
    replace F_vol_lo = e(widstat)                         if _n == `h'+1
}
di "KP F-stat (Low Volatility): " F_vol_lo[1]

* ── Volatility: High ──────────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)      ///
        l(1/$lags).pi_nontrad l(1/$lags).pi_trad    ///
        l(1/$lags).ur l(1/$lags).dln_neer           ///
        if high_vol == 1                            ///
        , absorb(id) vce(robust)
    replace b_vol_hi = _b[pi_trad]                       if _n == `h'+1
    replace u_vol_hi = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h'+1
    replace d_vol_hi = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h'+1
    replace F_vol_hi = e(widstat)                         if _n == `h'+1
}
di "KP F-stat (High Volatility): " F_vol_hi[1]

*===============================================================================
* Grab F-stats for graph notes
*===============================================================================
local f_base    = string(round(F_base[1],    0.1))
local f_gscpi_lo = string(round(F_gscpi_lo[1], 0.1))
local f_gscpi_hi = string(round(F_gscpi_hi[1], 0.1))
local f_neer_ap  = string(round(F_neer_ap[1],  0.1))
local f_neer_dep = string(round(F_neer_dep[1], 0.1))
local f_vol_lo   = string(round(F_vol_lo[1],   0.1))
local f_vol_hi   = string(round(F_vol_hi[1],   0.1))

*===============================================================================
* Individual Graphs (to be combined with graph combine)
*===============================================================================

* ── GSCPI: Low ────────────────────────────────────────────────────────────────
twoway ///
    (rarea u_gscpi_lo d_gscpi_lo Months,                ///
        fcolor(green%20) lcolor(green%20) lw(none))     ///
    (line b_gscpi_lo Months,                            ///
        lcolor(green) lpattern(solid) lwidth(thick))    ///
    (line Zero Months,                                  ///
        lcolor(black) lpattern(dash) lwidth(thin)),     ///
    legend(off)                                         ///
    title("Low Supply Chain Stress", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small))        ///
    xtitle("", size(small))                             ///
    xlabel(0(2)12) xscale(range(0 12))                  ///
    ylabel(, labsize(small) format(%5.2f))              ///
    note("F = `f_gscpi_lo'", size(vsmall))              ///
    graphregion(color(white))
gr rename g_gscpi_lo, replace

* ── GSCPI: High ───────────────────────────────────────────────────────────────
twoway ///
    (rarea u_gscpi_hi d_gscpi_hi Months,                ///
        fcolor(red%20) lcolor(red%20) lw(none))         ///
    (line b_gscpi_hi Months,                            ///
        lcolor(red) lpattern(solid) lwidth(thick))      ///
    (line Zero Months,                                  ///
        lcolor(black) lpattern(dash) lwidth(thin)),     ///
    legend(off)                                         ///
    title("High Supply Chain Stress", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small))        ///
    xtitle("", size(small))                             ///
    xlabel(0(2)12) xscale(range(0 12))                  ///
    ylabel(, labsize(small) format(%5.2f))              ///
    note("F = `f_gscpi_hi'", size(vsmall))              ///
    graphregion(color(white))
gr rename g_gscpi_hi, replace

* ── NEER: Appreciation ────────────────────────────────────────────────────────
twoway ///
    (rarea u_neer_ap d_neer_ap Months,                  ///
        fcolor(green%20) lcolor(green%20) lw(none))     ///
    (line b_neer_ap Months,                             ///
        lcolor(green) lpattern(solid) lwidth(thick))    ///
    (line Zero Months,                                  ///
        lcolor(black) lpattern(dash) lwidth(thin)),     ///
    legend(off)                                         ///
    title("Appreciation", size(small) color(black))     ///
    ytitle("{&theta}{subscript:h}", size(small))        ///
    xtitle("", size(small))                             ///
    xlabel(0(2)12) xscale(range(0 12))                  ///
    ylabel(, labsize(small) format(%5.2f))              ///
    note("F = `f_neer_ap'", size(vsmall))               ///
    graphregion(color(white))
gr rename g_neer_ap, replace

* ── NEER: Depreciation ────────────────────────────────────────────────────────
twoway ///
    (rarea u_neer_dep d_neer_dep Months,                ///
        fcolor(red%20) lcolor(red%20) lw(none))         ///
    (line b_neer_dep Months,                            ///
        lcolor(red) lpattern(solid) lwidth(thick))      ///
    (line Zero Months,                                  ///
        lcolor(black) lpattern(dash) lwidth(thin)),     ///
    legend(off)                                         ///
    title("Depreciation", size(small) color(black))     ///
    ytitle("{&theta}{subscript:h}", size(small))        ///
    xtitle("", size(small))                             ///
    xlabel(0(2)12) xscale(range(0 12))                  ///
    ylabel(, labsize(small) format(%5.2f))              ///
    note("F = `f_neer_dep'", size(vsmall))              ///
    graphregion(color(white))
gr rename g_neer_dep, replace

* ── Volatility: Low ───────────────────────────────────────────────────────────
twoway ///
    (rarea u_vol_lo d_vol_lo Months,                    ///
        fcolor(green%20) lcolor(green%20) lw(none))     ///
    (line b_vol_lo Months,                              ///
        lcolor(green) lpattern(solid) lwidth(thick))    ///
    (line Zero Months,                                  ///
        lcolor(black) lpattern(dash) lwidth(thin)),     ///
    legend(off)                                         ///
    title("Low Inflation Volatility", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small))        ///
    xtitle("Months after shock", size(small))           ///
    xlabel(0(2)12) xscale(range(0 12))                  ///
    ylabel(, labsize(small) format(%5.2f))              ///
    note("F = `f_vol_lo'", size(vsmall))                ///
    graphregion(color(white))
gr rename g_vol_lo, replace

* ── Volatility: High ──────────────────────────────────────────────────────────
twoway ///
    (rarea u_vol_hi d_vol_hi Months,                    ///
        fcolor(red%20) lcolor(red%20) lw(none))         ///
    (line b_vol_hi Months,                              ///
        lcolor(red) lpattern(solid) lwidth(thick))      ///
    (line Zero Months,                                  ///
        lcolor(black) lpattern(dash) lwidth(thin)),     ///
    legend(off)                                         ///
    title("High Inflation Volatility", size(small) color(black)) ///
    ytitle("{&theta}{subscript:h}", size(small))        ///
    xtitle("Months after shock", size(small))           ///
    xlabel(0(2)12) xscale(range(0 12))                  ///
    ylabel(, labsize(small) format(%5.2f))              ///
    note("F = `f_vol_hi'", size(vsmall))                ///
    graphregion(color(white))
gr rename g_vol_hi, replace

*===============================================================================
* Combined Grid: Row 0 = baseline (spanning), Rows 1-3 = heterogeneity pairs
* Stata graph combine does not support true spanning so we use a 4x2 grid
* with the baseline in position (1,1) and a blank in (1,2)
*===============================================================================

graph combine                                           ///
    g_gscpi_lo  g_gscpi_hi                              ///
    g_neer_ap   g_neer_dep                              ///
    g_vol_lo    g_vol_hi                                ///
    , cols(2)                                           ///
    iscale(0.55)                                        ///
    imargin(2 4 2 4)                                    ///
    title("Second-Round Pass-Through: Heterogeneity",   ///
          size(medsmall) color(black))                  ///
    note("LP-IV. Bartik IV: w{subscript:trad} x BH oil price shock. 12 lags. Country FE. vce(robust). 90% CI.", ///
         size(vsmall))                                  ///
    graphregion(color(white))                           ///
    xsize(8) ysize(10)

gr rename g_heterogeneity, replace
graph export "../output/figures/g_regimes.pdf", replace
