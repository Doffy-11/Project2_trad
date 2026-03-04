
* Phase 3 — Step 2: EU27 Extension and EMU20 vs EU27 Comparison
* Run from the code/ directory:
*   cd code && do 6.1_LP_EU27_extension.do
*
* Extends the baseline LP-IV to the full EU27 sample (27 countries).
* The EMU20 vs EU27 comparison is the core identification robustness
* check: if the mechanism is driven by monetary union membership
* (common monetary policy + exchange rate), pass-through should be
* stronger in EMU20 than in EU27.
*
* Specification identical to baseline in both samples.
* NEER variable:
*   EMU20 countries — log change in Euro EER-40 (common, from ECB)
*   EU7 countries   — log change in bilateral EUR spot rate (country-
*                     specific, from ECB EXR; national currency per EUR)

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
* Load full EU27 panel
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

gen w_trad = w_food + w_clothing + w_furnishing + w_transport + w_alcohol

gen pi_trad = (pi_food        * w_food        ///
             + pi_clothing    * w_clothing     ///
             + pi_furnishing  * w_furnishing   ///
             + pi_transport   * w_transport    ///
             + pi_alcohol     * w_alcohol)     ///
             / w_trad

label var pi_trad "Tradable Inflation"

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

gen Z_bartik = w_trad * bh_oil_price_exp_shock
label var Z_bartik "Bartik IV: Tradables Weight x BH Oil Price Shock"

global hmax = 13
global lags  = 12

* ── Forward LHS Variables ─────────────────────────────────────────────────────
forv h = 0/$hmax {
    gen pi_nt_h`h' = F`h'.pi_nontrad
    label var pi_nt_h`h' "Non-Tradable Inflation, horizon h=`h'"
}

* ── IRF Storage: two specs (emu20 and eu27) ───────────────────────────────────
cap drop Months Zero
foreach stub in emu20 eu27 {
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

* ── LP-IV Loop: EMU20 only ────────────────────────────────────────────────────
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

* ── LP-IV Loop: Full EU27 ─────────────────────────────────────────────────────
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

* ── Diagnostics ───────────────────────────────────────────────────────────────
di ""
di "==================================================================="
di "PHASE 3: EU27 EXTENSION — RESULTS"
di "==================================================================="
di ""
di "KP F-statistics — EMU20:"
list Months Fstat_emu20 if Months != .

di ""
di "KP F-statistics — EU27:"
list Months Fstat_eu27 if Months != .

di ""
di "Point estimates comparison (EMU20 vs EU27):"
list Months b_emu20 b_eu27 if Months != .

* ── Save IRF to CSV ───────────────────────────────────────────────────────────
preserve
keep if Months != .
keep Months b_emu20 u90_emu20 d90_emu20 u68_emu20 d68_emu20 Fstat_emu20 ///
           b_eu27  u90_eu27  d90_eu27  u68_eu27  d68_eu27  Fstat_eu27
export delimited using "../output/tables/irf_eu27_comparison.csv", replace
restore

* ── Publication-Quality Overlay Figure ───────────────────────────────────────
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
graph export "../output/figures/g_lp_eu27_comparison.pdf", replace

di ""
di "Figure saved: output/figures/g_lp_eu27_comparison.pdf"
di "IRF data saved: output/tables/irf_eu27_comparison.csv"
