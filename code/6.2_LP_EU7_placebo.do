
* Phase 3 — Step 3: EU7 Placebo Test (Non-EMU Countries)
* Run from the code/ directory:
*   cd code && do 6.2_LP_EU7_placebo.do
*
* Runs the baseline LP-IV on the 7 non-EMU EU countries only.
* The placebo logic: if second-round pass-through is driven by the
* shared monetary policy constraint (no independent monetary policy
* to offset tradable price shocks), it should be absent or
* significantly weaker for non-EMU countries that retain their own
* monetary policy and exchange rate.
*
* Identification note: the Bartik instrument (w_trad x BH oil shock)
* retains relevance for non-EMU countries since global oil prices
* still feed into their tradables basket. However, the channel that
* generates the second-round effect (wage-price spiral under shared
* monetary policy) does not apply. Weak first stage is therefore
* informative in itself.

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
* Load panel — EU7 only
*===============================================================================
use "../data/clean/panel.dta"

keep if emu == 0

di "EU7 placebo — countries:"
tab code

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

* ── IRF Storage ───────────────────────────────────────────────────────────────
cap drop b u90 d90 u68 d68 Months Zero Fstat
gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1
gen b    = .
gen u90  = .
gen d90  = .
gen u68  = .
gen d68  = .
gen Fstat = .

* ── LP-IV Loop ────────────────────────────────────────────────────────────────
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

* ── Diagnostics ───────────────────────────────────────────────────────────────
di ""
di "==================================================================="
di "PHASE 3: EU7 PLACEBO — RESULTS"
di "==================================================================="
di ""
di "Kleibergen-Paap F-statistics by horizon:"
list Months Fstat if Months != .

di ""
di "Point estimates (EU7 placebo):"
list Months b u90 d90 if Months != .

* ── Save IRF to CSV ───────────────────────────────────────────────────────────
preserve
keep if Months != .
keep Months b u90 d90 u68 d68 Fstat
gen sample = "EU7_placebo"
export delimited using "../output/tables/irf_eu7_placebo.csv", replace
restore

* ── Publication-Quality IRF Figure ───────────────────────────────────────────
twoway ///
    (rarea u90 d90 Months,                                      ///
        fcolor(red%15) lcolor(red%15) lw(none))                 ///
    (rarea u68 d68 Months,                                      ///
        fcolor(red%30) lcolor(red%30) lw(none))                 ///
    (line b Months,                                             ///
        lcolor(red) lpattern(solid) lwidth(thick))              ///
    (line Zero Months,                                          ///
        lcolor(black) lpattern(dash) lwidth(thin)),             ///
    legend(order(                                               ///
        3 "Point estimate (EU7)"                                ///
        2 "68% CI"                                              ///
        1 "90% CI")                                             ///
        size(small) rows(3) pos(5) ring(0))                     ///
    title("Placebo: Pass-Through in Non-EMU EU Countries",      ///
          color(black) size(medsmall))                          ///
    ytitle("{&theta}{subscript:h}", size(medsmall))             ///
    xtitle("Months after shock", size(medsmall))                ///
    xlabel(0(2)12) xscale(range(0 12))                          ///
    ylabel(, labsize(small) format(%5.2f))                      ///
    note("90% and 68% CI. Bartik IV: w{subscript:trad} {&times} BH oil shock." ///
         "12 lags. Country FE. vce(robust). EU7 non-EMU countries only.",      ///
         size(vsmall))                                          ///
    graphregion(color(white))

gr rename g_lp_eu7_placebo, replace
graph export "../output/figures/g_lp_eu7_placebo.pdf", replace

di ""
di "Figure saved: output/figures/g_lp_eu7_placebo.pdf"
di "IRF data saved: output/tables/irf_eu7_placebo.csv"
