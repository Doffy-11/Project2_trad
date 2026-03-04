
* Run this do-file from the code/ directory.
* From the project root in Stata: cd code
* Then: do 2.0_LP_samples.do
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

label var pi_trad "Tradable Inflation (Food, Clothing, Furnishing, Transport, Alcohol)"

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

label var pi_nontrad "Non-Tradable Inflation (Housing, Health, Education, Restaurants, Other, Comm, Recreation)"

*===============================================================================
* Sample Split Overlay
*   Spec 1: Full sample
*   Spec 2: Pre-2020
*   Spec 3: Post-2020
*
* Baseline instrument: Z_bartik = w_trad x bh_oil_price_exp_shock
* Control: bh_oil_supply_shock
* FE: Country only | Lags: 12
*===============================================================================

global hmax = 13
global lags  = 12

* ── Instrument ────────────────────────────────────────────────────────────────
gen Z_bartik = w_trad * bh_oil_price_exp_shock
label var Z_bartik "Bartik IV: Tradables Weight x Market Oil Price Shock"

* ── Forward LHS Variables ─────────────────────────────────────────────────────
forv h = 0/$hmax {
    gen pi_nt_h`h' = F`h'.pi_nontrad
    label var pi_nt_h`h' "Non-Tradable Inflation, horizon h=`h'"
}

* ── IRF Storage ───────────────────────────────────────────────────────────────
cap drop Months Zero
cap drop b_full u_full d_full Fstat_full
cap drop b_pre  u_pre  d_pre  Fstat_pre
cap drop b_post u_post d_post Fstat_post

gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1

foreach stub in full pre post {
    gen b_`stub'     = .
    gen u_`stub'     = .
    gen d_`stub'     = .
    gen Fstat_`stub' = .
}

* ── LP-IV Loop: Full Sample ───────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        , absorb(id ) vce(robust)
    replace b_full     = _b[pi_trad]                       if _n == `h' + 1
    replace u_full     = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h' + 1
    replace d_full     = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h' + 1
    replace Fstat_full = e(widstat)                         if _n == `h' + 1
}

* ── LP-IV Loop: Pre-2020 ──────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        if year < 2020                           ///
        , absorb(id ) vce(robust)
    replace b_pre     = _b[pi_trad]                       if _n == `h' + 1
    replace u_pre     = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h' + 1
    replace d_pre     = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h' + 1
    replace Fstat_pre = e(widstat)                         if _n == `h' + 1
}

* ── LP-IV Loop: Post-2020 ─────────────────────────────────────────────────────
qui forv h = 0/$hmax {
    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        if year >= 2020                          ///
        , absorb(id ) vce(robust)
    replace b_post     = _b[pi_trad]                       if _n == `h' + 1
    replace u_post     = _b[pi_trad] + 1.645*_se[pi_trad]  if _n == `h' + 1
    replace d_post     = _b[pi_trad] - 1.645*_se[pi_trad]  if _n == `h' + 1
    replace Fstat_post = e(widstat)                         if _n == `h' + 1
}

* ── First Stage Diagnostics ───────────────────────────────────────────────────
di "KP F-statistics — Full sample:"
list Months Fstat_full if Months != .

di "KP F-statistics — Pre-2020:"
list Months Fstat_pre if Months != .

di "KP F-statistics — Post-2020:"
list Months Fstat_post if Months != .

* ── Overlay Plot ──────────────────────────────────────────────────────────────
twoway ///
    (rarea u_full d_full Months,                            ///
        fcolor(blue%15) lcolor(blue%15) lw(none))           ///
    (line b_full Months,                                    ///
        lcolor(blue) lpattern(solid) lwidth(thick))         ///
    (line b_pre Months,                                     ///
        lcolor(gs8) lpattern(shortdash) lwidth(medthick))   ///
    (line b_post Months,                                    ///
        lcolor(red) lpattern(dash) lwidth(medthick))        ///
    (line Zero Months,                                      ///
        lcolor(black) lpattern(dash) lwidth(thin)),         ///
    legend(order(                                           ///
        2 "Full sample"                                     ///
        3 "Pre-2020"                                        ///
        4 "Post-2020")                                      ///
        size(small) rows(3) pos(5) ring(0))                 ///
    title("Second-Round Pass-Through: Sample Splits",       ///
          color(black) size(medsmall))                      ///
    ytitle("{&theta}{subscript:h}", size(medsmall))         ///
    xtitle("Months after shock", size(medsmall))            ///
    xlabel(0(2)12) xscale(range(0 12))                      ///
    ylabel(, labsize(small) format(%5.2f))                  ///
    note("90% CI shown for full sample only. 12 lags. Country FE." ///
         "Bartik IV: w{subscript:trad} x BH oil price shock.",     ///
         size(vsmall))                                      ///
    graphregion(color(white))

gr rename g_lp_samples, replace

graph export "../output/figures/g_lp_samples.pdf", replace
