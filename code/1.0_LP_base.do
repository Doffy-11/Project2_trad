
* Run this do-file from the code/ directory.
* From the project root in Stata: cd code
* Then: do 1.0_LP_base.do
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
* LP-IV: Second-Round Pass-Through
* Baseline specification
*   Instrument:  Z_bartik = w_trad x bh_oil_price_exp_shock
*   Control:     
*   FE:          Country only (time FE would absorb common-shock instrument)
*   Lags:        12 (standard for monthly data)
*   Sample:      Full
*===============================================================================

global hmax = 13
global lags  = 12

* ── Bartik Instrument ─────────────────────────────────────────────────────────
* Exposure weight: tradables share of CPI basket (w_trad)
* Shock: market-based oil price shock (Baumeister & Hamilton 2019)
gen Z_bartik = w_trad * bh_oil_price_exp_shock
label var Z_bartik "Bartik IV: Tradables Weight x Market Oil Price Shock"



* ── Forward LHS Variables ─────────────────────────────────────────────────────
forv h = 0/$hmax {
    gen pi_nt_h`h' = F`h'.pi_nontrad
    label var pi_nt_h`h' "Non-Tradable Inflation, horizon h=`h'"
}

* ── IRF Storage Variables ─────────────────────────────────────────────────────
cap drop b u d Months Zero Fstat
gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1
gen b      = .
gen u      = .
gen d      = .
gen Fstat  = .

* ── LP-IV Loop ────────────────────────────────────────────────────────────────
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'                        ///
        (pi_trad = Z_bartik)                     ///
        l(1/$lags).pi_nontrad                    ///
        l(1/$lags).pi_trad                       ///
        l(1/$lags).ur                            ///
        l(1/$lags).dln_neer                      ///
        , absorb(id ) vce(robust)

    replace b     = _b[pi_trad]                            if _n == `h' + 1
    replace u     = _b[pi_trad] + 1.645*_se[pi_trad]      if _n == `h' + 1
    replace d     = _b[pi_trad] - 1.645*_se[pi_trad]      if _n == `h' + 1
    replace Fstat = e(widstat)                             if _n == `h' + 1
}

* ── First Stage Diagnostics ───────────────────────────────────────────────────
di "First-stage Kleibergen-Paap F-statistics by horizon:"
list Months Fstat if Months != .

* ── Plot IRF ──────────────────────────────────────────────────────────────────
twoway ///
    (rarea u d Months,                              ///
        fcolor(blue%20) lcolor(blue%20)             ///
        lw(none) lpattern(solid))                   ///
    (line b Months,                                 ///
        lcolor(blue) lpattern(solid) lwidth(thick)) ///
    (line Zero Months,                              ///
        lcolor(black) lpattern(dash)),              ///
    legend(off)                                     ///
    title("Second-Round Pass-Through:"              ///
          "Tradable {&rarr} Non-Tradable Inflation", ///
          color(black) size(medsmall))              ///
    ytitle("{&theta}{subscript:h}", size(medsmall)) ///
    xtitle("Months after shock", size(medsmall))    ///
    xlabel(0(2)12) xscale(range(0 12))              ///
    ylabel(, labsize(small) format(%5.2f))          ///
    note("90% CI. Bartik IV: w{subscript:trad} {&times} BH oil price shock." ///
         "12 lags. Country FE.", size(vsmall)) ///
    graphregion(color(white))

gr rename g_lp_base, replace
graph export "../output/figures/g_lp_base.pdf", replace
