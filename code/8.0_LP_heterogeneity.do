
* Phase 5 — Heterogeneity Analysis
* Splits the EMU20 into three economically motivated groups and runs the
* baseline LP-IV specification separately for each:
*
*   Core         (country_group_n == 1): DEU, FRA, NLD, AUT, FIN, BEL  (N=6)
*   Periphery    (country_group_n == 2): ITA, ESP, PRT, GRC              (N=4)
*   Small open   (country_group_n == 3): IRL, LUX, EST, LVA, LTU, SVK,
*                                        SVN, MLT, CYP, HRV             (N=10)
*
* Specification identical to baseline (6.0_LP_EMU20_replication.do):
*   ivreghdfe pi_nt_h`h' (pi_trad = Z_bartik)
*             l(1/12).pi_nontrad l(1/12).pi_trad l(1/12).ur l(1/12).dln_neer
*             , absorb(id) vce(robust)
*
* Produces:
*   (1) Overlay IRF figure: all three groups on the same axes
*   (2) IRF CSV for table production: irf_heterogeneity.csv
*   (3) Pairwise group-difference t-statistics (independent samples):
*       t = (b_g1 - b_g2) / sqrt(se_g1^2 + se_g2^2)
*       reported at selected horizons h = 0, 3, 6, 9, 12
*
* Note on small samples: Periphery has only 4 countries. The first-stage
* F-statistic is reported; if KP F < 10 for any group the result is flagged.

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
* Load full panel
*===============================================================================
use "../data/clean/panel.dta"

* Restrict to EMU20 (country_group_n 1-3)
keep if emu == 1

egen id = group(code)
sort code year month
gen time = ym(year, month)
format time %tm
xtset id time

*===============================================================================
* Tradable & Non-Tradable Inflation (identical to baseline)
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
label var Z_bartik "Bartik IV: w_trad x BH oil shock"

*===============================================================================
* LP parameters
*===============================================================================
global hmax = 13
global lags  = 12

*===============================================================================
* Forward LHS: Non-Tradable Inflation
*===============================================================================
forv h = 0/$hmax {
    gen pi_nt_h`h' = F`h'.pi_nontrad
    label var pi_nt_h`h' "Non-Tradable Inflation, h=`h'"
}

*===============================================================================
* IRF storage: point estimates, 90/68% CI, KP F-stat, SE
*===============================================================================
cap drop Months Zero
foreach stub in core peri soe {
    cap drop b_`stub' u90_`stub' d90_`stub' u68_`stub' d68_`stub' ///
             Fstat_`stub' se_`stub'
    gen b_`stub'     = .
    gen u90_`stub'   = .
    gen d90_`stub'   = .
    gen u68_`stub'   = .
    gen d68_`stub'   = .
    gen Fstat_`stub' = .
    gen se_`stub'    = .   // stored for pairwise t-tests
}
gen Months = _n - 1 if _n <= $hmax + 1
gen Zero   = 0      if _n <= $hmax + 1

*===============================================================================
* LP-IV: Core (country_group_n == 1: DEU FRA NLD AUT FIN BEL)
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'                    ///
        (pi_trad = Z_bartik)                 ///
        l(1/$lags).pi_nontrad                ///
        l(1/$lags).pi_trad                   ///
        l(1/$lags).ur                        ///
        l(1/$lags).dln_neer                  ///
        if country_group_n == 1             ///
        , absorb(id) vce(robust)

    replace b_core     = _b[pi_trad]                         if _n == `h' + 1
    replace u90_core   = _b[pi_trad] + 1.645*_se[pi_trad]   if _n == `h' + 1
    replace d90_core   = _b[pi_trad] - 1.645*_se[pi_trad]   if _n == `h' + 1
    replace u68_core   = _b[pi_trad] + 1.000*_se[pi_trad]   if _n == `h' + 1
    replace d68_core   = _b[pi_trad] - 1.000*_se[pi_trad]   if _n == `h' + 1
    replace Fstat_core = e(widstat)                          if _n == `h' + 1
    replace se_core    = _se[pi_trad]                        if _n == `h' + 1
}

*===============================================================================
* LP-IV: Periphery (country_group_n == 2: ITA ESP PRT GRC)
* NOTE: N=4 countries. If KP F < 10, results are flagged as potentially
* unreliable due to limited cross-sectional variation in the first stage.
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'                    ///
        (pi_trad = Z_bartik)                 ///
        l(1/$lags).pi_nontrad                ///
        l(1/$lags).pi_trad                   ///
        l(1/$lags).ur                        ///
        l(1/$lags).dln_neer                  ///
        if country_group_n == 2             ///
        , absorb(id) vce(robust)

    replace b_peri     = _b[pi_trad]                         if _n == `h' + 1
    replace u90_peri   = _b[pi_trad] + 1.645*_se[pi_trad]   if _n == `h' + 1
    replace d90_peri   = _b[pi_trad] - 1.645*_se[pi_trad]   if _n == `h' + 1
    replace u68_peri   = _b[pi_trad] + 1.000*_se[pi_trad]   if _n == `h' + 1
    replace d68_peri   = _b[pi_trad] - 1.000*_se[pi_trad]   if _n == `h' + 1
    replace Fstat_peri = e(widstat)                          if _n == `h' + 1
    replace se_peri    = _se[pi_trad]                        if _n == `h' + 1
}

*===============================================================================
* LP-IV: Small open economies (country_group_n == 3)
*===============================================================================
qui forv h = 0/$hmax {

    ivreghdfe pi_nt_h`h'                    ///
        (pi_trad = Z_bartik)                 ///
        l(1/$lags).pi_nontrad                ///
        l(1/$lags).pi_trad                   ///
        l(1/$lags).ur                        ///
        l(1/$lags).dln_neer                  ///
        if country_group_n == 3             ///
        , absorb(id) vce(robust)

    replace b_soe     = _b[pi_trad]                          if _n == `h' + 1
    replace u90_soe   = _b[pi_trad] + 1.645*_se[pi_trad]    if _n == `h' + 1
    replace d90_soe   = _b[pi_trad] - 1.645*_se[pi_trad]    if _n == `h' + 1
    replace u68_soe   = _b[pi_trad] + 1.000*_se[pi_trad]    if _n == `h' + 1
    replace d68_soe   = _b[pi_trad] - 1.000*_se[pi_trad]    if _n == `h' + 1
    replace Fstat_soe = e(widstat)                           if _n == `h' + 1
    replace se_soe    = _se[pi_trad]                         if _n == `h' + 1
}

*===============================================================================
* Diagnostics: F-statistics and point estimates
*===============================================================================
di ""
di "KP F-statistics by group:"
list Months Fstat_core Fstat_peri Fstat_soe if Months != .

di ""
di "Point estimates — Core vs Periphery vs SOE:"
list Months b_core b_peri b_soe if Months != .

*===============================================================================
* Pairwise t-statistics (independent samples: t = Δb / sqrt(se1² + se2²))
* Evaluated at selected horizons h = 0, 3, 6, 9, 12
*===============================================================================
di ""
di "Pairwise group-difference t-statistics (two-sided, N(0,1) critical 1.96):"
di ""
di "  h     Core-Peri   Core-SOE   Peri-SOE"
foreach h in 0 3 6 9 12 {
    local n = `h' + 1

    qui su b_core  if _n == `n'
    local bc = r(mean)
    qui su b_peri  if _n == `n'
    local bp = r(mean)
    qui su b_soe   if _n == `n'
    local bs = r(mean)
    qui su se_core if _n == `n'
    local sc = r(mean)
    qui su se_peri if _n == `n'
    local sp = r(mean)
    qui su se_soe  if _n == `n'
    local ss = r(mean)

    local t_cp = (`bc' - `bp') / sqrt(`sc'^2 + `sp'^2)
    local t_cs = (`bc' - `bs') / sqrt(`sc'^2 + `ss'^2)
    local t_ps = (`bp' - `bs') / sqrt(`sp'^2 + `ss'^2)

    di "  `h'     " %6.2f `t_cp' "     " %6.2f `t_cs' "     " %6.2f `t_ps'
}

*===============================================================================
* Save IRF data for table
*===============================================================================
preserve
keep if Months != .
keep Months ///
     b_core u90_core d90_core u68_core d68_core Fstat_core se_core ///
     b_peri u90_peri d90_peri u68_peri d68_peri Fstat_peri se_peri ///
     b_soe  u90_soe  d90_soe  u68_soe  d68_soe  Fstat_soe  se_soe
export delimited "../output/tables/irf_heterogeneity.csv", replace
restore

di ""
di "IRF data saved: output/tables/irf_heterogeneity.csv"

*===============================================================================
* Overlay Figure: Three groups on the same axes
*===============================================================================
twoway ///
    (rarea u90_core d90_core Months,                                   ///
        fcolor(navy%15) lcolor(navy%15) lw(none))                      ///
    (rarea u68_core d68_core Months,                                   ///
        fcolor(navy%30) lcolor(navy%30) lw(none))                      ///
    (rarea u90_peri d90_peri Months,                                   ///
        fcolor(cranberry%12) lcolor(cranberry%12) lw(none))            ///
    (rarea u68_peri d68_peri Months,                                   ///
        fcolor(cranberry%25) lcolor(cranberry%25) lw(none))            ///
    (rarea u90_soe d90_soe Months,                                     ///
        fcolor(forest_green%12) lcolor(forest_green%12) lw(none))      ///
    (rarea u68_soe d68_soe Months,                                     ///
        fcolor(forest_green%25) lcolor(forest_green%25) lw(none))      ///
    (line b_core Months,                                               ///
        lcolor(navy) lpattern(solid) lwidth(thick))                    ///
    (line b_peri Months,                                               ///
        lcolor(cranberry) lpattern(dash) lwidth(thick))                ///
    (line b_soe  Months,                                               ///
        lcolor(forest_green) lpattern(longdash) lwidth(thick))         ///
    (line Zero Months,                                                  ///
        lcolor(black) lpattern(dash) lwidth(thin)),                    ///
    legend(order(                                                       ///
        7 "Core (N=6)"                                                 ///
        8 "Periphery (N=4)"                                            ///
        9 "Small open (N=10)"                                          ///
        2 "68% CI"                                                     ///
        1 "90% CI")                                                    ///
        size(small) rows(5) pos(5) ring(0))                            ///
    title("Second-Round Pass-Through: Heterogeneity Across EMU Groups", ///
          color(black) size(medsmall))                                 ///
    ytitle("{&theta}{subscript:h}", size(medsmall))                    ///
    xtitle("Months after shock", size(medsmall))                       ///
    xlabel(0(2)12) xscale(range(0 12))                                 ///
    ylabel(, labsize(small) format(%5.2f))                             ///
    note("EMU20. Core: DEU FRA NLD AUT FIN BEL. Periphery: ITA ESP PRT GRC." ///
         "Small open: IRL LUX EST LVA LTU SVK SVN MLT CYP HRV."       ///
         "Bartik IV: w{subscript:trad} {&times} BH oil shock. 12 lags. Country FE. vce(robust)." ///
         "90% and 68% CI shown.", size(vsmall))                        ///
    graphregion(color(white))

gr rename g_lp_heterogeneity, replace
graph export "../output/figures/g_lp_heterogeneity.pdf", replace

di ""
di "Figure saved: output/figures/g_lp_heterogeneity.pdf"
