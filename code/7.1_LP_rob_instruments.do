
* Phase 4 — Instrument Robustness
* Compares the baseline Baumeister-Hamilton instrument against two alternatives:
*   (1) GSCPI shocks: AR(2) innovations on the NY Fed Global Supply Chain Pressure Index
*   (2) IMF non-fuel commodity shocks: AR(1) innovations on log-differences of the
*       IMF non-fuel primary commodity price index (PALLFNFINDEXM via FRED)
* In all cases the instrument enters in Bartik-style: Z = w_trad * shock
* Sample: EMU20 only (emu==1). Controls, FE, and lag structure identical to baseline.
* BDI: skipped (FRED series unavailable; documented in Phase 2 report).

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

*===============================================================================
* Baseline Bartik instrument (BH oil price expectation shock)
*===============================================================================
gen Z_bh = w_trad * bh_oil_price_exp_shock
label var Z_bh "Bartik IV: w_trad x BH oil price shock"

*===============================================================================
* Merge GSCPI shocks (AR(2) innovations)
*===============================================================================
preserve
import delimited "../data/clean/gscpi_shocks.csv", clear varnames(1)
tempfile gscpi_tmp
save `gscpi_tmp'
restore
merge m:1 year month using `gscpi_tmp', keep(match master) nogen

gen Z_gscpi = w_trad * gscpi_shock
label var Z_gscpi "Bartik IV: w_trad x GSCPI AR(2) shock"

* Also create a direct (non-Bartik) GSCPI instrument as diagnostic
* The direct version uses GSCPI shock uniformly (no tradable-exposure weighting)
gen Z_gscpi_direct = gscpi_shock
label var Z_gscpi_direct "Direct IV: GSCPI AR(2) shock (no Bartik weight)"

*===============================================================================
* Merge IMF non-fuel commodity shocks (AR(1) innovations on dln_imf)
*===============================================================================
preserve
import delimited "../data/clean/imf_shocks.csv", clear varnames(1)
tempfile imf_tmp
save `imf_tmp'
restore
merge m:1 year month using `imf_tmp', keep(match master) nogen

gen Z_imf = w_trad * imf_shock
label var Z_imf "Bartik IV: w_trad x IMF non-fuel AR(1) shock"

*===============================================================================
* Re-sort and re-declare panel after merges
*===============================================================================
sort id time
xtset id time

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
* IRF storage matrices
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
* LP-IV: GSCPI shocks
* NOTE — WEAK INSTRUMENT: GSCPI is retained here for diagnostic completeness
* only. KP F-statistic is approximately 2 across all horizons in both the
* Bartik-weighted and direct variants, far below the conventional threshold of
* 10 (and the Stock-Yogo 10%-size critical value of 16.38 for a single
* endogenous regressor). With such a weak first stage, IV estimates are
* severely biased toward OLS and standard errors are inflated, causing the
* point estimates to diverge explosively at longer horizons (reaching ~14 at
* h=13). The failure is economically interpretable: GSCPI measures supply-
* chain pressures that transmit broadly across all CPI components, so the
* Bartik wrapper w_trad × gscpi_shock does not create meaningful cross-country
* variation in tradable inflation beyond what country FE and lags already
* absorb. The direct version (Z_gscpi_direct = gscpi_shock) also fails (KP
* F ≈ 2) because GSCPI variation is purely time-series with no cross-sectional
* leverage. Conclusion: GSCPI is excluded from the robustness table.
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
* LP-IV: GSCPI direct (no Bartik weight) — diagnostic for weak instrument
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
* NOTABLE DIVERGENCE: IMF non-fuel estimates are systematically larger than
* the BH baseline across all horizons (e.g. 1.98 vs 0.96 at h=6; 2.45 vs
* 0.82 at h=9) and do not exhibit the same reversion after h=6. The instrument
* remains strong (KP F ≈ 176) so this is not a weak-IV artefact. The likely
* explanation is that the IMF non-fuel commodity basket captures a broader set
* of tradable price pressures — agricultural commodities, metals, raw
* materials — beyond the oil channel that dominates BH. These broader
* commodity shocks may transmit with longer lags into non-tradable inflation
* through wage and input-cost channels that are less responsive to monetary
* policy accommodation. Author to address this interpretation in the paper.
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
di "Point estimates — b_bh vs b_gscpi (Bartik) vs b_gscpi_d (direct) vs b_imf:"
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
export delimited "../output/tables/irf_rob_instruments.csv", replace
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
        8 "GSCPI AR(2) shocks"                                    ///
        9 "IMF non-fuel AR(1) shocks"                             ///
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
         "Baseline: BH oil price expectation shock. GSCPI: AR(2) residuals (NY Fed)." ///
         "IMF: AR(1) residuals on dln(non-fuel commodity index, PALLFNFINDEXM via FRED)." ///
         "90% and 68% CI shown. 12 lags. Country FE. vce(robust).",             ///
         size(vsmall))                                            ///
    graphregion(color(white))

gr rename g_lp_rob_instruments, replace
graph export "../output/figures/g_lp_rob_instruments.pdf", replace

di ""
di "Figure saved: output/figures/g_lp_rob_instruments.pdf"
