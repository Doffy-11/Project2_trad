
* _setup.do — Shared panel load, graph settings, and variable construction.
* Called at the top of each do-file with: do _setup.do
* Calling file is responsible for any sample filter (e.g. keep if emu==1),
* the panel xtset declaration, and LP parameters / forward LHS variables.
pwd
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
* Load panel
*===============================================================================

use "data/clean/panel.dta"

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
label var Z_bartik "Bartik IV: w_trad x BH oil shock"
