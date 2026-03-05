
# 2_panel_construction.R — Panel Construction (EMU20 + EU7)
# Reads clean data files produced by 1_data_collection.R.
# Builds a unified panel for all 27 EU countries in one pass.
# NEER: EMU20 countries use the common EER-40 series (neer_m.csv);
#       EU7 countries use their bilateral EUR spot rate (neer_eu7.csv).
# Saves data/clean/panel.dta (Stata format).
# Run from the code/ directory.

suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(countrycode)
  library(haven)
  library(labelled)
})

out_dir <- "../data/clean"

# ── Country classification ─────────────────────────────────────────────────────
core_codes   <- c("DEU","FRA","NLD","AUT","FIN","BEL")
periph_codes <- c("ITA","ESP","PRT","GRC")
soe_codes    <- c("IRL","LUX","EST","LVA","LTU","SVK","SVN","MLT","CYP","HRV")
nonemu_codes <- c("BGR","CZE","DNK","HUN","POL","ROU","SWE")
emu20_codes  <- c(core_codes, periph_codes, soe_codes)

assign_group <- function(code) {
  case_when(
    code %in% core_codes   ~ "Core",
    code %in% periph_codes ~ "Periphery",
    code %in% soe_codes    ~ "Small open economies",
    code %in% nonemu_codes ~ "Non-EMU",
    TRUE ~ NA_character_
  )
}

assign_group_n <- function(code) {
  case_when(
    code %in% core_codes   ~ 1L,
    code %in% periph_codes ~ 2L,
    code %in% soe_codes    ~ 3L,
    code %in% nonemu_codes ~ 4L,
    TRUE ~ NA_integer_
  )
}

# ── HICP prices → wide panel ──────────────────────────────────────────────────
pi_final <- read.xlsx(file.path(out_dir, "pi_final.xlsx"))

panel <- pi_final %>%
  pivot_longer(matches("^\\d{4}-\\d{2}$"), names_to = "time", values_to = "value") %>%
  mutate(
    year    = as.integer(substr(time, 1, 4)),
    month   = as.integer(substr(time, 6, 7)),
    quarter = ceiling(month / 3),
    code    = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998) %>%
  select(year, month, quarter, code, country, var, value) %>%
  pivot_wider(names_from = var, values_from = value)

# ── HICP weights ──────────────────────────────────────────────────────────────
pi_weights_final <- read.xlsx(file.path(out_dir, "pi_weights_final.xlsx"))

panel_w <- pi_weights_final %>%
  pivot_longer(matches("^\\d{4}-\\d{2}$"), names_to = "time", values_to = "value") %>%
  mutate(
    year  = as.integer(substr(time, 1, 4)),
    month = as.integer(substr(time, 6, 7)),
    code  = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998) %>%
  select(year, month, code, w, value) %>%
  pivot_wider(names_from = w, values_from = value)

panel <- left_join(panel, panel_w, by = c("year", "month", "code"))

# ── Unemployment ──────────────────────────────────────────────────────────────
u <- read.csv(file.path(out_dir, "u.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month = as.integer(substr(month, 6, 7)),
    code  = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998) %>%
  select(code, month, year, ur)

panel <- left_join(panel, u, by = c("year", "month", "code"))

# ── Industrial production ─────────────────────────────────────────────────────
ip <- read.csv(file.path(out_dir, "ip.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month = as.integer(substr(month, 6, 7)),
    code  = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998) %>%
  select(code, month, year, ip_gr_yoy)

panel <- left_join(panel, ip, by = c("year", "month", "code"))

# ── Import exposure ────────────────────────────────────────────────────────────
ex_monthly <- read.csv(file.path(out_dir, "ex_monthly.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year       = as.integer(substr(month, 1, 4)),
    month      = as.integer(substr(month, 6, 7)),
    code       = countrycode(country, origin = "country.name", destination = "iso3c"),
    wi_imp_gdp = imp_gdp
  ) %>%
  filter(year >= 1998) %>%
  select(code, month, year, wi_imp_gdp)

panel <- left_join(panel, ex_monthly, by = c("year", "month", "code"))

# ── NEER ──────────────────────────────────────────────────────────────────────
# EMU20: common EER-40, merged by (year, month) — same value for all EMU20 rows
neer_emu20 <- read.csv(file.path(out_dir, "neer_m.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month = as.integer(substr(month, 6, 7))
  ) %>%
  filter(year >= 1998) %>%
  select(month, year, dln_neer_emu = dln_neer)

# EU7: bilateral EUR spot rates, merged by (year, month, code)
neer_eu7 <- read.csv(file.path(out_dir, "neer_eu7.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month = as.integer(substr(month, 6, 7)),
    code  = case_when(
      geo == "BG" ~ "BGR", geo == "CZ" ~ "CZE", geo == "DK" ~ "DNK",
      geo == "HU" ~ "HUN", geo == "PL" ~ "POL", geo == "RO" ~ "ROU",
      geo == "SE" ~ "SWE"
    )
  ) %>%
  filter(year >= 1998) %>%
  select(code, month, year, dln_neer_eu7 = dln_neer)

panel <- panel %>%
  left_join(neer_emu20, by = c("year", "month")) %>%
  left_join(neer_eu7,   by = c("year", "month", "code")) %>%
  mutate(dln_neer = if_else(!is.na(dln_neer_eu7), dln_neer_eu7, dln_neer_emu)) %>%
  select(-dln_neer_emu, -dln_neer_eu7)

# ── Global series ─────────────────────────────────────────────────────────────
oil_shock <- read.csv(file.path(out_dir, "oil_shock.csv"), stringsAsFactors = FALSE)
gscpi     <- read.xlsx(file.path(out_dir, "gscpi.xlsx"))
imf_comm  <- read.csv(file.path(out_dir, "imf_commodity.csv"), stringsAsFactors = FALSE) %>%
  mutate(year  = as.integer(substr(month, 1, 4)),
         month = as.integer(substr(month, 6, 7)))

panel <- panel %>%
  left_join(gscpi,     by = c("year", "month")) %>%
  left_join(oil_shock, by = c("year", "month")) %>%
  left_join(imf_comm,  by = c("year", "month"))

# ── Classification variables ──────────────────────────────────────────────────
panel <- panel %>%
  mutate(
    emu             = if_else(code %in% emu20_codes, 1L, 0L),
    country_group   = assign_group(code),
    country_group_n = assign_group_n(code)
  ) %>%
  arrange(code, year, month)

# ── Variable labels ────────────────────────────────────────────────────────────
panel <- panel %>%
  set_variable_labels(
    year                   = "Year",
    month                  = "Month (1-12)",
    quarter                = "Quarter (1-4)",
    code                   = "ISO3C Country Code",
    country                = "Country Name",
    pi_alcohol             = "Price Index: Alcoholic Beverages & Tobacco",
    pi_clothing            = "Price Index: Clothing & Footwear",
    pi_communication       = "Price Index: Communication",
    pi_education           = "Price Index: Education",
    pi_food                = "Price Index: Food & Non-Alcoholic Beverages",
    pi_furnishing          = "Price Index: Furnishings & Household Equipment",
    pi_headline            = "Price Index: Headline HICP",
    pi_health              = "Price Index: Health",
    pi_housing             = "Price Index: Housing, Water, Electricity, Gas",
    pi_other               = "Price Index: Miscellaneous Goods & Services",
    pi_recreation          = "Price Index: Recreation & Culture",
    pi_restaurants         = "Price Index: Restaurants & Hotels",
    pi_transport           = "Price Index: Transport",
    w_alcohol              = "Weight: Alcoholic Beverages & Tobacco",
    w_clothing             = "Weight: Clothing & Footwear",
    w_communication        = "Weight: Communication",
    w_education            = "Weight: Education",
    w_food                 = "Weight: Food & Non-Alcoholic Beverages",
    w_furnishing           = "Weight: Furnishings & Household Equipment",
    w_health               = "Weight: Health",
    w_housing              = "Weight: Housing, Water, Electricity, Gas",
    w_other                = "Weight: Miscellaneous Goods & Services",
    w_recreation           = "Weight: Recreation & Culture",
    w_restaurants          = "Weight: Restaurants & Hotels",
    w_transport            = "Weight: Transport",
    ip_gr_yoy              = "Industrial Production Growth Rate (Year-on-Year, %)",
    ur                     = "Unemployment Rate (%)",
    wi_imp_gdp             = "Import Exposure Weight (Imports/GDP, %)",
    dln_neer               = "Log Change in Nominal Effective Exchange Rate",
    gscpi                  = "Global Supply Chain Pressure Index",
    bh_oil_supply_shock    = "Structural Oil Supply Shock",
    bh_oil_price_exp_shock = "Market-Based Oil Price Shocks",
    imf_nonfuel            = "IMF Primary Commodity Price Index excl. fuel (PALLFNF, 2016=100)",
    emu                    = "EMU membership dummy (1=EMU20, 0=Non-EMU EU)",
    country_group          = "Country group: Core / Periphery / Small open economies / Non-EMU",
    country_group_n        = "Country group numeric: 1=Core, 2=Periphery, 3=Small open econ, 4=Non-EMU"
  )

# ── Save ──────────────────────────────────────────────────────────────────────
write_dta(panel, file.path(out_dir, "panel.dta"))
cat("Panel saved to data/clean/panel.dta\n")
cat(sprintf("  Countries: %d | Observations: %d\n",
            n_distinct(panel$code), nrow(panel)))
