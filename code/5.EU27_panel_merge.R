
# Phase 2: EU27 Panel Merge
# Run from the code/ directory:
#   cd code && Rscript 5.EU27_panel_merge.R
#
# Takes the existing EMU20 panel.dta and adds:
#   - 7 non-EMU EU countries (BG, CZ, DK, HU, PL, RO, SE)
#   - NEER for EU7 = bilateral EUR exchange rate (national currency per EUR,
#     log change). Documented clearly: EMU20 countries use the common
#     Euro NEER-40; EU7 countries use their bilateral EUR spot rate as
#     the closest available proxy for exchange rate pressure.
#   - emu:            1 = EMU20, 0 = EU7
#   - country_group:  "Core" | "Periphery" | "Small open economies" | "Non-EMU"
#   - country_group_n: 1=Core, 2=Periphery, 3=Small open economies, 4=Non-EMU
#   - BDI:            not included — series unavailable on public platforms
#                     (FRED DBDI discontinued; Baltic Exchange requires subscription)
#
# Overwrites data/clean/panel.dta.

suppressPackageStartupMessages({
  library(tidyverse)
  library(haven)
  library(labelled)
  library(countrycode)
  library(zoo)
  library(openxlsx)
})

out_dir <- "../data/clean"

# ─────────────────────────────────────────────────────────────────
# Country classification tables
# ─────────────────────────────────────────────────────────────────

# ISO3C codes for each group
core_codes   <- c("DEU","FRA","NLD","AUT","FIN","BEL")
periph_codes <- c("ITA","ESP","PRT","GRC")
soe_codes    <- c("IRL","LUX","EST","LVA","LTU","SVK","SVN","MLT","CYP","HRV")
nonemu_codes <- c("BGR","CZE","DNK","HUN","POL","ROU","SWE")
emu20_codes  <- c(core_codes, periph_codes, soe_codes)
eu27_codes   <- c(emu20_codes, nonemu_codes)

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

# ─────────────────────────────────────────────────────────────────
# Helper: clean date column names from read.csv (X1999.01 -> 1999-01)
# ─────────────────────────────────────────────────────────────────
fix_date_cols <- function(df) {
  names(df) <- gsub("^X(\\d{4})\\.(\\d{2})$", "\\1-\\2", names(df))
  df
}

# ─────────────────────────────────────────────────────────────────
# 1. Build EU7 HICP prices panel
# ─────────────────────────────────────────────────────────────────
pi_eu7_wide <- read.csv(file.path(out_dir, "pi_final_eu7.csv"),
                        check.names = FALSE, stringsAsFactors = FALSE)

pi_eu7 <- pi_eu7_wide %>%
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

# ─────────────────────────────────────────────────────────────────
# 2. EU7 HICP weights
# ─────────────────────────────────────────────────────────────────
w_eu7_wide <- read.csv(file.path(out_dir, "pi_weights_final_eu7.csv"),
                       check.names = FALSE, stringsAsFactors = FALSE)

w_eu7 <- w_eu7_wide %>%
  pivot_longer(matches("^\\d{4}-\\d{2}$"), names_to = "time", values_to = "value") %>%
  mutate(
    year  = as.integer(substr(time, 1, 4)),
    month = as.integer(substr(time, 6, 7)),
    code  = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998) %>%
  select(year, month, code, w, value) %>%
  pivot_wider(names_from = w, values_from = value)

# ─────────────────────────────────────────────────────────────────
# 3. EU7 unemployment
# ─────────────────────────────────────────────────────────────────
u_eu7 <- read.csv(file.path(out_dir, "u_eu7.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month = as.integer(substr(month, 6, 7)),
    code  = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998) %>%
  select(code, month, year, ur)

# ─────────────────────────────────────────────────────────────────
# 4. EU7 industrial production
# ─────────────────────────────────────────────────────────────────
ip_eu7 <- read.csv(file.path(out_dir, "ip_eu7.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month = as.integer(substr(month, 6, 7)),
    code  = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998) %>%
  select(code, month, year, ip_gr_yoy)

# ─────────────────────────────────────────────────────────────────
# 5. EU7 import exposure (imports/GDP)
# ─────────────────────────────────────────────────────────────────
ex_eu7 <- read.csv(file.path(out_dir, "ex_monthly_eu7.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month = as.integer(substr(month, 6, 7)),
    code  = countrycode(country, origin = "country.name", destination = "iso3c"),
    wi_imp_gdp = imp_gdp
  ) %>%
  filter(year >= 1998) %>%
  select(code, month, year, wi_imp_gdp)

# ─────────────────────────────────────────────────────────────────
# 6. EU7 bilateral EUR rates (used as NEER proxy)
#    dln_neer = 100 * log change in national currency per EUR
#    Sign convention: same as Euro NEER (positive = appreciation of EUR
#    relative to national currency = depreciation of national currency)
# ─────────────────────────────────────────────────────────────────
neer_eu7 <- read.csv(file.path(out_dir, "neer_eu7.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month_int = as.integer(substr(month, 6, 7)),
    code  = case_when(
      geo == "BG" ~ "BGR",
      geo == "CZ" ~ "CZE",
      geo == "DK" ~ "DNK",
      geo == "HU" ~ "HUN",
      geo == "PL" ~ "POL",
      geo == "RO" ~ "ROU",
      geo == "SE" ~ "SWE"
    )
  ) %>%
  filter(year >= 1998) %>%
  select(code, month = month_int, year, dln_neer)

# ─────────────────────────────────────────────────────────────────
# 7. Global series (shared by all 27 countries)
# ─────────────────────────────────────────────────────────────────
oil_shock <- read.csv(file.path(out_dir, "oil_shock.csv"), stringsAsFactors = FALSE)
gscpi     <- read.xlsx(file.path(out_dir, "gscpi.xlsx"))
imf_comm  <- read.csv(file.path(out_dir, "imf_commodity.csv"), stringsAsFactors = FALSE) %>%
  mutate(year  = as.integer(substr(month, 1, 4)),
         month = as.integer(substr(month, 6, 7)))

# ─────────────────────────────────────────────────────────────────
# 8. Assemble EU7 panel
# ─────────────────────────────────────────────────────────────────
panel_eu7 <- pi_eu7 %>%
  left_join(w_eu7,   by = c("year", "month", "code")) %>%
  left_join(ip_eu7,  by = c("year", "month", "code")) %>%
  left_join(u_eu7,   by = c("year", "month", "code")) %>%
  left_join(ex_eu7,  by = c("year", "month", "code")) %>%
  left_join(neer_eu7, by = c("year", "month", "code")) %>%
  left_join(gscpi,   by = c("year", "month")) %>%
  left_join(oil_shock, by = c("year", "month"))

# ─────────────────────────────────────────────────────────────────
# 9. Load EMU20 panel and add common Euro NEER label
#    (already has dln_neer from neer_m.csv — the EER-40)
# ─────────────────────────────────────────────────────────────────
panel_emu20 <- read_dta(file.path(out_dir, "panel.dta"))

# ─────────────────────────────────────────────────────────────────
# 10. Align columns between EMU20 and EU7 panels, then stack
# ─────────────────────────────────────────────────────────────────

# Ensure EU7 has same column set; fill missing cols with NA
emu20_cols <- names(panel_emu20)
eu7_cols   <- names(panel_eu7)

for (col in setdiff(emu20_cols, eu7_cols)) {
  panel_eu7[[col]] <- NA
}
panel_eu7 <- panel_eu7[, emu20_cols]

# Stack
panel_eu27 <- bind_rows(panel_emu20, panel_eu7)

# ─────────────────────────────────────────────────────────────────
# 11. Add classification variables
# ─────────────────────────────────────────────────────────────────
panel_eu27 <- panel_eu27 %>%
  mutate(
    emu             = if_else(code %in% emu20_codes, 1L, 0L),
    country_group   = assign_group(code),
    country_group_n = assign_group_n(code)
  )

# ─────────────────────────────────────────────────────────────────
# 12. Add IMF non-fuel commodity index (common to all countries)
# ─────────────────────────────────────────────────────────────────
panel_eu27 <- panel_eu27 %>%
  left_join(imf_comm, by = c("year", "month"))

# ─────────────────────────────────────────────────────────────────
# 13. Sort
# ─────────────────────────────────────────────────────────────────
panel_eu27 <- panel_eu27 %>%
  arrange(code, year, month)

# ─────────────────────────────────────────────────────────────────
# 14. Variable labels
# ─────────────────────────────────────────────────────────────────
panel_eu27 <- panel_eu27 %>%
  set_variable_labels(
    emu             = "EMU membership dummy (1=EMU20, 0=Non-EMU EU)",
    country_group   = "Country group: Core / Periphery / Small open economies / Non-EMU",
    country_group_n = "Country group numeric: 1=Core, 2=Periphery, 3=Small open econ, 4=Non-EMU",
    imf_nonfuel     = "IMF Primary Commodity Price Index excl. fuel (PALLFNF, 2016=100, FRED)"
  )

# ─────────────────────────────────────────────────────────────────
# 15. Report panel dimensions
# ─────────────────────────────────────────────────────────────────
cat("\n=============================================================\n")
cat("EU27 PANEL — DIMENSION REPORT\n")
cat("=============================================================\n\n")

cat(sprintf("Total observations : %d\n", nrow(panel_eu27)))
cat(sprintf("Countries          : %d\n", n_distinct(panel_eu27$code)))
ymin <- min(panel_eu27$year); ymax <- max(panel_eu27$year)
mmin <- min(panel_eu27$month[panel_eu27$year == ymin])
mmax <- max(panel_eu27$month[panel_eu27$year == ymax])
cat(sprintf("Time coverage      : %d-m%02d to %d-m%02d\n\n", ymin, mmin, ymax, mmax))

cat("─── By EMU status ───────────────────────────────────────────\n")
emu_tab <- panel_eu27 %>%
  group_by(emu) %>%
  summarise(countries = n_distinct(code), obs = n(), .groups = "drop") %>%
  mutate(label = if_else(emu == 1, "EMU20", "Non-EMU EU7"))
print(emu_tab[, c("label","countries","obs")])

cat("\n─── By country group ────────────────────────────────────────\n")
grp_tab <- panel_eu27 %>%
  group_by(country_group_n, country_group) %>%
  summarise(countries = n_distinct(code), obs = n(), .groups = "drop") %>%
  arrange(country_group_n)
print(grp_tab[, c("country_group_n","country_group","countries","obs")])

cat("\n─── Countries by group ──────────────────────────────────────\n")
for (g in c("Core","Periphery","Small open economies","Non-EMU")) {
  codes <- sort(unique(panel_eu27$code[panel_eu27$country_group == g]))
  cat(sprintf("  %-22s: %s\n", g, paste(codes, collapse=", ")))
}

cat("\n─── NEER variable notes ─────────────────────────────────────\n")
cat("  EMU20 countries : dln_neer = log change in Euro EER-40 (ECB, common)\n")
cat("  EU7 countries   : dln_neer = log change in bilateral EUR spot rate\n")
cat("                    (national currency per EUR; source: ECB EXR)\n")
cat("  BDI             : not included — FRED series DBDI discontinued;\n")
cat("                    Baltic Exchange requires paid subscription.\n")
cat("                    IMF non-fuel commodity index (PALLFNF) covers\n")
cat("                    the clean non-energy instrument role.\n")

cat("\n=============================================================\n")
cat("Panel NOT yet saved. Awaiting author confirmation.\n")
cat("=============================================================\n")

write_dta(panel_eu27, file.path(out_dir, "panel.dta"))
cat("Saved: data/clean/panel.dta\n")
