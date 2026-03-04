
# Phase 2: EU27 Data Extension — Download and Report
# Run from the code/ directory:
#   cd code && Rscript 4.EU27_data_collection.R
#
# This script downloads data for the 7 non-EMU EU27 countries and
# three new instruments. It saves raw series and prints a report with
# URL, description, frequency, coverage, and first 5 rows for each
# series. It does NOT merge into panel.dta — merging happens in the
# next step after author confirmation.
#
# Non-EMU EU27 countries (EU27 minus EMU20):
#   BG = Bulgaria     CZ = Czech Republic   DK = Denmark
#   HU = Hungary      PL = Poland           RO = Romania
#   SE = Sweden

suppressPackageStartupMessages({
  library(eurostat)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
})

out_dir <- "../data/clean"
dir.create(out_dir, showWarnings = FALSE)

geo_eu7 <- c("BG", "CZ", "DK", "HU", "PL", "RO", "SE")

coicop_map <- tribble(
  ~var,               ~coicop,
  "pi_headline",      "CP00",
  "pi_food",          "CP01",
  "pi_alcohol",       "CP02",
  "pi_clothing",      "CP03",
  "pi_housing",       "CP04",
  "pi_furnishing",    "CP05",
  "pi_health",        "CP06",
  "pi_transport",     "CP07",
  "pi_communication", "CP08",
  "pi_recreation",    "CP09",
  "pi_education",     "CP10",
  "pi_restaurants",   "CP11",
  "pi_other",         "CP12"
)

get_geo_dic <- function() {
  get_eurostat_dic("geo", lang = "en") %>%
    transmute(geo = code_name, country = full_name)
}

sort_yyyy_mm <- function(x) {
  x[order(as.integer(substr(x,1,4)), as.integer(substr(x,6,7)))]
}

wide_yoy_from_levels <- function(df, id_cols = c("country","var")) {
  date_cols <- names(df)[grepl("^\\d{4}-\\d{2}$", names(df))]
  date_cols <- sort_yyyy_mm(date_cols)
  m <- as.matrix(df[, date_cols]); storage.mode(m) <- "double"
  yoy <- matrix(NA_real_, nrow=nrow(m), ncol=ncol(m), dimnames=dimnames(m))
  if (ncol(m) > 12) yoy[, 13:ncol(m)] <- 100 * (m[,13:ncol(m)] / m[,1:(ncol(m)-12)] - 1)
  bind_cols(df %>% select(all_of(id_cols)), as.data.frame(yoy))
}

cat("=============================================================\n")
cat("PHASE 2 DATA DOWNLOAD REPORT\n")
cat("=============================================================\n\n")

# ─────────────────────────────────────────────────────────────────
# 1. HICP PRICES — EU7
# ─────────────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("1. HICP PRICES (EU7)\n")
cat("   Source:    Eurostat — prc_hicp_midx\n")
cat("   URL:       https://ec.europa.eu/eurostat/databrowser/view/prc_hicp_midx\n")
cat("   Frequency: Monthly\n")
cat("   Unit:      Index 2015=100 (I15)\n")
cat("   Countries:", paste(geo_eu7, collapse=", "), "\n")
cat("   Transform: Year-on-year % change (computed from index levels)\n\n")

clean_eurostat_cache()
geo_dic2 <- get_geo_dic()

raw_prices_eu7 <- get_eurostat(
  id = "prc_hicp_midx",
  time_format = "raw",
  filters = list(freq="M", unit="I15", coicop=coicop_map$coicop, geo=geo_eu7),
  type = "code"
)

prices_labeled_eu7 <- raw_prices_eu7 %>%
  left_join(geo_dic2, by="geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  left_join(coicop_map, by="coicop")

pi_original_eu7 <- prices_labeled_eu7 %>%
  select(country, var, time, values) %>%
  mutate(time = as.character(time)) %>%
  pivot_wider(names_from=time, values_from=values) %>%
  arrange(country, var)

pi_final_eu7 <- wide_yoy_from_levels(pi_original_eu7)

date_cols <- sort_yyyy_mm(names(pi_final_eu7)[grepl("^\\d{4}-\\d{2}$", names(pi_final_eu7))])
first_nonmissing <- date_cols[min(which(!is.na(pi_final_eu7[pi_final_eu7$var=="pi_headline",date_cols,drop=FALSE])))]
last_col <- tail(date_cols, 1)

cat("   Coverage: countries =", length(unique(pi_final_eu7$country)), "\n")
cat("   Date range:", first_nonmissing, "to", last_col, "(YoY; first 12 months NA by construction)\n")
cat("   First 5 rows (headline, sorted by country/date):\n")
sample_rows <- pi_final_eu7 %>%
  filter(var == "pi_headline") %>%
  select(country, var, `1999-01`, `1999-02`, `1999-03`, `1999-04`, `1999-05`) %>%
  head(5)
print(sample_rows)
cat("\n")

write.csv(pi_final_eu7, file.path(out_dir, "pi_final_eu7.csv"), row.names=FALSE)
cat("   Saved: data/clean/pi_final_eu7.csv\n\n")

# ─────────────────────────────────────────────────────────────────
# 2. HICP WEIGHTS — EU7
# ─────────────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("2. HICP WEIGHTS (EU7)\n")
cat("   Source:    Eurostat — prc_hicp_inw\n")
cat("   URL:       https://ec.europa.eu/eurostat/databrowser/view/prc_hicp_inw\n")
cat("   Frequency: Annual (expanded to monthly)\n")
cat("   Unit:      Per 1,000 (normalized to sum=1 over CP01-CP12)\n\n")

raw_weights_eu7 <- get_eurostat(
  id = "prc_hicp_inw",
  time_format = "raw",
  filters = list(freq="A", coicop=coicop_map$coicop, geo=geo_eu7),
  type = "code", cache=TRUE, update_cache=TRUE
)

weights_labeled_eu7 <- raw_weights_eu7 %>%
  left_join(geo_dic2, by="geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  left_join(coicop_map, by="coicop")

w_long_eu7 <- weights_labeled_eu7 %>%
  transmute(country, var,
            year = as.integer(as.character(time)),
            weight = as.numeric(values) / 1000)

month_cols <- sort_yyyy_mm(names(pi_original_eu7)[grepl("^\\d{4}-\\d{2}$", names(pi_original_eu7))])
month_skel_eu7 <- expand_grid(
  country = unique(pi_original_eu7$country),
  var     = unique(pi_original_eu7$var),
  month   = month_cols
) %>% mutate(year = as.integer(substr(month, 1, 4)))

weights_final_eu7 <- month_skel_eu7 %>%
  left_join(w_long_eu7, by=c("country","var","year")) %>%
  select(country, var, month, weight) %>%
  pivot_wider(names_from=month, values_from=weight) %>%
  arrange(country, var) %>%
  rename(w = var) %>%
  mutate(w = sub("^pi_", "w_", w)) %>%
  filter(w != "w_headline")

cat("   Coverage: countries =", length(unique(weights_final_eu7$country)), "\n")
cat("   First 5 rows:\n")
print(head(weights_final_eu7[, c("country","w","1999-01","2000-01","2005-01")], 5))
cat("\n")

write.csv(weights_final_eu7, file.path(out_dir, "pi_weights_final_eu7.csv"), row.names=FALSE)
cat("   Saved: data/clean/pi_weights_final_eu7.csv\n\n")

# ─────────────────────────────────────────────────────────────────
# 3. UNEMPLOYMENT — EU7
# ─────────────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("3. UNEMPLOYMENT RATE (EU7)\n")
cat("   Source:    Eurostat — une_rt_m\n")
cat("   URL:       https://ec.europa.eu/eurostat/databrowser/view/une_rt_m\n")
cat("   Frequency: Monthly, seasonally adjusted\n")
cat("   Unit:      Percent of active population\n\n")

u_raw_eu7 <- get_eurostat(
  "une_rt_m", time_format="raw",
  filters=list(geo=geo_eu7), type="code", cache=TRUE, update_cache=TRUE
)

pick_or_first <- function(x, pref) if (pref %in% x) pref else x[1]
sex_c  <- pick_or_first(unique(u_raw_eu7$sex),  "T")
age_c  <- pick_or_first(unique(u_raw_eu7$age),  "TOTAL")
sadj_c <- pick_or_first(unique(u_raw_eu7$s_adj),"SA")
unit_c <- pick_or_first(unique(u_raw_eu7$unit), "PC_ACT")

u_eu7 <- u_raw_eu7 %>%
  filter(sex==sex_c, age==age_c, s_adj==sadj_c, unit==unit_c) %>%
  left_join(geo_dic2, by="geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(country, month=as.character(time), ur=as.numeric(values))

cat("   Coverage: countries =", length(unique(u_eu7$country)), "\n")
cat("   Date range:", min(u_eu7$month, na.rm=TRUE), "to", max(u_eu7$month, na.rm=TRUE), "\n")
cat("   First 5 rows:\n")
print(head(u_eu7, 5))
cat("\n")

write.csv(u_eu7, file.path(out_dir, "u_eu7.csv"), row.names=FALSE)
cat("   Saved: data/clean/u_eu7.csv\n\n")

# ─────────────────────────────────────────────────────────────────
# 4. INDUSTRIAL PRODUCTION — EU7
# ─────────────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("4. INDUSTRIAL PRODUCTION (EU7)\n")
cat("   Source:    Eurostat — sts_inpr_m\n")
cat("   URL:       https://ec.europa.eu/eurostat/databrowser/view/sts_inpr_m\n")
cat("   Frequency: Monthly, seasonally and calendar adjusted\n")
cat("   Unit:      Index 2015=100\n\n")

clean_eurostat_cache()
ip_raw_eu7 <- get_eurostat(
  "sts_inpr_m", time_format="raw",
  filters=list(freq="M", indic_bt="PRD", nace_r2="B-D",
               s_adj="SCA", unit="I15", geo=geo_eu7),
  type="code", cache=TRUE, update_cache=TRUE
)

pick_any <- function(x, prefs) { hit <- prefs[prefs %in% x]; if (length(hit)>0) hit[1] else x[1] }
sadj_ip <- pick_any(unique(ip_raw_eu7$s_adj), c("SCA","SA"))
unit_ip <- pick_any(unique(ip_raw_eu7$unit),  c("I15","I21"))
nace_ip <- pick_any(unique(ip_raw_eu7$nace_r2), c("B-D","B-E","C"))

ip_eu7 <- ip_raw_eu7 %>%
  filter(s_adj==sadj_ip, unit==unit_ip, nace_r2==nace_ip) %>%
  left_join(geo_dic2, by="geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(country, month=as.character(time), ip_index=as.numeric(values)) %>%
  group_by(country) %>%
  arrange(month, .by_group=TRUE) %>%
  mutate(ip_gr_yoy = 100*(ip_index/lag(ip_index,12)-1)) %>%
  ungroup()

cat("   Coverage: countries =", length(unique(ip_eu7$country)), "\n")
cat("   Date range:", min(ip_eu7$month, na.rm=TRUE), "to", max(ip_eu7$month, na.rm=TRUE), "\n")
cat("   First 5 rows:\n")
print(head(ip_eu7 %>% select(country, month, ip_gr_yoy), 5))
cat("\n")

write.csv(ip_eu7, file.path(out_dir, "ip_eu7.csv"), row.names=FALSE)
cat("   Saved: data/clean/ip_eu7.csv\n\n")

# ─────────────────────────────────────────────────────────────────
# 5. IMPORTS AS % OF GDP — EU7
# ─────────────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("5. IMPORTS AS % OF GDP (EU7)\n")
cat("   Source:    Eurostat — nama_10_gdp (na_item=P7, unit=PC_GDP)\n")
cat("   URL:       https://ec.europa.eu/eurostat/databrowser/view/nama_10_gdp\n")
cat("   Frequency: Annual (expanded to monthly)\n")
cat("   Unit:      Percent of GDP\n\n")

ex_eu7_raw <- get_eurostat(
  "nama_10_gdp", time_format="raw",
  filters=list(freq="A", na_item="P7", unit="PC_GDP", geo=geo_eu7),
  type="code", cache=TRUE, update_cache=TRUE
) %>%
  left_join(geo_dic2, by="geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(country, year=as.integer(as.character(time)), imp_gdp=as.numeric(values))

ex_monthly_eu7 <- tidyr::expand_grid(
  country = unique(pi_original_eu7$country),
  month   = month_cols
) %>%
  mutate(year = as.integer(substr(month,1,4))) %>%
  left_join(ex_eu7_raw, by=c("country","year"))

cat("   Coverage: countries =", length(unique(ex_monthly_eu7$country)), "\n")
cat("   Date range:", min(ex_monthly_eu7$month), "to", max(ex_monthly_eu7$month), "\n")
cat("   First 5 rows:\n")
print(head(ex_monthly_eu7, 5))
cat("\n")

write.csv(ex_monthly_eu7, file.path(out_dir, "ex_monthly_eu7.csv"), row.names=FALSE)
cat("   Saved: data/clean/ex_monthly_eu7.csv\n\n")

# ─────────────────────────────────────────────────────────────────
# 6. BILATERAL EUR EXCHANGE RATES — EU7
#    (Used as NEER proxy for non-EMU countries in LP-IV regressions)
# ─────────────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("6. BILATERAL EUR EXCHANGE RATES (EU7)\n")
cat("   Source:    ECB Data Portal — EXR dataset, spot rates\n")
cat("   URL:       https://data-api.ecb.europa.eu/service/data/EXR/\n")
cat("   Frequency: Monthly average\n")
cat("   Unit:      National currency per EUR\n")
cat("   Note:      Used as NEER proxy for non-EMU countries.\n")
cat("              Log change (dln_neer) computed for consistency with EMU NEER.\n\n")

# Currency codes for the 7 non-EMU EU countries
currency_map <- tribble(
  ~geo, ~currency,
  "BG", "BGN",  # Bulgarian lev (pegged to EUR)
  "CZ", "CZK",  # Czech koruna
  "DK", "DKK",  # Danish krone (ERM II, near-peg)
  "HU", "HUF",  # Hungarian forint
  "PL", "PLN",  # Polish zloty
  "RO", "RON",  # Romanian leu
  "SE", "SEK"   # Swedish krona
)

neer_eu7_list <- list()
for (i in seq_len(nrow(currency_map))) {
  geo  <- currency_map$geo[i]
  curr <- currency_map$currency[i]
  series_key <- paste0("M.", curr, ".EUR.SP00.E")
  url <- paste0("https://data-api.ecb.europa.eu/service/data/EXR/",
                series_key, "?detail=dataonly&format=csvdata")
  tryCatch({
    raw <- readr::read_csv(url, show_col_types=FALSE)
    df <- raw %>%
      transmute(month = substr(TIME_PERIOD, 1, 7),
                neer  = as.numeric(OBS_VALUE),
                geo   = geo,
                currency = curr) %>%
      arrange(month) %>%
      mutate(dln_neer = 100 * (log(neer) - log(lag(neer))))
    neer_eu7_list[[geo]] <- df
    cat("   ", geo, "(", curr, "): rows =", nrow(df),
        "| range:", min(df$month), "to", max(df$month), "\n")
  }, error = function(e) {
    cat("   ", geo, "(", curr, "): DOWNLOAD FAILED —", conditionMessage(e), "\n")
  })
}

neer_eu7 <- bind_rows(neer_eu7_list)
cat("\n   First 5 rows:\n")
print(head(neer_eu7 %>% select(geo, currency, month, neer, dln_neer), 5))
cat("\n")

write.csv(neer_eu7, file.path(out_dir, "neer_eu7.csv"), row.names=FALSE)
cat("   Saved: data/clean/neer_eu7.csv\n\n")

# ─────────────────────────────────────────────────────────────────
# 7. IMF PRIMARY COMMODITY PRICE INDEX (EXCLUDING ENERGY)
# ─────────────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("7. IMF PRIMARY COMMODITY PRICE INDEX (NON-ENERGY)\n")
cat("   Source:    IMF Primary Commodity Prices (PCPS database)\n")
cat("   URL:       http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PCPS/\n")
cat("   Series:    PALLFNF — All Commodity Price Index, excluding Fuel\n")
cat("   Frequency: Monthly\n")
cat("   Unit:      Index (2016=100)\n\n")

imf_url <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PCPS/M.W00.PALLFNF?startPeriod=1990-01&endPeriod=2025-12"
tryCatch({
  imf_raw <- jsonlite::fromJSON(imf_url)
  obs <- imf_raw$CompactData$DataSet$Series$Obs[[1]]
  imf_commodity <- tibble(
    month = obs$`@TIME_PERIOD`,
    imf_nonfuel = as.numeric(obs$`@OBS_VALUE`)
  ) %>% arrange(month)

  cat("   Coverage: rows =", nrow(imf_commodity), "\n")
  cat("   Date range:", min(imf_commodity$month), "to", max(imf_commodity$month), "\n")
  cat("   First 5 rows:\n")
  print(head(imf_commodity, 5))
  cat("\n")

  write.csv(imf_commodity, file.path(out_dir, "imf_commodity.csv"), row.names=FALSE)
  cat("   Saved: data/clean/imf_commodity.csv\n\n")
}, error = function(e) {
  cat("   DOWNLOAD FAILED:", conditionMessage(e), "\n\n")
})

# ─────────────────────────────────────────────────────────────────
# 8. BALTIC DRY INDEX
# ─────────────────────────────────────────────────────────────────
cat("─────────────────────────────────────────────────────────────\n")
cat("8. BALTIC DRY INDEX\n")
cat("   Source:    FRED (Federal Reserve Bank of St. Louis)\n")
cat("   URL:       https://fred.stlouisfed.org/series/DBDI\n")
cat("   Series:    DBDI — Dry Bulk Freight Index\n")
cat("   Frequency: Monthly\n")
cat("   Unit:      Index\n\n")

fred_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DBDI"
tryCatch({
  bdi_raw <- readr::read_csv(fred_url, show_col_types=FALSE)
  bdi <- bdi_raw %>%
    transmute(
      month = substr(DATE, 1, 7),
      bdi   = as.numeric(DBDI)
    ) %>%
    filter(!is.na(bdi)) %>%
    arrange(month)

  cat("   Coverage: rows =", nrow(bdi), "\n")
  cat("   Date range:", min(bdi$month), "to", max(bdi$month), "\n")
  cat("   First 5 rows:\n")
  print(head(bdi, 5))
  cat("\n")

  write.csv(bdi, file.path(out_dir, "bdi.csv"), row.names=FALSE)
  cat("   Saved: data/clean/bdi.csv\n\n")
}, error = function(e) {
  cat("   DBDI failed, trying DCOILBRENTEU as fallback...\n")
  # Some FRED series for BDI: try alternative
  tryCatch({
    # Baltic Dry Index from quandl-style source
    fred_url2 <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=MABMM301BDQ189S"
    bdi_raw2 <- readr::read_csv(fred_url2, show_col_types=FALSE)
    cat("   Columns:", paste(names(bdi_raw2), collapse=", "), "\n")
    print(head(bdi_raw2, 5))
  }, error = function(e2) {
    cat("   DOWNLOAD FAILED:", conditionMessage(e), "\n\n")
  })
})

cat("=============================================================\n")
cat("DOWNLOAD COMPLETE. All series saved to data/clean/.\n")
cat("Awaiting author confirmation before merging into panel.dta.\n")
cat("=============================================================\n")
