
# 1_data_collection.R — Data Collection (All 27 EU Countries)
# Downloads and saves clean data files for all EU27 countries using single
# Eurostat API calls. Sources: Eurostat (HICP prices/weights, unemployment,
# IP, imports/GDP), ECB API (EMU20 EER-40 NEER and EU7 bilateral EUR rates),
# IMF (non-fuel primary commodity price index).
# Run from the code/ directory.

suppressPackageStartupMessages({
  library(eurostat)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(jsonlite)
})

out_dir <- "../data/clean"
dir.create(out_dir, showWarnings = FALSE)

# ── Country lists ──────────────────────────────────────────────────────────────
geo_emu20 <- c("AT","BE","CY","DE","EE","ES","FI","FR","EL","HR",
               "IE","IT","LT","LU","LV","MT","NL","PT","SI","SK")
geo_eu7   <- c("BG","CZ","DK","HU","PL","RO","SE")
geo_all   <- c(geo_emu20, geo_eu7)

# ── COICOP map ─────────────────────────────────────────────────────────────────
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

# ── Helpers ────────────────────────────────────────────────────────────────────
get_geo_dic <- function() {
  get_eurostat_dic("geo", lang = "en") %>%
    transmute(geo = code_name, country = full_name)
}

sort_yyyy_mm <- function(x) {
  x[order(as.integer(substr(x, 1, 4)), as.integer(substr(x, 6, 7)))]
}

wide_yoy_from_levels <- function(df, id_cols = c("country", "var")) {
  date_cols <- sort_yyyy_mm(names(df)[grepl("^\\d{4}-\\d{2}$", names(df))])
  m <- as.matrix(df[, date_cols])
  storage.mode(m) <- "double"
  yoy <- matrix(NA_real_, nrow = nrow(m), ncol = ncol(m), dimnames = dimnames(m))
  if (ncol(m) > 12)
    yoy[, 13:ncol(m)] <- 100 * (m[, 13:ncol(m)] / m[, 1:(ncol(m) - 12)] - 1)
  bind_cols(df %>% select(all_of(id_cols)), as.data.frame(yoy))
}

pick_or_first <- function(x, pref) if (pref %in% x) pref else x[1]
pick_any <- function(x, prefs) {
  hit <- prefs[prefs %in% x]
  if (length(hit) > 0) hit[1] else x[1]
}

# ── 1. HICP prices ─────────────────────────────────────────────────────────────
geo_dic <- get_geo_dic()
clean_eurostat_cache()

raw_prices <- get_eurostat(
  id = "prc_hicp_midx",
  time_format = "raw",
  filters = list(freq = "M", unit = "I15", coicop = coicop_map$coicop, geo = geo_all),
  type = "code"
)

pi_original <- raw_prices %>%
  left_join(geo_dic, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  left_join(coicop_map, by = "coicop") %>%
  select(country, var, time, values) %>%
  mutate(time = as.character(time)) %>%
  pivot_wider(names_from = time, values_from = values) %>%
  arrange(country, var)

pi_final <- wide_yoy_from_levels(pi_original)

write.csv(pi_final, file.path(out_dir, "pi_final.csv"), row.names = FALSE)
write.xlsx(pi_final, file.path(out_dir, "pi_final.xlsx"))

# ── 2. HICP weights ────────────────────────────────────────────────────────────
raw_weights <- get_eurostat(
  id = "prc_hicp_inw",
  time_format = "raw",
  filters = list(freq = "A", coicop = coicop_map$coicop, geo = geo_all),
  type = "code", cache = TRUE, update_cache = TRUE
)

month_cols <- sort_yyyy_mm(names(pi_original)[grepl("^\\d{4}-\\d{2}$", names(pi_original))])

w_long <- raw_weights %>%
  left_join(geo_dic, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  left_join(coicop_map, by = "coicop") %>%
  transmute(country, var,
            year   = as.integer(as.character(time)),
            weight = as.numeric(values) / 1000)

weights_final <- expand_grid(
  country = unique(pi_original$country),
  var     = unique(pi_original$var),
  month   = month_cols
) %>%
  mutate(year = as.integer(substr(month, 1, 4))) %>%
  left_join(w_long, by = c("country", "var", "year")) %>%
  select(country, var, month, weight) %>%
  pivot_wider(names_from = month, values_from = weight) %>%
  arrange(country, var) %>%
  rename(w = var) %>%
  mutate(w = sub("^pi_", "w_", w)) %>%
  filter(w != "w_headline")

write.csv(weights_final, file.path(out_dir, "pi_weights_final.csv"), row.names = FALSE)
write.xlsx(weights_final, file.path(out_dir, "pi_weights_final.xlsx"))

# ── 3. Unemployment ────────────────────────────────────────────────────────────
u_raw <- get_eurostat(
  "une_rt_m", time_format = "raw",
  filters = list(geo = geo_all),
  type = "code", cache = TRUE, update_cache = TRUE
)

sex_c  <- pick_or_first(unique(u_raw$sex),  "T")
age_c  <- pick_or_first(unique(u_raw$age),  "TOTAL")
sadj_c <- pick_or_first(unique(u_raw$s_adj), "SA")
unit_c <- pick_or_first(unique(u_raw$unit), "PC_ACT")

u <- u_raw %>%
  filter(sex == sex_c, age == age_c, s_adj == sadj_c, unit == unit_c) %>%
  left_join(geo_dic, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(country, month = as.character(time), ur = as.numeric(values))

write.csv(u, file.path(out_dir, "u.csv"), row.names = FALSE)

# ── 4. Industrial production ───────────────────────────────────────────────────
clean_eurostat_cache()
ip_raw <- get_eurostat(
  "sts_inpr_m", time_format = "raw",
  filters = list(freq = "M", indic_bt = "PRD", nace_r2 = "B-D",
                 s_adj = "SCA", unit = "I15", geo = geo_all),
  type = "code", cache = TRUE, update_cache = TRUE
)

sadj_ip <- pick_any(unique(ip_raw$s_adj),   c("SCA", "SA"))
unit_ip  <- pick_any(unique(ip_raw$unit),    c("I15", "I21"))
nace_ip  <- pick_any(unique(ip_raw$nace_r2), c("B-D", "B-E", "C"))

ip <- ip_raw %>%
  filter(s_adj == sadj_ip, unit == unit_ip, nace_r2 == nace_ip) %>%
  left_join(geo_dic, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(country, month = as.character(time), ip_index = as.numeric(values)) %>%
  group_by(country) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(
    ip_gr_mom = 100 * (ip_index / lag(ip_index, 1)  - 1),
    ip_gr_yoy = 100 * (ip_index / lag(ip_index, 12) - 1)
  ) %>%
  ungroup()

write.csv(ip, file.path(out_dir, "ip.csv"), row.names = FALSE)

# ── 5. Imports / GDP (annual → monthly) ───────────────────────────────────────
ex_raw <- get_eurostat(
  "nama_10_gdp", time_format = "raw",
  filters = list(freq = "A", na_item = "P7", unit = "PC_GDP", geo = geo_all),
  type = "code", cache = TRUE, update_cache = TRUE
) %>%
  left_join(geo_dic, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(country, year = as.integer(as.character(time)), imp_gdp = as.numeric(values))

ex_monthly <- expand_grid(
  country = unique(pi_original$country),
  month   = month_cols
) %>%
  mutate(year = as.integer(substr(month, 1, 4))) %>%
  left_join(ex_raw, by = c("country", "year"))

write.csv(ex_monthly, file.path(out_dir, "ex_monthly.csv"), row.names = FALSE)

# ── 6. EMU20 NEER (EER-40, common series) ─────────────────────────────────────
ecb_url <- paste0("https://data-api.ecb.europa.eu/service/data/EXR/",
                  "M.E03.EUR.EN00.A?detail=dataonly&format=csvdata")
neer_csv <- read_csv(ecb_url, show_col_types = FALSE)

neer_m <- neer_csv %>%
  transmute(month = substr(TIME_PERIOD, 1, 7), neer = as.numeric(OBS_VALUE)) %>%
  arrange(month) %>%
  mutate(dln_neer = 100 * (log(neer) - log(lag(neer))))

write.csv(neer_m, file.path(out_dir, "neer_m.csv"), row.names = FALSE)

# ── 7. EU7 NEER (bilateral EUR spot rates) ─────────────────────────────────────
currency_map <- tribble(
  ~geo, ~currency,
  "BG", "BGN",
  "CZ", "CZK",
  "DK", "DKK",
  "HU", "HUF",
  "PL", "PLN",
  "RO", "RON",
  "SE", "SEK"
)

neer_eu7_list <- list()
for (i in seq_len(nrow(currency_map))) {
  geo  <- currency_map$geo[i]
  curr <- currency_map$currency[i]
  url  <- paste0("https://data-api.ecb.europa.eu/service/data/EXR/",
                 "M.", curr, ".EUR.SP00.E?detail=dataonly&format=csvdata")
  tryCatch({
    raw <- read_csv(url, show_col_types = FALSE)
    neer_eu7_list[[geo]] <- raw %>%
      transmute(month    = substr(TIME_PERIOD, 1, 7),
                neer     = as.numeric(OBS_VALUE),
                geo      = geo,
                currency = curr) %>%
      arrange(month) %>%
      mutate(dln_neer = 100 * (log(neer) - log(lag(neer))))
  }, error = function(e) {
    warning(paste("NEER download failed for", geo, ":", conditionMessage(e)))
  })
}

neer_eu7 <- bind_rows(neer_eu7_list)
write.csv(neer_eu7, file.path(out_dir, "neer_eu7.csv"), row.names = FALSE)

# ── 8. IMF non-fuel commodity index ───────────────────────────────────────────
imf_url <- paste0("http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/",
                  "PCPS/M.W00.PALLFNF?startPeriod=1990-01&endPeriod=2025-12")
tryCatch({
  imf_raw <- fromJSON(imf_url)
  obs <- imf_raw$CompactData$DataSet$Series$Obs[[1]]
  imf_commodity <- tibble(
    month       = obs$`@TIME_PERIOD`,
    imf_nonfuel = as.numeric(obs$`@OBS_VALUE`)
  ) %>% arrange(month)
  write.csv(imf_commodity, file.path(out_dir, "imf_commodity.csv"), row.names = FALSE)
}, error = function(e) {
  warning(paste("IMF commodity download failed:", conditionMessage(e)))
})

cat("Data collection complete. Files saved to data/clean/\n")
