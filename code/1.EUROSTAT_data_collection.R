
# Run this script from the code/ directory:
#   cd code && Rscript 1.EUROSTAT_data_collection.R

rm(list=ls())

# ============================================================
# Eurostat HICP (EMU) -> pi_original (levels) -> pi_final (YoY %)
# Eurostat HICP item weights -> weights_final (monthly, normalized to sum to 1 over CP01-CP12)
# ============================================================

suppressPackageStartupMessages({
  library(eurostat)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

# ----------------------------
# User settings
# ----------------------------

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

# EMU countries (EA20 members as of 2023, in Eurostat geo codes)
geo_emu <- c("AT","BE","CY","DE","EE","ES","FI","FR","GR","HR","IE","IT","LT","LU","LV","MT","NL","PT","SI","SK")

# If you want the EA aggregate too, uncomment:
# geo_emu <- c("EA20", geo_emu)

unit_base <- "I15"   # HICP index base (often 2015=100)

out_dir <- "../data/clean"
dir.create(out_dir, showWarnings = FALSE)

# ----------------------------
# Helpers
# ----------------------------

get_geo_dic <- function() {
  get_eurostat_dic("geo", lang = "en") %>%
    transmute(geo = code_name, country = full_name)
}

sort_yyyy_mm <- function(x) {
  x[order(as.integer(substr(x, 1, 4)), as.integer(substr(x, 6, 7)))]
}

wide_yoy_from_levels <- function(df, id_cols = c("country", "var")) {
  date_cols <- names(df)[grepl("^\\d{4}-\\d{2}$", names(df))]
  if (length(date_cols) == 0) stop("No monthly columns found. Expected names like '1996-01'.")
  
  date_cols <- sort_yyyy_mm(date_cols)
  
  m <- as.matrix(df[, date_cols])
  storage.mode(m) <- "double"
  
  yoy <- matrix(NA_real_, nrow = nrow(m), ncol = ncol(m), dimnames = dimnames(m))
  if (ncol(m) > 12) {
    yoy[, 13:ncol(m)] <- 100 * (m[, 13:ncol(m)] / m[, 1:(ncol(m) - 12)] - 1)
  }
  
  bind_cols(df %>% select(all_of(id_cols)), as.data.frame(yoy))
}

# ----------------------------
# 1) Prices: prc_hicp_midx -> pi_original -> pi_final
# ----------------------------

clean_eurostat_cache()

raw_prices <- get_eurostat(
  id = "prc_hicp_midx",
  time_format = "raw",
  filters = list(
    freq  = "M",
    unit  = unit_base,
    coicop = coicop_map$coicop,
    geo   = geo_emu
  ),
  type = "code"
)

geo_dic2 <- get_geo_dic()

prices_labeled <- raw_prices %>%
  left_join(geo_dic2, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  left_join(coicop_map, by = "coicop")

pi_original <- prices_labeled %>%
  select(country, var, time, values) %>%
  mutate(time = as.character(time)) %>%
  pivot_wider(names_from = time, values_from = values) %>%
  arrange(country, var)

pi_final <- wide_yoy_from_levels(pi_original)

#write.csv(pi_original, file.path(out_dir, "pi_original.csv"), row.names = FALSE)
write.csv(pi_final,    file.path(out_dir, "pi_final.csv"),    row.names = FALSE)
# Writing to Excel (.xlsx)
write.xlsx(pi_final, file = file.path(out_dir, "pi_final.xlsx"))


# ----------------------------
# 2) Weights: prc_hicp_inw -> weights_final (monthly, normalized)
# ----------------------------
raw_weights <- get_eurostat(
  id = "prc_hicp_inw",
  time_format = "raw",
  filters = list(
    freq  = "A",
    coicop = coicop_map$coicop,
    geo   = geo_emu
  ),
  type = "code",
  cache = TRUE,
  update_cache = TRUE
)
weights_labeled <- raw_weights %>%
  left_join(geo_dic2, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  left_join(coicop_map, by = "coicop")
w_long <- weights_labeled %>%
  transmute(
    country,
    var,
    year   = as.integer(as.character(time)),
    weight = as.numeric(values) / 1000  # normalize from 1000 to 1
  )
month_cols <- names(pi_original)[grepl("^\\d{4}-\\d{2}$", names(pi_original))]
month_cols <- sort_yyyy_mm(month_cols)
month_skel <- expand_grid(
  country = unique(pi_original$country),
  var     = unique(pi_original$var),
  month   = month_cols
) %>%
  mutate(year = as.integer(substr(month, 1, 4)))
weights_final <- month_skel %>%
  left_join(w_long, by = c("country", "var", "year")) %>%
  select(country, var, month, weight) %>%
  pivot_wider(names_from = month, values_from = weight) %>%
  arrange(country, var) %>%
  # Change column name from 'var' to 'w' and prefix entries with 'w_'
  rename(w = var) %>%
  mutate(w = sub("^pi_", "w_", w))

# If you want weights that sum to 1 over components (CP01-CP12), drop headline weights
weights_final <- weights_final %>% filter(w != "w_headline")
write.csv(weights_final, file.path(out_dir, "pi_weights_final.csv"), row.names = FALSE)
write.xlsx(weights_final, file = file.path(out_dir, "pi_weights_final.xlsx"))


# ----------------------------
# 3) Domestic Variables
# ----------------------------

# 3A Unemployment (monthly)
u_raw <- get_eurostat(
  "une_rt_m",
  time_format = "raw",
  filters = list(
    geo = geo_emu
    # add filters after we inspect available codes below
  ),
  type = "code",
  cache = TRUE,
  update_cache = TRUE
)

# Pick common codes if they exist in your pull
pick_or_first <- function(x, preferred) if (preferred %in% x) preferred else x[1]

u_raw <- u_raw %>%
  group_by() %>% ungroup()

# Inspect unique codes once
# lapply(u_raw[c("sex","age","s_adj","unit")], unique)

sex_choice  <- pick_or_first(unique(u_raw$sex),  "T")
age_choice  <- pick_or_first(unique(u_raw$age),  "TOTAL")
sadj_choice <- pick_or_first(unique(u_raw$s_adj), "SA")
unit_choice <- pick_or_first(unique(u_raw$unit), "PC_ACT")

u <- u_raw %>%
  filter(sex == sex_choice, age == age_choice, s_adj == sadj_choice, unit == unit_choice) %>%
  left_join(geo_dic2, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(country, month = as.character(time), ur = as.numeric(values))

write.csv(u,    file.path(out_dir, "u.csv"),    row.names = FALSE)

# 3B Industrial production (monthly)
clean_eurostat_cache()
ip_raw <- get_eurostat(
  "sts_inpr_m",
  time_format = "raw",
  filters = list(
    freq     = "M",
    indic_bt = "PRD",
    nace_r2  = "B-D",
    s_adj    = "SCA",
    unit     = "I15",
    geo      = geo_emu
  ),
  type = "code",
  cache = TRUE,
  update_cache = TRUE
)



pick_any <- function(x, prefs) {
  hit <- prefs[prefs %in% x]
  if (length(hit) > 0) hit[1] else x[1]
}

sadj_ip <- pick_any(unique(ip_raw$s_adj),  c("SCA","SA"))
unit_ip <- pick_any(unique(ip_raw$unit),   c("I15","I21"))
nace_ip <- pick_any(unique(ip_raw$nace_r2), c("B-D","B-E","C"))  # total industry preferred, else manufacturing

ip <- ip_raw %>%
  filter(s_adj == sadj_ip, unit == unit_ip, nace_r2 == nace_ip) %>%
  left_join(geo_dic2, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(country, month = as.character(time), ip_index = as.numeric(values))

# Convert to growth rates (choose one)
ip <- ip %>%
  group_by(country) %>%
  arrange(month, .by_group = TRUE) %>%
  mutate(
    ip_gr_mom = 100 * (ip_index / lag(ip_index, 1) - 1),
    ip_gr_yoy = 100 * (ip_index / lag(ip_index, 12) - 1)
  ) %>%
  ungroup()

write.csv(ip,    file.path(out_dir, "ip.csv"),    row.names = FALSE)
# 3C Imports as % of GDP (exposure), then expand to monthly
# Exposure: imports (% of GDP), annual
ex_euro <- get_eurostat(
  "nama_10_gdp",
  time_format = "raw",
  filters = list(
    freq   = "A",
    na_item = "P7",       # imports of goods and services
    unit   = "PC_GDP",    # percent of GDP
    geo    = geo_emu
  ),
  type = "code",
  cache = TRUE,
  update_cache = TRUE
) %>%
  left_join(geo_dic2, by = "geo") %>%
  mutate(country = ifelse(is.na(country), geo, country)) %>%
  transmute(
    country,
    year = as.integer(as.character(time)),
    imp_gdp = as.numeric(values)
  )

# Expand to monthly using pi_original month columns
month_cols <- names(pi_original)[grepl("^\\d{4}-\\d{2}$", names(pi_original))]
month_cols <- month_cols[order(as.integer(substr(month_cols,1,4)), as.integer(substr(month_cols,6,7)))]

ex_monthly <- tidyr::expand_grid(
  country = unique(pi_original$country),
  month   = month_cols
) %>%
  mutate(year = as.integer(substr(month, 1, 4))) %>%
  left_join(ex_euro, by = c("country","year"))


write.csv(ex_monthly,    file.path(out_dir, "ex_monthly.csv"),    row.names = FALSE)

