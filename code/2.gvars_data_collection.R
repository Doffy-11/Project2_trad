# Run this script from the code/ directory:
#   cd code && Rscript 2.gvars_data_collection.R

library(readr)
library(dplyr)

out_dir <- "../data/clean"
dir.create(out_dir, showWarnings = FALSE)

# ECB Data Portal API (CSV), series: Nominal EER-40/Euro, Monthly (Average)
# Series key: EXR.M.E03.EUR.EN00.A
ecb_url <- "https://data-api.ecb.europa.eu/service/data/EXR/M.E03.EUR.EN00.A?detail=dataonly&format=csvdata"

neer_csv <- readr::read_csv(ecb_url, show_col_types = FALSE)

neer_m <- neer_csv %>%
  transmute(
    month = substr(TIME_PERIOD, 1, 7),     # "YYYY-MM"
    neer  = as.numeric(OBS_VALUE)
  ) %>%
  arrange(month) %>%
  mutate(dln_neer = 100 * (log(neer) - log(lag(neer))))

neer_m
# write.csv(neer_m, "data_out/neer_eer40_monthly.csv", row.names = FALSE)
write.csv(neer_m,    file.path(out_dir, "neer_m.csv"),    row.names = FALSE)
