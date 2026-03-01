# Run this script from the code/ directory:
#   cd code && Rscript 3.panel_construction.R

#panelsync

#This code assembles the panel data

rm(list=ls())

library(openxlsx)
library(tidyverse)
library(tidyr)
library(lubridate)
library(countrycode)
library(dplyr)
library(zoo)
library(haven)
library(labelled)


# uploading data ---------------------------------------------------------
date = read.csv("../data/clean/date.csv")



pi_final = read.xlsx("../data/clean/pi_final.xlsx") #pi_items
pi_weights_final = read.xlsx("../data/clean/pi_weights_final.xlsx") #W_items
oil_shock =read.csv("../data/clean/oil_shock.csv")
neer_m = read.csv("../data/clean/neer_m.csv")
u = read.csv("../data/clean/u.csv")
exposure_weights = read.csv("../data/clean/ex_monthly.csv")
ip = read.csv("../data/clean/ip.csv")
gscpi = read.xlsx("../data/clean/gscpi.xlsx")


#Transposing data in panel format
pi_final = pi_final %>% 
  pivot_longer(!(country:var), names_to = "time", values_to = "value") %>% 
  mutate(year = as.numeric(substring(time,1,4)),
         month = as.numeric(substring(time,6,7)),
         quarter = ceiling(month/3),
         code = countrycode(country, origin = "country.name", destination = "iso3c")) %>% 
  select(year, month, quarter, code, country, var, value ) %>% 
  pivot_wider(names_from = var, values_from = value)

pi_weights_final = pi_weights_final %>% 
  pivot_longer(!(country:w), names_to = "time", values_to = "value") %>% 
  mutate(year = as.numeric(substring(time,1,4)),
         month = as.numeric(substring(time,6,7)),
         code = countrycode(country, origin = "country.name", destination = "iso3c") ) %>% 
  select(year, month, code,  w, value ) %>% 
  pivot_wider(names_from = w, values_from = value)

#IP panel
ip <- ip %>%
  mutate(
    year = as.integer(substr(month, 1, 4)),  # Extract year from month
    month = as.integer(substr(month, 6, 7)),              # Extract month digits only
    code = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998, month >= "01") %>%    # Filter from 1998-01 onwards
  select( code, month, year, ip_gr_yoy)

#ur panel 
u <- u %>%
  mutate(
    year = as.integer(substr(month, 1, 4)),  # Extract year
    month = as.integer(substr(month, 6, 7)),              # Extract month digits only
    code = countrycode(country, origin = "country.name", destination = "iso3c")
  ) %>%
  filter(year >= 1998, month >= "01") %>%    # Filter from 1998-01 onwards
  select(code, month, year, ur)

#cross sectional exposure weights measured as imp_gdp
exposure_weights <- exposure_weights %>%
  mutate(
    month = as.integer(substr(month, 6, 7)),  # Extract last 2 digits (positions 6-7)
    code = countrycode(country, origin = "country.name", destination = "iso3c"),
    wi_imp_gdp = imp_gdp
  ) %>%
  select( code, month, year, wi_imp_gdp)

#Global variable time series 
neer_m <- neer_m %>%
  mutate(
    year = as.integer(substr(month, 1, 4)),  # Extract year
    month =as.integer(substr(month, 6, 7))               # Extract month digits only
  ) %>%
  filter(year >= 1998, month >= "01") %>%    # Filter from 1998-01 onwards
  select(month, year, dln_neer)



#Merging datasets
panel <- left_join(date,pi_final, by=c("year", "month"))
panel <- left_join(panel,pi_weights_final, by=c("year", "month", "code"))
panel <- left_join(panel, ip, by=c("year", "month", "code")) 
panel <- left_join(panel, u, by=c("year", "month", "code")) 
panel <- left_join(panel, exposure_weights,  by=c("year", "month", "code")) #
panel <- left_join(panel, neer_m,  by=c("year", "month")) #
panel <- left_join(panel, gscpi,  by=c("year", "month")) 
panel <- left_join(panel, oil_shock,  by=c("year", "month")) 

colnames(panel)
colnames(neer_m)



#Saving everything
# Save as Stata .dta file





# Define output directory
out_dir <- "../data/clean"

# Add variable labels
panel <- panel %>%
  set_variable_labels(
    year = "Year",
    month = "Month (1-12)",
    quarter = "Quarter (1-4)",
    code = "ISO3C Country Code",
    country = "Country Name",
    pi_alcohol = "Price Index: Alcoholic Beverages & Tobacco",
    pi_clothing = "Price Index: Clothing & Footwear",
    pi_communication = "Price Index: Communication",
    pi_education = "Price Index: Education",
    pi_food = "Price Index: Food & Non-Alcoholic Beverages",
    pi_furnishing = "Price Index: Furnishings & Household Equipment",
    pi_headline = "Price Index: Headline HICP",
    pi_health = "Price Index: Health",
    pi_housing = "Price Index: Housing, Water, Electricity, Gas",
    pi_other = "Price Index: Miscellaneous Goods & Services",
    pi_recreation = "Price Index: Recreation & Culture",
    pi_restaurants = "Price Index: Restaurants & Hotels",
    pi_transport = "Price Index: Transport",
    w_alcohol = "Weight: Alcoholic Beverages & Tobacco",
    w_clothing = "Weight: Clothing & Footwear",
    w_communication = "Weight: Communication",
    w_education = "Weight: Education",
    w_food = "Weight: Food & Non-Alcoholic Beverages",
    w_furnishing = "Weight: Furnishings & Household Equipment",
    w_health = "Weight: Health",
    w_housing = "Weight: Housing, Water, Electricity, Gas",
    w_other = "Weight: Miscellaneous Goods & Services",
    w_recreation = "Weight: Recreation & Culture",
    w_restaurants = "Weight: Restaurants & Hotels",
    w_transport = "Weight: Transport",
    ip_gr_yoy = "Industrial Production Growth Rate (Year-on-Year, %)",
    ur = "Unemployment Rate (%)",
    wi_imp_gdp = "Import Exposure Weight (Imports/GDP, %)",
    dln_neer = "Log Change in Nominal Effective Exchange Rate",
    gscpi = "Global Supply Chain Pressure Index",
    bh_oil_supply_shock = "Structural Oil Supply Shock",
    bh_oil_price_exp_shock = "Market-Based Oil Price Shocks"
  )

# Save as Stata .dta file (version 14)
write_dta(panel, file.path(out_dir, "panel.dta"))

# Confirmation message
cat("Panel data saved to:", file.path(out_dir, "panel.dta"), "\n")
