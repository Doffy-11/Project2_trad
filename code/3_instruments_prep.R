
# 3_instruments_prep.R — Instrument Preparation
# Constructs AR innovations for two alternative instruments:
#   1. GSCPI shocks:         AR(p) residuals on the GSCPI level series
#   2. IMF commodity shocks: AR(p) residuals on monthly log-differences of the
#                             IMF non-fuel primary commodity price index
# AR order selected by AIC over p = 1..12.
# Innovations saved to data/clean/ for use in 6_LP_rob_instruments.do.
# Run from code/ directory or project root.

suppressMessages({
  library(openxlsx)
  library(dplyr)
  library(readr)
})

out_dir <- "../output/tables"
data_dir <- "../data/clean"

# ── 1. Load GSCPI ──────────────────────────────────────────────────────────────
gscpi_raw <- read.xlsx(file.path(data_dir, "gscpi.xlsx"))
# Columns: year, month, gscpi

cat("GSCPI: N =", nrow(gscpi_raw), " | from",
    paste(gscpi_raw$year[1], gscpi_raw$month[1], sep="-"), "to",
    paste(tail(gscpi_raw$year,1), tail(gscpi_raw$month,1), sep="-"), "\n")

# ── 2. Load IMF non-fuel commodity index ───────────────────────────────────────
imf_raw <- read_csv(file.path(data_dir, "imf_commodity.csv"), show_col_types = FALSE)
# Columns: month (string "YYYY-MM"), imf_nonfuel

imf_clean <- imf_raw %>%
  mutate(
    year  = as.integer(substr(month, 1, 4)),
    month = as.integer(substr(month, 6, 7)),
    dln_imf = c(NA, diff(log(imf_nonfuel)))   # monthly log-change
  ) %>%
  select(year, month, dln_imf)

cat("IMF non-fuel (dln): N =", sum(!is.na(imf_clean$dln_imf)),
    " | from",
    paste(imf_clean$year[2], imf_clean$month[2], sep="-"), "to",
    paste(tail(imf_clean$year,1), tail(imf_clean$month,1), sep="-"), "\n")

# ── 3. AR order selection (AIC, p = 1..12) ─────────────────────────────────────
select_ar_order <- function(x, pmax = 12) {
  x <- na.omit(x)
  aic_vec <- sapply(1:pmax, function(p) {
    fit <- ar(x, order.max = p, method = "ols", aic = FALSE, demean = TRUE)
    n   <- length(fit$resid[!is.na(fit$resid)])
    k   <- p + 1   # p AR coefs + intercept
    aic <- n * log(mean(fit$resid^2, na.rm = TRUE)) + 2 * k
    aic
  })
  best_p <- which.min(aic_vec)
  cat("  AIC by order:", round(aic_vec, 2), "\n")
  cat("  Best p (AIC):", best_p, "\n")
  best_p
}

cat("\n--- AR order for GSCPI ---\n")
p_gscpi <- select_ar_order(gscpi_raw$gscpi)

cat("\n--- AR order for dln_imf ---\n")
p_imf <- select_ar_order(imf_clean$dln_imf)

# ── 4. Fit AR and extract residuals ────────────────────────────────────────────
fit_ar_resid <- function(x, p, labels_df) {
  n <- length(x)
  y <- x[(p+1):n]
  X <- do.call(cbind, lapply(1:p, function(k) x[(p+1-k):(n-k)]))
  X <- cbind(1, X)
  ok <- complete.cases(y, X)
  y_ok <- y[ok]; X_ok <- X[ok, ]
  coef <- solve(t(X_ok) %*% X_ok) %*% t(X_ok) %*% y_ok
  resid_ok <- y_ok - X_ok %*% coef

  shock_full <- rep(NA_real_, n)
  shock_full[(p+1):n][ok] <- as.numeric(resid_ok)

  labels_df$shock <- shock_full
  labels_df
}

gscpi_shocks <- fit_ar_resid(gscpi_raw$gscpi, p_gscpi,
                              data.frame(year = gscpi_raw$year, month = gscpi_raw$month))

imf_shocks <- fit_ar_resid(imf_clean$dln_imf, p_imf,
                            data.frame(year = imf_clean$year, month = imf_clean$month))

# ── 5. Diagnostics ─────────────────────────────────────────────────────────────
cat("\nGSCPI shocks: N non-NA =", sum(!is.na(gscpi_shocks$shock)),
    "| SD =", round(sd(gscpi_shocks$shock, na.rm=TRUE), 4), "\n")
cat("IMF shocks:   N non-NA =", sum(!is.na(imf_shocks$shock)),
    "| SD =", round(sd(imf_shocks$shock, na.rm=TRUE), 4), "\n")

cat("\nFirst 5 GSCPI shocks:\n")
print(head(na.omit(gscpi_shocks), 5))

cat("\nFirst 5 IMF commodity shocks:\n")
print(head(na.omit(imf_shocks), 5))

# ── 6. Save CSVs ───────────────────────────────────────────────────────────────
gscpi_out <- gscpi_shocks %>% rename(gscpi_shock = shock)
imf_out   <- imf_shocks   %>% rename(imf_shock   = shock)

write_csv(gscpi_out, file.path(data_dir, "gscpi_shocks.csv"), na = "")
write_csv(imf_out,   file.path(data_dir, "imf_shocks.csv"),   na = "")

cat("\nSaved: data/clean/gscpi_shocks.csv\n")
cat("Saved: data/clean/imf_shocks.csv\n")
cat("\nAR orders used:  GSCPI =", p_gscpi, " | IMF non-fuel =", p_imf, "\n")
