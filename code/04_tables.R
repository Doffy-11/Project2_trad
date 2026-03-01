# Run this script from the code/ directory:
#   cd code && Rscript 04_tables.R

library(haven)
library(dplyr)

panel <- read_dta("../data/clean/panel.dta")

# ── Construct tradable and non-tradable inflation (mirrors Stata do-files) ────
panel <- panel %>%
  mutate(
    w_trad    = w_food + w_clothing + w_furnishing + w_transport + w_alcohol,
    w_nontrad = w_housing + w_health + w_education + w_restaurants +
                w_other + w_communication + w_recreation,
    pi_trad    = (pi_food * w_food + pi_clothing * w_clothing +
                  pi_furnishing * w_furnishing + pi_transport * w_transport +
                  pi_alcohol * w_alcohol) / w_trad,
    pi_nontrad = (pi_housing * w_housing + pi_health * w_health +
                  pi_education * w_education + pi_restaurants * w_restaurants +
                  pi_other * w_other + pi_communication * w_communication +
                  pi_recreation * w_recreation) / w_nontrad
  )

# ── Helper: write a booktabs LaTeX table ─────────────────────────────────────
write_booktabs <- function(df, caption, label, file, note = NULL,
                           col_align = NULL) {
  nc <- ncol(df)
  if (is.null(col_align)) col_align <- paste(c("l", rep("r", nc - 1)),
                                              collapse = "")
  header <- paste(names(df), collapse = " & ")

  rows <- apply(df, 1, function(r) paste(r, collapse = " & "))

  out <- c(
    "\\begin{table}[htbp]",
    "  \\centering",
    paste0("  \\caption{", caption, "}"),
    paste0("  \\label{", label, "}"),
    paste0("  \\begin{tabular}{", col_align, "}"),
    "    \\toprule",
    paste0("    ", header, " \\\\"),
    "    \\midrule",
    paste0("    ", rows, " \\\\")
  )
  if (!is.null(note)) {
    out <- c(out,
             "    \\midrule",
             paste0("    \\multicolumn{", nc,
                    "}{l}{\\footnotesize\\textit{Note:} ", note, "}"))
  }
  out <- c(out,
           "    \\bottomrule",
           "  \\end{tabular}",
           "\\end{table}")

  writeLines(out, file)
  cat("Written:", file, "\n")
}

# ── Table 1: Summary Statistics ───────────────────────────────────────────────
var_labels <- list(
  pi_headline            = "Headline inflation",
  pi_trad                = "Tradable inflation",
  pi_nontrad             = "Non-tradable inflation",
  bh_oil_price_exp_shock = "Market oil price shock",
  bh_oil_supply_shock    = "Oil supply shock",
  gscpi                  = "GSCPI",
  dln_neer               = "$\\Delta \\ln$ NEER",
  ur                     = "Unemployment rate (\\%)",
  ip_gr_yoy              = "IP growth, YoY (\\%)",
  wi_imp_gdp             = "Import exposure (\\% of GDP)"
)

tab1_rows <- lapply(names(var_labels), function(v) {
  x <- panel[[v]]
  x <- x[!is.na(x)]
  data.frame(
    Variable  = var_labels[[v]],
    N         = formatC(length(x), format = "d", big.mark = ","),
    Mean      = formatC(mean(x), digits = 2, format = "f"),
    SD        = formatC(sd(x),   digits = 2, format = "f"),
    Min       = formatC(min(x),  digits = 2, format = "f"),
    Max       = formatC(max(x),  digits = 2, format = "f"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
})
tab1 <- do.call(rbind, tab1_rows)
names(tab1) <- c("Variable", "$N$", "Mean", "Std.\\ Dev.", "Min", "Max")

write_booktabs(
  df      = tab1,
  caption = "Summary Statistics",
  label   = "tab:summary",
  file    = "../output/tables/tab1_summary_stats.tex",
  note    = paste0("Inflation rates are year-over-year percent changes. ",
                   "GSCPI is standardized with mean zero and unit variance. ",
                   "NEER change and oil shocks are in log points. ",
                   "Sample: 19 euro area countries, January 1998 -- December 2025."),
  col_align = "lrrrrr"
)

# ── Table 2: Data Sources ─────────────────────────────────────────────────────
tab2 <- data.frame(
  Variable = c(
    "\\texttt{pi\\_headline}",
    "\\texttt{pi\\_trad}",
    "\\texttt{pi\\_nontrad}",
    "\\texttt{w\\_food}, \\texttt{w\\_clothing}, \\ldots",
    "\\texttt{bh\\_oil\\_price\\_exp\\_shock}",
    "\\texttt{bh\\_oil\\_supply\\_shock}",
    "\\texttt{gscpi}",
    "\\texttt{dln\\_neer}",
    "\\texttt{ur}",
    "\\texttt{ip\\_gr\\_yoy}",
    "\\texttt{wi\\_imp\\_gdp}"
  ),
  Description = c(
    "Headline HICP inflation (YoY, \\%)",
    "Tradable inflation, Eq.~(\\ref{eq:basket}), $\\mathcal{B}=\\mathcal{T}$",
    "Non-tradable inflation, Eq.~(\\ref{eq:basket}), $\\mathcal{B}=\\mathcal{N}$",
    "Annual ECOICOP expenditure weights (normalized)",
    "Market-based oil price expectation shock",
    "Structural oil supply shock",
    "Global Supply Chain Pressure Index",
    "Log monthly change in nominal effective exchange rate",
    "Unemployment rate, seasonally adjusted (\\% of active population)",
    "Manufacturing IP growth, year-over-year (\\%)",
    "Imports of goods and services (\\% of GDP)"
  ),
  Source = c(
    "Eurostat \\texttt{prc\\_hicp\\_midx}",
    "Authors' construction",
    "Authors' construction",
    "Eurostat \\texttt{prc\\_hicp\\_inw}",
    "\\citet{baumeister2019}",
    "\\citet{baumeister2019}",
    "\\citet{benigno2022}",
    "ECB Data Portal API",
    "Eurostat \\texttt{une\\_rt\\_m}",
    "Eurostat \\texttt{sts\\_inpr\\_m}",
    "Eurostat \\texttt{nama\\_10\\_gdp}"
  ),
  stringsAsFactors = FALSE
)

write_booktabs(
  df      = tab2,
  caption = "Data Sources",
  label   = "tab:sources",
  file    = "../output/tables/tab2_data_sources.tex",
  note    = paste0("ECOICOP tradable basket $\\mathcal{T}$: CP01, CP02, CP03, CP05, CP07. ",
                   "Non-tradable basket $\\mathcal{N}$: CP04, CP06, CP08, CP09, CP10, CP11, CP12."),
  col_align = "llp{5cm}"
)

# ── Table 3: Country Coverage ─────────────────────────────────────────────────
coverage <- panel %>%
  filter(!is.na(pi_headline)) %>%
  group_by(country, code) %>%
  summarise(
    start_yr  = min(year),
    start_mo  = min(month[year == min(year)]),
    end_yr    = max(year),
    end_mo    = max(month[year == max(year)]),
    obs       = n(),
    .groups   = "drop"
  ) %>%
  arrange(country) %>%
  mutate(
    Start = sprintf("%04dm%02d", start_yr, start_mo),
    End   = sprintf("%04dm%02d", end_yr,   end_mo),
    N     = formatC(obs, format = "d", big.mark = ",")
  ) %>%
  select(Country = country, ISO = code, Start, End, N)

write_booktabs(
  df      = coverage,
  caption = "Country Coverage",
  label   = "tab:coverage",
  file    = "../output/tables/tab3_country_coverage.tex",
  note    = paste0("$N$ counts non-missing observations of headline HICP inflation. ",
                   "Sample: 19 euro area member states, January 1998 -- December 2025."),
  col_align = "llccc"
)

cat("\nAll tables written to output/tables/\n")
