library(data.table)
library(dplyr)
library(fixest)

# --- Load data ----
datapath <- "~/EU_capacity/data/"
tablepath <- "~/EU_capacity/overleaf/tables/"
load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

# Exclude titles that are components of or near-duplicates of other titles,
# to avoid double-counting and correlated errors across observations.
overlapping_titles <- c(
  "Actual social contributions received: general government ",
  "Imputed social contributions: general government ",
  "Social contributions received: general government ",
  "Interest: general government ",
  "Collective consumption expenditure ",
  "Compensation of employees: general government ",
  "Intermediate consumption: general government ",
  "Social transfers in kind supplied to households via market producers: general government ",
  "Current taxes on income and wealth (direct taxes): general government ",
  "Taxes linked to imports and production (indirect taxes): general government ",
  "Capital taxes: general government ",
  "Other current revenue: general government ",
  "Net saving: general government "
)
dfpg <- dfpg %>% filter(!title %in% overlapping_titles)

# --- Remove COVID-affected forecasts for the appendix ----
# The rule stated in the appendix is: drop any forecast made FOR 2020 or 2021,
# and any forecast made IN 2020 or 2021. Both conditions are written out here
# rather than hand-enumerated by vintage and horizon.
#
# The previous version enumerated the target-year cases individually and missed
# two of them: Spring 2018 at py = 2 (which targets 2020) and Spring 2019 at
# py = 2 (which targets 2021). Those 1,510 rows stayed in a sample described as
# excluding forecasts for the pandemic years.
COVID_YEARS <- c(2020, 2021)

dfpg_covid <- dfpg %>%
  mutate(
    target_year = floor(ysp) + py,
    vintage_year = floor(ysp)
  ) %>%
  filter(
    !target_year %in% COVID_YEARS,
    !vintage_year %in% COVID_YEARS
  )

stopifnot(
  !any((floor(dfpg_covid$ysp) + dfpg_covid$py) %in% COVID_YEARS),
  !any(floor(dfpg_covid$ysp) %in% COVID_YEARS)
)

# Subsets
dfpg_no_a_covid <- dfpg_covid %>% filter(aeoy == 0)
dfpg_no_eoy_covid <- dfpg_covid %>% filter(py != 0)

# --- Define regression formula ----
base_formula <- log(err_sq) ~
  ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py

# --- Helper to run rev/exp/all models for a given dataset ----
run_models <- function(data, label) {
  list(
    rev = feols(base_formula, data = filter(data, rev == 1)),
    exp = feols(base_formula, data = filter(data, exp == 1)),
    all = feols(base_formula, data = filter(data, rev == 1 | exp == 1)),
    label = label
  )
}

# --- Estimate models ----
models_covid <- run_models(dfpg_covid, "All (no 2020–2022)")
models_no_a_covid <- run_models(dfpg_no_a_covid, "Excl. AEoY")
models_no_eoy_covid <- run_models(dfpg_no_eoy_covid, "Excl. EoY")

# --- Tables ----
etable(
  models_covid$rev,
  models_covid$exp,
  models_covid$all,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

etable(
  models_no_a_covid$rev,
  models_no_a_covid$exp,
  models_no_a_covid$all,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

etable(
  models_no_eoy_covid$rev,
  models_no_eoy_covid$exp,
  models_no_eoy_covid$all,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

# --- LaTeX table ---

get_model_stats <- function(model) {
  b <- coef(model)[["ecfin"]]
  s <- se(model)[["ecfin"]]
  p <- pvalue(model)[["ecfin"]]
  stars <- ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**",
           ifelse(p < 0.1,  "*", "")))
  coef_str <- if (nchar(stars) > 0) {
    paste0(sprintf("%.3f", b), "$^{", stars, "}$")
  } else {
    sprintf("%.3f", b)
  }
  r2_stats <- r2(model)
  list(
    coef_str = coef_str,
    se_str   = paste0("(", sprintf("%.3f", s), ")"),
    n_obs    = formatC(nobs(model), format = "d", big.mark = ","),
    r2       = sprintf("%.3f", as.numeric(r2_stats["r2"])),
    wr2      = sprintf("%.3f", as.numeric(r2_stats["wr2"]))
  )
}

make_panel <- function(header, s_rev, s_exp, s_all, multicolumn = TRUE) {
  hdr <- if (multicolumn) {
    paste0("   \\multicolumn{4}{l}{\\emph{", header, "}}\\\\")
  } else {
    paste0("   \\emph{", header, "}\\\\")
  }
  c(
    hdr,
    paste0("   National Expertise & ", s_rev$coef_str, " & ", s_exp$coef_str, " & ", s_all$coef_str, "\\\\"),
    paste0("    & ", s_rev$se_str, " & ", s_exp$se_str, " & ", s_all$se_str, "\\\\"),
    "   \\midrule",
    paste0("   Observations & ", s_rev$n_obs, " & ", s_exp$n_obs, " & ", s_all$n_obs, "\\\\"),
    paste0("   R$^2$ & ", s_rev$r2, " & ", s_exp$r2, " & ", s_all$r2, "\\\\"),
    paste0("   Within R$^2$ & ", s_rev$wr2, " & ", s_exp$wr2, " & ", s_all$wr2, "\\\\")
  )
}

s <- list(
  A_rev = get_model_stats(models_covid$rev),
  A_exp = get_model_stats(models_covid$exp),
  A_all = get_model_stats(models_covid$all),
  B_rev = get_model_stats(models_no_a_covid$rev),
  B_exp = get_model_stats(models_no_a_covid$exp),
  B_all = get_model_stats(models_no_a_covid$all),
  C_rev = get_model_stats(models_no_eoy_covid$rev),
  C_exp = get_model_stats(models_no_eoy_covid$exp),
  C_all = get_model_stats(models_no_eoy_covid$all)
)

lines <- c(
  "\\begin{table}[]",
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lccc}",
  "   \\tabularnewline \\midrule \\midrule",
  "   Dependent Variable: & \\multicolumn{3}{c}{Log Forecast Error Squared}\\\\",
  "   Indicator Category: & Revenue & Expenditure & All\\\\",
  "   Model: & (1) & (2) & (3)\\\\",
  "   \\midrule",
  make_panel("Panel A: All Forecasts", s$A_rev, s$A_exp, s$A_all, multicolumn = FALSE),
  "   \\midrule",
  make_panel("Panel B: Excluding EOY Forecasts made in November", s$B_rev, s$B_exp, s$B_all),
  "   \\midrule",
  make_panel("Panel C: Excluding EOY Forecasts", s$C_rev, s$C_exp, s$C_all),
  "   \\midrule \\midrule",
  "\\end{tabular}",
  "",
  "   Clustered (country) standard-errors in parentheses. All models include country, time, forecast period, indicator fixed effects and controls for population, GDP, and GDP per capita. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1",
  "\\par\\endgroup",
  "    \\caption{Main Model, excluding COVID Pandemic Forecasts}",
  "    \\label{tab:exclude_covid}",
  "\\end{table}"
)

cat(paste(lines, collapse = "\n"), "\n")

dir.create(tablepath, recursive = TRUE, showWarnings = FALSE)
writeLines(
  paste(lines, collapse = "\n"),
  paste0(tablepath, "exclude_covid_table.tex")
)
