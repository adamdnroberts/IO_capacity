library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)

datapath = "~/EU_capacity/data/"

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

df_exclude_UKBE <- dfpg %>%
  filter(country != "Belgium" & country != "United Kingdom")

df_exclude_UKBE_noA <- df_exclude_UKBE %>% filter(aeoy == 0)
df_exclude_UKBE_noEOY <- df_exclude_UKBE %>% filter(py != 0)

run_models <- function(data) {
  list(
    rev = feols(
      log(err_sq) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1)
    ),
    exp = feols(
      log(err_sq) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(exp == 1)
    )
  )
}

# Run models
models <- run_models(df_exclude_UKBE)
models_noA <- run_models(df_exclude_UKBE_noA)
models_noEOY <- run_models(df_exclude_UKBE_noEOY)

etable(
  models$rev,
  models$exp,
  tex = T,
  digits = 3,
  digits.stats = 3
)

etable(
  models_noA$rev,
  models_noA$exp,
  tex = T,
  digits = 3,
  digits.stats = 3
)

etable(
  models_noEOY$rev,
  models_noEOY$exp,
  tex = T,
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

make_panel <- function(header, s_rev, s_exp, multicolumn = TRUE) {
  hdr <- if (multicolumn) {
    paste0("   \\multicolumn{3}{l}{\\emph{", header, "}}\\\\")
  } else {
    paste0("   \\emph{", header, "}\\\\")
  }
  c(
    hdr,
    paste0("   National Expertise & ", s_rev$coef_str, " & ", s_exp$coef_str, "\\\\"),
    paste0("    & ", s_rev$se_str, " & ", s_exp$se_str, "\\\\"),
    "   \\midrule",
    paste0("   Observations & ", s_rev$n_obs, " & ", s_exp$n_obs, "\\\\"),
    paste0("   R$^2$ & ", s_rev$r2, " & ", s_exp$r2, "\\\\"),
    paste0("   Within R$^2$ & ", s_rev$wr2, " & ", s_exp$wr2, "\\\\")
  )
}

s <- list(
  A_rev = get_model_stats(models$rev),
  A_exp = get_model_stats(models$exp),
  B_rev = get_model_stats(models_noA$rev),
  B_exp = get_model_stats(models_noA$exp),
  C_rev = get_model_stats(models_noEOY$rev),
  C_exp = get_model_stats(models_noEOY$exp)
)

lines <- c(
  "\\begin{table}[]",
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcc}",
  "   \\tabularnewline \\midrule \\midrule",
  "   Dependent Variable: & \\multicolumn{2}{c}{Log Forecast Error Squared}\\\\",
  "   Indicator Category: & Revenue & Expenditure\\\\",
  "   Model: & (1) & (2)\\\\",
  "   \\midrule",
  make_panel("Panel A: All Forecasts", s$A_rev, s$A_exp, multicolumn = FALSE),
  "   \\midrule",
  make_panel("Panel B: Excluding EOY Forecasts made in November", s$B_rev, s$B_exp),
  "   \\midrule",
  make_panel("Panel C: Excluding EOY Forecasts", s$C_rev, s$C_exp),
  "   \\midrule \\midrule",
  "\\end{tabular}",
  "",
  "   Clustered (country) standard-errors in parentheses. All models include country, time, forecast period, indicator fixed effects and controls for population, GDP, and GDP per capita. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1",
  "\\par\\endgroup",
  "    \\caption{Main Model, excluding UK and Belgium}",
  "    \\label{tab:exclude_UKBE}",
  "\\end{table}"
)

cat(paste(lines, collapse = "\n"), "\n")
