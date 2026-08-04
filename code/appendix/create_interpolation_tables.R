library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)

datapath = "data/"

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

dfpg_noA   <- subset(dfpg, aeoy == 0)
dfpg_noEOY <- subset(dfpg, py != 0)

# --- Linear interpolation ---

rev_linear  <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, rev == 1)
)
exp_linear  <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, exp == 1)
)
all_linear  <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, rev == 1 | exp == 1)
)
rev_linear2 <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noA, rev == 1)
)
exp_linear2 <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noA, exp == 1)
)
all_linear2 <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noA, rev == 1 | exp == 1)
)
rev_linear3 <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noEOY, rev == 1)
)
exp_linear3 <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noEOY, exp == 1)
)
all_linear3 <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noEOY, rev == 1 | exp == 1)
)

# --- Spline interpolation ---

rev_spline  <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, rev == 1)
)
exp_spline  <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, exp == 1)
)
all_spline  <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, rev == 1 | exp == 1)
)
rev_spline2 <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noA, rev == 1)
)
exp_spline2 <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noA, exp == 1)
)
all_spline2 <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noA, rev == 1 | exp == 1)
)
rev_spline3 <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noEOY, rev == 1)
)
exp_spline3 <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noEOY, exp == 1)
)
all_spline3 <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noEOY, rev == 1 | exp == 1)
)

# --- Table helpers ---

get_model_stats <- function(model, var) {
  b <- coef(model)[[var]]
  s <- se(model)[[var]]
  p <- pvalue(model)[[var]]
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

make_panel <- function(header, s_rev, s_exp, s_all, method_label, multicolumn = TRUE) {
  hdr <- if (multicolumn) {
    paste0("   \\multicolumn{4}{l}{\\emph{", header, "}}\\\\")
  } else {
    paste0("   \\emph{", header, "}\\\\")
  }
  c(
    hdr,
    paste0("   National Expertise: & ", s_rev$coef_str, " & ", s_exp$coef_str, " & ", s_all$coef_str, "\\\\"),
    paste0("   ", method_label, " & ", s_rev$se_str, " & ", s_exp$se_str, " & ", s_all$se_str, "\\\\"),
    "   \\midrule",
    paste0("   Observations & ", s_rev$n_obs, " & ", s_exp$n_obs, " & ", s_all$n_obs, "\\\\"),
    paste0("   R$^2$ & ", s_rev$r2, " & ", s_exp$r2, " & ", s_all$r2, "\\\\"),
    paste0("   Within R$^2$ & ", s_rev$wr2, " & ", s_exp$wr2, " & ", s_all$wr2, "\\\\")
  )
}

make_interp_table <- function(
  rev1, exp1, all1, rev2, exp2, all2, rev3, exp3, all3,
  var, method_label, caption, label
) {
  s <- list(
    A_rev = get_model_stats(rev1, var),
    A_exp = get_model_stats(exp1, var),
    A_all = get_model_stats(all1, var),
    B_rev = get_model_stats(rev2, var),
    B_exp = get_model_stats(exp2, var),
    B_all = get_model_stats(all2, var),
    C_rev = get_model_stats(rev3, var),
    C_exp = get_model_stats(exp3, var),
    C_all = get_model_stats(all3, var)
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
    make_panel("Panel A: All Forecasts",
               s$A_rev, s$A_exp, s$A_all, method_label, multicolumn = FALSE),
    "   \\midrule",
    make_panel("Panel B: Excluding EOY Forecasts made in November",
               s$B_rev, s$B_exp, s$B_all, method_label),
    "   \\midrule",
    make_panel("Panel C: Excluding EOY Forecasts",
               s$C_rev, s$C_exp, s$C_all, method_label),
    "   \\midrule \\midrule",
    "\\end{tabular}",
    "",
    "   Clustered (country) standard-errors in parentheses. All models include country, time, forecast period, indicator fixed effects and controls for population, GDP, and GDP per capita. Signif. Codes: ***: 0.01, **: 0.05, *: 0.1",
    "\\par\\endgroup",
    paste0("    \\caption{", caption, "}"),
    paste0("    \\label{", label, "}"),
    "\\end{table}"
  )

  paste(lines, collapse = "\n")
}

# --- Output tables ---

tablepath <- "overleaf/tables/"
dir.create(tablepath, recursive = TRUE, showWarnings = FALSE)

linear_table <- make_interp_table(
  rev_linear, exp_linear, all_linear,
  rev_linear2, exp_linear2, all_linear2,
  rev_linear3, exp_linear3, all_linear3,
  var          = "ecfin_int",
  method_label = "Linear Interpolation",
  caption      = "Main Model with Linear Interpolation",
  label        = "tab:linear_int"
)

spline_table <- make_interp_table(
  rev_spline, exp_spline, all_spline,
  rev_spline2, exp_spline2, all_spline2,
  rev_spline3, exp_spline3, all_spline3,
  var          = "ecfin_spline",
  method_label = "Spline Interpolation",
  caption      = "Main Model with Spline Interpolation",
  label        = "tab:spline_int"
)

cat(linear_table)
cat("\n\n")
cat(spline_table)

writeLines(linear_table, paste0(tablepath, "linear_int_table.tex"))
writeLines(spline_table, paste0(tablepath, "spline_int_table.tex"))
