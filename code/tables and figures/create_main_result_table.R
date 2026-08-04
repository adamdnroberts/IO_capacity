library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)

datapath = "~/EU_Capacity/data/"

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

dfpg_noA <- dfpg %>% filter(aeoy == 0)
dfpg_noEOY <- dfpg %>% filter(py != 0)

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
    ),
    all = feols(
      log(err_sq) ~
        ecfin +
          log(pop_int) +
          log(gdp) +
          gdppc |
          country + ysp + title + py,
      data = data %>% filter(rev == 1 | exp == 1)
    ),
    all_epu = feols(
      log(err_sq) ~
        ecfin +
          epu +
          log(pop_int) +
          log(gdp) +
          gdppc |
          country + ysp + title + py,
      data = data %>% filter(rev == 1 | exp == 1)
    )
  )
}

# Run models
models <- run_models(dfpg)
models_noA <- run_models(dfpg_noA)
models_noEOY <- run_models(dfpg_noEOY)

etable(
  models$rev,
  models$exp,
  models$all,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

etable(
  models_noA$rev,
  models_noA$exp,
  models_noA$all,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

etable(
  models_noEOY$rev,
  models_noEOY$exp,
  models_noEOY$all,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

##COMBINED PANEL TABLE##
fmt_coef <- function(s, var) {
  coef_val <- coef(s)[var]
  se_val <- se(s)[var]
  pval <- pvalue(s)[var]
  stars <- ifelse(
    pval < 0.01,
    "***",
    ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", ""))
  )
  coef_str <- if (nchar(stars) > 0) {
    paste0(sprintf("%.3f", coef_val), "$^{", stars, "}$")
  } else {
    sprintf("%.3f", coef_val)
  }
  list(
    coef_str = coef_str,
    se_str = paste0("(", sprintf("%.3f", se_val), ")")
  )
}

get_model_stats <- function(model, include_epu = FALSE) {
  s <- summary(model, vcov = ~country)
  ne <- fmt_coef(s, "ecfin")
  r2_stats <- r2(model)
  out <- list(
    coef_str = ne$coef_str,
    se_str = ne$se_str,
    n_obs = formatC(nobs(model), format = "d", big.mark = ","),
    r2 = sprintf("%.3f", as.numeric(r2_stats["r2"])),
    wr2 = sprintf("%.3f", as.numeric(r2_stats["wr2"]))
  )
  if (include_epu) {
    epu <- fmt_coef(s, "epu")
    out$epu_coef_str <- epu$coef_str
    out$epu_se_str <- epu$se_str
  }
  out
}

make_panel <- function(label, s_rev, s_exp, s_all, s_epu, multicolumn = TRUE) {
  header <- if (multicolumn) {
    paste0("\\multicolumn{5}{l}{\\emph{", label, "}}\\\\")
  } else {
    paste0("\\emph{", label, "}\\\\")
  }
  c(
    header,
    paste0(
      "National Expertise & ",
      s_rev$coef_str,
      " & ",
      s_exp$coef_str,
      " & ",
      s_all$coef_str,
      " & ",
      s_epu$coef_str,
      "\\\\"
    ),
    paste0(
      "& ",
      s_rev$se_str,
      " & ",
      s_exp$se_str,
      " & ",
      s_all$se_str,
      " & ",
      s_epu$se_str,
      "\\\\"
    ),
    paste0("EPU & & & & ", s_epu$epu_coef_str, "\\\\"),
    paste0("& & & & ", s_epu$epu_se_str, "\\\\"),
    "\\midrule",
    paste0(
      "Observations   & ",
      s_rev$n_obs,
      " & ",
      s_exp$n_obs,
      " & ",
      s_all$n_obs,
      " & ",
      s_epu$n_obs,
      "\\\\"
    ),
    paste0(
      "   R$^2$          & ",
      s_rev$r2,
      " & ",
      s_exp$r2,
      " & ",
      s_all$r2,
      " & ",
      s_epu$r2,
      "\\\\"
    ),
    paste0(
      "   Within R$^2$   & ",
      s_rev$wr2,
      " & ",
      s_exp$wr2,
      " & ",
      s_all$wr2,
      " & ",
      s_epu$wr2,
      "\\\\"
    )
  )
}

s <- list(
  A_rev = get_model_stats(models$rev),
  A_exp = get_model_stats(models$exp),
  A_all = get_model_stats(models$all),
  A_epu = get_model_stats(models$all_epu, include_epu = TRUE),
  B_rev = get_model_stats(models_noA$rev),
  B_exp = get_model_stats(models_noA$exp),
  B_all = get_model_stats(models_noA$all),
  B_epu = get_model_stats(models_noA$all_epu, include_epu = TRUE),
  C_rev = get_model_stats(models_noEOY$rev),
  C_exp = get_model_stats(models_noEOY$exp),
  C_all = get_model_stats(models_noEOY$all),
  C_epu = get_model_stats(models_noEOY$all_epu, include_epu = TRUE)
)

tex_lines <- c(
  "\\begin{table}[]",
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "\\tabularnewline",
  "\\midrule \\midrule",
  "Dependent Variable: & \\multicolumn{4}{c}{Log Error Squared}\\\\",
  "Forecasts Category: & Revenue & Expenditure & All & All$^{\\dagger}$\\\\",
  "Model: & (1) & (2) & (3) & (4)\\\\",
  "\\midrule",
  make_panel(
    "Panel A: All forecasts",
    s$A_rev,
    s$A_exp,
    s$A_all,
    s$A_epu,
    multicolumn = FALSE
  ),
  "\\midrule",
  make_panel(
    "Panel B: Excluding EOY Forecasts made in November",
    s$B_rev,
    s$B_exp,
    s$B_all,
    s$B_epu
  ),
  "\\midrule",
  make_panel(
    "Panel C: Excluding EOY Forecasts",
    s$C_rev,
    s$C_exp,
    s$C_all,
    s$C_epu
  ),
  "\\midrule \\midrule",
  "\\multicolumn{5}{l}{Clustered (country) standard-errors in parentheses}\\\\",
  "\\multicolumn{5}{l}{Fixed effects: econ.\\ indicator, period, state, forecast year}\\\\",
  "\\multicolumn{5}{l}{Signif. Codes: ***: 0.01, **: 0.05, *: 0.1}\\\\",
  "\\multicolumn{5}{p{14cm}}{$\\dagger$ Column (4) adds EPU index \\citep{baker2016measuring} as a control; sample restricted to 14 EU member states with available EPU data.}\\\\",
  "\\end{tabular}",
  "\\par\\endgroup",
  "\\caption{Effect of National Expertise on Member State Forecast Accuracy}",
  "\\label{tab:main_model}",
  "\\end{table}"
)

tex_table <- paste(tex_lines, collapse = "\n")
cat(tex_table, "\n")

# writeLines(
#   tex_table,
#   "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/tables/main_table.tex"
# )
