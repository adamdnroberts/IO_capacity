library(fixest)
library(dplyr)
library(plyr)
library(data.table)

datapath = "data/"
tablepath = "overleaf/tables/"

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

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

# MAPE: absolute error as share of realized value
dfpg$mape <- abs(dfpg$err) / abs(dfpg$true)

# Subset datasets
dfpg_noA   <- dfpg %>% filter(aeoy == 0)
dfpg_noEOY <- dfpg %>% filter(py != 0)

# Shared helpers (identical to create_main_result_table.R)
get_model_stats <- function(model) {
  s <- summary(model, vcov = ~country)
  coef_val <- coef(s)["ecfin"]
  se_val   <- se(s)["ecfin"]
  pval     <- pvalue(s)["ecfin"]
  stars <- ifelse(
    pval < 0.01, "***",
    ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", ""))
  )
  coef_str <- if (nchar(stars) > 0) {
    paste0(sprintf("%.3f", coef_val), "$^{", stars, "}$")
  } else {
    sprintf("%.3f", coef_val)
  }
  r2_stats <- r2(model)
  list(
    coef_str = coef_str,
    se_str   = paste0("(", sprintf("%.3f", se_val), ")"),
    n_obs    = formatC(nobs(model), format = "d", big.mark = ","),
    r2       = sprintf("%.3f", as.numeric(r2_stats["r2"])),
    wr2      = sprintf("%.3f", as.numeric(r2_stats["wr2"]))
  )
}

make_panel <- function(label, s_rev, s_exp, s_all, multicolumn = TRUE) {
  header <- if (multicolumn) {
    paste0("\\multicolumn{4}{l}{\\emph{", label, "}}\\\\")
  } else {
    paste0("\\emph{", label, "}\\\\")
  }
  c(
    header,
    paste0(
      "National Expertise & ",
      s_rev$coef_str, " & ",
      s_exp$coef_str, " & ",
      s_all$coef_str, "\\\\"
    ),
    paste0("& ", s_rev$se_str, " & ", s_exp$se_str, " & ", s_all$se_str, "\\\\"),
    "\\midrule",
    paste0("Observations   & ", s_rev$n_obs, " & ", s_exp$n_obs, " & ", s_all$n_obs, "\\\\"),
    paste0("   R$^2$          & ", s_rev$r2,  " & ", s_exp$r2,  " & ", s_all$r2,  "\\\\"),
    paste0("   Within R$^2$   & ", s_rev$wr2, " & ", s_exp$wr2, " & ", s_all$wr2, "\\\\")
  )
}

make_combined_table <- function(s1, s2, caption, label) {
  tex_lines <- c(
    "\\begin{table}[]",
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lcccccc}",
    "\\tabularnewline",
    "\\midrule \\midrule",
    "Dependent Variable: & \\multicolumn{3}{c}{Log Absolute Error} & \\multicolumn{3}{c}{Log MAPE}\\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "Forecasts Category: & Revenue & Expenditure & All & Revenue & Expenditure & All\\\\",
    "Model: & (1) & (2) & (3) & (4) & (5) & (6)\\\\",
    "\\midrule",
    make_panel6("Panel A: All forecasts", s1$A_rev, s1$A_exp, s1$A_all, s2$A_rev, s2$A_exp, s2$A_all, multicolumn = FALSE),
    "\\midrule",
    make_panel6("Panel B: Excluding EOY Forecasts made in November", s1$B_rev, s1$B_exp, s1$B_all, s2$B_rev, s2$B_exp, s2$B_all),
    "\\midrule",
    make_panel6("Panel C: Excluding EOY Forecasts", s1$C_rev, s1$C_exp, s1$C_all, s2$C_rev, s2$C_exp, s2$C_all),
    "\\midrule \\midrule",
    "\\multicolumn{7}{l}{Clustered (state) standard-errors in parentheses}\\\\",
    "\\multicolumn{7}{l}{Fixed effects: econ.\\ indicator, period, state, forecast year}\\\\",
    "\\multicolumn{7}{l}{Signif. Codes: ***: 0.01, **: 0.05, *: 0.1}\\\\",
    "\\end{tabular}",
    "\\par\\endgroup",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\end{table}"
  )
  paste(tex_lines, collapse = "\n")
}

make_panel6 <- function(label, s1_rev, s1_exp, s1_all, s2_rev, s2_exp, s2_all, multicolumn = TRUE) {
  header <- if (multicolumn) {
    paste0("\\multicolumn{7}{l}{\\emph{", label, "}}\\\\")
  } else {
    paste0("\\emph{", label, "}\\\\")
  }
  c(
    header,
    paste0(
      "National Expertise & ",
      s1_rev$coef_str, " & ", s1_exp$coef_str, " & ", s1_all$coef_str, " & ",
      s2_rev$coef_str, " & ", s2_exp$coef_str, " & ", s2_all$coef_str, "\\\\"
    ),
    paste0(
      "& ", s1_rev$se_str, " & ", s1_exp$se_str, " & ", s1_all$se_str, " & ",
      s2_rev$se_str, " & ", s2_exp$se_str, " & ", s2_all$se_str, "\\\\"
    ),
    "\\midrule",
    paste0(
      "Observations & ",
      s1_rev$n_obs, " & ", s1_exp$n_obs, " & ", s1_all$n_obs, " & ",
      s2_rev$n_obs, " & ", s2_exp$n_obs, " & ", s2_all$n_obs, "\\\\"
    ),
    paste0(
      "   R$^2$ & ",
      s1_rev$r2, " & ", s1_exp$r2, " & ", s1_all$r2, " & ",
      s2_rev$r2, " & ", s2_exp$r2, " & ", s2_all$r2, "\\\\"
    ),
    paste0(
      "   Within R$^2$ & ",
      s1_rev$wr2, " & ", s1_exp$wr2, " & ", s1_all$wr2, " & ",
      s2_rev$wr2, " & ", s2_exp$wr2, " & ", s2_all$wr2, "\\\\"
    )
  )
}

##LOG(ABS(ERR))##
run_models_logabs <- function(data) {
  list(
    rev = feols(
      log(abs(err)) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1)
    ),
    exp = feols(
      log(abs(err)) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(exp == 1)
    ),
    all = feols(
      log(abs(err)) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1 | exp == 1)
    )
  )
}

models_logabs       <- run_models_logabs(dfpg)
models_logabs_noA   <- run_models_logabs(dfpg_noA)
models_logabs_noEOY <- run_models_logabs(dfpg_noEOY)

s_logabs <- list(
  A_rev = get_model_stats(models_logabs$rev),
  A_exp = get_model_stats(models_logabs$exp),
  A_all = get_model_stats(models_logabs$all),
  B_rev = get_model_stats(models_logabs_noA$rev),
  B_exp = get_model_stats(models_logabs_noA$exp),
  B_all = get_model_stats(models_logabs_noA$all),
  C_rev = get_model_stats(models_logabs_noEOY$rev),
  C_exp = get_model_stats(models_logabs_noEOY$exp),
  C_all = get_model_stats(models_logabs_noEOY$all)
)

##MEAN ABSOLUTE PERCENTAGE ERROR##
run_models_mape <- function(data) {
  list(
    rev = feols(
      log(mape) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1, is.finite(log(mape)))
    ),
    exp = feols(
      log(mape) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(exp == 1, is.finite(log(mape)))
    ),
    all = feols(
      log(mape) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1 | exp == 1, is.finite(log(mape)))
    )
  )
}

models_mape       <- run_models_mape(dfpg)
models_mape_noA   <- run_models_mape(dfpg_noA)
models_mape_noEOY <- run_models_mape(dfpg_noEOY)

s_mape <- list(
  A_rev = get_model_stats(models_mape$rev),
  A_exp = get_model_stats(models_mape$exp),
  A_all = get_model_stats(models_mape$all),
  B_rev = get_model_stats(models_mape_noA$rev),
  B_exp = get_model_stats(models_mape_noA$exp),
  B_all = get_model_stats(models_mape_noA$all),
  C_rev = get_model_stats(models_mape_noEOY$rev),
  C_exp = get_model_stats(models_mape_noEOY$exp),
  C_all = get_model_stats(models_mape_noEOY$all)
)

##COMBINED TABLE##
# Caption distinguishes this from the main results table, which previously
# carried a byte-identical one.
alt_outcome_table <- make_combined_table(
  s1      = s_logabs,
  s2      = s_mape,
  caption = "Effect of National Expertise on Member State Forecast Accuracy Under Alternative Error Measures",
  label   = "tab:alt_outcome_model"
)

cat(alt_outcome_table, "\n")

dir.create(tablepath, recursive = TRUE, showWarnings = FALSE)
writeLines(alt_outcome_table, paste0(tablepath, "alt_outcome_table.tex"))
