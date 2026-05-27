library(fixest)
library(robomit)
library(dplyr)
library(data.table)
library(ggplot2)

datapath <- "~/EU_capacity/data/"
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

dfpg_noA   <- dfpg %>% filter(aeoy == 0)
dfpg_noEOY <- dfpg %>% filter(py != 0)

# ---------------------------------------------------------------------------
# robomit cannot handle multi-way fixed effects directly, so we partial them
# out first using the Frisch-Waugh-Lovell theorem. Residuals from regressing
# each variable on the FEs are the within-transformed values; running OLS on
# those is equivalent to the feols() specification.
# ---------------------------------------------------------------------------

partial_out_fe <- function(data_subset) {
  d <- data_subset %>%
    mutate(
      log_err_sq  = log(err_sq),
      log_pop_int = log(pop_int),
      log_gdp_v   = log(gdp)
    ) %>%
    filter(
      is.finite(log_err_sq), is.finite(log_pop_int), is.finite(log_gdp_v),
      !is.na(ecfin), !is.na(gdppc)
    )

  data.frame(
    log_err_sq  = residuals(feols(log_err_sq  ~ 1 | country + ysp + title + py, data = d)),
    ecfin       = residuals(feols(ecfin       ~ 1 | country + ysp + title + py, data = d)),
    log_pop_int = residuals(feols(log_pop_int ~ 1 | country + ysp + title + py, data = d)),
    log_gdp_v   = residuals(feols(log_gdp_v   ~ 1 | country + ysp + title + py, data = d)),
    gdppc       = residuals(feols(gdppc       ~ 1 | country + ysp + title + py, data = d))
  )
}

get_r2max <- function(data_subset) {
  long <- feols(
    log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
    data = data_subset
  )
  min(1.3 * as.numeric(r2(long)["wr2"]), 1)
}

run_robomit <- function(data_subset) {
  data_all <- data_subset %>% filter(rev == 1 | exp == 1)
  dm    <- partial_out_fe(data_all)
  r2max <- get_r2max(data_all)
  list(
    dm    = dm,
    r2max = r2max,
    delta = o_delta(
      y = "log_err_sq", x = "ecfin",
      con = "log_pop_int + log_gdp_v + gdppc",
      R2max = r2max, type = "lm", data = dm
    ),
    beta  = o_beta(
      y = "log_err_sq", x = "ecfin",
      con = "log_pop_int + log_gdp_v + gdppc",
      delta = 1, R2max = r2max, type = "lm", data = dm
    )
  )
}

panels <- list(
  A = run_robomit(dfpg),
  B = run_robomit(dfpg_noA),
  C = run_robomit(dfpg_noEOY)
)

# ---------------------------------------------------------------------------
# LaTeX table
# o_delta columns: Name, Value
#   "delta*", "Uncontrolled Coefficient", "Controlled Coefficient",
#   "Uncontrolled R-square", "Controlled R-square", "Max R-square"
# o_beta columns: Name, Value
#   "beta*", "Uncontrolled Coefficient", "Controlled Coefficient", ...
# ---------------------------------------------------------------------------

pull_val <- function(tbl, row_name) {
  as.numeric(tbl[tbl$Name == row_name, "Value"])
}

fmt3 <- function(x) sprintf("%.3f", x)
fmt2 <- function(x) sprintf("%.2f", x)

make_row <- function(label, A, B, C, fmt = fmt3) {
  paste0(label, " & ", fmt(A), " & ", fmt(B), " & ", fmt(C), "\\\\")
}

tex_lines <- c(
  "\\begin{table}[]",
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lccc}",
  "\\tabularnewline",
  "\\midrule \\midrule",
  "& Panel A & Panel B & Panel C\\\\",
  "& All forecasts & Excl.~Nov.~EOY & Excl.~EOY\\\\",
  "\\midrule",
  make_row(
    "Short $\\hat{\\beta}$ (FE only)",
    pull_val(panels$A$delta, "Uncontrolled Coefficient"),
    pull_val(panels$B$delta, "Uncontrolled Coefficient"),
    pull_val(panels$C$delta, "Uncontrolled Coefficient")
  ),
  make_row(
    "Long $\\hat{\\beta}$ (+ controls)",
    pull_val(panels$A$delta, "Controlled Coefficient"),
    pull_val(panels$B$delta, "Controlled Coefficient"),
    pull_val(panels$C$delta, "Controlled Coefficient")
  ),
  make_row(
    "Within $R^2$, short",
    pull_val(panels$A$delta, "Uncontrolled R-square"),
    pull_val(panels$B$delta, "Uncontrolled R-square"),
    pull_val(panels$C$delta, "Uncontrolled R-square")
  ),
  make_row(
    "Within $R^2$, long",
    pull_val(panels$A$delta, "Controlled R-square"),
    pull_val(panels$B$delta, "Controlled R-square"),
    pull_val(panels$C$delta, "Controlled R-square")
  ),
  make_row(
    "$R_{\\max}$",
    pull_val(panels$A$delta, "Max R-square"),
    pull_val(panels$B$delta, "Max R-square"),
    pull_val(panels$C$delta, "Max R-square")
  ),
  "\\midrule",
  make_row(
    "$\\delta^*$",
    pull_val(panels$A$delta, "delta*"),
    pull_val(panels$B$delta, "delta*"),
    pull_val(panels$C$delta, "delta*"),
    fmt = fmt2
  ),
  make_row(
    "$\\beta^*$ ($\\delta = 1$)",
    pull_val(panels$A$beta, "beta*"),
    pull_val(panels$B$beta, "beta*"),
    pull_val(panels$C$beta, "beta*")
  ),
  "\\midrule \\midrule",
  "\\multicolumn{4}{p{12cm}}{All forecasts (revenue and expenditure) with fixed effects: country, period, economic indicator, forecast year}\\\\",
  "\\multicolumn{4}{p{12cm}}{Fixed effects partialled out via FWL theorem before applying \\citet{oster2019unobservable} estimator}\\\\",
  "\\multicolumn{4}{p{12cm}}{$\\delta^*$: selection on unobservables relative to observables required to zero out $\\hat{\\beta}$; $\\delta^* > 1$ indicates robustness}\\\\",
  "\\multicolumn{4}{p{12cm}}{$\\beta^*$: bias-adjusted estimate assuming equal selection ($\\delta = 1$)}\\\\",
  "\\multicolumn{4}{p{12cm}}{$R_{\\max} = 1.3 \\times$ within $R^2$ of long model}\\\\",
  "\\end{tabular}",
  "\\par\\endgroup",
  "\\caption{Oster (2019) Sensitivity Analysis for the National Expertise Coefficient}",
  "\\label{tab:oster}",
  "\\end{table}"
)

cat(paste(tex_lines, collapse = "\n"), "\n")

# writeLines(
#   paste(tex_lines, collapse = "\n"),
#   "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/tables/oster_table.tex"
# )

# ---------------------------------------------------------------------------
# Sensitivity plots: delta* and beta* across a range of R_max values
# o_delta_rsq_viz and o_beta_rsq_viz plot across the full R_max range
# ---------------------------------------------------------------------------

make_delta_plot <- function(p, panel_label) {
  o_delta_rsq_viz(
    y = "log_err_sq", x = "ecfin",
    con = "log_pop_int + log_gdp_v + gdppc",
    type = "lm", data = p$dm
  ) +
    ggtitle(panel_label) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "italic", size = 9))
}

print(make_delta_plot(panels$A, "Panel A: All forecasts"))
print(make_delta_plot(panels$B, "Panel B: Excl. Nov. EOY forecasts"))
print(make_delta_plot(panels$C, "Panel C: Excl. all EOY forecasts"))
