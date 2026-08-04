library(fixest)
library(robomit)
library(dplyr)
library(data.table)
library(ggplot2)

datapath <- "~/EU_capacity/data/"
tablepath <- "~/EU_capacity/overleaf/tables/"
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

# Panel C: exclude EOY forecasts (py != 0), all fiscal categories
data_c <- dfpg %>% filter(py != 0, rev == 1 | exp == 1)

# ---------------------------------------------------------------------------
# Partial out fixed effects via FWL before passing to robomit, which does
# not support multi-way FEs directly.
# ---------------------------------------------------------------------------

d <- data_c %>%
  mutate(
    log_err_sq = log(err_sq),
    log_pop_int = log(pop_int),
    log_gdp_v = log(gdp)
  ) %>%
  filter(
    is.finite(log_err_sq),
    is.finite(log_pop_int),
    is.finite(log_gdp_v),
    !is.na(ecfin),
    !is.na(gdppc)
  )

dm <- data.frame(
  log_err_sq = residuals(feols(
    log_err_sq ~ 1 | country + ysp + title + py,
    data = d
  )),
  ecfin = residuals(feols(ecfin ~ 1 | country + ysp + title + py, data = d)),
  log_pop_int = residuals(feols(
    log_pop_int ~ 1 | country + ysp + title + py,
    data = d
  )),
  log_gdp_v = residuals(feols(
    log_gdp_v ~ 1 | country + ysp + title + py,
    data = d
  )),
  gdppc = residuals(feols(gdppc ~ 1 | country + ysp + title + py, data = d))
)

long <- feols(
  log(err_sq) ~
    ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = d
)
r2max <- min(2 * as.numeric(r2(long)["wr2"]), 1)

res_delta <- o_delta(
  y = "log_err_sq",
  x = "ecfin",
  con = "log_pop_int + log_gdp_v + gdppc",
  R2max = r2max,
  type = "lm",
  data = dm
)

res_beta <- o_beta(
  y = "log_err_sq",
  x = "ecfin",
  con = "log_pop_int + log_gdp_v + gdppc",
  delta = 1,
  R2max = r2max,
  type = "lm",
  data = dm
)

res_beta_neg <- o_beta(
  y = "log_err_sq",
  x = "ecfin",
  con = "log_pop_int + log_gdp_v + gdppc",
  delta = -1,
  R2max = r2max,
  type = "lm",
  data = dm
)

# cat("Oster sensitivity results (Panel C, all forecasts):\n")
# print(res_delta)
# print(res_beta)

# ---------------------------------------------------------------------------
# LaTeX table
# ---------------------------------------------------------------------------

pull <- function(tbl, row_name) as.numeric(tbl[tbl$Name == row_name, "Value"])

fmt3 <- function(x) sprintf("%.3f", x)
fmt2 <- function(x) sprintf("%.2f", x)

tex_lines <- c(
  "\\begin{table}[]",
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lc}",
  "\\tabularnewline",
  "\\midrule \\midrule",
  paste0(
    "Controls ommitted $\\hat{\\beta}$ (FE only)     & ",
    fmt3(pull(res_delta, "Uncontrolled Coefficient")),
    "\\\\"
  ),
  paste0(
    "Fully specified $\\hat{\\beta}$ (+ controls)   & ",
    fmt3(pull(res_delta, "Controlled Coefficient")),
    "\\\\"
  ),
  paste0(
    "Within $R^2$, short                 & ",
    fmt3(pull(res_delta, "Uncontrolled R-square")),
    "\\\\"
  ),
  paste0(
    "Within $R^2$, long                  & ",
    fmt3(pull(res_delta, "Controlled R-square")),
    "\\\\"
  ),
  paste0(
    "$R_{\\max}$                          & ",
    fmt3(pull(res_delta, "Max R-square")),
    "\\\\"
  ),
  "\\midrule",
  paste0(
    "$\\delta^*$                          & ",
    fmt2(pull(res_delta, "delta*")),
    "\\\\"
  ),
  paste0(
    "$\\beta^*$ ($\\delta = 1$)           & ",
    fmt3(pull(res_beta, "beta*")),
    "\\\\"
  ),
  paste0(
    "$\\beta^*$ ($\\delta = -1$)          & ",
    fmt3(pull(res_beta_neg, "beta*")),
    "\\\\"
  ),
  "\\midrule \\midrule",
  "\\multicolumn{2}{p{10cm}}{Panel C (EOY forecasts excluded), including revenue and expenditure.}\\\\",
  "\\end{tabular}",
  "\\par\\endgroup",
  "\\caption{Oster (2019) Sensitivity Analysis for the National Expertise Coefficient}",
  "\\label{tab:oster}",
  "\\end{table}"
)

cat(paste(tex_lines, collapse = "\n"), "\n")

dir.create(tablepath, recursive = TRUE, showWarnings = FALSE)
writeLines(paste(tex_lines, collapse = "\n"), paste0(tablepath, "oster_table.tex"))

# ---------------------------------------------------------------------------
# Plot: delta* across range of R_max values
# ---------------------------------------------------------------------------

r2_long <- as.numeric(r2(long)["wr2"])

oster_plot <- o_delta_rsq_viz(
  y = "log_err_sq",
  x = "ecfin",
  con = "log_pop_int + log_gdp_v + gdppc",
  type = "lm",
  data = dm
) +
  coord_cartesian(xlim = c(2 * r2_long, 10 * r2_long)) +
  scale_x_continuous(
    breaks = r2_long * seq(2, 10, by = 2),
    labels = paste0(seq(2, 10, by = 2), "×")
  ) +
  xlab(expression(R[max] ~ "(multiples of fully specified model within " * R^2 * ")")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

suppressMessages(print(oster_plot))

ggsave(
  "C:/Users/adamd/Documents/EU_Capacity/overleaf/images/oster_plot.pdf",
  plot = oster_plot,
  width = 6,
  height = 4
)
