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

# Panel C: exclude EOY forecasts (py != 0), all fiscal categories
data_c <- dfpg %>% filter(py != 0, rev == 1 | exp == 1)

# ---------------------------------------------------------------------------
# Partial out fixed effects via FWL before passing to robomit, which does
# not support multi-way FEs directly.
# ---------------------------------------------------------------------------

d <- data_c %>%
  mutate(
    log_err_sq  = log(err_sq),
    log_pop_int = log(pop_int),
    log_gdp_v   = log(gdp)
  ) %>%
  filter(
    is.finite(log_err_sq), is.finite(log_pop_int), is.finite(log_gdp_v),
    !is.na(ecfin), !is.na(gdppc)
  )

dm <- data.frame(
  log_err_sq  = residuals(feols(log_err_sq  ~ 1 | country + ysp + title + py, data = d)),
  ecfin       = residuals(feols(ecfin       ~ 1 | country + ysp + title + py, data = d)),
  log_pop_int = residuals(feols(log_pop_int ~ 1 | country + ysp + title + py, data = d)),
  log_gdp_v   = residuals(feols(log_gdp_v   ~ 1 | country + ysp + title + py, data = d)),
  gdppc       = residuals(feols(gdppc       ~ 1 | country + ysp + title + py, data = d))
)

long  <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py, data = d)
r2max <- min(1.3 * as.numeric(r2(long)["wr2"]), 1)

res_delta <- o_delta(
  y = "log_err_sq", x = "ecfin",
  con = "log_pop_int + log_gdp_v + gdppc",
  R2max = r2max, type = "lm", data = dm
)

res_beta <- o_beta(
  y = "log_err_sq", x = "ecfin",
  con = "log_pop_int + log_gdp_v + gdppc",
  delta = 1, R2max = r2max, type = "lm", data = dm
)

cat("Oster sensitivity results (Panel C, all forecasts):\n")
print(res_delta)
print(res_beta)

# ---------------------------------------------------------------------------
# Plot: delta* across range of R_max values
# ---------------------------------------------------------------------------

oster_plot <- o_delta_rsq_viz(
  y = "log_err_sq", x = "ecfin",
  con = "log_pop_int + log_gdp_v + gdppc",
  type = "lm", data = dm
) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

print(oster_plot)
