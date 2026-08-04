library(ggplot2)
library(fixest)
library(dplyr)
library(plyr)
library(data.table)

datapath = "data/"

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

extract_coefs <- function(model) {
  coef  <- coef(model)["ecfin"]
  ci90  <- confint(model, level = 0.9)["ecfin", ]
  ci95  <- confint(model, level = 0.95)["ecfin", ]
  list(
    coef       = coef,
    ci_lower90 = ci90[1],
    ci_upper90 = ci90[2],
    ci_lower95 = ci95[1],
    ci_upper95 = ci95[2]
  )
}

##LOG(ERR_SQ) — main specification##
run_models_errsq <- function(data) {
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
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1 | exp == 1)
    )
  )
}

models_errsq       <- run_models_errsq(dfpg)
models_errsq_noA   <- run_models_errsq(dfpg_noA)
models_errsq_noEOY <- run_models_errsq(dfpg_noEOY)

etable(models_errsq$rev, models_errsq$exp, models_errsq$all, tex = FALSE)
etable(models_errsq_noA$rev, models_errsq_noA$exp, models_errsq_noA$all, tex = FALSE)
etable(models_errsq_noEOY$rev, models_errsq_noEOY$exp, models_errsq_noEOY$all, tex = FALSE)

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

etable(models_logabs$rev, models_logabs$exp, models_logabs$all, tex = FALSE)
etable(models_logabs_noA$rev, models_logabs_noA$exp, models_logabs_noA$all, tex = FALSE)
etable(models_logabs_noEOY$rev, models_logabs_noEOY$exp, models_logabs_noEOY$all, tex = FALSE)

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

etable(models_mape$rev, models_mape$exp, models_mape$all, tex = FALSE)
etable(models_mape_noA$rev, models_mape_noA$exp, models_mape_noA$all, tex = FALSE)
etable(models_mape_noEOY$rev, models_mape_noEOY$exp, models_mape_noEOY$all, tex = FALSE)

##COMBINED PLOT: all-model coefficient across outcomes and specifications##
# Each row is one outcome x specification combination, using the all model only.
# transform: function converting raw coefficient to % change in |err|.
#   log(err_sq): divide by 2 first (coefficient = 2 * effect on log|err|)
#   log(abs(err)) and log(mape): no division needed
coef_rows <- list(
  list(outcome = "Log Err Sq",    spec = "All forecasts",   model = models_errsq$all,    transform = function(x) 100 * (exp(x / 2) - 1)),
  list(outcome = "Log Err Sq",    spec = "Exclude Nov EOY", model = models_errsq_noA$all, transform = function(x) 100 * (exp(x / 2) - 1)),
  list(outcome = "Log Err Sq",    spec = "Exclude All EOY", model = models_errsq_noEOY$all, transform = function(x) 100 * (exp(x / 2) - 1)),
  list(outcome = "Log Abs Error", spec = "All forecasts",   model = models_logabs$all,   transform = function(x) 100 * (exp(x) - 1)),
  list(outcome = "Log Abs Error", spec = "Exclude Nov EOY", model = models_logabs_noA$all, transform = function(x) 100 * (exp(x) - 1)),
  list(outcome = "Log Abs Error", spec = "Exclude All EOY", model = models_logabs_noEOY$all, transform = function(x) 100 * (exp(x) - 1)),
  list(outcome = "MAPE",          spec = "All forecasts",   model = models_mape$all,     transform = function(x) 100 * (exp(x) - 1)),
  list(outcome = "MAPE",          spec = "Exclude Nov EOY", model = models_mape_noA$all, transform = function(x) 100 * (exp(x) - 1)),
  list(outcome = "MAPE",          spec = "Exclude All EOY", model = models_mape_noEOY$all, transform = function(x) 100 * (exp(x) - 1))
)

coef_df <- do.call(rbind, lapply(coef_rows, function(row) {
  e <- extract_coefs(row$model)
  t <- row$transform
  data.frame(
    Outcome       = row$outcome,
    Specification = row$spec,
    Coefficient   = t(as.numeric(e$coef)),
    CI_Lower90    = t(as.numeric(e$ci_lower90)),
    CI_Upper90    = t(as.numeric(e$ci_upper90)),
    CI_Lower95    = t(as.numeric(e$ci_lower95)),
    CI_Upper95    = t(as.numeric(e$ci_upper95))
  )
}))

coef_df$Specification <- factor(
  coef_df$Specification,
  levels = c("All forecasts", "Exclude Nov EOY", "Exclude All EOY")
)

coef_df$Outcome <- factor(
  coef_df$Outcome,
  levels = c("Log Err Sq", "Log Abs Error", "MAPE")
)

alt_outcome_plot <- ggplot(
  coef_df,
  aes(y = Outcome, x = Coefficient, color = Specification)
) +
  geom_errorbar(
    aes(xmin = CI_Lower95, xmax = CI_Upper95),
    width = 0, linewidth = 0.5,
    position = position_dodge(width = -.75)
  ) +
  geom_errorbar(
    aes(xmin = CI_Lower90, xmax = CI_Upper90),
    width = 0, linewidth = 2, alpha = 0.4,
    position = position_dodge(width = -.75)
  ) +
  geom_point(position = position_dodge(width = -.75), size = 3) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(title = "", x = "% Change in Forecast Error", y = "") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

print(alt_outcome_plot)

ggsave(
  filename = "overleaf/images/alt_outcome_plot.pdf",
  plot = alt_outcome_plot,
  width = 6,
  height = 4
)
