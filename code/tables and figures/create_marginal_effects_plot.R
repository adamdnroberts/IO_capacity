library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(marginaleffects)
library(mgcv)

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

#make subset datasets
dfpg_noEOY <- dfpg %>% filter(py != 0)

# Run combined model (revenue and expenditure together)
model_combined <- feols(
  log(err_sq) ~
    ecfin *
      diff_iv +
      log(pop_int) +
      log(gdp) +
      gdppc |
      country + ysp + title + py,
  data = dfpg_noEOY
)

etable(
  model_combined,
  tex = FALSE,
  digits = 3,
  digits.stats = 3
)

# Average marginal effects of ecfin across diff_iv values
mfx <- slopes(model_combined, variables = "ecfin", by = "diff_iv")

mfx$est_pct <- 100 * (exp(mfx$estimate / 2) - 1)
mfx$conf_low_pct <- 100 * (exp(mfx$conf.low / 2) - 1)
mfx$conf_high_pct <- 100 * (exp(mfx$conf.high / 2) - 1)

marginal_effects <- ggplot(mfx, aes(x = diff_iv, y = est_pct)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_line() +
  geom_ribbon(
    aes(ymin = conf_low_pct, ymax = conf_high_pct),
    alpha = 0.2
  ) +
  labs(
    y = "Marginal Effect of Representation",
    x = "Over-Representation"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()

print(marginal_effects)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/images/marginal_effects_by_guiderate_plot.pdf",
  plot = marginal_effects,
  width = 6,
  height = 4
)

# --- GAM version ---
# Varying-coefficient GAM: allows the marginal effect of ecfin to be a
# smooth (non-linear) function of diff_iv rather than a linear interaction.
# Fixed effects are absorbed as factors. bam() is used for efficiency.
# Log transforms are pre-computed so formula references plain column names,
# which is required for marginaleffects::slopes() to find the data.

dfpg_noEOY$log_pop_int <- log(dfpg_noEOY$pop_int)
dfpg_noEOY$log_gdp <- log(dfpg_noEOY$gdp)
dfpg_noEOY$log_err_sq <- log(dfpg_noEOY$err_sq)
dfpg_noEOY$country_f <- factor(dfpg_noEOY$country)
dfpg_noEOY$ysp_f <- factor(dfpg_noEOY$ysp)
dfpg_noEOY$title_f <- factor(dfpg_noEOY$title)
dfpg_noEOY$py_f <- factor(dfpg_noEOY$py)

gam_model <- bam(
  log_err_sq ~
    s(diff_iv, by = ecfin) + # smooth marginal effect of ecfin over diff_iv
      ecfin +
      s(diff_iv) + # main smooth effect of diff_iv
      log_pop_int +
      log_gdp +
      gdppc +
      country_f +
      ysp_f +
      title_f +
      py_f,
  data = dfpg_noEOY,
  method = "fREML",
  discrete = TRUE
)

summary(gam_model)

# Marginal effects of ecfin across diff_iv values
mfx_gam <- slopes(
  gam_model,
  variables = "ecfin",
  by = "diff_iv",
  newdata = dfpg_noEOY,
  type = "response"
)

mfx_gam$est_pct <- 100 * (exp(mfx_gam$estimate / 2) - 1)
mfx_gam$conf_low_pct <- 100 * (exp(mfx_gam$conf.low / 2) - 1)
mfx_gam$conf_high_pct <- 100 * (exp(mfx_gam$conf.high / 2) - 1)

gam_marginal_effects <- ggplot(mfx_gam, aes(x = diff_iv, y = est_pct)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_line() +
  geom_ribbon(
    aes(ymin = conf_low_pct, ymax = conf_high_pct),
    alpha = 0.2
  ) +
  labs(
    y = "Marginal Effect of Representation",
    x = "Over-Representation"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()

print(gam_marginal_effects)

ggsave(
  filename = "C:/Users/adamd/Documents/EU_Capacity/overleaf/images/gam_marginal_effects_plot.pdf",
  plot = gam_marginal_effects,
  width = 6,
  height = 4
)
