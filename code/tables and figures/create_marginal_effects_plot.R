library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(marginaleffects)

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
    y = "Marginal Effect of National Expertise",
    x = "Representation (percentage points)"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_minimal()

print(marginal_effects)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/images/marginal_effects_by_guiderate_plot.pdf",
  plot = marginal_effects,
  width = 6,
  height = 4
)
