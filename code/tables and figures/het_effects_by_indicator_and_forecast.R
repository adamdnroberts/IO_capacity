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

mod_horizon <- feols(
  log(err_sq) ~
    ecfin *
      as.factor(py) +
      log(pop_int) +
      log(gdp) +
      gdppc |
      country + ysp + title + py,
  data = dfpg %>% filter(rev == 1 | exp == 1)
)
etable(mod_horizon)
wald(mod_horizon, "ecfin:")

mod_rev <- feols(
  log(err_sq) ~
    ecfin *
      as.factor(title) +
      log(pop_int) +
      log(gdp) +
      gdppc |
      country + ysp + title + py,
  data = dfpg %>% filter(rev == 1)
)
etable(mod_rev)
wald(mod_rev, "ecfin:")

mod_exp <- feols(
  log(err_sq) ~
    ecfin *
      as.factor(title) +
      log(pop_int) +
      log(gdp) +
      gdppc |
      country + ysp + title + py,
  data = dfpg %>% filter(exp == 1)
)
etable(mod_exp)
wald(mod_exp, "ecfin:")
