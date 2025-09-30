library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)

datapath = "~/ec_project/data/"

# pooled predictions

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

#make subset datasets
dfpg_noA <- subset(dfpg, aeoy == 0)
dfpg_noEOY <- subset(dfpg, dfpg$py != 0)

#with ecfin interpolated, linear interpolation

#table 1
rev_linear <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, rev == 1)
)
exp_linear <- feols(
  log(err_sq) ~
    ecfin_int + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, exp == 1)
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

etable(rev_linear, exp_linear, tex = T, digits = 3, digits.stats = 3)
etable(rev_linear2, exp_linear2, tex = T, digits = 3, digits.stats = 3)
etable(rev_linear3, exp_linear3, tex = T, digits = 3, digits.stats = 3)

#with ecfin interpolated, spline interpolation

#table 1
rev_spline <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, rev == 1)
)
exp_spline <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg, exp == 1)
)

rev2_spline <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noA, rev == 1)
)
exp_spline <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noA, exp == 1)
)

dfpg_noEOY <- subset(dfpg, dfpg$py != 0)

rev3_spline <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noEOY, rev == 1)
)
exp3_spline <- feols(
  log(err_sq) ~
    ecfin_spline + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = subset(dfpg_noEOY, exp == 1)
)

etable(rev_spline, exp_spline, tex = T, digits = 3, digits.stats = 3)
etable(rev_spline2, exp_spline2, tex = T, digits = 3, digits.stats = 3)
etable(rev_spline3, exp_spline3, tex = T, digits = 3, digits.stats = 3)
