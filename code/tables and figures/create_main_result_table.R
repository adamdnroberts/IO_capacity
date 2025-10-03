library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)

datapath = "~/ec_project/data/"

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

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
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

etable(
  models_noA$rev,
  models_noA$exp,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

etable(
  models_noEOY$rev,
  models_noEOY$exp,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)
