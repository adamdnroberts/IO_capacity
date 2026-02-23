library(data.table)
library(dplyr)
library(fixest)

datapath = "~/ec_project/data/"

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

dfpg$pos_error <- as.numeric(dfpg$err > 0)

dfpg_noA <- dfpg %>% filter(aeoy == 0)
dfpg_noEOY <- dfpg %>% filter(py != 0)

run_models <- function(data) {
  list(
    rev = feols(
      pos_error ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1)
    ),
    exp = feols(
      pos_error ~
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
