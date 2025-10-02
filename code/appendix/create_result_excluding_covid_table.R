library(data.table)
library(dplyr)
library(fixest)

# --- Load data ----
datapath <- "~/ec_project/data/"
load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

# --- Remove 2020–2022 forecasts for appendix ----
dfpg_covid <- dfpg %>%
  filter(
    !(ysp == 2018.5 & py == 2),
    !(ysp == 2019 & py == 1),
    !(ysp == 2019.5 & py %in% c(1, 2)),
    !(ysp >= 2020 & ysp <= 2021.5)
  )

# Subsets
dfpg_no_a_covid <- dfpg_covid %>% filter(aeoy == 0)
dfpg_no_eoy_covid <- dfpg_covid %>% filter(py != 0)

# --- Define regression formula ----
base_formula <- log(err_sq) ~
  ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py

# --- Helper to run rev/exp models for a given dataset ----
run_models <- function(data, label) {
  list(
    rev = feols(base_formula, data = filter(data, rev == 1)),
    exp = feols(base_formula, data = filter(data, exp == 1)),
    label = label
  )
}

# --- Estimate models ----
models_covid <- run_models(dfpg_covid, "All (no 2020–2022)")
models_no_a_covid <- run_models(dfpg_no_a_covid, "Excl. AEoY")
models_no_eoy_covid <- run_models(dfpg_no_eoy_covid, "Excl. EoY")

# --- Tables ----
etable(
  models_covid$rev,
  models_covid$exp,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

etable(
  models_no_a_covid$rev,
  models_no_a_covid$exp,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)

etable(
  models_no_eoy_covid$rev,
  models_no_eoy_covid$exp,
  tex = TRUE,
  digits = 3,
  digits.stats = 3
)
