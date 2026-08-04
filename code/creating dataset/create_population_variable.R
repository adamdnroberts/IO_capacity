library(wbstats)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

# Define EU countries
EU_COUNTRIES <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "CZE",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Netherlands",
  "Poland",
  "Portugal",
  "Romania",
  "Slovak Republic",
  "Slovenia",
  "Spain",
  "Sweden",
  "United Kingdom"
)

# Highest ysp the forecast panel reaches. The interpolation grid is trimmed to
# this below: `create_dataset.R` joins population with full_join, so any ysp
# present here but absent from the panel would create orphan rows.
PANEL_MAX_YSP <- 2023

# Fetch World Bank population data. The fetch runs one year past PANEL_MAX_YSP
# so that the final half-year has a real anchor on both sides to interpolate
# between, rather than being dropped for want of an endpoint. Fetching only
# through 2022 previously left population.csv ending at ysp 2022.0, which gave
# every Autumn 2022 (ysp 2022.5) row a missing pop_int -- and since every
# specification includes log(pop_int) and gdppc, feols silently deleted the
# entire vintage.
df_full <- wb_data(
  indicator = "SP.POP.TOTL",
  country = EU_COUNTRIES,
  start_date = 2011,
  end_date = PANEL_MAX_YSP + 1
)

# Clean and prepare data
df <- df_full %>%
  select(iso2c, iso3c, country, date, SP.POP.TOTL) %>%
  dplyr::rename(pop = SP.POP.TOTL) %>%
  mutate(country = if_else(country == "Slovak Republic", "Slovakia", country))

pop <- df %>%
  filter(!is.na(date)) %>%
  mutate(ysp = as.numeric(date), pop = as.numeric(pop))

# Create half-year time series for interpolation
ysp_seq <- seq(
  from = min(pop$ysp, na.rm = TRUE),
  to = max(pop$ysp, na.rm = TRUE),
  by = 0.5
)

# Expand grid and interpolate
pop_final <- expand.grid(
  ysp = ysp_seq,
  country = unique(pop$country),
  stringsAsFactors = FALSE
) %>%
  left_join(pop, by = c("ysp", "country")) %>%
  group_by(country) %>%
  mutate(pop_int = na.approx(pop, na.rm = FALSE)) %>%
  ungroup() %>%
  # Trim the extra year fetched for interpolation; see PANEL_MAX_YSP above.
  filter(ysp <= PANEL_MAX_YSP)

# A UK/Greece exclusion after 2022 previously sat here. It was a no-op -- the
# data stopped at 2022 -- and would have started dropping rows the moment the
# fetch was extended. It removed 0 UK rows (the UK has no estimation-eligible
# observations after ysp 2020.5 for unrelated reasons) and 50 Greek rows, and
# the World Bank has both countries through 2025, so it was not about data
# availability. Any real sample restriction belongs in the build, applied to
# the estimation sample explicitly, not smuggled in through a covariate's
# coverage.

stopifnot(
  !any(is.na(pop_final$pop_int)),
  max(pop_final$ysp) >= PANEL_MAX_YSP,
  !anyDuplicated(pop_final[, c("country", "ysp")])
)

write.csv(
  pop_final,
  file = "data/population.csv",
  row.names = FALSE
)
