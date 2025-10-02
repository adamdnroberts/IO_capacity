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

# Fetch World Bank population data (2011-2022)
df_full <- wb_data(
  indicator = "SP.POP.TOTL",
  country = EU_COUNTRIES,
  start_date = 2011,
  end_date = 2022
)

# Clean and prepare data
df <- df_full %>%
  select(iso2c, iso3c, country, date, SP.POP.TOTL) %>%
  dplyr::rename(pop = SP.POP.TOTL) %>%
  mutate(country = if_else(country == "Slovak Republic", "Slovakia", country))

# Load and prepare 2023 Eurostat data
eurostat_pop <- read.csv("~/ec_project/raw/estat_tps00001.csv")

es23 <- eurostat_pop %>%
  transmute(
    iso2c = geo.TIME_PERIOD,
    date = 2023,
    pop = as.numeric(X2023)
  )

# Merge 2023 data with country codes
ccs <- df %>%
  distinct(iso2c, iso3c, country)

pop23 <- es23 %>%
  inner_join(ccs, by = "iso2c") %>%
  filter(!is.na(country))

# Combine all years
pop <- bind_rows(df, pop23) %>%
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
  filter(
    # Exclude UK and Greece data after 2022
    (ysp <= 2022 & country == "United Kingdom") |
      (ysp <= 2022 & country == "Greece") |
      (!country %in% c("United Kingdom", "Greece"))
  )

write.csv(
  pop_final,
  file = "~/ec_project/data/population.csv",
  row.names = FALSE
)
