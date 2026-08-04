library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)

#epu_belgium is the average of french and dutch
belgium <- read_excel("raw/EPU/EPU_Belgium_data.xlsx") %>%
  clean_names()
belgium$date <- as.Date(belgium$date)

croatia <- read_excel("raw/EPU/Croatia_EPU.xlsx", skip = 1) %>%
  clean_names()
croatia$date <- as.Date(paste0(croatia$time, "01"), format = "%YM%m%d")

# BBD includes only four categories of words: uncertainty, economy, policy, and geographic location
# extended uses more Danish-context words
denmark <- read.csv("raw/EPU/Denmark_monthly.csv", skip = 2) %>%
  clean_names()
denmark$date <- as.Date(denmark$date, format = "%m/%d/%Y")

europe <- read_excel(
  "raw/EPU/Europe_Policy_Uncertainty_Data.xlsx"
) %>%
  clean_names()
europe$date <- as.Date(
  paste(europe$year, europe$month, "1", sep = "-"),
  format = "%Y-%m-%d"
)

greece <- read_excel(
  "raw/EPU/FKT_Greece_Policy_Uncertainty_Data.xlsx",
  skip = 1
) %>%
  clean_names()
greece$date <- as.Date(
  paste(greece$year, greece$month, "1", sep = "-"),
  format = "%Y-%m-%d"
)

ireland <- read_excel(
  "raw/EPU/Ireland_Policy_Uncertainty_Data_Zalla.xlsx"
) %>%
  clean_names()
ireland$date <- as.Date(as.numeric(ireland$date), origin = "1899-12-30")
ireland <- ireland %>% filter(!is.na(date))

#EBO-NL is stricter than EBO, excluding non-relevant international terms and general terms (e.g. policy) without mention of dutch/holland/netherlands/etc,
netherlands <- read_excel(
  "raw/EPU/Netherlands_Policy_Uncertainty_Data.xlsx"
) %>%
  clean_names()
netherlands$date <- as.Date(as.numeric(netherlands$x1), origin = "1899-12-30")
netherlands <- netherlands %>% filter(!is.na(date))

#short one only goes from 2018 to 2024
poland <- read_excel(
  "raw/EPU/POL_EPU_INDICES.xls"
) %>%
  clean_names()
poland$date <- as.Date(as.numeric(poland$date), origin = "1899-12-30")
poland <- poland %>% filter(!is.na(date))

portugal <- read_excel(
  "raw/EPU/epuptindex_data.xlsx"
) %>%
  clean_names()
portugal$date <- as.Date(
  paste0(gsub("m", "-", portugal$x1), "-01"),
  format = "%Y-%m-%d"
)

sweden <- read_excel(
  "raw/EPU/Sweden_Policy_Uncertainty_Data.xlsx"
) %>%
  clean_names()
sweden$date <- as.Date(
  paste(sweden$year, sweden$month, "1", sep = "-"),
  format = "%Y-%m-%d"
)

# Column names for each dataset
for (name in c(
  "belgium",
  "croatia",
  "denmark",
  "europe",
  "greece",
  "ireland",
  "netherlands",
  "poland",
  "portugal",
  "sweden"
)) {
  cat(name, ":", paste(colnames(get(name)), collapse = ", "), "\n\n")
}

##MERGE##
# Single-country datasets: select date + epu column, add country label
single_country <- bind_rows(
  belgium %>% select(date, epu = epu_belgium) %>% mutate(country = "Belgium"),
  croatia %>% select(date, epu = epu) %>% mutate(country = "Croatia"),
  denmark %>% select(date, epu = epu_bbd) %>% mutate(country = "Denmark"),
  greece %>% select(date, epu = epu_gr) %>% mutate(country = "Greece"),
  ireland %>% select(date, epu = epu_index) %>% mutate(country = "Ireland"),
  netherlands %>%
    select(date, epu = ebo_nl_index) %>%
    mutate(country = "Netherlands"),
  poland %>% select(date, epu = epu_poland_long) %>% mutate(country = "Poland"),
  portugal %>% select(date, epu = epu_pt) %>% mutate(country = "Portugal"),
  sweden %>%
    select(date, epu = swedish_news_based_epu_index) %>%
    mutate(country = "Sweden")
)

# Europe dataset: pivot from wide to long
europe_long <- europe %>%
  select(
    date,
    Germany = germany_news_index,
    France = france_news_index,
    Italy = italy_news_index,
    Spain = spain_news_index,
    `United Kingdom` = uk_news_index
  ) %>%
  pivot_longer(-date, names_to = "country", values_to = "epu")

# Combine
epu <- bind_rows(single_country, europe_long) %>%
  arrange(country, date)

# Subset to 2011-2023 and aggregate to ysp (six-month) averages.
# Spring (Jan-Jun) = year, Autumn (Jul-Dec) = year + 0.5, matching main dataset.
epu <- epu %>%
  mutate(
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    spring = ifelse(month <= 6, 1, 0),
    ysp = year + 0.5 * (1 - spring)
  ) %>%
  filter(year >= 2011, year <= 2023) %>%
  group_by(country, ysp) %>%
  dplyr::summarise(epu = mean(epu, na.rm = TRUE), .groups = "drop")

save(epu, file = "data/epu.Rdata")

ggplot(epu, aes(x = ysp, y = epu)) +
  geom_line() +
  facet_wrap(~country, scales = "free_y") +
  labs(x = "", y = "EPU Index") +
  theme_minimal() +
  theme(strip.text = element_text(size = 7))
