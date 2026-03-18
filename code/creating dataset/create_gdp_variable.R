library(dplyr)
library(tidyr)
library(zoo)

#gdp, millions of euro
estat_gdp <- read.csv("~/EU_capacity/raw/namq_10_gdp_page_linear.csv")

iso_alpha2 <- read.csv("~/EU_capacity/raw/Staff/iso_alpha2.csv")

gdp_iso <- merge(estat_gdp, iso_alpha2, by.x = "geo", by.y = "code")

gdp_iso_v2 <- gdp_iso %>% separate(TIME_PERIOD, c("year", "quarter"))
gdp_iso_v2$year <- as.numeric(gdp_iso_v2$year)
gdp_iso_v2$spring <- 0
gdp_iso_v2$spring[gdp_iso_v2$quarter == "Q1" | gdp_iso_v2$quarter == "Q2"] <- 1

gdp <- gdp_iso_v2 %>%
  group_by(country, spring, year) %>%
  dplyr::summarise(
    year = first(year),
    iso2c = first(geo),
    country = first(country),
    spring = first(spring),
    gdp = mean(OBS_VALUE, na.rm = TRUE)
  )

gdp$ysp <- gdp$year + 0.5 * (1 - gdp$spring)

ggplot(data = gdp) +
  geom_line(aes(x = ysp, y = gdp)) +
  facet_wrap(~country)

write.csv(gdp, file = "~/EU_capacity/data/gdp.csv", row.names = FALSE)
