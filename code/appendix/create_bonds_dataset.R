library(lubridate)
library(fixest)
library(data.table)

countryname <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czech Republic",
  "Denmark",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland",
  "Italy",
  "Lithuania",
  "Malta",
  "Netherlands",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovenia",
  "Spain",
  "Sweden",
  "United Kingdom"
)

list <- list()
for (i in 1:length(countryname)) {
  filename <- paste0(
    "~/EU_capacity/raw/Bonds/",
    countryname[i],
    " 10-Year Bond Yield Historical Data.csv"
  )
  list[[i]] <- read.csv(filename)
  ifelse(
    countryname[i] == "Czech Republic",
    list[[i]]$country <- "Czechia",
    list[[i]]$country <- countryname[i]
  )
}

bonds <- do.call(plyr::rbind.fill, list)

bonds <- dplyr::rename(bonds, change_pct = Change.., yield = Price) #for some reason they label the yields Price, but I checked with another source and this is the yield
bonds$change_pct <- as.numeric(gsub("%|,", "", bonds$change_pct))
bonds$date <- as.Date(bonds$Date, "%m/%d/%Y")

bonds <- subset(bonds, select = c(country, date, yield, change_pct, Date))

us <- read.csv(
  "~/EU_capacity/raw/Bonds/United States 10-Year Bond Yield Historical Data.csv"
)
us <- dplyr::rename(us, us_change_pct = Change.., us_yield = Price)
us$us_change_pct <- as.numeric(gsub("%|,", "", us$us_change_pct))
us$date <- as.Date(us$Date, "%m/%d/%Y")
us <- subset(us, select = c(date, us_yield, us_change_pct))

bonds <- merge(bonds, us, by = "date")
bonds$spread_us <- (bonds$yield - bonds$us_yield) * 100

bonds$year <- year(ymd(bonds$date))
bonds$month <- month(ymd(bonds$date))
bonds$week <- isoweek(ymd(bonds$date))
bonds$day <- day(ymd(bonds$date))

setDT(bonds)[,
  c(
    "yield_lag1",
    "change_pct_lag1",
    "change_pct_lead1",
    "change_pct_lead2",
    "change_pct_lead3"
  ) := shift(yield, n = -1:3, type = "lag"),
  by = country
]

save(bonds, file = "~/EU_capacity/data/bonds.Rdata")
