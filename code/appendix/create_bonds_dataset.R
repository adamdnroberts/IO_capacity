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

# Every one of these five columns previously held a shift of `yield`, and none
# matched its own name: `n = -1:3` is c(-1, 0, 1, 2, 3), so with type = "lag"
# the column called yield_lag1 was a *lead*, change_pct_lag1 was `yield`
# unshifted, and the three "lead" columns were lags 1-3 of `yield`. Nothing
# downstream reads them -- create_bonds_analysis_table.R recomputes yield_lag1
# itself -- but they were saved into bonds.Rdata for anyone to pick up.
setDT(bonds)
bonds[, yield_lag1 := shift(yield, 1L, type = "lag"), by = country]
bonds[, change_pct_lag1 := shift(change_pct, 1L, type = "lag"), by = country]
bonds[,
  c("change_pct_lead1", "change_pct_lead2", "change_pct_lead3") :=
    shift(change_pct, 1:3, type = "lead"),
  by = country
]

save(bonds, file = "~/EU_capacity/data/bonds.Rdata")
