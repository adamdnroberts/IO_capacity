library(lubridate)
library(fixest)
library(rdd)
library(ggplot2)

countryname <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Lithuania","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United Kingdom")

list <- list()
for (i in 1:length(countryname)){
  filename <- paste0("~/ec_project/raw/Bonds/",countryname[i]," 10-Year Bond Yield Historical Data.csv")
  list[[i]] <- read.csv(filename)
  ifelse(countryname[i] == "Czech Republic",list[[i]]$country <- "Czechia",list[[i]]$country <- countryname[i])
}

bonds <- do.call(plyr::rbind.fill, list)

bonds <- dplyr::rename(bonds, change_pct = Change.., yield = Price) #for some reason they label the yields Price, but I checked with another source and this is the yield
bonds$change_pct <- as.numeric(gsub("%|,", "", bonds$change_pct))
bonds$date <- as.Date(bonds$Date, "%m/%d/%Y")

bonds <- subset(bonds, select = c(country,date,yield,change_pct,Date))

us <- read.csv("~/ec_project/raw/Bonds/United States 10-Year Bond Yield Historical Data.csv")
us <- dplyr::rename(us, us_change_pct = Change.., us_yield = Price)
us$us_change_pct <- as.numeric(gsub("%|,", "", us$us_change_pct))
us$date <- as.Date(us$Date, "%m/%d/%Y")
us <- subset(us, select = c(date,us_yield,us_change_pct))

bonds <- merge(bonds, us, by = "date")
bonds$spread_us <- (bonds$yield - bonds$us_yield)*100

#bonds$rv <- as.numeric(bonds$date) - 19862 

 # ggplot(bonds) +
 #   geom_line(aes(x = date, y = yield)) +
 #   #geom_line(aes(x = date, y = spread_us/100), color = "red") +
 #   facet_wrap(~country)

bonds$year <- year(ymd(bonds$date))
bonds$month <- month(ymd(bonds$date))
bonds$week <- isoweek(ymd(bonds$date))
bonds$day <- day(ymd(bonds$date))

setDT(bonds)[, c("yield_lag1","change_pct_lag1","change_pct_lead1","change_pct_lead2", "change_pct_lead3", "change_pct_lead4", "date_lead1") :=
               .(shift(yield, 1L, fill = NA, type = "lag"),
                 # shift(change_pct, 1L, fill = NA, type = "lag"),
                 # shift(change_pct, 1L, fill = NA, type = "lead"),
                 # shift(change_pct, 2L, fill = NA, type = "lead"),
                 # shift(change_pct, 3L, fill = NA, type = "lead"),
                 # shift(change_pct, 4L, fill = NA, type = "lead"),
                 shift(date, 1L, fill = NA, type = "lead")), by = country]

# test <- subset(bonds, date == "2024-06-09" | date == "2024-06-16" | date == "2024-06-23" | date == "2024-06-02")

# bonds$lead1_year <- year(ymd(bonds$date_lead1))
# bonds$lead1_month <- month(ymd(bonds$date_lead1))
# bonds$lead1_week <- isoweek(ymd(bonds$date_lead1))
# bonds$lead1_day <- date(ymd(bonds$date_lead1))

# bonds$lead2_year <- year(ymd(bonds$date_lead2))
# bonds$lead2_month <- month(ymd(bonds$date_lead2))
# bonds$lead2_week <- isoweek(ymd(bonds$date_lead2))
# bonds$lead2_day <- date(ymd(bonds$date_lead2))
# 
# bonds$lead3_year <- year(ymd(bonds$date_lead3))
# bonds$lead3_month <- month(ymd(bonds$date_lead3))
# bonds$lead3_week <- isoweek(ymd(bonds$date_lead3))
# bonds$lead3_day <- date(ymd(bonds$date_lead3))
# 
# bonds$lead4_year <- year(ymd(bonds$date_lead4))
# bonds$lead4_month <- month(ymd(bonds$date_lead4))
# bonds$lead4_week <- isoweek(ymd(bonds$date_lead4))
# bonds$lead4_day <- date(ymd(bonds$date_lead4))

save(bonds,file="~/ec_project/data/bonds.Rdata")
