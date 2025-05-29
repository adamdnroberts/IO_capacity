library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)

#https://data-explorer.oecd.org/vis?tm=dsd_earnings&pg=0&snb=8&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_EARNINGS%40AV_AN_WAGE&df[ag]=OECD.ELS.SAE&df[vs]=1.0&dq=..EUR..V..&pd=2011%2C&to[TIME_PERIOD]=false

wages_full <- read.csv("~/ec_project/raw/average_annual_wages.csv")

wages <- wages_full[c("REF_AREA", "Reference.area", "TIME_PERIOD", "OBS_VALUE")] #current prices, Euro
wages <- dplyr::rename(wages, ccode = REF_AREA, country = Reference.area, year = TIME_PERIOD, wages = OBS_VALUE)

wages <- wages %>% arrange(country, year)

ggplot(wages) +
  geom_line(aes(x=year, y = wages)) +
  facet_wrap(~country)

ysp <- seq(min(wages$year),max(wages$year),by=0.5)
countries <- unique(wages$country)
temp <- expand.grid(ysp,countries)
temp <- temp %>% dplyr::rename(ysp = Var1, country = Var2)

wages <- wages %>% dplyr::rename(ysp = year)

wag <- full_join(temp,wages, by = join_by(ysp, country)) %>%
  mutate(wages_int = na.approx(wages))

ggplot(wag) +
  geom_point(aes(x=ysp,y=wages)) +
  geom_line(aes(x=ysp,y=wages_int), color="red", alpha = 0.6) +
  facet_wrap(~country)

save(wag,file="~/ecb_project/data/avg_wages.Rdata")

#using AD/AST pay schedule

salary_full <- read.csv("~/ec_project/data/AD_AST_salaries.csv")

ad5 <- subset(salary_full, grades == 5)

ad5 <- ad5[c("year", "step1")] #current prices, Euro
ad5 <- dplyr::rename(ad5, ad5_month = step1)

test <- merge(wages, ad5, by.x = "ysp", by.y = "year", all.x = TRUE)

test2 <- na.omit(test)

ggplot(test) +
  geom_point(aes(x=ysp,y=wages_diff_int)) +
  facet_wrap(~country)

test2$wages_diff <- test2$wages - test2$ad5_month*12

ysp <- seq(min(test2$ysp),max(test2$ysp),by=0.5)
countries <- unique(test2$country)
temp <- expand.grid(ysp,countries)
temp <- temp %>% dplyr::rename(ysp = Var1, country = Var2)

for_merge <- full_join(temp,test2, by = join_by(ysp, country)) %>%
  mutate(wages_diff_int = na.approx(wages_diff))

#total country wages data
total_wages <- read.csv("~/ecb_project/raw/ECB Data Portal_20240624235353.csv")

total_wages <- total_wages[1:(length(total_wages) - 7)]

colnames(total_wages) <- c("date", "time", "Cyprus", "Germany", "Denmark", "Netherlands", "Norway", "Czech Republic", "Estonia", "Poland", "Spain", "Portugal", "Finland", "Romania","France", "Sweden","Slovenia", "United Kingdom","Greece", "Croatia", "Slovakia", "Hungary","Ireland","Austria", "Italy","Lithuania","Luxembourg", "Latvia", "Belgium", "Malta", "Bulgaria")

total_wages <- total_wages %>% separate(time, c("year","quarter"), sep="Q")
total_wages$year <- as.numeric(total_wages$year)
total_wages$quarter <- as.numeric(total_wages$quarter)
wages <- subset(total_wages, year >= 2011)

countryname <- colnames(total_wages)[4:length(colnames(total_wages))]

list <- list()
for (i in 1:length(countryname)){
  list[[i]] <- wages[c("year","quarter",countryname[i])]
  colnames(list[[i]]) <- c("year", "quarter", "wages")
  list[[i]]$country <- countryname[i]
}

wag <- do.call(plyr::rbind.fill, list)
wag <- wag[c("country", "year", "quarter", "wages")]
wag <- wag[order(wag$country), ]

wag$wages <- wag$wages * 1000000

#employee data
total_employ <- read.csv("~/ecb_project/raw/ECB_employment.csv")

total_employ <- total_employ[1:(length(total_employ) - 4)]

colnames(total_employ) <- c("DATE","TIME.PERIOD","AT","BG","CY","CZ","DE","DK","EE","ES","FI","GB","GR","HR","HU","IE","IT","LT","LU","LV","MT","NL","NO","PL","PT","RO","SE","SI","SK","FR")

iso_alpha2 <- read.csv("~/ecb_project/raw/Staff/iso_alpha2.csv")

total_employ <- total_employ %>% separate(TIME.PERIOD, c("year","quarter"), sep="Q")
total_employ$year <- as.numeric(total_employ$year)
total_employ$quarter <- as.numeric(total_employ$quarter)

employ <- subset(total_employ, year >= 2011)

countryname <- colnames(total_employ)[4:length(colnames(total_employ))]

list <- list()
for (i in 1:length(countryname)){
  list[[i]] <- employ[c("year","quarter",countryname[i])]
  colnames(list[[i]]) <- c("year", "quarter", "employ")
  list[[i]]$ccode <- countryname[i]
}

emp <- do.call(plyr::rbind.fill, list)

emp <- merge(emp, iso_alpha2, by.x = "ccode", by.y = "code")

emp <- emp[c("ccode", "country", "year", "quarter", "employ")]

emp$employ <- emp$employ*1000 #bc unit is thousands of hours worked

#merge
full <- merge(wag, emp, by = c("country", "year", "quarter"), all = TRUE)

full$yq <- full$year + (full$quarter - 1)*0.25

ggplot(data = full) +
  geom_line(aes(x=yq, y = wages)) +
  geom_line(aes(x=yq,y=employ), color = "red") +
  facet_wrap(~country)

full$wages_avg <- full$wages/full$employ

ggplot(data = full) +
  geom_line(aes(x=yq, y = wages_avg)) +
  facet_wrap(~country)

full$spring <- ifelse(full$quarter < 3, 1, 0)

avwag <- full %>% filter(year <= 2022) %>% group_by(country, year, spring) %>%
  dplyr::summarise(country = first(country), year = first(year), spring = first(spring), employ = mean(employ, na.rm = TRUE), wages = mean(wages, na.rm = TRUE), wages_avg = mean(wages_avg, na.rm = TRUE))

avwag$ysp <- avwag$year + (1 - avwag$spring)*0.5
avwag$aawag <- avwag$wages_avg*40*50

ggplot(data = avwag) +
  geom_line(aes(x=ysp, y = wages)) +
  geom_line(aes(x=ysp,y=employ), color = "red") +
  facet_wrap(~country)

ggplot(data = avwag) +
  geom_line(aes(x = ysp, y = aawag), color = "blue") +
  facet_wrap(~country)

save(avwag,file="~/ecb_project/data/avwag.Rdata")
