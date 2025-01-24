library(wbstats)
library(ggplot2)
library(dplyr)
library(tidyr)

#population, yearly
EU_countries <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus","CZE","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovak Republic","Slovenia","Spain","Sweden","United Kingdom")

df_full <- wb_data("SP.POP.TOTL", country = EU_countries, start_date = 2011, end_date = 2022)
df <- df_full[,1:5]
df <- df %>% dplyr::rename(pop = SP.POP.TOTL)

df$country[df$country == "Slovak Republic"] <- "Slovakia"

#add 2023
eurostat_pop <- read.csv("~/ecb_project/raw/estat_tps00001.csv")

es23 <- as.data.frame(cbind(eurostat_pop$geo.TIME_PERIOD, rep(2023,nrow(eurostat_pop)), eurostat_pop$X2023))
colnames(es23) <- c("iso2c", "date","pop")

ccs <- unique(df[,1:3])

merged <- merge(ccs, es23, by = "iso2c", all = TRUE)
pop23 <- subset(merged, !is.na(country))
pop23$pop <- as.numeric(pop23$pop)

pop_merged <- rbind(df,pop23)
pop <- subset(pop_merged, !is.na(date))
pop$ysp <- as.numeric(pop$date)

ggplot(data = pop) +
  geom_point(aes(x = date, y = pop)) +
  facet_wrap(~country) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ysp <- seq(min(pop$ysp[!is.na(pop$ysp)]),max(pop$ysp[!is.na(pop$ysp)]),by=0.5)
countries <- unique(pop$country)
temp <- expand.grid(ysp,countries)
temp <- temp %>% 
  dplyr::rename(ysp = Var1, country = Var2)

pop_wysp <- full_join(temp,pop, by = join_by(ysp, country)) %>%
  mutate(pop_int = zoo::na.approx(pop))

pop_final <- subset(pop_wysp, (ysp <= 2022 & country == "United Kingdom") | (ysp <= 2022 & country == "Greece") | (country != "United Kingdom" & country != "Greece"))

ggplot(data = pop_final) +
  geom_line(aes(x = ysp, y = pop)) +
  geom_point(aes(x = ysp, y = pop_int), color = "red") +
  facet_wrap(~country) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

write.csv(pop_final, file = "~/ecb_project/data/population.csv", row.names = FALSE)

#gdp, millions of euro
estat_gdp <- read.csv("~/ecb_project/raw/namq_10_gdp_page_linear.csv")

iso_alpha2 <- read.csv("~/ecb_project/raw/Staff/iso_alpha2.csv")

new <- merge(estat_gdp, iso_alpha2, by.x = "geo", by.y = "code")

test <- new %>% separate(TIME_PERIOD, c("year","quarter"))
test$year <- as.numeric(test$year)
test$spring <- 0
test$spring[test$quarter == "Q1" | test$quarter == "Q2"] <- 1

gdp <- test %>% group_by(country, spring, year) %>%
  dplyr::summarise(year = first(year), iso2c = first(geo),country = first(country), spring = first(spring), gdp = mean(OBS_VALUE, na.rm = TRUE))

gdp$ysp <- gdp$year + 0.5*(1 - gdp$spring)

ggplot(data = gdp) +
  geom_line(aes(x = ysp, y = gdp)) +
  facet_wrap(~country)

write.csv(gdp, file = "~/ecb_project/data/gdp.csv", row.names = FALSE)

#using eurostat API
# library(eurostat)
# library(countrycode)
# library(lubridate)
# 
# #toc <- get_eurostat_toc()
# 
# pop <- get_eurostat("tps00001") #population on January 1, but only goes back to 2013????
# pop$country <- countrycode(pop$geo, origin = "eurostat", destination = "country.name")
# pop$ysp <- year(ymd(pop$TIME_PERIOD))
# pop <- dplyr::rename(pop, pop = values)
# 
# pop <- pop[c("ysp", "country", "pop")]
# 
# 
# save(pop, file = "~/ec_project/data/pop.Rdata")


