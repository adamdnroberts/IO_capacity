library(countrycode)
library(dplyr)
library(ggplot2)

#https://data-explorer.oecd.org/vis?tm=dsd_earnings&pg=0&snb=8&vw=tb&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_EARNINGS%40AV_AN_WAGE&df[ag]=OECD.ELS.SAE&df[vs]=1.0&dq=..EUR..V..&pd=2011%2C&to[TIME_PERIOD]=false

wages_full <- read.csv("~/ec_project/raw/average_annual_wages.csv")

wages <- wages_full[c("Reference.area", "TIME_PERIOD", "OBS_VALUE")] #current prices, Euro
wages <- dplyr::rename(wages, country = Reference.area, year = TIME_PERIOD, wages = OBS_VALUE)

wages$country <- countryname(wages$country, destination = 'country.name')

wages <- wages %>% arrange(country, year)

ysp <- seq(min(wages$year),max(wages$year),by=0.5)
countries <- unique(wages$country)
temp <- expand.grid(ysp,countries)
temp <- temp %>% dplyr::rename(ysp = Var1, country = Var2)

wages <- wages %>% dplyr::rename(ysp = year)

wag <- full_join(temp,wages, by = join_by(ysp, country))

gdp <- read.csv("~/ec_project/data/gdp.csv")

gdp$country <- countryname(gdp$country, destination = 'country.name')

gdp_new <- gdp[c("ysp", "country", "gdp")]

wag2 <- left_join(wag,gdp_new, by = join_by(ysp, country))

pop <- read.csv("~/ec_project/data/population.csv")

pop$country <- countryname(pop$country, destination = 'country.name')
pop_new <- pop[c("ysp", "country", "pop")]

wag3 <- left_join(wag2,pop_new, by = join_by(ysp, country))

load("~/ec_project/data/staff_nat.Rdata")
staff_nat$country <- countryname(staff_nat$country, destination = 'country.name')
staff_nat_new <- staff_nat[c("ysp", "country", "ecfin")]

wag4 <- left_join(wag3,staff_nat_new, by = join_by(ysp, country))

prof_wages <- read.csv("~/ec_project/raw/earn_ses_monthly_page_linear.csv") #managers and professionals
prof_wages$country <- countrycode(prof_wages$geo, origin = "eurostat", destination = 'country.name')
prof_wages <- dplyr::rename(prof_wages, ysp = TIME_PERIOD, prof_wages = OBS_VALUE)

prof_wages <- prof_wages[c("ysp", "country", "prof_wages")]
prof_wages <- subset(prof_wages, ysp >=2010 & !is.na(country))
prof_wages <- prof_wages %>% arrange(country, ysp)

wag5 <- full_join(wag4,prof_wages, by = join_by(ysp, country))

man_wages <- read.csv("~/ec_project/raw/eurostat_manual_workers.csv") #manual wages
man_wages$country <- countrycode(man_wages$geo, origin = "eurostat", destination = 'country.name')
man_wages <- dplyr::rename(man_wages, ysp = TIME_PERIOD, man_wages = OBS_VALUE)

man_wages <- man_wages[c("ysp", "country", "man_wages")]
man_wages <- subset(man_wages, ysp >=2010 & !is.na(country))
man_wages <- man_wages %>% arrange(country, ysp)

wag6 <- full_join(wag5,man_wages, by = join_by(ysp, country))

other_prof_wages <- read.csv("~/ec_project/raw/non_manual_non_professional_wages.csv")

other_prof_wages$country <- countrycode(other_prof_wages$geo, origin = "eurostat", destination = 'country.name')
other_prof_wages <- dplyr::rename(other_prof_wages, ysp = TIME_PERIOD, other_prof_wages = OBS_VALUE)

other_prof_wages <- other_prof_wages[c("ysp", "country", "other_prof_wages")]
other_prof_wages <- subset(other_prof_wages, ysp >=2010 & !is.na(country))
other_prof_wages <- other_prof_wages %>% arrange(country, ysp)

wag7 <- full_join(wag6,other_prof_wages, by = join_by(ysp, country))

d <- subset(wag7, !is.na(gdp))

# ggplot(wag7) +
#   geom_point(aes(ysp,prof_wages)) +
#   geom_point(aes(ysp,other_prof_wages), color = "red") +
#   geom_point(aes(ysp,man_wages), color = "blue") +
#   facet_wrap(~country)