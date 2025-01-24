library(fixest)

load("~/ec_project/data/avg_wages.Rdata")
load("~/ec_project/data/final_dataset_euro_pooled")

dfw <- merge(dfp, wag, by = c("country","ysp"), all = TRUE)

unique(dfw$country[is.na(dfw$wages_int)])

test <- subset(dfw, is.na(wages_int) & !is.na(ecfin_int))

iv_all  <- feols(log(err_sq) ~ pop_int+gdp+gdp/pop_int|country+ysp+title+py| ecfin ~ log(wages), data = test)
iv_rev  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ wages_diff_int, data = subset(test, rev == 1))
iv_exp  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ wages_diff_int, data = subset(test, exp == 1))

iv_allfs <- iv_all$iv_first_stage 
iv_revfs <- iv_all$iv_first_stage 
iv_expfs <- iv_all$iv_first_stage 

etable(iv_allfs)
iv_allfs

etable(iv_allfs,iv_revfs,iv_expfs, tex = F)
etable(iv_all, iv_rev, iv_exp, tex=F)

iv_all

library(countrycode)

#https://ec.europa.eu/eurostat/databrowser/view/NAMA_10_FTE__custom_11954925/default/table?lang=en
sal_full <- read.csv("~/ecb_project/raw/nama_10_fte_page_linear.csv")

sal_full$country <- countrycode(sal_full$geo, origin = "iso2c", destination = "country.name")
sal_full$country[sal_full$geo == "EL"] <- "Greece"
sal_full$country[sal_full$geo == "EU27_2020"] <- "Z EU27_2020"

ggplot(sal_full) +
  geom_line(aes(x=TIME_PERIOD, y = OBS_VALUE)) +
  facet_wrap(~country) +
  labs(title = "Average Full Time Salary by Country", x = "year", y = "Euros")
