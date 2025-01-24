library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(car)

datapath = "~/ec_project/data/"

# pooled predictions

load("~/ec_project/data/final_dataset_euro_pooled.Rdata")
setDT(dfp)

## RUN THIS STUFF EVERYTIME!
#exclude variables that are just sums of other variables
vars_to_exclude <- c("Total current expenditure excluding interest: general government ","Total current expenditure: general government " ,"Total current revenue: general government " ,"Total expenditure excluding interest: general government ","Total expenditure: general government " ,"Total revenue: general government ", "Net lending (+) or net borrowing (-) excluding gross fixed capital formation: general government ", "Net lending (+) or net borrowing (-) excluding interest: general government ", "Net lending (+) or net borrowing (-): general government ", "Other capital expenditure, including capital transfers: general government ", "Social transfers in kind ", "Total expenditure: general government ")

dfp <- dfp %>%
  filter(!(title %in% vars_to_exclude))

#make subset datasets
dfp_noA <- subset(dfp, aeoy == 0)
dfp_noEOY <- subset(dfp, dfp$py != 0)

#create quartiles
dfp_noEOY$gdp_quartile <- ntile(dfp_noEOY$gdp, 4)


##TABLE 1##
all <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp)

all2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noA)

all3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
etable(all3)

true_coef <- all3$coefficients[1]
true_r2 <- r2(all3, type = "r2")
true_wr2 <- r2(all3, type = "wr2")


#prepare df for randomizations
sn_full <- subset(dfp_noEOY, !is.na(ecfin))
sn_dupes <- subset(sn_full, select = c(ysp,country,ecfin))
sn <- sn_dupes[!duplicated(sn_dupes)]
sn <- dplyr::rename(sn, ecfin_rand = ecfin)

countrynames <- unique(sn$country)

#randomize
r2 <- list()
wr2 <- list()
coef <- list()

for (i in 1:10000) {
  dict <- data.frame(
    OldValue = countrynames,
    NewValue = sample(countrynames)
  )
  
  df <- sn %>%
    left_join(dict, by = c("country" = "OldValue")) %>%
    mutate(country = NewValue) %>%
    select(-NewValue)
  
  dfp_rand <- dfp_noEOY %>%
    left_join(df, by = c("country", "ysp"))
  
  rand <- feols(log(err_sq) ~ ecfin_rand + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_rand, notes = FALSE)
  
  r2[i] <- r2(rand, type = "r2")
  wr2[i] <- r2(rand, type = "wr2")
  coef[i] <- rand$coefficients[1]
  if (i %% 1000 == 0) {
    print(i)
  }
}

hist(as.numeric(wr2), breaks = 100, main = "", xlab = "Within R^2")
abline(v = true_wr2, col = "red")

hist(as.numeric(coef), breaks = 100, main = "", xlab = "Coefficients")
abline(v = true_coef, col = "red")

(length(wr2) - length(wr2[wr2 < true_wr2]))/length(wr2) #kinda like p-value
mean(as.numeric(wr2))

(length(coef) - length(coef[coef > true_coef]))/length(coef)
(length(coef) - length(coef[abs(as.numeric(coef)) < abs(as.numeric(true_coef))]))/length(coef) 

hist(r2)
(length(r2) - length(r2[r2 < 0.67301]))/length(r2) #kinda like p-value


ggplot(dfp_rand) +
  geom_point(aes(x=ysp, y=ecfin)) +
  geom_point(aes(x=ysp,y=ecfin_rand), color = "red") +
  facet_wrap(~country)
