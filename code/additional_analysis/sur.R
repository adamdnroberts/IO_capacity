library(systemfit)
library(car)
library(fixest)
library(caret)

load("~/ec_project/data/final_dataset_euro_pooled.Rdata")

dfp$gdppc <- dfp$gdp/dfp$pop_int

#creating variables
dfp$pos_err <- NA
dfp$pos_err[dfp$err < 0] <- 0
dfp$pos_err[dfp$err > 0] <- 1

dfp$neg_err <- NA
dfp$neg_err[dfp$err > 0] <- 0
dfp$neg_err[dfp$err < 0] <- 1

#binary positive error
all <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp)
rev <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, rev == 1))
exp <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, exp == 1))
totalrev <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, title == "Total revenue: general government "))
totalexp <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, title == "Total expenditure: general government "))

etable(all,rev,exp,totalrev,totalexp, tex=F)

all <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp)

summary(all)

dfp_noA <- subset(dfp, aeoy == 0)

all2 <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noA)
rev2 <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, rev == 1))
exp2 <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, exp == 1))

etable(all2,rev2,exp2, tex = F)

dfp_noEOY <- subset(dfp, dfp$py != 0)

all3 <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
rev3 <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp3 <- feols(pos_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))

etable(all3,rev3,exp3, tex = F)

#binary negative error

allp <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp)
revp <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, rev == 1))
expp <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, exp == 1))

etable(allp,revp,expp, tex=F)

all2 <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noA)
rev2 <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, rev == 1))
exp2 <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, exp == 1))

etable(all2,rev2,exp2, tex = F)

all3 <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
rev3 <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp3 <- feols(neg_err ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))

etable(all3,rev3,exp3, tex = F)

#sur positive error
dfp$sperr <- dfp$err
dfp$sperr[dfp$err < 0] <- 0

#sur negative error
dfp$snerr <- dfp$err
dfp$snerr[dfp$err > 0] <- 0

eu_dfp <- subset(dfp, !is.na(ecfin))

ind <- dummyVars("~ title + country", data = eu_dfp)
indicators <- data.frame(predict(ind, newdata = eu_dfp))

dfp_surm <- as.data.frame(cbind(eu_dfp,indicators))

f1 <- sperr ~ ecfin + pop_int + gdp + ysp
f2 <- snerr ~ ecfin + pop_int + gdp + ysp

fitsur <- systemfit(list(mpos = f1, mneg = f2), method="SUR", data=dfp)
summary(fitsur)

restriction <- "mpos_ecfin- mneg_ecfin"
linearHypothesis(fitsur, restriction, test = "Chisq") #no fixed effects for title or country

#scaling error to be mean 0 and sd 1
dfp$err_scaled <- scale(dfp$err)

allp <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp)
revp <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, rev == 1))
expp <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, exp == 1))

etable(allp,revp,expp, tex=F)

dfp_noA <- subset(dfp, aeoy == 0)

all2 <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noA)
rev2 <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, rev == 1))
exp2 <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, exp == 1))

etable(all2,rev2,exp2, tex = F)

dfp_noEOY <- subset(dfp, dfp$py != 0)

all3 <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
rev3 <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp3 <- feols(err_scaled ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))

etable(all3,rev3,exp3, tex = F)