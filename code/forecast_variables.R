library(tidyverse)

df <- read.csv("~/ecb_project/data/final_dataset_euro.csv")

#Total revenue = Total tax burden including imputed social security contributions: total economy + Other current revenue including sales: general government + Capital transfers received: general government - Capital taxes: general government (because counted twice: in total tax and capital transfers received)

sum(df$title == "Capital taxes: general government ")

rev_names <- c("Total revenue: general government ", "Total tax burden including imputed social security contributions: total economy ", "Other current revenue including sales: general government ", "Capital transfers received: general government ", "Capital taxes: general government ")

rev_cnames <- c("p0tr", "p0ttb", "p0ocr", "p0ctr", "p0ct")

list <- list()

for (i in 1:length(rev_names)){
  temp <- subset(df, title == rev_names[i])
  temp <- temp[, c("country", "ysp", "p0")]
  temp <- rename_with(temp, ~ rev_cnames[i], "p0")
  list[[i]] <- temp
}

test <- list %>% reduce(full_join, by=c("country","ysp"))
test <- na.omit(test)

test$p0tr_calc <- test$p0ttb + test$p0ctr + test$p0ocr - test$p0ct 

test$diff <- test$p0tr - test$p0tr_calc

summary(test$ysp)

##pooled
load("~/ecb_project/data/final_dataset_euro_pooled.Rdata")
setDT(dfp)

#table 1
tr_full <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, title %in% rev_names[-1]))
ttb <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, title == rev_names[2]))
ocr <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, title == rev_names[3]))
ctr <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, title == rev_names[4]))
ct <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, title == rev_names[5]))

etable(tr_full,ttb,ocr,ctr,ct, tex=F)

exp <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, title == "Total expenditure: general government "))

etable(tr,exp, tex=F)

#new thing

titles = sort(unique(df$title))

titles <- titles[!titles %in% c("Capital transfers paid: general government ", "Net disposable income: general government ")]

mlist <- list()

for (i in 1:length(titles)){
  print(i)
  temp <- subset(dfp, title == titles[i])
  mlist[[i]] <- tryCatch(feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+py, data = temp))
}

etable(mlist, tex=F)

#lending?

titles <- c("Total expenditure: general government ", "Total revenue: general government ", "Net lending (+) or net borrowing (-) excluding gross fixed capital formation: general government ", "Net lending (+) or net borrowing (-) excluding interest: general government ", "Net lending (+) or net borrowing (-): general government ")

df_new <- subset(df, !is.na(ecfin))

list <- list()

for (i in 1:length(titles)){
  temp <- subset(df_new, title == titles[i])
  temp <- temp[, c("country", "ysp", "p0", "p1", "p2")]
  temp <- rename_with(temp, ~ paste0("p0", i), "p0")
  temp <- rename_with(temp, ~ paste0("p1", i), "p1")
  temp <- rename_with(temp, ~ paste0("p2", i), "p2")
  list[[i]] <- temp
}

test <- list %>% reduce(full_join, by=c("country","ysp"))
test <- test[!duplicated(test), ]



test$lend <- test$p02 - test$p01
test$diffp0 <- test$p05 - test$lend
summary(test$diffp0)
hist(test$diffp0)

ggplot(data = subset(test, country != "United Kingdom")) +
  geom_point(aes(x=ysp,y=diffp0)) +
  facet_wrap(~country)

test$lend1 <- test$p12 - test$p11
test$diffp1 <- test$p15 - test$lend1
summary(test$diffp1)
hist(test$diffp1)

ggplot(data = subset(test, country != "United Kingdom")) +
  geom_point(aes(x=ysp,y=diffp1)) +
  facet_wrap(~country)

test$lend2 <- test$p22 - test$p21
test$diffp2 <- test$p25 - test$lend2
summary(test$diffp2)
hist(test$diffp2)

ggplot(data = subset(test, country != "United Kingdom")) +
  geom_point(aes(x=ysp,y=diffp2)) +
  facet_wrap(~country)

# so no differences here
