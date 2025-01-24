library(tidyverse)

# pool predictions dataset
load("~/ec_project/data/final_dataset_euro.Rdata")

rid <- c("code","country_s","date","date_ecfin","date_pred","err0_sq_s","err1","err1_sq","err1_sq_s","err2","err2_sq","err2_sq_s","esa","exp2","iso2c","iso3c","lend2","p0","p1","p2","pct_ecfin","pop","pos_err0","pos_err1","pos_err2","rate_commission","rate_ecfin","rev2","subchapter","total","total_commission","total_ecfin","true0","true1","true2","unit","year")

p0 <- subset(df, select = -c(code,country_s,date,date_ecfin,date_pred,err1,err1_sq,err2,err2_sq,esa,exp2,iso2c,iso3c,lend2,p0,p1,p2,pct_ecfin,pop,pos_err0,pos_err1,pos_err2,rate_commission,rate_ecfin,rev2,subchapter,total,total_commission,total_ecfin,true0,true1,true2,unit,year))
p0$py <- 0
p0$err_sq <- p0$err0_sq
p0$err <- p0$err0

p1 <- subset(df, select = -c(code,country_s,date,date_ecfin,date_pred,err0,err0_sq,err2,err2_sq,esa,exp2,iso2c,iso3c,lend2,p0,p1,p2,pct_ecfin,pop,pos_err0,pos_err1,pos_err2,rate_commission,rate_ecfin,rev2,subchapter,total,total_commission,total_ecfin,true0,true1,true2,unit,year))
p1$py <- 1
p1$err_sq <- p1$err1_sq
p1$err <- p1$err1

p2 <- subset(df, select = -c(code,country_s,date,date_ecfin,date_pred,err0,err0_sq,err1,err1_sq,esa,exp2,iso2c,iso3c,lend2,p0,p1,p2,pct_ecfin,pop,pos_err0,pos_err1,pos_err2,rate_commission,rate_ecfin,rev2,subchapter,total,total_commission,total_ecfin,true0,true1,true2,unit,year))
p2$py <- 2
p2$err_sq <- p2$err2_sq
p2$err <- p2$err2

p_list <- list(p0, p1, p2)
dfp <- do.call(plyr::rbind.fill, p_list)

dfp$aeoy <- 0
dfp$aeoy[dfp$py == 0 & dfp$spring == 0] <- 1

save(dfp, file = "~/ec_project/data/final_dataset_euro_pooled.Rdata")
