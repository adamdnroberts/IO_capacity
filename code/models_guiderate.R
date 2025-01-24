library(fixest)

datapath = "~/ec_project/data/"

load(paste0(datapath,"final_dataset_euro_pooled.Rdata"))

#diff = true commission wide percentage - guide rate
iv_all  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2 + diff_lag3, data = dfp)
iv_rev  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2 + diff_lag3, data = subset(dfp, rev == 1))
iv_exp  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2 + diff_lag3, data = subset(dfp, exp == 1))

iv_allfs <- iv_all$iv_first_stage 
iv_revfs <- iv_rev$iv_first_stage 
iv_expfs <- iv_exp$iv_first_stage 

etable(iv_allfs,iv_revfs,iv_expfs, tex=F) #fstat is like 5000 for all fs models
etable(iv_all,iv_rev,iv_exp, tex=F)

df2 <- subset(dfp, aeoy != 1)

iv_all2  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2, data = df2)
iv_rev2  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2, data = subset(df2, rev == 1))
iv_exp2  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2, data = subset(df2, exp == 1))

iv_all2fs <- iv_all2$iv_first_stage 
iv_rev2fs <- iv_all2$iv_first_stage 
iv_exp2fs <- iv_all2$iv_first_stage 

etable(iv_all2fs,iv_rev2fs,iv_exp2fs, tex=F)
etable(iv_all2,iv_rev2,iv_exp2, tex=F)

df3 <- subset(dfp, py != 0)

iv_all3  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2, data = df3)
iv_rev3  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2, data = subset(df3, rev == 1))
iv_exp3  <- feols(log(err_sq) ~ pop_int+gdp|country+ysp+title+py| ecfin ~ diff_lag1 + diff_lag2, data = subset(df3, exp == 1))

iv_all3fs <- iv_all3$iv_first_stage 
iv_rev3fs <- iv_all3$iv_first_stage 
iv_exp3fs <- iv_all3$iv_first_stage

etable(iv_all3fs,iv_rev3fs,iv_exp3fs, tex=F)
etable(iv_all3,iv_rev3,iv_exp3, tex=F)

#non-pooled iv

load("~/ec_project/data/final_dataset_euro.RData")

ggplot(data = subset(df, ysp >= 2018)) +
  geom_line(aes(x = ysp, y = diff_iv)) +
  geom_line(aes(x = ysp, y = diff_lag1), color = "red", alpha = 0.6) +
  facet_wrap(~country)

iv0 <- feols(log(err0_sq) ~ gdp + pop_int | country + ysp + title | ecfin ~ diff_lag1 + diff_lag2, data = df)
iv1 <- feols(log(err1_sq) ~ gdp + pop_int | country + ysp + title | ecfin ~ diff_lag1 + diff_lag2, data = df)
#iv2 <- feols(log(err2_sq) ~ gdp + pop_int | country + ysp + title | ecfin ~ diff_lag1 + diff_lag2, data = df)

iv0fs <- iv0$iv_first_stage
iv1fs <- iv1$iv_first_stage
#iv2fs <- iv2$iv_first_stage

etable(iv0fs,iv1fs, tex=F)
etable(list(iv0, iv1), tex=F)
