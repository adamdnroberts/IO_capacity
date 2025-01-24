library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)

df_full <- read.csv("~/ecb_project/data/final_dataset.csv")

df <- subset(df_full, unit == "(Percentage of GDP at market prices (excessive deficit procedure)) ")

##MODELS

#basic
m1 <- feols(err0 ~ ecfin|country+ysp+title, data = df)
m2 <- feols(err1 ~ ecfin|country+ysp+title, data = df)
m3 <- feols(err2 ~ ecfin|country+ysp+title, data = df)

etable(list(m1, m2, m3), tex=FALSE)

#log sq err
m4 <- feols(log(err0_sq) ~ ecfin|country+ysp+title, data = df)
m5 <- feols(log(err1_sq) ~ ecfin|country+ysp+title, data = df)
m6 <- feols(log(err2_sq) ~ ecfin|country+ysp+title, data = df)

etable(list(m4, m5, m6), tex=FALSE)

#log sq err w/ controls
a <- feols(log(err0_sq) ~ ecfin + pop + gdp|country+ysp+title, data = df)
b <- feols(log(err1_sq) ~ ecfin + pop + gdp|country+ysp+title, data = df)

etable(list(a,b), tex=FALSE)

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin + log(pop_int) + log(gdp)|country+ysp+title, data = df)
d <- feols(log(err1_sq) ~ ecfin + log(pop_int) + log(gdp)|country+ysp+title, data = df)
e <- feols(log(err2_sq) ~ ecfin + log(pop_int) + log(gdp)|country+ysp+title, data = df)

etable(list(c,d,e), tex=F)

notlend_df <- subset(df, lend != 1)

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin + log(pop_int) + log(gdp)|country+ysp+title, data = notlend_df)
d <- feols(log(err1_sq) ~ ecfin + log(pop_int) + log(gdp)|country+ysp+title, data = notlend_df)
e <- feols(log(err2_sq) ~ ecfin + log(pop_int) + log(gdp)|country+ysp+title, data = notlend_df)

etable(list(c,d,e), tex=F)

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = df)
d <- feols(log(err1_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = df)
e <- feols(log(err2_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = df)

etable(list(c,d,e), tex=F)

rev_df <- subset(df, rev == 1)

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = rev_df)
d <- feols(log(err1_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = rev_df)
e <- feols(log(err2_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = rev_df)

etable(list(c,d,e), tex=F)

exp_df <- subset(df, exp == 1)

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = exp_df)
d <- feols(log(err1_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = exp_df)
e <- feols(log(err2_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = exp_df)

etable(list(c,d,e), tex=T)

lend_df <- subset(df, lend == 1)

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)
d <- feols(log(err1_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)
e <- feols(log(err2_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)

etable(list(c,d,e), tex=F)

#positive error rate for lending
c <- feols(pos_err0 ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)
d <- feols(pos_err1 ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)
e <- feols(pos_err2 ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)

etable(list(c,d,e), tex=F)

#error rate for lending
c <- feols(err0 ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)
d <- feols(err1 ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)
e <- feols(err2 ~ ecfin + pop_int + gdp|country+ysp+title, data = lend_df)

etable(list(c,d,e), tex=F)

ggplot(data = lend_df, aes(x = true0, y = err0)) +
  geom_point() +
  facet_wrap(~country)

vat_df <- subset(df, vat == 1)

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = vat_df)
d <- feols(log(err1_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = vat_df)
e <- feols(log(err2_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = vat_df)

etable(list(c,d,e), tex=T)

fixedEffects = fixef(e)
summary(fixedEffects)

ggplot(data = subset(df, country == "Croatia"), aes(x = ysp, y = err0)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~country)

m <- lm(test ~ ecfin, data = test3)
summary(m)

df <- df %>%
        filter(!is.na(ecfin)) %>%
        group_by(country) %>%
        mutate(ecfin_cmean = mean(ecfin)) %>%
        as.data.frame()

df$ecfin_cmean <- ave(df$ecfin, df$country)
df$ecfin_dm <- df$ecfin - df$ecfin_cmean

test <- subset(df, country == "Austria")
summary(test$ecfin_dm)

df_2y <- subset(df, !is.na(err2_sq))

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin_dm + pop_int + gdp|country+ysp+title, data = df_2y)
d <- feols(log(err1_sq) ~ ecfin_dm + pop_int + gdp|country+ysp+title, data = df_2y)
e <- feols(log(err2_sq) ~ ecfin_dm + pop_int + gdp|country+ysp+title, data = df_2y)

etable(list(c,d,e), tex=F)

ggplot(data = df, aes(x = err0)) +
  geom_histogram() +
  facet_wrap(~title) +
  theme_bw()

variance <- df %>%
  group_by(country) %>%
  filter(!is.na(err0)) %>%
  summarise(var_err0 = round(var(err0),2))

ggplot(data = variance) +
  geom_histogram(aes(x = var_err0))

#log sq err w/ interpolated data
m4i <- feols(log(err0_sq) ~ ecfin_int|country+ysp+title, data = df)
m5i <- feols(log(err1_sq) ~ ecfin_int|country+ysp+title, data = df)
m6i <- feols(log(err2_sq) ~ ecfin_int|country+ysp+title, data = df)

etable(list(m4i, m5i, m6i), tex=FALSE)

#log sq err w/ interpolated data and controls
m4i2 <- feols(log(err0_sq) ~ ecfin_int + pop_int + gdp|country+ysp+title, data = df)
m5i2 <- feols(log(err1_sq) ~ ecfin_int + pop_int + gdp|country+ysp+title, data = df)
m6i2 <- feols(log(err2_sq) ~ ecfin_int + pop_int + gdp|country+ysp+title, data = df)

etable(list(m4i2, m5i2, m6i2), tex=FALSE)

#positive error rate
m7 <- feols(pos_err0 ~ ecfin|country+ysp+unit+title, data = df)
m8 <- feols(pos_err1 ~ ecfin|country+ysp+unit+title, data = df)
m9 <- feols(pos_err2 ~ ecfin|country+ysp+unit+title, data = df)

etable(list(m7, m8, m9), tex=F)

#just revenue
m13 <- feols(log(err0_sq) ~ ecfin|country+ysp, data = subset(df, title == "Total revenue: general government :- ESA 2010 "))

m14 <- feols(log(err1_sq) ~ ecfin|country+ysp, data = subset(df, title == "Total revenue: general government :- ESA 2010 "))

m15 <- feols(log(err2_sq) ~ ecfin|country+ysp, data = subset(df, title == "Total revenue: general government :- ESA 2010 "))
etable(list(m13, m14, m15), tex=F)

#just expenditure
m16 <- feols(log(err0_sq) ~ ecfin|country+ysp, data = subset(df, title == "Total expenditure excluding interest: general government :- ESA 2010 "))

m17 <- feols(log(err1_sq) ~ ecfin|country+ysp, data = subset(df, title == "Total expenditure excluding interest: general government :- ESA 2010 "))

m18 <- feols(log(err2_sq) ~ ecfin|country+ysp, data = subset(df, title == "Total expenditure excluding interest: general government :- ESA 2010 "))
etable(list(m16, m17, m18), tex=F)

# pool predictions
test <- subset(df, select = c("ecfin", "ecfin_int", "pop_int", "gdp", "country", "ysp", "title", "err0_sq", "rev", "exp", "lend"))
test$py <- 0
test$err_sq <- test$err0_sq

test1 <- subset(df, select = c("ecfin", "ecfin_int", "pop_int", "gdp", "country", "ysp", "title", "err1_sq", "rev", "exp", "lend"))
test1$py <- 1
test1$err_sq <- test1$err1_sq

test2 <- subset(df, select = c("ecfin", "ecfin_int", "pop_int", "gdp", "country", "ysp", "title", "err2_sq", "rev", "exp", "lend"))
test2$py <- 2
test2$err_sq <- test2$err2_sq

test_list <- list(test, test1, test2)
new <- do.call(rbind.fill, test_list)

all <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = new)
rev <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(new, rev == 1))
exp <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(new, exp == 1))
lend <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(new, lend == 1))

etable(all,rev,exp,lend, tex=T)

test_list2 <- list(test1, test2)
new2 <- do.call(rbind.fill, test_list2)

all <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = new2)
rev <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(new2, rev == 1))
exp <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(new2, exp == 1))
lend <- feols(log(err_sq) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(new2, lend == 1))

etable(all2, rev2, exp2, lend2, tex = F)

df_taxes <- subset(df, title == "Taxes linked to imports and production (indirect taxes): general government :- ESA 2010 " | title == "Current taxes on income and wealth (direct taxes): general government :- ESA 2010 " | title == "Capital taxes: general government :- ESA 2010 ")

#log sq err
m4 <- feols(log(err0_sq) ~ ecfin|country+date+unit+title, data = df_taxes)

m5 <- feols(log(err1_sq) ~ ecfin|country+date+unit+title, data = df_taxes)

m6 <- feols(log(err2_sq) ~ ecfin|country+date+unit+title, data = df_taxes)

etable(list(m4, m5, m6), tex=FALSE)

df_tax1 <- subset(df, title == "Taxes linked to imports and production (indirect taxes): general government :- ESA 2010 ")

df_tax2 <- subset(df, title == "Current taxes on income and wealth (direct taxes): general government :- ESA 2010 ")

df_tax3 <- subset(df, title == "Capital taxes: general government :- ESA 2010 ")

ggplot(data = subset(df_tax1, country !="United States" & country != "Japan")) +
  geom_point(aes(x = ecfin, y = err1)) +
  facet_wrap(~country) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#IV models
df <- read.csv("~/ecb_project/data/final_dataset_euro_plus_guide.csv")

iv0 <- feols(log(err0_sq) ~ gdp + pop_int | country + ysp + title | ecfin ~ rate, data = df)
iv1 <- feols(log(err1_sq) ~ gdp + pop_int | country + ysp + title | ecfin ~ rate, data = df)
iv2 <- feols(log(err2_sq) ~ gdp + pop_int | country + ysp + title | ecfin ~ rate, data = df)

etable(list(iv0, iv1, iv2), tex=FALSE)
