library(fixest)

load("~/ec_project/data/final_dataset_euro.Rdata")

vars_to_exclude <- c(
  "Total current expenditure excluding interest: general government ",
  "Total current expenditure: general government " ,
  "Total current revenue: general government " ,
  "Total expenditure excluding interest: general government ",
  "Total expenditure: general government " ,
  "Total revenue: general government ", 
  "Net lending (+) or net borrowing (-) excluding gross fixed capital formation: general government ", 
  "Net lending (+) or net borrowing (-) excluding interest: general government ", 
  "Net lending (+) or net borrowing (-): general government ", 
  "Other capital expenditure, including capital transfers: general government ", 
  "Social transfers in kind ", 
  "Total expenditure: general government ",
  "Total tax burden excluding imputed social security contributions: total economy ",
  "Total tax burden including imputed social security contributions: total economy ")

df_old <- df

df <- df %>%
  filter(!(title %in% vars_to_exclude))

##MODELS

#log sq err w/ controls
a <- feols(log(err0_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = df)
b <- feols(log(err1_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = df)
c <- feols(log(err2_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = df)

etable(list(a,b,c), tex=F)

#log sq err w/ controls (interpolated population and ecfin)
d <- feols(log(err0_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = df)
e <- feols(log(err1_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = df)
f <- feols(log(err2_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = df)

etable(list(d,e,f), tex=F)

rev_df <- subset(df, rev == 1)

#log sq err w/ controls (interpolated population)
g <- feols(log(err0_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = rev_df)
h <- feols(log(err1_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = rev_df)
i <- feols(log(err2_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = rev_df)

etable(list(g,h,i), tex=F)

#log sq err w/ controls (interpolated population and ecfin) not working for some reason
j <- feols(log(err0_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = rev_df)
k <- feols(log(err1_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = rev_df)
l <- feols(log(err2_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = rev_df)

etable(list(j,k,l), tex=F)

exp_df <- subset(df, exp == 1)

#log sq err w/ controls (interpolated population)
m <- feols(log(err0_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = exp_df)
n <- feols(log(err1_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = exp_df)
o <- feols(log(err2_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = exp_df)

etable(list(m,n,o), tex=F)

#log sq err w/ controls (interpolated population + ecfin)
p <- feols(log(err0_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = exp_df)
q <- feols(log(err1_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = exp_df)
r <- feols(log(err2_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title, data = exp_df)

etable(list(p,q,r), tex=F)


vat_df <- subset(df, vat == 1)

#log sq err w/ controls (interpolated population)
c <- feols(log(err0_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = vat_df)
d <- feols(log(err1_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = vat_df)
e <- feols(log(err2_sq) ~ ecfin + pop_int + gdp|country+ysp+title, data = vat_df)

etable(list(c,d,e), tex=F)

fixedEffects = fixef(e)
summary(fixedEffects)

ggplot(data = subset(df, country == "Croatia"), aes(x = ysp, y = err0)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~country)

df <- df %>%
  filter(!is.na(ecfin)) %>%
  group_by(country) %>%
  mutate(ecfin_cmean = mean(ecfin)) %>%
  as.data.frame()

df$ecfin_cmean <- ave(df$ecfin, df$country)
df$ecfin_dm <- df$ecfin - df$ecfin_cmean

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