library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(car)
library(robomit)

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
rev <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, rev == 1))
exp <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, exp == 1))

etable(all,rev,exp, tex=F)
etable(all,rev,exp, tex=T)


all2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noA)
rev2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, exp == 1))

etable(all2,rev2,exp2, tex = F)

# hist(all2$residuals)
# all2_special <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp)|country+ysp+title+py, data = dfp_noA)
# hist(all2_special$residuals) #essentially no change here

all3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
rev3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))

etable(all3,rev3,exp3, tex = F)

#plot
coef_all <- coef(all)["ecfin"]
ci_all <- confint(all)["ecfin", ]

coef_all2 <- coef(all2)["ecfin"]
ci_all2 <- confint(all2)["ecfin", ]

coef_all3 <- coef(all3)["ecfin"]
ci_all3 <- confint(all3)["ecfin", ]

coef_rev <- coef(rev)["ecfin"]
ci_rev <- confint(rev)["ecfin", ]

coef_rev2 <- coef(rev2)["ecfin"]
ci_rev2 <- confint(rev2)["ecfin", ]

coef_rev3 <- coef(rev3)["ecfin"]
ci_rev3 <- confint(rev3)["ecfin", ]

coef_exp <- coef(exp)["ecfin"]
ci_exp <- confint(exp)["ecfin", ]

coef_exp2 <- coef(exp2)["ecfin"]
ci_exp2 <- confint(exp2)["ecfin", ]

coef_exp3 <- coef(exp3)["ecfin"]
ci_exp3 <- confint(exp3)["ecfin", ]

# Create a data frame for plotting
coef_df <- data.frame(
  Model = rep(c("All", "Revenue", "Expenditure"),3),
  Specification = as.factor(c(rep(c("Panel A", "Panel B", "Panel C"), each = 3))),
  Coefficient = c(coef_all, coef_rev, coef_exp, coef_all2, coef_all3, coef_rev2, coef_rev3, coef_exp2, coef_exp3),
  CI_Lower = as.numeric(c(ci_all[1], ci_rev[1], ci_exp[1], ci_all2[1], ci_all3[1], ci_rev2[1], ci_rev3[1], ci_exp2[1], ci_exp3[1])),
  CI_Upper = as.numeric(c(ci_all[2], ci_rev[2], ci_exp[2], ci_all2[2], ci_all3[2], ci_rev2[2], ci_rev3[2], ci_exp2[2], ci_exp3[2])
))

coef_df$mod_spec <- paste0(coef_df$Model,as.character(coef_df$Specification))

ggplot(coef_df, aes(y = Model, x = Coefficient, color = Specification)) +
  geom_point(position=position_dodge(width=.75)) +
  geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0.2, position=position_dodge(width=.75)) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(title = "",
       x = "Coefficient",
       y = "") +
  #guides(color="none") +
  theme_minimal() +
  theme(legend.position =  "top", legend.title = element_blank())

#Oster sensitivity analysis
dfp_noEOY$ln_err_sq <- log(dfp_noEOY$err_sq)
dfp_noEOY$ln_pop_int <- log(dfp_noEOY$pop_int)
dfp_noEOY$ln_gdp <- log(dfp_noEOY$gdp)

dfp_sens <- subset(dfp_noEOY, ln_err_sq != -Inf & !is.na(ln_err_sq))

delta_sens <- o_delta_rsq(y = "ln_err_sq", x = "ecfin", con = "ln_pop_int + ln_gdp + gdppc + country + ysp + title + py", type = "lm", data = dfp_sens)
print(delta_sens, n = 40)

sens_plot <- o_delta_rsq_viz(y = "ln_err_sq", x = "ecfin", con = "ln_pop_int + ln_gdp + gdppc + country + ysp + title + py", type = "lm", data = dfp_sens)
  
sens_plot + labs(title = "Sensitivity analysis of Panel C Model 1")
  
#this took too long, so I ditched it
#sens_plot2 <- o_delta_boot_viz(y = "ln_err_sq", x = "ecfin", con = "ln_pop_int + ln_gdp + gdppc + country + ysp + title + py", type = "lm", data = dfp_sens, sim = 1000, obs = 20000, rep = T, R2max = 0.7)

o_beta_rsq_viz(y = "ln_err_sq", x = "ecfin", con = "ln_pop_int + ln_gdp + gdppc + country + ysp + title + py", type = "lm", data = dfp_sens)

#interaction w/ gdp (no EOY)

all_gdp <- feols(log(err_sq) ~ i(gdp_quartile,ecfin) + gdp_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
rev_gdp <- feols(log(err_sq) ~ i(gdp_quartile,ecfin) + gdp_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp_gdp <- feols(log(err_sq) ~ i(gdp_quartile,ecfin) + gdp_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))

etable(all_gdp,rev_gdp,exp_gdp, tex=F)

iplot(list(all_gdp,rev_gdp,exp_gdp), main = "", xlab = "GDP Quartile")
legend("topright", col = 1:3, pch = c(20,17,15), lwd = 1, lty = 1, legend = c("All", "Revenue","Expenditure"))

linearHypothesis(rev_gdp, "gdp_quartile::1:ecfin - gdp_quartile::2:ecfin = 0")
linearHypothesis(rev_gdp, "gdp_quartile::1:ecfin - gdp_quartile::3:ecfin = 0")
linearHypothesis(rev_gdp, "gdp_quartile::1:ecfin - gdp_quartile::4:ecfin = 0")

#gdppc quartiles (no EOY)
dfp_noEOY$gdppc_quartile <- ntile(dfp_noEOY$gdppc, 4)

all_gdppc <- feols(log(err_sq) ~ i(gdppc_quartile,ecfin) + gdppc_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
rev_gdppc <- feols(log(err_sq) ~ i(gdppc_quartile,ecfin) + gdppc_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp_gdppc <- feols(log(err_sq) ~ i(gdppc_quartile,ecfin) + gdppc_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))

etable(all_gdppc,rev_gdppc,exp_gdppc, tex=F)

iplot(list(all_gdppc,rev_gdppc,exp_gdppc), main = "", xlab = "GDP per capita Quartile")
legend("topleft", col = 1:3, pch = c(20,17,15), lwd = 1, lty = 1, legend = c("All", "Revenue","Expenditure"))

## APPENDIX

#with ecfin interpolated, linear interpolation

#table 1
all <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp)
rev <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, rev == 1))
exp <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, exp == 1))

etable(all,rev,exp, tex=T)

dfp_noA <- subset(dfp, aeoy == 0)

all2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noA)
rev2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, exp == 1))

etable(all2,rev2,exp2, tex = T)

dfp_noEOY <- subset(dfp, dfp$py != 0)

all3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
rev3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))

etable(all3,rev3,exp3, tex = T)

#with ecfin interpolated, spline interpolation

#table 1
all <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp)
rev <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, rev == 1))
exp <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp, exp == 1))

etable(all,rev,exp, tex=T)

dfp_noA <- subset(dfp, aeoy == 0)

all2 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noA)
rev2 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA, exp == 1))

etable(all2,rev2,exp2, tex = T)

dfp_noEOY <- subset(dfp, dfp$py != 0)

all3 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY)
rev3 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))

etable(all3,rev3,exp3, tex = T)

#remove pre 2015 for appendix

dfp_2015 <- subset(dfp, ysp >= 2015)

all <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_2015)
rev <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_2015, rev == 1))
exp <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_2015, exp == 1))

etable(all,rev,exp, tex=F)
etable(all,rev,exp, tex=T)

dfp_noA2015 <- subset(dfp_2015, aeoy == 0)

all2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noA2015)
rev2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA2015, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noA2015, exp == 1))

etable(all2,rev2,exp2, tex = F)

dfp_noEOY2015 <- subset(dfp_2015, py != 0)

all3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOY2015)
rev3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY2015, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOY2015, exp == 1))

etable(all3,rev3,exp3, tex = F)

#remove 2020, 2021, and 2022 forecasts for appendix

dfp_covid <- subset(dfp, !(ysp == 2018.5 & py == 2) & !(ysp == 2019 & py == 1) & !(ysp == 2019.5 & py == 1) & !(ysp == 2019.5 & py == 2) & !(ysp >= 2020 & ysp <= 2021.5))

all <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_covid)
rev <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_covid, rev == 1))
exp <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_covid, exp == 1))

etable(all,rev,exp, tex=F)
etable(all,rev,exp, tex=T)

dfp_noAcovid <- subset(dfp_covid, aeoy == 0)

all2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noAcovid)
rev2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noAcovid, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noAcovid, exp == 1))

etable(all2,rev2,exp2, tex = T)

dfp_noEOYcovid <- subset(dfp_covid, py != 0)

all3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfp_noEOYcovid)
rev3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOYcovid, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfp_noEOYcovid, exp == 1))

etable(all3,rev3,exp3, tex = T)

#OLD CODE

dfp %>% group_by(country) %>% summarize(cor=cor(ecfin,gdp, use = "pairwise.complete.obs"))

#with plain error rate
allp <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = dfp)
revp <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, rev == 1))
expp <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, exp == 1))
lendp <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, lend == 1))
notlendp <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, lend != 1))

test <- feols(err ~ ecfin*lend + pop_int + gdp|country+ysp+title+py, data = dfp)
etable(test, tex=T)
etable(lendp,notlendp, tex=F)

test <- feols(err ~ i(lend,ecfin) + pop_int + gdp|country+ysp+py, data = dfp)
etable(test)

all3 <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = dfp_noA)
rev3 <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noA, rev == 1))
exp3 <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noA, exp == 1))
lend3 <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noA, lend == 1))

etable(all3,rev3,exp3,lend3, tex=F)

all2 <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = dfp_noEOY)
rev2 <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp2 <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))
lend2 <- feols(err ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noEOY, lend == 1))

etable(all2, rev2, exp2, lend2, tex = F)

ggplot(dfp) +
  
  
  #with plain log(err+1)or rate
  all <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = dfp)
rev <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, rev == 1))
exp <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, exp == 1))
lend <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp, lend == 1))

etable(all,rev,exp,lend, tex=F)


all3 <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = dfp_noA)
rev3 <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noA, rev == 1))
exp3 <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noA, exp == 1))
lend3 <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noA, lend == 1))

etable(all3,rev3,exp3,lend3, tex=F)

all2 <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = dfp_noEOY)
rev2 <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noEOY, rev == 1))
exp2 <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noEOY, exp == 1))
lend2 <- feols(log(err+1) ~ ecfin + pop_int + gdp|country+ysp+title+py, data = subset(dfp_noEOY, lend == 1))

etable(all2, rev2, exp2, lend2, tex = F)
