library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(car)
library(lubridate)
library(data.table)
library(margins)
#library(svMisc)

load("~/ec_project/data/bonds.Rdata")
load("~/ec_project/data/final_dataset_euro.Rdata")

pred_dates <- sort(unique(df$date_pred))
pred_dates <- pred_dates[-1]
pred_dates_plus_7 <- sort(pred_dates) + 7
pred_dates_plus_14 <- sort(pred_dates) + 14
pred_dates_minus_7 <- sort(pred_dates) - 7
pred_dates_minus_14 <- sort(pred_dates) - 14

dates_full <- sort(c(pred_dates, pred_dates_minus_14, pred_dates_minus_7, pred_dates_plus_14, pred_dates_plus_7))

#to start from here
load("~/ec_project/data/bonds_with_min.Rdata")
bonds <- bonds_with_min

#relevant_bonds <- subset(bonds, min_abs <= 14)

# ggplot(subset(relevant_bonds, min_abs == 6)) +
#   geom_point(aes(date,min)) +
#   facet_wrap(~country)

#relevant_bonds$wday <- wday(relevant_bonds$date)

rb <- bonds[bonds$date %in% dates_full]
#relevant_bonds$post <- ifelse(relevant_bonds$min > 0, 1, 0)

# m <- feols(change_pct ~ treat| country, data = relevant_bonds)
# summary(m)

df$year_pred <- year(ymd(df$date_pred))
df$month_pred <- month(ymd(df$date_pred))
df$week_pred <- isoweek(ymd(df$date_pred))
df$day_pred <- day(ymd(df$date_pred))

df2 <- subset(df, esa != " Excessive deficit procedure " & !is.na(ecfin))

df2.5 <- df2 %>% arrange(country, title, date_pred) %>% group_by(country, title) %>% mutate(p0_l1 = lag(p0,n=1), p1_l1 = lag(p1,n=1), p2_l1 = lag(p2,n=1))

duplicates <- duplicated(df2.5)

df2.8<- df2.5[!duplicates, ]

df3 <- df2.8 %>%  mutate(p0_update = ifelse(month_pred == 11, p0 - p0_l1, p0 - p1_l1), p1_update = ifelse(month_pred == 11, p1 - p1_l1, p1 - p2_l1))

df3.5 <- df3[,c("country", "month_pred", "year_pred", "title", "p0","p0_l1","p0_update","p1","p1_l1","p1_update","p2","p2_l1","err0","err1")]

rev_updates <- subset(df3.5, title == "Total current revenue: general government ")
rev_updates <- dplyr::rename(rev_updates, rev0_update = p0_update, rev1_update = p1_update)

exp_updates <- subset(df3.5, title == "Total current expenditure: general government ")
exp_updates <- dplyr::rename(exp_updates, exp0_update = p0_update, exp1_update = p1_update)

lend_updates <- subset(df3.5, title == "Net lending (+) or net borrowing (-): general government ")
lend_updates <- dplyr::rename(lend_updates, lend0_update = p0_update, lend1_update = p1_update)

updates <- merge(rev_updates, exp_updates, by = c("country","month_pred","year_pred"))
updates <- merge(updates, lend_updates, by = c("country","month_pred","year_pred"))


######new specification

#rb <- subset(relevant_bonds, min_abs == 0 | min_abs == 1)
#rb <- subset(relevant_bonds, min == 0 | min == 1)

# rb <- subset(relevant_bonds,  min_abs == 1)
# rb_5_2022 <- subset(relevant_bonds,  year == 2022 & month == 5 & min == -3)
# rb <- rbind(rb,rb_5_2022)

# rb <- subset(relevant_bonds, wday == 2)
# mondays <- subset(bonds, date == mdy("11/13/2012") | date == mdy("4/30/2012"))
# rb <- rbind(rb,mondays)

rb <- bonds[bonds$date %in% dates_full]
rb$post <- ifelse(rb$min > 0, 1, 0)

rb <- arrange(rb, date)

setDT(rb)[, c("yield_lag1") :=
            .(shift(yield, 1L, fill = NA, type = "lag")), by = country]

rb$my_change_yield <- rb$yield - rb$yield_lag1
rb$my_change_pct <- rb$my_change_yield/abs(rb$yield)

#just for the merge, so that the dates in months before/after get merged to the right value
rb2 <- rb
rb2$month[rb2$month==4] <- 5
rb2$month[rb2$month==10] <- 11
rb2$month[rb2$month==12] <- 11

bd <- merge(rb2, updates, by.x = c("country", "year", "month"), by.y = c("country", "year_pred", "month_pred"))

bd$year_month <- paste0(bd$year,"-",bd$month)

cross_tab <- bd %>%
  group_by(year_month, country) %>%
  tally() %>%
  spread(year_month, n)

bd <- bd[!(bd$date %in% pred_dates_minus_14) & !(bd$date %in% pred_dates_plus_14)]

cross_tab <- bd %>%
  group_by(year_month, country) %>%
  tally() %>%
  spread(year_month, n)
#it worked!!!

ggplot(bd) +
  geom_point(aes(date,my_change_pct)) +
  facet_wrap(~country)

test <- rb[rb$date %in% pred_dates_minus_14]
  
## Using day before, day of, and day after
bd$rev_post0 <- bd$post*bd$rev0_update
bd$rev_post1 <- bd$post*bd$rev1_update
bd$exp_post0 <- bd$post*bd$exp0_update
bd$exp_post1 <- bd$post*bd$exp1_update
bd$lend_post0 <- bd$post*bd$lend0_update
bd$lend_post1 <- bd$post*bd$lend1_update

# ggplot(bd) +
#   geom_point(aes(date,my_change_pct)) +
#   facet_wrap(~country)

m4 <- feols(my_change_pct ~ post + p0.x + p1.x + p0.y + p1.y + rev_post0 + rev_post1 + exp_post0 + exp_post1 | country + month + year, data = bd)
m5 <- feols(my_change_pct ~ post + p0.x + p1.x + p0.y + p1.y + rev_post0 + rev_post1 | country + month + year, data = bd)
m6 <- feols(my_change_pct ~ post + p0.x + p1.x + p0.y + p1.y + exp_post0 + exp_post1 | country + month + year, data = bd)

etable(m4,m5,m6, tex = F)
etable(m4, tex = T)

linearHypothesis(m4, c("rev_post0=0", "rev_post1=0"))
linearHypothesis(m5, c("p0.x=0","p1.x=0","p0.y=0","p1.y=0","rev_post0=0", "rev_post1=0"))

linearHypothesis(m4, c("exp_post0=0", "exp_post1=0"))
linearHypothesis(m6, c("exp_post0=0", "exp_post1=0"))

linearHypothesis(m4, c("rev_post0=0", "rev_post1=0", "exp_post0=0", "exp_post1=0"))
linearHypothesis(m4, c("rev_post0=0", "exp_post0=0"))
linearHypothesis(m4, c("rev_post1=0", "exp_post1=0"))

bd$may <- ifelse(bd$month == 5,1,0)

m4 <- feols(my_change_pct ~ post + rev_post0 + rev_post1 + exp_post0 + exp_post1 | country + month + year, data = subset(bd, min != 0))
m5 <- feols(my_change_pct ~ post + rev_post0 + rev_post1 | country + month + year, data = subset(bd, min != 0))
m6 <- feols(my_change_pct ~ post + exp_post0 + exp_post1 | country + month + year, data = subset(bd, min != 0))
m7 <- feols(my_change_pct ~ post + lend_post0 + lend_post1 | country + month + year, data = subset(bd, min != 0))

etable(m4,m5,m6,m7, tex = F)
etable(m4,m5,m6,m7, tex = T)

linearHypothesis(m4, c("rev_post0=0", "rev_post1=0"))
linearHypothesis(m5, c("rev_post0=0", "rev_post1=0"))

linearHypothesis(m4, c("exp_post0=0", "exp_post1=0"))
linearHypothesis(m6, c("exp_post0=0", "exp_post1=0"))

linearHypothesis(m4, c("rev_post0=0", "rev_post1=0", "exp_post0=0", "exp_post1=0"))
linearHypothesis(m4, c("rev_post0=0", "exp_post0=0"))
linearHypothesis(m4, c("rev_post1=0", "exp_post1=0"))

linearHypothesis(m7, c("lend_post0=0", "lend_post1=0"))


bd <- rename(bd, rev0_err = err0.x, rev1_err = err1.x, exp0_err = err0.y, exp1_err = err1.y)

test <- feols(my_change_pct ~ post + rev_post0 + rev_post1 + exp_post0 + exp_post1 + rev0_err + rev1_err + exp0_err + exp1_err | country + month + year, data = subset(bd, min != 0))
etable(test)

linearHypothesis(test, c("rev_post0=0", "rev_post1=0", "exp_post0=0", "exp_post1=0"))


#means and variances

#rev0
mean(bd$rev_post0[bd$rev0_err < -4.8660 | bd$rev0_err > -0.1984], na.rm=T)
sd(bd$rev_post0[bd$rev0_err < -4.8660 | bd$rev0_err > -0.1984], na.rm=T)

mean(bd$rev_post0[bd$rev0_err > -4.8660 & bd$rev0_err < -0.1984], na.rm=T)
sd(bd$rev_post0[bd$rev0_err > -4.8660 & bd$rev0_err < -0.1984], na.rm=T)

#rev1
mean(bd$rev_post1[bd$rev1_err > -8.8659 & bd$rev1_err < -0.2019], na.rm=T)
sd(bd$rev_post1[bd$rev1_err > -8.8659 & bd$rev1_err < -0.2019], na.rm=T)

mean(bd$rev_post1[bd$rev1_err < -8.8659 | bd$rev1_err > -0.2019], na.rm=T)
sd(bd$rev_post1[bd$rev1_err < -8.8659 | bd$rev1_err > -0.2019], na.rm=T)

#exp0
mean(bd$exp_post0[bd$exp0_err < -1.2554 | bd$exp0_err > 1.0543], na.rm=T)
var(bd$exp_post0[bd$exp0_err < -1.2554 | bd$exp0_err > 1.0543], na.rm=T)

mean(bd$exp_post0[bd$exp0_err > -1.2554 & bd$exp0_err < 1.0543], na.rm=T)
var(bd$exp_post0[bd$exp0_err > -1.2554 & bd$exp0_err < 1.0543], na.rm=T)

#exp1
mean(bd$exp_post1[bd$exp1_err > -6.9131 & bd$exp1_err < 0.1912], na.rm=T)
var(bd$exp_post1[bd$exp1_err > -6.9131 & bd$exp1_err < 0.1912], na.rm=T)

mean(bd$exp_post1[bd$exp1_err < -6.9131 | bd$exp1_err > 0.1912], na.rm=T)
var(bd$exp_post1[bd$exp1_err < -6.9131 | bd$exp1_err > 0.1912], na.rm=T)

# m1 <- feols(spread_us ~ post + rev_post0 + rev_post1 + exp_post0 + exp_post1 | country + month + year, data = bd)
# m2 <- feols(spread_us ~ post + rev_post0 + rev_post1 | country + month + year, data = bd)
# m3 <- feols(spread_us ~ post | country + month + year, data = bd)
# 
# etable(m1,m2,m3, tex = F)
# 
# linearHypothesis(m1, c("rev_post0=0", "rev_post1=0"))
# linearHypothesis(m2, c("rev_post0=0", "rev_post1=0"))
# 
# linearHypothesis(m1, c("exp_post0=0", "exp_post1=0"))

# relevant.bonds.6 <- subset(relevant_bonds, min_abs == 6)
# 
# relevant.bonds.6 <- arrange(relevant.bonds.6, date)
# 
# setDT(relevant.bonds.6)[, c("yield_lag1") :=
#                .(shift(yield, 1L, fill = NA, type = "lag")), by = country]
# 
# relevant.bonds.6$my_change_pct <- relevant.bonds.6$yield - relevant.bonds.6$yield_lag1
# 
# bondsdf6 <- merge(relevant.bonds.6, updates, by.x = c("country", "year", "month"), by.y = c("country", "year_pred", "month_pred"))
# 
# bondsdf6$year_month <- paste0(bondsdf6$year,"-",bondsdf6$month)
# 
# cross_tab <- bondsdf6 %>%
#   group_by(year_month, country) %>%
#   tally() %>%
#   spread(year_month, n)
# 
# ## MODELS
# bondsdf5$rev_post0 <- bondsdf5$post*bondsdf5$rev0_update
# bondsdf5$rev_post1 <- bondsdf5$post*bondsdf5$rev1_update
# bondsdf5$exp_post0 <- bondsdf5$post*bondsdf5$exp0_update
# bondsdf5$exp_post1 <- bondsdf5$post*bondsdf5$exp1_update
# 
# m0 <- feols(spread_us ~ rev_post0 + rev_post1 + exp_post0 + exp_post1 | country + month + year + post, data = bondsdf5)
# m1 <- feols(spread_us ~ post + rev_post0 + rev_post1 + exp_post0 + exp_post1 | country + month + year, data = bondsdf5)
# m2 <- feols(spread_us ~ post + rev_post0 + rev_post1 | country + month + year, data = bondsdf5)
# m3 <- feols(spread_us ~ post | country + month + year, data = bondsdf5)
# 
# etable(m0,m1,m2,m3, tex = F)
# 
# linearHypothesis(m1, c("rev_post0=0", "rev_post1=0"))
# linearHypothesis(m2, c("rev_post0=0", "rev_post1=0"))
# 
# linearHypothesis(m1, c("exp_post0=0", "exp_post1=0"))
# 
# ggplot(bondsdf5) +
#   geom_point(aes(date,my_change_pct)) +
#   facet_wrap(~country)
# 
# m4 <- feols(my_change_pct ~ post + rev_post0 + rev_post1 + exp_post0 + exp_post1 | country + month + year, data = bondsdf5)
# m5 <- feols(my_change_pct ~ post + rev_post0 + rev_post1 | country + month + year, data = bondsdf5)
# m6 <- feols(my_change_pct ~ post | country + month + year, data = bondsdf5)
# 
# etable(m4,m5,m6, tex = F)
# 
# linearHypothesis(m4, c("rev_post0=0", "rev_post1=0"))
# linearHypothesis(m5, c("rev_post0=0", "rev_post1=0"))
# 
# linearHypothesis(m4, c("exp_post0=0", "exp_post1=0"))
# 
# linearHypothesis(m4, c("rev_post0=0", "rev_post1=0", "exp_post0=0", "exp_post1=0"))
# linearHypothesis(m4, c("rev_post0=0", "exp_post0=0"))
# linearHypothesis(m4, c("rev_post1=0", "exp_post1=0"))

# my_matrix <- matrix(nrow = nrow(bonds), ncol = 1)
# my_df <- as.data.frame(my_matrix)
# 
# for (i in 1:length(pred_dates)) {
#   my_df[,paste0("num_days",pred_dates[i])] <- bonds$date - pred_dates[i]
# }
# 
# my_df <- my_df[,-1]
# 
# for (i in 1:nrow(my_df)) {
#   row <- unlist(as.vector(my_df[i,]))
#   bonds$min_abs[i] <- min(abs(row), na.rm = T)
# }
# 
# pb <- txtProgressBar(min = 0, max = length(nrow(my_df)), initial = 0)
# 
# i=0
# for (i in 1:nrow(my_df)) {
#   row <- unlist(as.vector(my_df[i,]))
#   temp_abs_min <- min(abs(row), na.rm = T)
#   temp_min <- row[abs(row) == temp_abs_min]
#   bonds$min[i] <- temp_min[1]
#   setTxtProgressBar(pb, i)
# }
# close(pb)
# 
# bonds_with_min <- bonds
# 
# save(bonds_with_min,file="~/ec_project/data/bonds_with_min.Rdata")