library(dplyr)
library(data.table)

datapath = "~/ec_project/data/"

#creating guiding rate
pop <- read.csv(paste0(datapath,"population.csv"))
EP <- read.csv(paste0(datapath,"EP.csv"))
council <- read.csv(paste0(datapath,"council.csv"))

ep09 <- as.data.frame(EP$State)
ep09 <- ep09 %>% dplyr::rename(country = 'EP$State')
ep20 <- ep14 <- ep11 <- ep09

ep09$ysp <- 2009
ep11$ysp <- 2011
ep14$ysp <- 2014
ep20$ysp <- 2020

ep09$mep <- as.numeric(EP$X2009)
ep11$mep <- as.numeric(EP$X2011)
ep14$mep <- as.numeric(EP$X2014)
ep20$mep <- as.numeric(EP$X2020)

ep09$pct_mep <- (ep09$mep/sum(ep09$mep[!is.na(ep09$mep)]))*100
ep11$pct_mep <- (ep11$mep/sum(ep11$mep[!is.na(ep11$mep)]))*100
ep14$pct_mep <- (ep14$mep/sum(ep14$mep[!is.na(ep14$mep)]))*100
ep20$pct_mep <- (ep20$mep/sum(ep20$mep[!is.na(ep20$mep)]))*100

temp <- rbind(ep09,ep11,ep14,ep20)

country <- unique(pop$country)
ysp <- unique(pop$ysp)
cysp <- expand.grid(country, ysp)
cysp <- cysp %>% dplyr::rename(country = Var1, ysp = Var2)

ep_full <- full_join(temp,cysp, by = join_by(ysp, country)) %>%
  arrange(country,ysp) %>%
  mutate(mep = zoo::na.locf(mep), pct_mep = zoo::na.locf(pct_mep))

council_pop <- merge(x = pop, y = council, by = "country")
council_pop$weight[council_pop$country == "Croatia" & council_pop$ysp < 2014] <- NA
council_pop$weight[council_pop$country == "United Kingdom" & council_pop$ysp >= 2018] <- NA

council_pop$pop[council_pop$country == "Croatia" & council_pop$ysp < 2014] <- NA
council_pop$pop[council_pop$country == "United Kingdom" & council_pop$ysp >= 2018] <- NA

council_pop <- council_pop %>%
  dplyr::filter(!is.na(weight)) %>%
  dplyr::group_by(ysp) %>%
  dplyr::mutate(pct_weight = weight/sum(weight), pct_pop = pop_int/sum(pop_int))

council_pop$pct_weight <- council_pop$pct_weight*100
council_pop$pct_pop <- council_pop$pct_pop*100

guide <- merge(x = council_pop, y = ep_full, by = c("country", "ysp"))
guide$rate <- round((guide$pct_weight + guide$pct_mep + guide$pct_pop)/3,1)

# ggplot(data = guide) +
#   geom_point(aes(x = ysp, y = rate)) +
#   #geom_vline(xintercept = 2012, col = "red") +
#   #geom_vline(xintercept = 2019, col = "red") +
#   facet_wrap(~country)

#ecrate <- c(0.6, 0.8, 0.8, 0.8, 1.0, 1.0, 1.5, 1.6, 1.6, 1.8, 1.8, 1.8, 2.4, 2.6, 2.7, 3, 3.1, 3.1, 3.1, 3.1, 3.9, 4.5, 8.2, 8.9, 11.2, 11.6, 13.8)
# sum(ecrate) #sums to over 100
# hi <- sort(test3$rate)
# cor(hi, ecrate) #98% correlation

g <- as.data.table(guide)
load(paste0(datapath,"staff_nat.Rdata"))

gne <- merge(staff_nat, g, by = c("country", "ysp"))
gne$diff_iv <- gne$rate_commission - gne$rate

setDT(gne)[, c("diff_lag1","diff_lag2", "diff_lag3", "diff_lag4") := 
             .(shift(diff_iv, 1L, fill = NA, type = "lag"),
               shift(diff_iv, 2L, fill = NA, type = "lag"),
               shift(diff_iv, 3L, fill = NA, type = "lag"),
               shift(diff_iv, 4L, fill = NA, type = "lag")), by = country]

gne_merge <- subset(gne, select = c(country, ysp, diff_iv, diff_lag1, diff_lag2, diff_lag3, diff_lag4))

save(gne_merge,file=paste0(datapath,"guide_rate.Rdata"))


#out <- gse %>% 
#  group_by(country) %>%
#  summarise(mean_diff = mean(diff_iv)) 

load(paste0(datapath,"final_dataset_euro.Rdata"))

df_merged_FULL <- merge(df, gne_merge, by = c("country","ysp"), all = TRUE)

dfg <- subset(df_merged_FULL, select=which(!duplicated(names(df_merged_FULL)))) 

save(dfg,file=paste0(datapath,"final_dataset_euro_plus_guide.RData"))

# # pool predictions dataset (with guide)
# load(paste0(datapath,"final_dataset_euro_plus_guide.RData"))
# 
# p0 <- subset(dfg, select = c("ecfin", "ecfin_int", "pop_int", "gdp", "country", "ysp", "title", "err0", "err0_sq", "rev", "exp", "lend", "spring", "diff_iv", "diff_lag1", "diff_lag2", "diff_lag3", "diff_lag4"))
# p0$py <- 0
# p0$err_sq <- p0$err0_sq
# p0$err <- p0$err0
# 
# p1 <- subset(dfg, select = c("ecfin", "ecfin_int", "pop_int", "gdp", "country", "ysp", "title", "err1", "err1_sq", "rev", "exp", "lend", "spring", "diff_iv", "diff_lag1", "diff_lag2", "diff_lag3", "diff_lag4"))
# p1$py <- 1
# p1$err_sq <- p1$err1_sq
# p1$err <- p1$err1
# 
# p2 <- subset(dfg, select = c("ecfin", "ecfin_int", "pop_int", "gdp", "country", "ysp", "title", "err2", "err2_sq", "rev", "exp", "lend", "spring", "diff_iv", "diff_lag1", "diff_lag2", "diff_lag3", "diff_lag4"))
# p2$py <- 2
# p2$err_sq <- p2$err2_sq
# p2$err <- p2$err2
# 
# p_list <- list(p0, p1, p2)
# dfpg <- do.call(plyr::rbind.fill, p_list)
# 
# dfpg$aeoy <- 0
# dfpg$aeoy[dfpg$py == 0 & dfpg$spring == 0] <- 1
# 
# #exclude variables that are just sums of other variables
# vars_to_exclude <- c("Total current expenditure excluding interest: general government ","Total current expenditure: general government " ,"Total current revenue: general government " ,"Total expenditure excluding interest: general government ","Total expenditure: general government " ,"Total revenue: general government ", "Net lending (+) or net borrowing (-) excluding gross fixed capital formation: general government ", "Net lending (+) or net borrowing (-) excluding interest: general government ", "Net lending (+) or net borrowing (-): general government ")
# 
# dfpg <- dfpg %>%
#   filter(!(title %in% vars_to_exclude))
# 
# save(dfpg,file=paste0(datapath,"final_dataset_euro_pooled_plus_guide.Rdata"))
