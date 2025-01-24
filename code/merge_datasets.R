library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table)
library(countrycode)

load("~/ec_project/data/full_dataset11_14.Rdata")
load("~/ec_project/data/full_dataset15_23.Rdata")

df_full <- rbind(full_dataset11_14, full_dataset15_23)

df_full$ysp <- df_full$year + 0.5*(1 - df_full$spring)

df_full$country_s <- countryname(df_full$country, destination = 'country.name')

df_full$country[df_full$country == "Czech Republic"] <- "Czechia"

df_full <- dplyr::rename(df_full, date_pred = date)

title <- as.data.frame(str_split_fixed(df_full$title, ":-", 2))
title <- dplyr::rename(title, title = V1, esa = V2)

new_df_full <- subset(df_full, select = -title)
df_full_spl <- cbind(new_df_full, title)

load("~/ec_project/data/staff_nat.Rdata")
#staff_nat <- dplyr::rename(staff_nat, date_ecfin = date)
staff_nat$year <- staff_nat$spring <- NULL

df_merged <- merge(df_full_spl, staff_nat, by = c("country","ysp"), all = TRUE)

pop <- read.csv("~/ec_project/data/population.csv")

df_merged2 <- merge(df_merged, pop, by = c("country","ysp"), all = TRUE)

gdp_full <- read.csv("~/ec_project/data/gdp.csv")
gdp <- subset(gdp_full, select = -c(iso2c,year,spring))

df_merged3 <- merge(df_merged2, gdp, by = c("country","ysp"), all = TRUE)

df_merged3$title[df_merged3$title == "Interest : general government "] <- "Interest: general government "

df_merged3$edp <- ifelse(grepl('Excessive deficit procedure', df_merged3$subchapter, fixed=TRUE),1,0)

rev <- sort(unique(df_merged3$title[df_merged3$subchapter == "01 Revenue (ESA 2010)" & df_merged3$edp == 0]))

df_merged3$rev <- ifelse(df_merged3$title %in% rev,1,0) # includes total tax burden, which the last one didn't

exp <- sort(unique(df_merged3$title[df_merged3$subchapter == "02 Expenditure (ESA 2010)" & df_merged3$edp == 0]))

df_merged3$exp <- ifelse(df_merged3$title %in% exp,1,0) # I think I just missed or misspelled some of these?

lend <- sort(unique(df_merged3$title[df_merged3$subchapter == "03 Net lending (ESA 2010)" & df_merged3$edp == 0]))

df_merged3$lend <- ifelse(df_merged3$title %in% lend,1,0) # exact same as before, but doesn't get tracked until

# edp <- sort(unique(df_merged3$title[df_merged3$subchapter == "04 Excessive deficit procedure"]))
# 
# df_merged3$edp <- ifelse(df_merged3$title %in% edp,1,0) # exact same as before, but doesn't get tracked until 

df_merged3$rev2 <- ifelse(grepl('Revenue', df_merged3$subchapter, fixed=TRUE),1,0)
df_merged3$exp2 <- ifelse(grepl('Expenditure', df_merged3$subchapter, fixed=TRUE),1,0)
df_merged3$lend2 <- ifelse(grepl('Net lending', df_merged3$subchapter, fixed=TRUE),1,0)
#df_merged3$edp <- ifelse(grepl('Excessive deficit procedure', df_merged3$subchapter, fixed=TRUE),1,0)

did <- df_merged3

save(did, file = "~/ec_project/data/did_dataset.Rdata")

# #exclude variables that are just sums of other variables
# vars_to_exclude <- c("Total current expenditure excluding interest: general government ","Total current expenditure: general government " ,"Total current revenue: general government " ,"Total expenditure excluding interest: general government ","Total expenditure: general government " ,"Total revenue: general government ", "Net lending (+) or net borrowing (-) excluding gross fixed capital formation: general government ", "Net lending (+) or net borrowing (-) excluding interest: general government ", "Net lending (+) or net borrowing (-): general government ")

# df_merged3 <- df_merged3 %>%
#   filter(!(title %in% vars_to_exclude))

df <- df_merged3

#same year error rate
df$err0 <- df$p0 - df$true0
df$err0_sq <- df$err0^2

#1-year error rate
df$err1 <- df$p1 - df$true1
df$err1_sq <- df$err1^2

#2-year error rate
df$err2 <- df$p2 - df$true2
df$err2_sq <- df$err2^2

#positive error rate
df$pos_err0 <- NA
df$pos_err0[df$err0 < 0] <- 0
df$pos_err0[df$err0 > 0] <- 1

df$pos_err1 <- NA
df$pos_err1[df$err1 < 0] <- 0
df$pos_err1[df$err1 > 0] <- 1

df$pos_err2 <- NA
df$pos_err2[df$err2 < 0] <- 0
df$pos_err2[df$err2 > 0] <- 1

df$gdppc <- df$gdp/df$pop_int

save(df, file = "~/ec_project/data/final_dataset.Rdata")

#ONLY Euro estimates
df_euro <- subset(df, unit == "Mrd ECU/EUR")

#add guiding rate

load("~/ec_project/data/guide_rate.Rdata")

df_merged_FULL <- merge(df_euro, gne_merge, by = c("country","ysp"), all = TRUE)

df <- subset(df_merged_FULL, select=which(!duplicated(names(df_merged_FULL)))) 

save(df, file = "~/ec_project/data/final_dataset_euro.Rdata")
