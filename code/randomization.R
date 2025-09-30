library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(car)

datapath = "~/ec_project/data/"

# pooled predictions

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

#make subset datasets
dfpg_noA <- subset(dfpg, aeoy == 0)
dfpg_noEOY <- subset(dfpg, dfpg$py != 0)

#create quartiles
dfpg_noEOY$gdp_quartile <- ntile(dfpg_noEOY$gdp, 4)


##TABLE 1##
all <- feols(
  log(err_sq) ~
    ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = dfpg
)

all2 <- feols(
  log(err_sq) ~
    ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = dfpg_noA
)

all3 <- feols(
  log(err_sq) ~
    ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = dfpg_noEOY
)

true_coef <- all2$coefficients[1]
true_r2 <- r2(all2, type = "r2")
true_wr2 <- r2(all2, type = "wr2")


#prepare df for randomizations
sn_full <- subset(dfpg_noEOY, !is.na(ecfin))
sn_dupes <- subset(sn_full, select = c(ysp, country, ecfin))
sn <- sn_dupes[!duplicated(sn_dupes)]
sn <- dplyr::rename(sn, ecfin_rand = ecfin)

countrynames <- unique(sn$country)

#randomize
r2 <- list()
wr2 <- list()
coef <- list()

set.seed(42)

for (i in 1:1000) {
  dict <- data.frame(
    OldValue = countrynames,
    NewValue = sample(countrynames)
  )

  df <- sn %>%
    left_join(dict, by = c("country" = "OldValue")) %>%
    mutate(country = NewValue) %>%
    select(-NewValue)

  dfpg_rand <- dfpg_noEOY %>%
    left_join(df, by = c("country", "ysp"))

  rand <- feols(
    log(err_sq) ~
      ecfin_rand + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
    data = dfpg_rand,
    notes = FALSE
  )

  r2[i] <- r2(rand, type = "r2")
  wr2[i] <- r2(rand, type = "wr2")
  coef[i] <- rand$coefficients[1]
  if (i %% 100 == 0) {
    print(i)
  }
}

# Open a PDF device
pdf(
  "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/images/randomization_coefficient.pdf",
  width = 7,
  height = 5
)

# Create the plot
hist(as.numeric(coef), breaks = 100, main = "", xlab = "Coefficients")
abline(v = true_coef, col = "red")

# Close the PDF device
dev.off()

(length(coef) - length(coef[coef > true_coef])) / length(coef)
(length(coef) -
  length(coef[abs(as.numeric(coef)) < abs(as.numeric(true_coef))])) /
  length(coef)


ggplot(dfpg_rand) +
  geom_point(aes(x = ysp, y = ecfin)) +
  geom_point(aes(x = ysp, y = ecfin_rand), color = "red") +
  facet_wrap(~country)
