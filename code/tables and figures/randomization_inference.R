library(data.table)
library(fixest)
library(dplyr)

datapath = "~/ec_project/data/"

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

# Exclude titles that are components of or near-duplicates of other titles,
# to avoid double-counting and correlated errors across observations.
overlapping_titles <- c(
  "Actual social contributions received: general government ",
  "Imputed social contributions: general government ",
  "Social contributions received: general government ",
  "Interest: general government ",
  "Collective consumption expenditure ",
  "Compensation of employees: general government ",
  "Intermediate consumption: general government ",
  "Social transfers in kind supplied to households via market producers: general government ",
  "Current taxes on income and wealth (direct taxes): general government ",
  "Taxes linked to imports and production (indirect taxes): general government ",
  "Capital taxes: general government ",
  "Other current revenue: general government ",
  "Net saving: general government "
)
dfpg <- dfpg %>% filter(!title %in% overlapping_titles)

#make subset datasets
dfpg_noA <- subset(dfpg, aeoy == 0)

all2 <- feols(
  log(err_sq) ~
    ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = dfpg_noA
)

true_coef <- all2$coefficients[1]
true_r2 <- r2(all2, type = "r2")
true_wr2 <- r2(all2, type = "wr2")

#prepare df for randomizations
sn_full <- subset(dfpg_noA, !is.na(ecfin))
sn_dupes <- subset(sn_full, select = c(ysp, country, ecfin))
sn <- sn_dupes[!duplicated(sn_dupes)]
sn <- dplyr::rename(sn, ecfin_rand = ecfin)

countrynames <- unique(sn$country)

#randomize
r2 <- list()
wr2 <- list()
coef <- list()

set.seed(42)

start.time <- Sys.time()
for (i in 1:10000) {
  dict <- data.frame(
    OldValue = countrynames,
    NewValue = sample(countrynames)
  )

  df <- sn %>%
    left_join(dict, by = c("country" = "OldValue")) %>%
    mutate(country = NewValue) %>%
    select(-NewValue)

  dfpg_rand <- dfpg_noA %>%
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
    print(i / 10000)
  }
}
end.time <- Sys.time()
end.time - start.time

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

one_sided_test <- (length(coef) - length(coef[coef > true_coef])) / length(coef)
two_sided_test <- (length(coef) -
  length(coef[abs(as.numeric(coef)) < abs(as.numeric(true_coef))])) /
  length(coef)
