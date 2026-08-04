library(data.table)
library(fixest)
library(dplyr)

datapath = "~/EU_capacity/data/"

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
true_t <- summary(all2, vcov = "HC1")$coeftable["ecfin", "t value"]
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
t_stat <- list()

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
  t_stat[i] <- summary(rand, vcov = "HC1")$coeftable["ecfin_rand", "t value"]
  if (i %% 100 == 0) {
    print(i / 10000)
  }
}
end.time <- Sys.time()
end.time - start.time

# Open a PDF device
pdf(
  "C:/Users/adamd/Documents/EU_Capacity/overleaf/images/randomization_coefficient.pdf",
  width = 7,
  height = 5
)

# Create the plot
hist(as.numeric(t_stat), breaks = 100, main = "", xlab = "t-statistic")
abline(v = true_t, col = "black", lty = "longdash")

# Close the PDF device
dev.off()

# Raw-coefficient p-values (original)
one_sided_test <- (length(coef) - length(coef[coef > true_coef])) / length(coef)
two_sided_test <- (length(coef) -
  length(coef[abs(as.numeric(coef)) < abs(as.numeric(true_coef))])) /
  length(coef)

# Studentized p-values (asymptotically pivotal, robust to unequal variances)
t_vals <- as.numeric(t_stat)
one_sided_t <- mean(t_vals <= true_t)
two_sided_t <- mean(abs(t_vals) >= abs(true_t))

# Store the draws. This loop is 10,000 model fits; without saving them, redrawing
# the histogram or recomputing a p-value means running the whole thing again.
saveRDS(
  list(
    coef = as.numeric(coef), t_stat = t_vals,
    r2 = as.numeric(r2), wr2 = as.numeric(wr2),
    true_coef = true_coef, true_t = true_t, seed = 42, n_sim = 10000
  ),
  paste0(datapath, "randomization_draws.rds")
)

# The p-values were previously computed and then discarded when the session
# ended, so the numbers behind the figure were not recoverable.
cat("\n--- Randomization inference (", length(t_vals), " draws, seed 42) ---\n", sep = "")
cat("true coefficient       :", signif(true_coef, 4), "\n")
cat("true t-statistic       :", signif(true_t, 4), "\n")
cat("p, one-sided (raw coef):", one_sided_test, "\n")
cat("p, two-sided (raw coef):", two_sided_test, "\n")
cat("p, one-sided (t-stat)  :", one_sided_t, "\n")
cat("p, two-sided (t-stat)  :", two_sided_t, "\n")
