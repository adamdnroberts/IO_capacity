library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(car)

datapath = "~/ec_project/data/"

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

dfpg_noA <- subset(dfpg, aeoy == 0)


all2 <- feols(
  log(err_sq) ~
    ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
  data = dfpg_noA
)

true_coef <- all2$coefficients[1]

#prepare df for randomizations
sn_full <- subset(dfpg_noEOY, !is.na(ecfin))
sn_dupes <- subset(sn_full, select = c(ysp, country, ecfin))
sn <- sn_dupes[!duplicated(sn_dupes)]
sn <- dplyr::rename(sn, ecfin_rand = ecfin)

countrynames <- unique(sn$country)


handlers(global = TRUE)
handlers("txtprogressbar") # simple console bar

set.seed(42)
n_iter <- 10000
coef <- numeric(n_iter)

dfp_noEOY <- dfp_noEOY %>%
  mutate(
    log_pop = log(pop_int),
    log_gdp = log(gdp),
    gdppc = gdp / pop_int
  )

plan(multisession, workers = 4)

with_progress({
  p <- progressor(along = 1:n_iter)

  coef <- future_sapply(1:n_iter, function(i) {
    perm <- sample(countrynames)
    dict <- setNames(perm, countrynames)

    df <- sn
    df$country <- dict[df$country]

    dfp_rand <- merge(dfp_noEOY, df, by = c("country", "ysp"), all.x = TRUE)

    rand <- feols(
      log(err_sq) ~
        ecfin_rand + log_pop + log_gdp + gdppc | country + ysp + title + py,
      data = dfp_rand,
      notes = FALSE
    )

    p() # update progress
    rand$coefficients[1]
  })
})

print("All iterations finished ✅")


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
