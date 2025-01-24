library(ggplot2)
library(tidyverse)

fg <- read.csv("C:/Users/adamd/Downloads/function_group_DG.csv")

fg_totals <- tail(fg, n=1)

fg <- fg[-nrow(fg),]

adplot <- ggplot() +
  geom_histogram(aes(ADtotal_pct), data = fg) +
  geom_histogram(aes(ADtotal_pct), fill = "red", alpha = 0.5, data = subset(fg, dg == "ECFIN")) +
  theme_bw()

fgp <- fg %>% select(contains("total_pct"))

list <- list()

var_names <- colnames(fgp)

dfp$gdp_quartile <- ntile(dfp$gdp, 4)



