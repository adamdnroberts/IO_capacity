library(ggplot2)
library(fixest)
library(stargazer)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(car)
library(robomit)
library(viridis)

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
dfpg_noA <- dfpg %>% filter(aeoy == 0)
dfpg_noEOY <- dfpg %>% filter(py != 0)

#create quartiles
dfpg_noEOY$gdp_quartile <- ntile(dfpg_noEOY$gdp, 4)

##TABLE 1##
# Function to run models
run_models <- function(data) {
  list(
    rev = feols(
      log(err_sq) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1)
    ),
    exp = feols(
      log(err_sq) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(exp == 1)
    )
  )
}

# Run models
models <- run_models(dfpg)
models_noA <- run_models(dfpg_noA)
models_noEOY <- run_models(dfpg_noEOY)

etable(models$rev, models$exp, tex = FALSE)
etable(models_noA$rev, models_noA$exp, tex = FALSE)
etable(models_noEOY$rev, models_noEOY$exp, tex = FALSE)

#plot
# Extract coefficients and confidence intervals
extract_coefs <- function(model) {
  coef <- coef(model)["ecfin"]
  ci90 <- confint(model, level = 0.9)["ecfin", ]
  ci95 <- confint(model, level = 0.95)["ecfin", ]
  list(
    coef = coef,
    ci_lower90 = ci90[1],
    ci_upper90 = ci90[2],
    ci_lower95 = ci95[1],
    ci_upper95 = ci95[2]
  )
}

# Combine coefficients into a data frame
coef_list <- list(
  rev1 = extract_coefs(models$rev),
  exp1 = extract_coefs(models$exp),
  rev2 = extract_coefs(models_noA$rev),
  exp2 = extract_coefs(models_noA$exp),
  rev3 = extract_coefs(models_noEOY$rev),
  exp3 = extract_coefs(models_noEOY$exp)
)

coef_df <- do.call(
  rbind,
  lapply(names(coef_list), function(name) {
    data.frame(
      Model = rep(strsplit(name, "(?<=\\D)(?=\\d)", perl = TRUE)[[1]][1], 3),
      Specification = as.factor(rep(as.integer(gsub("\\D", "", name)), 3)),
      Coefficient = coef_list[[name]]$coef,
      CI_Lower90 = as.numeric(coef_list[[name]]$ci_lower90),
      CI_Upper90 = as.numeric(coef_list[[name]]$ci_upper90),
      CI_Lower95 = as.numeric(coef_list[[name]]$ci_lower95),
      CI_Upper95 = as.numeric(coef_list[[name]]$ci_upper95)
    )
  })
)

coef_df$Specification <- factor(
  coef_df$Specification,
  levels = c(1, 2, 3),
  labels = c("All forecasts", "Exclude Nov EOY", "Exclude All EOY")
)

coef_df$mod_spec <- paste0(coef_df$Model, as.character(coef_df$Specification))

coef_df <- coef_df %>%
  mutate(
    Model = case_match(
      Model,
      "rev" ~ "Revenue",
      "exp" ~ "Expenditure"
    ),
    Coefficient  = Coefficient  / 2,
    CI_Lower90   = CI_Lower90   / 2,
    CI_Upper90   = CI_Upper90   / 2,
    CI_Lower95   = CI_Lower95   / 2,
    CI_Upper95   = CI_Upper95   / 2
  )

main_results <- ggplot(
  subset(coef_df, Model != "Both"),
  aes(y = Model, x = Coefficient, color = Specification)
) +
  # 95% CI - thinner error bars
  geom_errorbar(
    aes(xmin = CI_Lower95, xmax = CI_Upper95),
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = -.75)
  ) +
  # 90% CI - fatter error bars
  geom_errorbar(
    aes(xmin = CI_Lower90, xmax = CI_Upper90),
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = -.75)
  ) +
  geom_point(position = position_dodge(width = -.75), size = 3) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(title = "", x = "% Change in Forecast Error", y = "") +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

print(main_results)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/images/main_plot.pdf",
  plot = main_results,
  width = 6,
  height = 4
)
