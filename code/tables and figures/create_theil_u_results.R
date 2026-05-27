library(ggplot2)
library(fixest)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)

datapath = "~/EU_Capacity/data/"

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

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

# Theil's U: forecast error relative to naive no-change benchmark.
# forecast_year = calendar year being forecast (floor(ysp) + py).
# true_naive = prior year's realized value for that country-title.
# theil_u = |err| / |true - true_naive|; values < 1 mean the EC forecast
# beats the naive benchmark.
dfpg <- dfpg %>% mutate(forecast_year = floor(ysp) + py)

true_lookup <- dfpg %>%
  group_by(country, title, forecast_year) %>%
  dplyr::summarise(true_naive = first(true), .groups = "drop") %>%
  mutate(forecast_year = forecast_year + 1)

dfpg <- left_join(dfpg, true_lookup, by = c("country", "title", "forecast_year"))
dfpg$theil_u <- abs(dfpg$err) / abs(dfpg$true - dfpg$true_naive)

# Subset datasets
dfpg_noA   <- dfpg %>% filter(aeoy == 0)
dfpg_noEOY <- dfpg %>% filter(py != 0)

##DESCRIPTIVE PLOT##
theil_descriptive <- dfpg %>%
  filter(rev == 1 | exp == 1, is.finite(theil_u), theil_u > 0) %>%
  mutate(type = if_else(rev == 1, "Revenue", "Expenditure")) %>%
  group_by(py, type) %>%
  dplyr::summarise(mean_theil = mean(theil_u, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(py), y = mean_theil, fill = type)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(x = "Projection horizon (years ahead)", y = "Mean Theil's U", fill = "") +
  theme_minimal() +
  theme(legend.position = "top")

print(theil_descriptive)
ggsave(
  filename = "C:/Users/adamd/Documents/EU_Capacity/overleaf/images/theil_u_descriptive.pdf",
  plot = theil_descriptive,
  width = 6, height = 4
)

##REGRESSION MODELS##
extract_coefs <- function(model) {
  coef <- coef(model)["ecfin"]
  ci90 <- confint(model, level = 0.9)["ecfin", ]
  ci95 <- confint(model, level = 0.95)["ecfin", ]
  list(
    coef        = coef,
    ci_lower90  = ci90[1],
    ci_upper90  = ci90[2],
    ci_lower95  = ci95[1],
    ci_upper95  = ci95[2]
  )
}

run_models_theil <- function(data) {
  list(
    rev = feols(
      log(theil_u) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1, is.finite(log(theil_u)), theil_u > 0)
    ),
    exp = feols(
      log(theil_u) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(exp == 1, is.finite(log(theil_u)), theil_u > 0)
    ),
    all = feols(
      log(theil_u) ~
        ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py,
      data = data %>% filter(rev == 1 | exp == 1, is.finite(log(theil_u)), theil_u > 0)
    )
  )
}

models_theil       <- run_models_theil(dfpg)
models_theil_noA   <- run_models_theil(dfpg_noA)
models_theil_noEOY <- run_models_theil(dfpg_noEOY)

etable(models_theil$rev, models_theil$exp, tex = FALSE)
etable(models_theil_noA$rev, models_theil_noA$exp, tex = FALSE)
etable(models_theil_noEOY$rev, models_theil_noEOY$exp, tex = FALSE)

##COEFFICIENT PLOT##
coef_list_theil <- list(
  rev1 = extract_coefs(models_theil$rev),
  exp1 = extract_coefs(models_theil$exp),
  all1 = extract_coefs(models_theil$all),
  rev2 = extract_coefs(models_theil_noA$rev),
  exp2 = extract_coefs(models_theil_noA$exp),
  all2 = extract_coefs(models_theil_noA$all),
  rev3 = extract_coefs(models_theil_noEOY$rev),
  exp3 = extract_coefs(models_theil_noEOY$exp),
  all3 = extract_coefs(models_theil_noEOY$all)
)

coef_df_theil <- do.call(
  rbind,
  lapply(names(coef_list_theil), function(name) {
    data.frame(
      Model         = rep(strsplit(name, "(?<=\\D)(?=\\d)", perl = TRUE)[[1]][1], 3),
      Specification = as.factor(rep(as.integer(gsub("\\D", "", name)), 3)),
      Coefficient   = coef_list_theil[[name]]$coef,
      CI_Lower90    = as.numeric(coef_list_theil[[name]]$ci_lower90),
      CI_Upper90    = as.numeric(coef_list_theil[[name]]$ci_upper90),
      CI_Lower95    = as.numeric(coef_list_theil[[name]]$ci_lower95),
      CI_Upper95    = as.numeric(coef_list_theil[[name]]$ci_upper95)
    )
  })
)

coef_df_theil$Specification <- factor(
  coef_df_theil$Specification,
  levels = c(1, 2, 3),
  labels = c("All forecasts", "Exclude Nov EOY", "Exclude All EOY")
)

coef_df_theil <- coef_df_theil %>%
  mutate(
    Model       = case_match(Model, "rev" ~ "Revenue", "exp" ~ "Expenditure", "all" ~ "All"),
    Coefficient = 100 * (exp(Coefficient) - 1),
    CI_Lower90  = 100 * (exp(CI_Lower90) - 1),
    CI_Upper90  = 100 * (exp(CI_Upper90) - 1),
    CI_Lower95  = 100 * (exp(CI_Lower95) - 1),
    CI_Upper95  = 100 * (exp(CI_Upper95) - 1)
  )

plot_theil <- ggplot(
  subset(coef_df_theil, Model != "Both"),
  aes(y = Model, x = Coefficient, color = Specification)
) +
  geom_errorbar(
    aes(xmin = CI_Lower95, xmax = CI_Upper95),
    width = 0, linewidth = 0.5,
    position = position_dodge(width = -.75)
  ) +
  geom_errorbar(
    aes(xmin = CI_Lower90, xmax = CI_Upper90),
    width = 0, linewidth = 2, alpha = 0.4,
    position = position_dodge(width = -.75)
  ) +
  geom_point(position = position_dodge(width = -.75), size = 3) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(title = "", x = "% Change in Theil's U", y = "") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

print(plot_theil)
ggsave(
  filename = "C:/Users/adamd/Documents/EU_Capacity/overleaf/images/theil_u_plot.pdf",
  plot = plot_theil,
  width = 6, height = 4
)
