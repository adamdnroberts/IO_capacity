library(ggplot2)
library(fixest)
library(dplyr)
library(plyr)
library(tidyr)
library(data.table)
library(car)
library(viridis)
library(purrr)
library(stringr)

datapath = "~/ec_project/data/"

# pooled predictions

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

## RUN THIS STUFF EVERYTIME!
#exclude variables that are just sums of other variables
vars_to_exclude <- c(
  "Total current expenditure excluding interest: general government ",
  "Total current expenditure: general government ",
  "Total current revenue: general government ",
  "Total expenditure excluding interest: general government ",
  "Total expenditure: general government ",
  "Total revenue: general government ",
  "Net lending (+) or net borrowing (-) excluding gross fixed capital formation: general government ",
  "Net lending (+) or net borrowing (-) excluding interest: general government ",
  "Net lending (+) or net borrowing (-): general government ",
  "Other capital expenditure, including capital transfers: general government ",
  "Social transfers in kind ",
  "Total expenditure: general government ",
  "Total tax burden excluding imputed social security contributions: total economy ",
  "Total tax burden including imputed social security contributions: total economy "
)

dfpg <- dfpg %>%
  filter(!(title %in% vars_to_exclude))

dfpg$gdppc <- dfpg$gdp / dfpg$pop_int

#make subset datasets
dfpg_noA <- subset(dfpg, aeoy == 0)
dfpg_noX <- subset(dfpg, dfpg$py != 0)

#create quartiles
dfpg_noA$gdp_quartile <- ntile(dfpg_noA$gdp, 4)

rev_gdp <- feols(
  log(err_sq) ~
    i(gdp_quartile, ecfin) +
      gdp_quartile +
      log(pop_int) +
      log(gdp) +
      gdppc |
      country + ysp + title + py,
  data = subset(dfpg_noA, rev == 1)
)
exp_gdp <- feols(
  log(err_sq) ~
    i(gdp_quartile, ecfin) +
      gdp_quartile +
      log(pop_int) +
      log(gdp) +
      gdppc |
      country + ysp + title + py,
  data = subset(dfpg_noA, exp == 1)
)

# Test whether the interaction terms are equal
linearHypothesis(
  rev_gdp,
  c(
    "gdp_quartile::3:ecfin = gdp_quartile::1:ecfin",
    "gdp_quartile::3:ecfin = gdp_quartile::2:ecfin",
    "gdp_quartile::4:ecfin = gdp_quartile::1:ecfin",
    "gdp_quartile::4:ecfin = gdp_quartile::2:ecfin"
  )
)


#plot
# Extract coefficients and confidence intervals
extract_coefs <- function(model) {
  coef <- coef(model)[grep("ecfin", names(coef(model)), ignore.case = TRUE)]
  ci90 <- confint(model, level = 0.9)[
    grep("ecfin", names(coef(model)), ignore.case = TRUE),
  ]
  ci95 <- confint(model, level = 0.95)[
    grep("ecfin", names(coef(model)), ignore.case = TRUE),
  ]
  list(
    coef = coef,
    ci_lower90 = ci90[1],
    ci_upper90 = ci90[2],
    ci_lower95 = ci95[1],
    ci_upper95 = ci95[2]
  )
}

models <- list(rev_gdp, exp_gdp)

# Combine coefficients into a data frame
coef_list_gdp <- list(
  rev = extract_coefs(rev_gdp),
  exp = extract_coefs(exp_gdp)
)

coef_df_gdp <- map_dfr(names(coef_list_gdp), function(name) {
  tibble(
    Model = str_extract(name, "\\D+"),
    Specification = as.integer(str_extract(name, "\\d+")),
    q = factor(1:4),
    coef = coef_list_gdp[[name]]$coef,
    low90 = coef_list_gdp[[name]]$ci_lower90[, 1],
    high90 = coef_list_gdp[[name]]$ci_upper90[, 1],
    low95 = coef_list_gdp[[name]]$ci_lower95[, 1],
    high95 = coef_list_gdp[[name]]$ci_upper95[, 1]
  )
})

coef_df_gdp <- coef_df_gdp %>%
  mutate(
    Model = case_match(
      Model,
      "rev" ~ "Revenue",
      "exp" ~ "Expenditure"
    )
  )

#save(coef_df, file = "~/ec_project/data/interaction_results_coef_df.Rdata")

interaction_result_gdp <- ggplot(
  subset(coef_df_gdp, Model != "Both"),
  aes(x = q, y = coef, color = Model)
) +
  # 95% CI - thinner error bars
  geom_errorbar(
    aes(ymin = low95, ymax = high95),
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = .75)
  ) +
  # 90% CI - fatter error bars
  geom_errorbar(
    aes(ymin = low90, ymax = high90),
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = .75)
  ) +
  geom_point(position = position_dodge(width = .75), size = 3) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  labs(
    title = "",
    x = "GDP Quartile",
    y = "",
    caption = "Thick lines: 90% confidence intervals, thin lines: 95% confidence intevals"
  ) +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

print(interaction_results_gdp)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/images/gdp_quartile_plot.pdf",
  plot = interaction_results_gdp,
  width = 6,
  height = 4
)

#gdppc quartiles (no EOY)
dfpg_noA$gdppc_quartile <- ntile(dfpg_noA$gdppc, 4)

rev_gdppc <- feols(
  log(err_sq) ~
    i(gdppc_quartile, ecfin) +
      gdppc_quartile +
      log(pop_int) +
      log(gdp) +
      gdppc |
      country + ysp + title + py,
  data = subset(dfpg_noA, rev == 1)
)
exp_gdppc <- feols(
  log(err_sq) ~
    i(gdppc_quartile, ecfin) +
      gdppc_quartile +
      log(pop_int) +
      log(gdp) +
      gdppc |
      country + ysp + title + py,
  data = subset(dfpg_noA, exp == 1)
)

etable(rev_gdppc, exp_gdppc, tex = F)

#plot
# Extract coefficients and confidence intervals
models <- list(rev_gdppc, exp_gdppc)

# Combine coefficients into a data frame
coef_list_gdppc <- list(
  rev = extract_coefs(rev_gdppc),
  exp = extract_coefs(exp_gdppc)
)

confint(rev_gdppc, level = 0.9)[
  grep("ecfin", names(coef(rev_gdppc)), ignore.case = TRUE),
]

coef_df_gdppc <- map_dfr(names(coef_list_gdppc), function(name) {
  tibble(
    Model = str_extract(name, "\\D+"),
    q = factor(1:4),
    coef = coef_list_gdppc[[name]]$coef,
    low90 = coef_list_gdppc[[name]]$ci_lower90[, 1],
    high90 = coef_list_gdppc[[name]]$ci_upper90[, 1],
    low95 = coef_list_gdppc[[name]]$ci_lower95[, 1],
    high95 = coef_list_gdppc[[name]]$ci_upper95[, 1]
  )
})

coef_df_gdppc <- coef_df_gdppc %>%
  mutate(
    Model = case_match(
      Model,
      "rev" ~ "Revenue",
      "exp" ~ "Expenditure"
    )
  )

#save(coef_df, file = "~/ec_project/data/interaction_results_coef_df.Rdata")

interaction_results_gdppc <- ggplot(
  subset(coef_df_gdppc, Model != "Both"),
  aes(x = q, y = coef, color = Model)
) +
  # 95% CI - thinner error bars
  geom_errorbar(
    aes(ymin = low95, ymax = high95),
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = .75)
  ) +
  # 90% CI - fatter error bars
  geom_errorbar(
    aes(ymin = low90, ymax = high90),
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = .75)
  ) +
  geom_point(position = position_dodge(width = .75), size = 3) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  labs(
    title = "",
    x = "GDP Per Capita Quartile",
    y = "",
    caption = "Thick lines: 90% confidence intervals, thin lines: 95% confidence intevals"
  ) +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

print(interaction_results_gdppc)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/images/gdppc_quartile_plot.pdf",
  plot = interaction_results_gdppc,
  width = 6,
  height = 4
)
