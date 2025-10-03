library(data.table)
library(dplyr)
library(fixest)
library(ggplot2)
library(marginaleffects)

datapath = "~/ec_project/data/"

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

#make subset datasets
dfpg_noA <- dfpg %>% filter(aeoy == 0)

##TABLE 1##
# Function to run models
run_models <- function(data) {
  list(
    rev = feols(
      log(err_sq) ~
        ecfin *
          diff_iv +
          log(pop_int) +
          log(gdp) +
          gdppc |
          country + ysp + title + py,
      data = data %>% filter(rev == 1)
    ),
    exp = feols(
      log(err_sq) ~
        ecfin *
          diff_iv +
          log(pop_int) +
          log(gdp) +
          gdppc |
          country + ysp + title + py,
      data = data %>% filter(exp == 1)
    )
  )
}

# Run models
models_noA <- run_models(dfpg_noA)

etable(
  models_noA$rev,
  models_noA$exp,
  tex = FALSE,
  digits = 3,
  digits.stats = 3
)

# Average marginal effects of ecfin across diff_iv values
mfx_rev <- slopes(models_noA$rev, variables = "ecfin", by = "diff_iv")
mfx_exp <- slopes(models_noA$exp, variables = "ecfin", by = "diff_iv")

mfx_rev$Forecast <- "Revenue"
mfx_exp$Forecast <- "Expenditure"

mfx <- rbind(mfx_rev, mfx_exp)

marginal_effects <- ggplot(mfx, aes(x = diff_iv, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_line(aes(color = Forecast)) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = Forecast),
    alpha = 0.2
  ) +
  labs(
    y = "Marginal Effect of National Expertise",
    x = "Representation (percentage points)"
  ) +
  theme_minimal()

print(marginal_effects)

ggsave(
  filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/images/marginal_effects_by_guiderate_plot.pdf",
  plot = marginal_effects,
  width = 6,
  height = 4
)
