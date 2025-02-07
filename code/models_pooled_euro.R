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

# pooled predictions

load(paste0(datapath,"final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

## RUN THIS STUFF EVERYTIME!
#exclude variables that are just sums of other variables
vars_to_exclude <- c(
  "Total current expenditure excluding interest: general government ",
  "Total current expenditure: general government " ,
  "Total current revenue: general government " ,
  "Total expenditure excluding interest: general government ",
  "Total expenditure: general government " ,
  "Total revenue: general government ", 
  "Net lending (+) or net borrowing (-) excluding gross fixed capital formation: general government ", 
  "Net lending (+) or net borrowing (-) excluding interest: general government ", 
  "Net lending (+) or net borrowing (-): general government ", 
  "Other capital expenditure, including capital transfers: general government ", 
  "Social transfers in kind ", 
  "Total expenditure: general government ",
  "Total tax burden excluding imputed social security contributions: total economy ",
  "Total tax burden including imputed social security contributions: total economy ")

dfpg <- dfpg %>%
  filter(!(title %in% vars_to_exclude))

dfpg$gdppc <- dfpg$gdp/dfpg$pop_int

#make subset datasets
dfpg_noA <- subset(dfpg, aeoy == 0)
dfpg_noEOY <- subset(dfpg, dfpg$py != 0)

#create quartiles
dfpg_noEOY$gdp_quartile <- ntile(dfpg_noEOY$gdp, 4)

##TABLE 1##
# Function to run models
run_models <- function(data) {
  list(
    all = feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py, data = data),
    rev = feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py, data = data %>% filter(rev == 1)),
    exp = feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc | country + ysp + title + py, data = data %>% filter(exp == 1))
  )
}

# Run models
models <- run_models(dfpg)
models_noA <- run_models(dfpg_noA)
models_noEOY <- run_models(dfpg_noEOY)

etable(models$all, models$rev, models$exp, tex = FALSE)
etable(models_noA$all, models_noA$rev, models_noA$exp, tex = FALSE)
etable(models_noEOY$all, models_noEOY$rev, models_noEOY$exp, tex = FALSE)

#plot
# Extract coefficients and confidence intervals
extract_coefs <- function(model) {
  coef <- coef(model)["ecfin"]
  ci <- confint(model)["ecfin", ]
  list(coef = coef, ci_lower = ci[1], ci_upper = ci[2])
}

# Combine coefficients into a data frame
coef_list <- list(
  all1 = extract_coefs(models$all),
  rev1 = extract_coefs(models$rev),
  exp1 = extract_coefs(models$exp),
  all2 = extract_coefs(models_noA$all),
  rev2 = extract_coefs(models_noA$rev),
  exp2 = extract_coefs(models_noA$exp),
  all3 = extract_coefs(models_noEOY$all),
  rev3 = extract_coefs(models_noEOY$rev),
  exp3 = extract_coefs(models_noEOY$exp)
)

coef_df <- do.call(rbind, lapply(names(coef_list), function(name) {
  data.frame(
    Model = rep(strsplit(name, "(?<=\\D)(?=\\d)", perl = TRUE)[[1]][1], 3),
    Specification = as.factor(rep(as.integer(gsub("\\D", "", name)), 3)),
    Coefficient = coef_list[[name]]$coef,
    CI_Lower = as.numeric(coef_list[[name]]$ci_lower),
    CI_Upper = as.numeric(coef_list[[name]]$ci_upper)
  )
}))

coef_df$Specification <- factor(coef_df$Specification, 
                                levels = c(1, 2, 3), 
                                labels = c("All forecasts", "Exclude Nov EOY", "Exclude All EOY"))

coef_df$mod_spec <- paste0(coef_df$Model, as.character(coef_df$Specification))

coef_df <- coef_df %>%
  mutate(Model = case_match(Model,
                        "rev" ~ "Revenue",
                        "exp" ~ "Expenditure",
                        "all" ~ "Both"))

# Plot with viridis colors
# ggplot(coef_df, aes(y = Model, x = Coefficient, color = Specification)) +
#   geom_point(position = position_dodge(width = -.75), size = 3) +
#   geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0.2, position = position_dodge(width = -.75), size = 1.5) +
#   geom_vline(xintercept = 0, color = "black", linetype = "solid") +
#   labs(title = "",
#        x = "Coefficient",
#        y = "") +
#   scale_color_viridis_d(option = "viridis") +
#   theme_minimal() +
#   theme(legend.position = "top",
#         legend.title = element_blank())

main_results <- ggplot(coef_df, aes(y = Model, x = Coefficient, color = Specification)) +
  geom_point(position = position_dodge(width = -.75), size = 3) +
  geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0.2, position = position_dodge(width = -.75)) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(title = "",
       x = "Coefficient",
       y = "") +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())
print(main_results)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/SYP Presentation/images/main_plot_no_UK.png", plot = main_results, width = 6, height = 4)

# Look at difference between underrepresented vs overrepresented
dfpg_noEOY$under <- ifelse(dfpg_noEOY$diff_iv <0, 1, 0)
dfpg_noEOY$under_quartile <- ntile(dfpg_noEOY$diff_iv, 4)

all_gdp <- feols(log(err_sq) ~ ecfin*diff_iv + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noEOY)
rev_gdp <- feols(log(err_sq) ~ ecfin*diff_iv + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, rev == 1))
exp_gdp <- feols(log(err_sq) ~ ecfin*diff_iv + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, exp == 1))

etable(all_gdp,rev_gdp,exp_gdp, tex=F)

#Oster sensitivity analysis
dfpg_noEOY$ln_err_sq <- log(dfpg_noEOY$err_sq)
dfpg_noEOY$ln_pop_int <- log(dfpg_noEOY$pop_int)
dfpg_noEOY$ln_gdp <- log(dfpg_noEOY$gdp)

dfpg_sens <- subset(dfpg_noEOY, ln_err_sq != -Inf & !is.na(ln_err_sq))

delta_sens <- o_delta_rsq(y = "ln_err_sq", x = "ecfin", con = "ln_pop_int + ln_gdp + gdppc + country + ysp + title + py", type = "lm", data = dfpg_sens)
print(delta_sens, n = 40)

sens_plot <- o_delta_rsq_viz(y = "ln_err_sq", x = "ecfin", con = "ln_pop_int + ln_gdp + gdppc + country + ysp + title + py", type = "lm", data = dfpg_sens)
  
sens_plot + labs(title = "Sensitivity analysis of Panel C Model 1")
  
#this took too long, so I ditched it
#sens_plot2 <- o_delta_boot_viz(y = "ln_err_sq", x = "ecfin", con = "ln_pop_int + ln_gdp + gdppc + country + ysp + title + py", type = "lm", data = dfpg_sens, sim = 1000, obs = 20000, rep = T, R2max = 0.7)

o_beta_rsq_viz(y = "ln_err_sq", x = "ecfin", con = "ln_pop_int + ln_gdp + gdppc + country + ysp + title + py", type = "lm", data = dfpg_sens)

#interaction w/ gdp (no EOY)

all_gdp <- feols(log(err_sq) ~ i(gdp_quartile,ecfin) + gdp_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noEOY)
rev_gdp <- feols(log(err_sq) ~ i(gdp_quartile,ecfin) + gdp_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, rev == 1))
exp_gdp <- feols(log(err_sq) ~ i(gdp_quartile,ecfin) + gdp_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, exp == 1))

etable(all_gdp,rev_gdp,exp_gdp, tex=F)

# Plot with grayscale colors
grayscale_colors <- gray.colors(3, start = 0.2, end = 0.8)
png("C:/Users/adamd/Dropbox/Apps/Overleaf/SYP Presentation/images/gdp_quartile_plot.png", width = 720, height = 480)
par(cex.axis = 1.5, cex.lab = 1.5)
iplot(list(all_gdp, rev_gdp, exp_gdp), main = "", xlab = "GDP Quartile", col = grayscale_colors, cex = 2)
legend("topright", col = grayscale_colors, pch = c(20, 17, 15), lwd = 1, lty = 1, legend = c("All", "Revenue", "Expenditure"), cex = 1.5)
dev.off()

linearHypothesis(rev_gdp, "gdp_quartile::1:ecfin - gdp_quartile::2:ecfin = 0")
linearHypothesis(rev_gdp, "gdp_quartile::1:ecfin - gdp_quartile::3:ecfin = 0")
linearHypothesis(rev_gdp, "gdp_quartile::1:ecfin - gdp_quartile::4:ecfin = 0")

#gdppc quartiles (no EOY)
dfpg_noEOY$gdppc_quartile <- ntile(dfpg_noEOY$gdppc, 4)

all_gdppc <- feols(log(err_sq) ~ i(gdppc_quartile,ecfin) + gdppc_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noEOY)
rev_gdppc <- feols(log(err_sq) ~ i(gdppc_quartile,ecfin) + gdppc_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, rev == 1))
exp_gdppc <- feols(log(err_sq) ~ i(gdppc_quartile,ecfin) + gdppc_quartile + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, exp == 1))

etable(all_gdppc,rev_gdppc,exp_gdppc, tex=F)

grayscale_colors <- gray.colors(3, start = 0.2, end = 0.8)
png("C:/Users/adamd/Dropbox/Apps/Overleaf/SYP Presentation/images/gdppc_quartile_plot.png", width = 720, height = 480)
par(cex.axis = 1.5, cex.lab = 1.5)
iplot(list(all_gdppc,rev_gdppc,exp_gdppc), main = "", xlab = "GDP per capita Quartile", col = grayscale_colors, cex = 2)
legend("topleft", col = grayscale_colors, pch = c(20, 17, 15), lwd = 1, lty = 1, legend = c("All", "Revenue", "Expenditure"), cex = 1.5)
dev.off()


## APPENDIX

#with ecfin interpolated, linear interpolation

#table 1
all <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg)
rev <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg, rev == 1))
exp <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg, exp == 1))

etable(all,rev,exp, tex=F)

dfpg_noA <- subset(dfpg, aeoy == 0)

all2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noA)
rev2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noA, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noA, exp == 1))

etable(all2,rev2,exp2, tex = F)

dfpg_noEOY <- subset(dfpg, dfpg$py != 0)

all3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noEOY)
rev3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, exp == 1))

etable(all3,rev3,exp3, tex = T)

#with ecfin interpolated, spline interpolation

#table 1
all <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg)
rev <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg, rev == 1))
exp <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg, exp == 1))

etable(all,rev,exp, tex=F)

dfpg_noA <- subset(dfpg, aeoy == 0)

all2 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noA)
rev2 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noA, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noA, exp == 1))

etable(all2,rev2,exp2, tex = T)

dfpg_noEOY <- subset(dfpg, dfpg$py != 0)

all3 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noEOY)
rev3 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin_spline + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY, exp == 1))

etable(all3,rev3,exp3, tex = T)

#remove pre 2015 for appendix

dfpg_2015 <- subset(dfpg, ysp >= 2015)

all <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_2015)
rev <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_2015, rev == 1))
exp <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_2015, exp == 1))

etable(all,rev,exp, tex=F)
etable(all,rev,exp, tex=T)

dfpg_noA2015 <- subset(dfpg_2015, aeoy == 0)

all2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noA2015)
rev2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noA2015, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noA2015, exp == 1))

etable(all2,rev2,exp2, tex = F)

dfpg_noEOY2015 <- subset(dfpg_2015, py != 0)

all3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noEOY2015)
rev3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY2015, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin_int + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOY2015, exp == 1))

etable(all3,rev3,exp3, tex = F)

#remove 2020, 2021, and 2022 forecasts for appendix

dfpg_covid <- subset(dfpg, !(ysp == 2018.5 & py == 2) & !(ysp == 2019 & py == 1) & !(ysp == 2019.5 & py == 1) & !(ysp == 2019.5 & py == 2) & !(ysp >= 2020 & ysp <= 2021.5))

all <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_covid)
rev <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_covid, rev == 1))
exp <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_covid, exp == 1))

etable(all,rev,exp, tex=F)
etable(all,rev,exp, tex=T)

dfpg_noAcovid <- subset(dfpg_covid, aeoy == 0)

all2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noAcovid)
rev2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noAcovid, rev == 1))
exp2 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noAcovid, exp == 1))

etable(all2,rev2,exp2, tex = F)

dfpg_noEOYcovid <- subset(dfpg_covid, py != 0)

all3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = dfpg_noEOYcovid)
rev3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOYcovid, rev == 1))
exp3 <- feols(log(err_sq) ~ ecfin + log(pop_int) + log(gdp) + gdppc|country+ysp+title+py, data = subset(dfpg_noEOYcovid, exp == 1))

etable(all3,rev3,exp3, tex = T)

# Run models
models <- run_models(dfpg_robust)
models_noA <- run_models(dfpg_robust_noA)
models_noEOY <- run_models(dfpg_robust_noEOY)

etable(models$all, models$rev, models$exp, tex = FALSE)
etable(models_noA$all, models_noA$rev, models_noA$exp, tex = FALSE)
etable(models_noEOY$all, models_noEOY$rev, models_noEOY$exp, tex = FALSE)

#plot

# Combine coefficients into a data frame
coef_list <- list(
  all1 = extract_coefs(models$all),
  rev1 = extract_coefs(models$rev),
  exp1 = extract_coefs(models$exp),
  all2 = extract_coefs(models_noA$all),
  rev2 = extract_coefs(models_noA$rev),
  exp2 = extract_coefs(models_noA$exp),
  all3 = extract_coefs(models_noEOY$all),
  rev3 = extract_coefs(models_noEOY$rev),
  exp3 = extract_coefs(models_noEOY$exp)
)

coef_df <- do.call(rbind, lapply(names(coef_list), function(name) {
  data.frame(
    Model = rep(strsplit(name, "(?<=\\D)(?=\\d)", perl = TRUE)[[1]][1], 3),
    Specification = as.factor(rep(as.integer(gsub("\\D", "", name)), 3)),
    Coefficient = coef_list[[name]]$coef,
    CI_Lower = as.numeric(coef_list[[name]]$ci_lower),
    CI_Upper = as.numeric(coef_list[[name]]$ci_upper)
  )
}))

coef_df$Specification <- factor(coef_df$Specification, 
                                levels = c(1, 2, 3), 
                                labels = c("All forecasts", "Exclude Nov EOY", "Exclude All EOY"))

coef_df$mod_spec <- paste0(coef_df$Model, as.character(coef_df$Specification))

coef_df <- coef_df %>%
  mutate(Model = case_match(Model,
                            "rev" ~ "Revenue",
                            "exp" ~ "Expenditure",
                            "all" ~ "Both"))

Exclude_Belgium_UK <- ggplot(coef_df, aes(y = Model, x = Coefficient, color = Specification)) +
  geom_point(position = position_dodge(width = -.75), size = 3) +
  geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0.2, position = position_dodge(width = -.75)) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(title = "",
       x = "Coefficient",
       y = "") +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())
print(Exclude_Belgium_UK)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/SYP Presentation/images/Exclude_Belgium_UK.png", plot = gdp_results, width = 6, height = 4)

#re-run with belgium/UK excluded
# Function to run models

dfpg_robust <- subset(dfpg, country != "United Kingdom" & country != "Belgium")
dfpg_robust_noA <- subset(dfpg_noA, country != "United Kingdom" & country != "Belgium")
dfpg_robust_noEOY <- subset(dfpg_noEOY, country != "United Kingdom" & country != "Belgium")


# Run models
models <- run_models(dfpg_robust)
models_noA <- run_models(dfpg_robust_noA)
models_noEOY <- run_models(dfpg_robust_noEOY)

etable(models$all, models$rev, models$exp, tex = FALSE)
etable(models_noA$all, models_noA$rev, models_noA$exp, tex = FALSE)
etable(models_noEOY$all, models_noEOY$rev, models_noEOY$exp, tex = FALSE)

#plot

# Combine coefficients into a data frame
coef_list <- list(
  all1 = extract_coefs(models$all),
  rev1 = extract_coefs(models$rev),
  exp1 = extract_coefs(models$exp),
  all2 = extract_coefs(models_noA$all),
  rev2 = extract_coefs(models_noA$rev),
  exp2 = extract_coefs(models_noA$exp),
  all3 = extract_coefs(models_noEOY$all),
  rev3 = extract_coefs(models_noEOY$rev),
  exp3 = extract_coefs(models_noEOY$exp)
)

coef_df <- do.call(rbind, lapply(names(coef_list), function(name) {
  data.frame(
    Model = rep(strsplit(name, "(?<=\\D)(?=\\d)", perl = TRUE)[[1]][1], 3),
    Specification = as.factor(rep(as.integer(gsub("\\D", "", name)), 3)),
    Coefficient = coef_list[[name]]$coef,
    CI_Lower = as.numeric(coef_list[[name]]$ci_lower),
    CI_Upper = as.numeric(coef_list[[name]]$ci_upper)
  )
}))

coef_df$Specification <- factor(coef_df$Specification, 
                                levels = c(1, 2, 3), 
                                labels = c("All forecasts", "Exclude Nov EOY", "Exclude All EOY"))

coef_df$mod_spec <- paste0(coef_df$Model, as.character(coef_df$Specification))

coef_df <- coef_df %>%
  mutate(Model = case_match(Model,
                            "rev" ~ "Revenue",
                            "exp" ~ "Expenditure",
                            "all" ~ "Both"))

Exclude_Belgium_UK <- ggplot(coef_df, aes(y = Model, x = Coefficient, color = Specification)) +
  geom_point(position = position_dodge(width = -.75), size = 3) +
  geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0.2, position = position_dodge(width = -.75)) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  labs(title = "",
       x = "Coefficient",
       y = "") +
  scale_color_grey(start = 0.2, end = 0.8) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank())
print(Exclude_Belgium_UK)

ggsave(filename = "C:/Users/adamd/Dropbox/Apps/Overleaf/SYP Presentation/images/Exclude_Belgium_UK.png", plot = gdp_results, width = 6, height = 4)