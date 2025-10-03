# Load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(data.table)
library(countrycode)
library(purrr)

# -------------------------
# 1. Load and combine base datasets
# -------------------------
load("~/ec_project/data/full_dataset11_14.Rdata")
load("~/ec_project/data/full_dataset15_23.Rdata")

df_full <- bind_rows(full_dataset11_14, full_dataset15_23) %>%
  mutate(
    ysp = year + 0.5 * (1 - spring),
    country_s = countryname(country, destination = "country.name"),
    country = if_else(country == "Czech Republic", "Czechia", country)
  ) %>%
  dplyr::rename(date_pred = date)

# Split title into two parts
title_split <- as.data.frame(str_split_fixed(df_full$title, ":-", 2)) %>%
  dplyr::rename(title = V1, esa = V2)
df_full_spl <- bind_cols(select(df_full, -title), title_split)

# -------------------------
# 2. Merge with external data
# -------------------------
load("~/ec_project/data/staff_nat.Rdata")
pop <- read.csv("~/ec_project/data/population.csv")
gdp <- read.csv("~/ec_project/data/gdp.csv") %>%
  select(-c(iso2c, year, spring))

# Sequential merges
df_merged3 <- reduce(
  list(df_full_spl, staff_nat, pop, gdp),
  ~ full_join(.x, .y, by = c("country", "ysp"))
)

# -------------------------
# 3. Clean and classify fiscal variables
# -------------------------
df_merged3 <- df_merged3 %>%
  mutate(
    title = str_replace(
      title,
      "Interest : general government ",
      "Interest: general government "
    ),
    edp = as.integer(str_detect(subchapter, "Excessive deficit procedure"))
  )

rev <- unique(df_merged3$title[
  df_merged3$subchapter == "01 Revenue (ESA 2010)" & df_merged3$edp == 0
])
exp <- unique(df_merged3$title[
  df_merged3$subchapter == "02 Expenditure (ESA 2010)" & df_merged3$edp == 0
])
lend <- unique(df_merged3$title[
  df_merged3$subchapter == "03 Net lending (ESA 2010)" & df_merged3$edp == 0
])

df_merged3 <- df_merged3 %>%
  mutate(
    rev = as.integer(title %in% rev),
    exp = as.integer(title %in% exp),
    lend = as.integer(title %in% lend),
    rev2 = as.integer(str_detect(subchapter, "Revenue")),
    exp2 = as.integer(str_detect(subchapter, "Expenditure")),
    lend2 = as.integer(str_detect(subchapter, "Net lending"))
  )

# -------------------------
# 4. Forecast error calculations
# -------------------------
df <- df_merged3 %>%
  mutate(
    # forecast errors and squared errors
    err0 = p0 - true0,
    err0_sq = (p0 - true0)^2,
    err1 = p1 - true1,
    err1_sq = (p1 - true1)^2,
    err2 = p2 - true2,
    err2_sq = (p2 - true2)^2,

    # error direction (1 = overpredict, 0 = underpredict)
    pos_err0 = case_when(err0 > 0 ~ 1, err0 < 0 ~ 0),
    pos_err1 = case_when(err1 > 0 ~ 1, err1 < 0 ~ 0),
    pos_err2 = case_when(err2 > 0 ~ 1, err2 < 0 ~ 0),

    # GDP per capita
    gdppc = gdp / pop_int
  )

# -------------------------
# 5. Euro-only subset + guiding rate
# -------------------------
df_euro <- dplyr::filter(df, unit == "Mrd ECU/EUR")

load("~/ec_project/data/guide_rate.Rdata")

dfg <- full_join(df_euro, gne_merge, by = c("country", "ysp")) %>%
  select(which(!duplicated(names(.))))

p0 <- subset(
  dfg,
  select = c(
    "ecfin",
    "ecfin_int",
    "ecfin_spline",
    "pop_int",
    "gdp",
    "country",
    "ysp",
    "title",
    "err0",
    "err0_sq",
    "rev",
    "exp",
    "lend",
    "diff_iv",
    "diff_lag1",
    "diff_lag2",
    "diff_lag3",
    "diff_lag4"
  )
)
p0$py <- 0
p0$err_sq <- p0$err0_sq
p0$err <- p0$err0

p1 <- subset(
  dfg,
  select = c(
    "ecfin",
    "ecfin_int",
    "ecfin_spline",
    "pop_int",
    "gdp",
    "country",
    "ysp",
    "title",
    "err1",
    "err1_sq",
    "rev",
    "exp",
    "lend",
    "diff_iv",
    "diff_lag1",
    "diff_lag2",
    "diff_lag3",
    "diff_lag4"
  )
)
p1$py <- 1
p1$err_sq <- p1$err1_sq
p1$err <- p1$err1

p2 <- subset(
  dfg,
  select = c(
    "ecfin",
    "ecfin_int",
    "ecfin_spline",
    "pop_int",
    "gdp",
    "country",
    "ysp",
    "title",
    "err2",
    "err2_sq",
    "rev",
    "exp",
    "lend",
    "diff_iv",
    "diff_lag1",
    "diff_lag2",
    "diff_lag3",
    "diff_lag4"
  )
)
p2$py <- 2
p2$err_sq <- p2$err2_sq
p2$err <- p2$err2

p_list <- list(p0, p1, p2)
dfpg <- do.call(plyr::rbind.fill, p_list)

dfpg$aeoy <- 0
dfpg$aeoy[dfpg$py == 0 & dfpg$ysp %% 1 == 0.5] <- 1

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
  filter(!(title %in% vars_to_exclude)) %>%
  mutate(
    gdppc = gdp / pop_int
  )

save(
  dfpg,
  file = paste0("~/ec_project/data/final_dataset_euro_pooled_plus_guide.Rdata")
)

write.csv(
  dfpg,
  file = paste0("~/ec_project/data/EU_Capacity_dataset.csv")
)
