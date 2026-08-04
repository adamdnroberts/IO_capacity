# Builds the "guiding rate": the benchmark share of Commission staff a member
# state would hold under geographically balanced recruitment, and the gap
# between that benchmark and the state's actual share.
#
# The rate is the simple average of three shares, following the Commission's own
# stated approach to geographic balance:
#   - the state's Council voting weight, as a share of the total
#   - its seats in the European Parliament, as a share of the total
#   - its population, as a share of the total
#
# diff_iv = rate_commission - rate, i.e. over-representation (positive) or
# under-representation (negative) at the Commission relative to that benchmark,
# with four lags. Consumed by create_dataset.R and used in the marginal-effects
# and interaction analyses.
#
# Run AFTER create_population_variable.R and create_ecfin_variable.R: it needs
# data/population.csv and data/staff_nat.Rdata.
#
# Recovered from code/guiderate.R, which was deleted in commit 3f6709f
# ("cleaning up code for submission", 2025-10-02) while data/guide_rate.Rdata
# remained in the repo -- leaving the main specification depending on a dataset
# no code could rebuild. The legacy tail of that script (which rebuilt
# final_dataset_euro_plus_guide and wrote presentation figures) is not restored:
# create_dataset.R now does the pooling, and those outputs are obsolete.

library(dplyr)
library(data.table)
library(zoo)

datapath <- "data/"

pop <- read.csv(paste0(datapath, "population.csv"))
EP <- read.csv(paste0(datapath, "EP.csv"))
council <- read.csv(paste0(datapath, "Council.csv"))

# --- European Parliament seat shares -----------------------------------------
# EP.csv holds seat counts at each apportionment. Build one frame per
# apportionment year, then carry the last observation forward across the panel.
ep_years <- c(2009, 2011, 2014, 2020)
ep_cols <- paste0("X", ep_years)

temp <- do.call(rbind, Map(function(yr, col) {
  data.frame(
    country = EP$State,
    ysp = yr,
    mep = as.numeric(EP[[col]])
  )
}, ep_years, ep_cols))

temp <- temp %>%
  group_by(ysp) %>%
  mutate(pct_mep = mep / sum(mep, na.rm = TRUE) * 100) %>%
  ungroup()

cysp <- expand.grid(
  country = unique(pop$country),
  ysp = unique(pop$ysp),
  stringsAsFactors = FALSE
)

ep_full <- full_join(temp, cysp, by = join_by(ysp, country)) %>%
  arrange(country, ysp) %>%
  group_by(country) %>%
  mutate(
    mep = zoo::na.locf(mep, na.rm = FALSE),
    pct_mep = zoo::na.locf(pct_mep, na.rm = FALSE)
  ) %>%
  ungroup()

# --- Council voting weight and population shares ------------------------------
# Shares are recomputed each period over the states actually in the Union then,
# so Croatia is absent before its 2013 accession and the UK after Brexit.
council_pop <- merge(x = pop, y = council, by = "country")

not_a_member <- with(
  council_pop,
  (country == "Croatia" & ysp < 2014) |
    (country == "United Kingdom" & ysp >= 2018)
)
council_pop$weight[not_a_member] <- NA
council_pop$pop[not_a_member] <- NA

council_pop <- council_pop %>%
  dplyr::filter(!is.na(weight)) %>%
  dplyr::group_by(ysp) %>%
  dplyr::mutate(
    pct_weight = weight / sum(weight) * 100,
    pct_pop = pop_int / sum(pop_int) * 100
  ) %>%
  ungroup()

guide <- merge(x = council_pop, y = ep_full, by = c("country", "ysp"))
guide$rate <- round((guide$pct_weight + guide$pct_mep + guide$pct_pop) / 3, 1)

# --- Gap between actual and benchmark representation, plus lags ---------------
load(paste0(datapath, "staff_nat.Rdata"))

gne <- merge(staff_nat, as.data.table(guide), by = c("country", "ysp"))
gne$diff_iv <- gne$rate_commission - gne$rate

setDT(gne)[,
  c("diff_lag1", "diff_lag2", "diff_lag3", "diff_lag4") := .(
    shift(diff_iv, 1L, fill = NA, type = "lag"),
    shift(diff_iv, 2L, fill = NA, type = "lag"),
    shift(diff_iv, 3L, fill = NA, type = "lag"),
    shift(diff_iv, 4L, fill = NA, type = "lag")
  ),
  by = country
]

gne_merge <- subset(
  gne,
  select = c(country, ysp, diff_iv, diff_lag1, diff_lag2, diff_lag3, diff_lag4)
)

# country x ysp must be a unique key: create_dataset.R joins on it as
# many-to-one, and a duplicate here would silently fan out the panel.
stopifnot(!anyDuplicated(gne_merge[, c("country", "ysp")]))

save(gne_merge, file = paste0(datapath, "guide_rate.Rdata"))
