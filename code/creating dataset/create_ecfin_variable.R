library(dplyr)
library(zoo)

load("~/EU_capacity/data/Commission_nationalities.Rdata")

staff_nat_avg <- staff %>%
  group_by(country, spring, year) %>%
  dplyr::summarise(
    ecfin = mean(ECFIN, na.rm = TRUE),
    total = mean(total, na.rm = TRUE),
    date = first(date)
  )

tots <- staff_nat_avg %>%
  group_by(spring, year) %>%
  dplyr::summarise(
    total_ecfin = sum(ecfin, na.rm = TRUE),
    total_commission = sum(total, na.rm = TRUE)
  )

staff_nat_avg$pct_ecfin <- staff_nat_avg$ecfin / staff_nat_avg$total

staff_nat_avg <- merge(staff_nat_avg, tots, by = c("spring", "year"))
staff_nat_avg$rate_ecfin <- (staff_nat_avg$ecfin / staff_nat_avg$total_ecfin) *
  100
staff_nat_avg$rate_commission <- (staff_nat_avg$total /
  staff_nat_avg$total_commission) *
  100

staff_nat_avg$country[
  staff_nat_avg$country ==
    "United Kingdom of Great Britain and Northern Ireland"
] <- "United Kingdom"
staff_nat_avg$country[
  staff_nat_avg$country == "Netherlands, Kingdom of the"
] <- "Netherlands"

staff_nat_avg$ysp <- staff_nat_avg$year + (1 - staff_nat_avg$spring) * 0.5

ysp <- seq(min(staff_nat_avg$ysp), max(staff_nat_avg$ysp), by = 0.5)
countries <- unique(staff_nat_avg$country[staff_nat_avg$country != "Other"])
temp <- expand.grid(ysp, countries)
temp <- temp %>%
  dplyr::rename(ysp = Var1, country = Var2)

staff_nat <- full_join(temp, staff_nat_avg, by = join_by(ysp, country))

staff_nat$ecfin[staff_nat$country == "Croatia" & staff_nat$ysp <= 2013] <- 0

# Interpolate WITHIN country. Without group_by, zoo treats the column as a
# single series and fills across country boundaries. That was not hypothetical:
# Romania has no observation at ysp 2011.5 or 2012.0, and those rows were being
# filled from Portugal's last value, which immediately precedes Romania in the
# row order -- giving Romania an ecfin_int of 22 and 23 drawn from another
# country's staff count.
#
# Passing x = ysp makes the interpolation respect the time spacing rather than
# row position, and na.rm = FALSE leaves a leading gap missing rather than
# carrying a neighbour's value into it.
#
# "Other" is the bucket for staff whose nationality code did not match a
# country. It is excluded from `countries` above, but the full_join re-admits
# it, so it was sitting in the same vector being interpolated.
#
# The dplyr:: prefixes are load-bearing, not decoration. `create_staff_
# nationality_dataset.R` calls library(plyr) after dplyr is already attached, so
# its own library(dplyr) is a no-op and plyr ends up ABOVE dplyr on the search
# path. Bare `mutate` then resolves to plyr::mutate, which silently ignores
# group_by() -- the interpolation runs on all 28 countries as one series with 28
# duplicate values at every ysp, and the result is nonsense (Austria's 2011.5
# value came out as 19.4 against an observed 17). It only ever worked because
# the scripts were run interactively in an order that left dplyr on top.
staff_nat <- staff_nat %>%
  dplyr::filter(country != "Other") %>%
  dplyr::arrange(country, ysp) %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(
    ecfin_int = na.approx(ecfin, x = ysp, na.rm = FALSE),
    ecfin_spline = na.spline(ecfin, x = ysp)
  ) %>%
  dplyr::ungroup()

# Spline interpolation can undershoot below zero; the appendix documents this.
staff_nat$ecfin_spline[staff_nat$ecfin_spline < 0] <- 0

stopifnot(!anyDuplicated(staff_nat[, c("country", "ysp")]))

# Interpolation fills gaps; it must never move an observed value. This is the
# check that would have caught the masking bug above immediately.
observed <- !is.na(staff_nat$ecfin)
stopifnot(
  all(abs(staff_nat$ecfin_int[observed] - staff_nat$ecfin[observed]) < 1e-8),
  all(abs(staff_nat$ecfin_spline[observed] - staff_nat$ecfin[observed]) < 1e-8)
)

save(staff_nat, file = "~/EU_capacity/data/staff_nat.Rdata")
