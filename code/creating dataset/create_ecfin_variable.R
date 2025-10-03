library(dplyr)
library(zoo)

load("~/ec_project/data/Commission_nationalities.Rdata")

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

staff_nat <- staff_nat %>%
  mutate(ecfin_int = na.approx(ecfin), ecfin_spline = na.spline(ecfin))

staff_nat$ecfin_spline[staff_nat$ecfin_spline < 0] <- 0

save(staff_nat, file = "~/ec_project/data/staff_nat.Rdata")
