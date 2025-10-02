library(dplyr)
library(lubridate)
library(data.table)
library(fixest)
library(car)

# --- Load data (check existence) ---
load("~/ec_project/data/bonds.Rdata")
load("~/ec_project/data/final_dataset_euro.Rdata")
load("~/ec_project/data/bonds_with_min.Rdata")

# bonds_with_min -> bonds_dt (data.table friendly)
bonds_dt <- copy(bonds_with_min)
setDT(bonds_dt)

# --- Prediction dates & full date window ---
pred_dates <- sort(unique(df$date_pred))
if (length(pred_dates) > 0) {
  pred_dates <- pred_dates[-1]
}

dates_full <- sort(c(
  pred_dates,
  pred_dates - 14,
  pred_dates - 7,
  pred_dates + 7,
  pred_dates + 14
))

# subset bonds to relevant dates
rb_dt <- bonds_dt[date %in% dates_full]

# --- Add date components to df (use lubridate ymd on date_pred) ---
df <- df %>%
  mutate(
    date_pred = ymd(date_pred),
    year_pred = year(date_pred),
    month_pred = month(date_pred),
    week_pred = isoweek(date_pred),
    day_pred = day(date_pred)
  )

# --- Filter invalid records ---
df_filtered <- df %>%
  filter(esa != " Excessive deficit procedure ", !is.na(ecfin))

# --- Add lags per (country, title) and remove exact duplicates ---
df_with_lags <- df_filtered %>%
  arrange(country, title, date_pred) %>%
  group_by(country, title) %>%
  mutate(
    p0_l1 = lag(p0, 1),
    p1_l1 = lag(p1, 1),
    p2_l1 = lag(p2, 1)
  ) %>%
  ungroup() %>%
  distinct() # keep one copy of identical rows

# --- Compute updates (keep same Nov rule) ---
df_updates <- df_with_lags %>%
  mutate(
    p0_update = if_else(month_pred == 11, p0 - p0_l1, p0 - p1_l1),
    p1_update = if_else(month_pred == 11, p1 - p1_l1, p1 - p2_l1)
  ) %>%
  select(
    country,
    year_pred,
    month_pred,
    title,
    p0,
    p0_l1,
    p0_update,
    p1,
    p1_l1,
    p1_update,
    p2,
    p2_l1,
    err0,
    err1
  )

# --- Extract the three series (revenue, expenditure, lending) and rename updates ---
rev_updates <- df_updates %>%
  filter(title == "Total current revenue: general government ") %>%
  rename(rev0_update = p0_update, rev1_update = p1_update)

exp_updates <- df_updates %>%
  filter(title == "Total current expenditure: general government ") %>%
  rename(exp0_update = p0_update, exp1_update = p1_update)

lend_updates <- df_updates %>%
  filter(
    title == "Net lending (+) or net borrowing (-): general government "
  ) %>%
  rename(lend0_update = p0_update, lend1_update = p1_update)

# --- Combine updates with inner joins (only keep rows where all three series exist) ---
updates <- rev_updates %>%
  inner_join(exp_updates, by = c("country", "year_pred", "month_pred")) %>%
  inner_join(lend_updates, by = c("country", "year_pred", "month_pred"))

# --- Prepare bond-level table (data.table) ---
# ensure rb_dt is sorted and create 'post' indicator
setorder(rb_dt, date)
rb_dt[, post := as.integer(min > 0)]

# create 1-lag of yield by country using data.table shift
rb_dt[, yield_lag1 := shift(yield, 1L, type = "lag"), by = country]
rb_dt[, my_change_yield := yield - yield_lag1]
rb_dt[, my_change_pct := my_change_yield / abs(yield)]

# --- Adjust months so pre/post-month merging matches original logic ---
# copy to rb2_dt and mutate months in place
rb2_dt <- copy(rb_dt)
# month 4 -> 5, month 10 -> 11, month 12 -> 11
rb2_dt[month == 4, month := 5]
rb2_dt[month == 10, month := 11]
rb2_dt[month == 12, month := 11]

# --- Merge bond data with updates on (country, year, month) ---
# convert updates to data.table for fast join
updates_dt <- as.data.table(updates)
setnames(
  updates_dt,
  old = c("year_pred", "month_pred"),
  new = c("year", "month")
)

bd_dt <- merge(
  rb2_dt,
  updates_dt,
  by = c("country", "year", "month"),
  all.x = FALSE,
  all.y = FALSE # inner join behaviour to match your inner_join above
)

# add a year_month string (useful for diagnostics)
bd_dt[, year_month := paste0(year, "-", month)]

# --- Remove observations exactly 14 days before or after prediction dates ---
pred_dates_plus_14 <- pred_dates + 14
pred_dates_minus_14 <- pred_dates - 14
bd_dt <- bd_dt[
  !(date %in% pred_dates_minus_14) & !(date %in% pred_dates_plus_14)
]

# --- Create interaction variables (post * update magnitudes) ---
bd_dt[, rev_post0 := post * rev0_update]
bd_dt[, rev_post1 := post * rev1_update]
bd_dt[, exp_post0 := post * exp0_update]
bd_dt[, exp_post1 := post * exp1_update]
bd_dt[, lend_post0 := post * lend0_update]
bd_dt[, lend_post1 := post * lend1_update]

# convenience indicator if month == May
bd_dt[, is_may := as.integer(month == 5)]

# --- Run regressions with fixed effects ---
bd_est <- bd_dt[min != 0]

# Helper to run feols safely and return object
run_feols <- function(formula, data) {
  feols(as.formula(formula), data = data)
}

m4 <- run_feols(
  "my_change_pct ~ post + rev_post0 + rev_post1 + exp_post0 + exp_post1 | country + month + year",
  bd_est
)
m5 <- run_feols(
  "my_change_pct ~ post + rev_post0 + rev_post1 | country + month + year",
  bd_est
)
m6 <- run_feols(
  "my_change_pct ~ post + exp_post0 + exp_post1 | country + month + year",
  bd_est
)
m7 <- run_feols(
  "my_change_pct ~ post + lend_post0 + lend_post1 | country + month + year",
  bd_est
)

# --- Output tables & hypothesis tests ---
etable(m4, m5, m6, m7, tex = FALSE, digits = 3, digits.stats = 3)

wald(m4, keep = c("rev_post0", "rev_post1"))
wald(m4, keep = c("exp_post0", "exp_post1"))
wald(m4, keep = c("rev_post0", "rev_post1", "exp_post0", "exp_post1"))

wald(m5, keep = c("rev_post0", "rev_post1"))

wald(m6, keep = c("exp_post0", "exp_post1"))

wald(m7, c("lend_post0", "lend_post1"))
