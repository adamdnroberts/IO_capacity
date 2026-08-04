library(dplyr)
library(data.table)

datapath = "data/"
tablepath = "overleaf/tables/"

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

# Exclude titles that are components of or near-duplicates of other titles,
# to match the sample used elsewhere in the analysis.
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

# The error variables vary by projection horizon as well as by country, ysp and
# title -- the p0/p1/p2 stacking is the whole point of the panel. Keying them on
# country + ysp + title was therefore wrong: distinct() keeps the first row per
# key, and p_list is bound in the order p0, p1, p2, so the table reported
# py == 0 (current-year nowcasts) only while the regression pools all three
# horizons. One- and two-year-ahead errors are much larger, so the reported mean
# squared error was understated by more than half.
#
# Summarise instead the rows that actually enter the pooled model in Panel A of
# the main table, so every figure describes the sample being estimated on.
est <- dfpg %>%
  filter(
    rev == 1 | exp == 1,
    !is.na(err_sq), err_sq > 0,
    !is.na(ecfin),
    !is.na(pop_int), pop_int > 0,
    !is.na(gdp), gdp > 0,
    !is.na(gdppc)
  )

# The remaining variables are constant within a country-year-period, so one row
# each -- drawn from the same estimation sample, not from the whole panel.
dfpg_err <- est
dfpg_cy <- est %>% distinct(country, ysp, .keep_all = TRUE)

# This must match the N of column (3), Panel A in the main results table.
stopifnot(nrow(dfpg_err) > 0)
cat("Estimation sample N (should equal main table Panel A col 3):",
    nrow(dfpg_err), "\n")

# Variables to summarize. Same set as the published table, with the
# non-logged squared error added alongside the log squared error.
vars <- list(
  "Representation (count)"        = dfpg_cy$ecfin,
  "Squared error"                 = dfpg_err$err_sq,
  "Log squared error"             = log(dfpg_err$err_sq),
  "Population (millions)"         = dfpg_cy$pop_int / 1e6,
  "GDP (\\euro\\,billions)"        = dfpg_cy$gdp / 1e3,
  "GDP per capita (\\euro\\,000s)" = dfpg_cy$gdppc * 1e3,
  "Economic Policy Uncertainty"   = dfpg_cy$epu
)

summarize_var <- function(x) {
  x <- x[is.finite(x)]
  data.frame(
    N      = length(x),
    Mean   = mean(x),
    SD     = sd(x),
    Min    = min(x),
    Median = median(x),
    Max    = max(x)
  )
}

stats <- do.call(rbind, lapply(vars, summarize_var))
stats <- cbind(Variable = names(vars), stats)
rownames(stats) <- NULL

# Console output for inspection
print(stats, row.names = FALSE)

# ---- LaTeX output (matches tab:sumstats formatting) ----
fmt_num <- function(v) {
  # wrap negatives in math mode so the minus sign renders correctly
  s <- formatC(v, format = "f", digits = 2, big.mark = ",")
  ifelse(v < 0, paste0("$", s, "$"), s)
}

body <- apply(stats, 1, function(r) {
  paste0(
    r["Variable"], " & ",
    formatC(as.integer(r["N"]), format = "d", big.mark = ","), " & ",
    fmt_num(as.numeric(r["SD"])), " & ",
    fmt_num(as.numeric(r["Min"])), " & ",
    fmt_num(as.numeric(r["Median"])), " & ",
    fmt_num(as.numeric(r["Max"])), " \\\\"
  )
})

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Variable & N & SD & Min & Median & Max \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}%",
  "}",
  "\\caption{Summary Statistics for Key Variables}",
  "\\label{tab:sumstats}",
  "\\end{table}"
)

tex_table <- paste(tex_lines, collapse = "\n")
cat("\n", tex_table, "\n")

dir.create(tablepath, recursive = TRUE, showWarnings = FALSE)
writeLines(tex_table, paste0(tablepath, "summary_stats.tex"))
