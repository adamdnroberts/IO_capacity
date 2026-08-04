library(dplyr)
library(data.table)

datapath = "~/EU_Capacity/data/"

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

# Deduplicate. The error variables vary by forecast title, so they are keyed
# on country + ysp + title (one row per country-year-period-title). The
# remaining variables are constant within a country-year-period, so they are
# keyed on country + ysp.
dfpg_err <- dfpg %>% distinct(country, ysp, title, .keep_all = TRUE)
dfpg_cy  <- dfpg %>% distinct(country, ysp, .keep_all = TRUE)

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
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Variable & N & SD & Min & Median & Max \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\caption{Summary Statistics for Key Variables}",
  "\\label{tab:sumstats}",
  "\\end{table}"
)

tex_table <- paste(tex_lines, collapse = "\n")
cat("\n", tex_table, "\n")

# writeLines(
#   tex_table,
#   "C:/Users/adamd/Dropbox/Apps/Overleaf/EU_Capacity/tables/summary_stats_table.tex"
# )
