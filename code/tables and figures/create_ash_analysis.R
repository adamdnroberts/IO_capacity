library(fixest)
library(dplyr)
library(tidyr)
library(data.table)
library(ashr)
library(broom)

datapath <- "~/EU_capacity/data/"

load(paste0(datapath, "final_dataset_euro_pooled_plus_guide.Rdata"))
setDT(dfpg)

dfpg_noA <- dfpg %>% filter(aeoy == 0)
dfpg_noEOY <- dfpg %>% filter(py != 0)

# ---------------------------------------------------------------------------
# ASH across titles: estimate ecfin effect for each budget title separately,
# then shrink the collection of estimates toward the empirical distribution.
# This is the appropriate use of ASH — pooling exchangeable effect estimates.
# ---------------------------------------------------------------------------

run_title_ash <- function(data, side_filter, verbose = TRUE) {
  sub <- data %>% filter(!!side_filter)

  all_titles <- unique(sub$title)
  all_titles <- all_titles[
    !all_titles %in%
      c(
        # Previously excluded
        "Capital transfers paid: general government ",
        "Net disposable income: general government ",
        # NA titles
        NA,
        # Components of "Social contributions received: general government"
        "Actual social contributions received: general government ",
        "Imputed social contributions: general government ",
        # Near-duplicate of "Net social contributions received"
        "Social contributions received: general government ",
        # Subset of "Interest including flows on swaps and FRAs"
        "Interest: general government ",
        # Components of "Final consumption expenditure of general government"
        "Collective consumption expenditure ",
        "Compensation of employees: general government ",
        "Intermediate consumption: general government ",
        "Social transfers in kind supplied to households via market producers: general government ",
        # Components of "Current tax burden: total economy"
        "Current taxes on income and wealth (direct taxes): general government ",
        "Taxes linked to imports and production (indirect taxes): general government ",
        # Subset of "Capital transfers received: general government"
        "Capital taxes: general government ",
        # Near-duplicate of "Other current revenue including sales: general government"
        "Other current revenue: general government ",
        # Derived from "Gross saving: general government"
        "Net saving: general government "
      )
  ]

  if (verbose) {
    cat("Total titles found:", length(all_titles), "\n")
  }

  n_too_few <- 0
  n_no_ecfin <- 0
  n_failed <- 0
  results_list <- vector("list", length(all_titles))

  for (i in seq_along(all_titles)) {
    current_title <- all_titles[i]
    sub_data <- sub[sub$title == current_title, ]

    if (nrow(sub_data) < 5) {
      if (verbose) {
        cat("  SKIP (< 5 obs):", current_title, "\n")
      }
      n_too_few <- n_too_few + 1
      next
    }

    result <- tryCatch(
      {
        m <- feols(
          log(err_sq + 1e-6) ~ ecfin + log(pop_int) + log(gdp) + gdppc | py,
          data = sub_data
        )

        s <- tidy(m, cluster = ~country) %>% filter(term == "ecfin")

        if (nrow(s) == 0) {
          if (verbose) {
            cat("  SKIP (ecfin not estimated):", current_title, "\n")
          }
          n_no_ecfin <- n_no_ecfin + 1
          NULL
        } else {
          s$title <- current_title
          s
        }
      },
      error = function(e) {
        if (verbose) {
          cat(
            "  SKIP (model failed):",
            current_title,
            "-",
            conditionMessage(e),
            "\n"
          )
        }
        n_failed <<- n_failed + 1
        NULL
      }
    )

    results_list[[i]] <- result
  }

  if (verbose) {
    cat("\nSummary:\n")
    cat("  Too few obs (< 50):     ", n_too_few, "\n")
    cat("  ecfin not estimated:    ", n_no_ecfin, "\n")
    cat("  Model failed:           ", n_failed, "\n")
    cat(
      "  Titles in ASH:          ",
      sum(!sapply(results_list, is.null)),
      "\n\n"
    )
  }

  title_results <- bind_rows(results_list)

  ash_out <- ash(
    title_results$estimate,
    title_results$std.error,
    mixcompdist = "normal"
  )

  title_results$shrunken_beta <- ash_out$result$PosteriorMean
  title_results$lfsr <- ash_out$result$lfsr
  title_results$svalue <- ash_out$result$svalue

  title_results %>%
    select(title, estimate, std.error, shrunken_beta, lfsr, svalue) %>%
    arrange(lfsr)
}

# Run for revenue and expenditure sides
rev_ash <- run_title_ash(dfpg, quote(rev == 1))
exp_ash <- run_title_ash(dfpg, quote(exp == 1))

# Same on the two subsamples
rev_ash_noA <- run_title_ash(dfpg_noA, quote(rev == 1))
exp_ash_noA <- run_title_ash(dfpg_noA, quote(exp == 1))
rev_ash_noEOY <- run_title_ash(dfpg_noEOY, quote(rev == 1))
exp_ash_noEOY <- run_title_ash(dfpg_noEOY, quote(exp == 1))

# ---------------------------------------------------------------------------
# Print summaries
# ---------------------------------------------------------------------------

cat("\n=== Revenue (full sample) ===\n")
print(rev_ash)

cat("\n=== Expenditure (full sample) ===\n")
print(exp_ash)

cat("\n=== Revenue (no adjustment years) ===\n")
print(rev_ash_noA)

cat("\n=== Expenditure (no adjustment years) ===\n")
print(exp_ash_noA)

cat("\n=== Revenue (no end-of-year) ===\n")
print(rev_ash_noEOY)

cat("\n=== Expenditure (no end-of-year) ===\n")
print(exp_ash_noEOY)
