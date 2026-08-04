# run_all.R -- rebuild every dataset, table and figure in the paper.
#
# Usage
# -----
#   Rscript run_all.R              # data + paper + appendix (the default)
#   Rscript run_all.R data         # intermediate datasets only
#   Rscript run_all.R paper        # main-text tables and figures only
#   Rscript run_all.R data paper   # any combination of stage names
#
# Stage names: data, paper, appendix, exploratory, fetch
#
#   fetch        re-downloads the EPU spreadsheets from policyuncertainty.com.
#                Off by default: raw/EPU/ is already populated and the site's
#                file names change without notice.
#   exploratory  console-only analyses that appear in no .tex file. Off by
#                default. Run them if you are changing the sample definition
#                and want to see whether anything unpublished moves.
#
# Every script runs in its own R process. This costs a few seconds per script
# and is worth it twice over:
#
#   * Objects do not carry over, which is how `create_ECFIN_nationality_figure.R`
#     was caught plotting a data frame an earlier script had left behind.
#   * The package search path does not carry over either. That one is not
#     hypothetical: `create_staff_nationality_dataset.R` calls library(plyr)
#     when dplyr is already attached, so its own library(dplyr) is a no-op and
#     plyr sits ABOVE dplyr for the rest of the session. Bare `mutate` then
#     resolves to plyr::mutate, which ignores group_by(). Sourcing everything
#     into one session silently corrupted the interpolated ECFIN series.
#
# So results must not depend on what ran before, and here they cannot.
#
# Paths
# -----
# The scripts address files as "~/EU_capacity/...". Under Rscript on Windows
# "~" expands to C:/Users/<you>, not C:/Users/<you>/Documents, so this project
# needs R_USER set:
#
#   $env:R_USER = "C:/Users/adamd/Documents"
#   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" run_all.R
#
# RStudio sets HOME differently and needs nothing. The check below stops with
# this instruction rather than letting a script fail 200 lines in on a missing
# file.
#
# Network
# -------
# `create_population_variable.R` calls the World Bank API through wbstats. It
# is the one step in the default path that needs a connection. World Bank
# population estimates are revised, so a rebuild will not always be
# byte-identical to a previous one -- see item 11 in REVIEW_FIXES.md.
#
# Raw inputs
# ----------
# raw/ is gitignored and is not distributed with this repository. Without it
# the `data` stage cannot run; the `paper` and `appendix` stages work from the
# built .Rdata files in data/.

# ---- Configuration ---------------------------------------------------------

ROOT <- "~/EU_capacity/"

STAGES <- list(
  data = list(
    label = "Stage 1 -- build intermediate datasets",
    scripts = c(
      # Forecast vintages. Independent of each other.
      "code/creating dataset/clean_data11_14.R",
      "code/creating dataset/clean_data15_23.R",

      # Staff nationality -> ECFIN share. Sequential.
      "code/creating dataset/create_staff_nationality_dataset.R",
      "code/creating dataset/create_ecfin_variable.R",

      # Covariates. population must precede guide_rate, which reads it.
      "code/creating dataset/create_population_variable.R",
      "code/creating dataset/create_gdp_variable.R",
      "code/creating dataset/create_guide_rate_variable.R",
      "code/creating dataset/clean_and_merge_epu.R",

      # The join. Reads everything above.
      "code/creating dataset/create_dataset.R",

      # Bond yields, for the appendix event study. Independent of the panel.
      "code/appendix/create_bonds_dataset.R"
    )
  ),

  paper = list(
    label = "Stage 2 -- main-text tables and figures",
    scripts = c(
      "code/tables and figures/create_main_result_table.R",
      "code/tables and figures/create_summary_stats_table.R",
      "code/tables and figures/create_oster_sensitivity_table.R",
      "code/tables and figures/create_ECFIN_nationality_figure.R",
      "code/tables and figures/create_main_result_coef_plot.R",
      "code/tables and figures/create_marginal_effects_plot.R",
      # ~10 minutes: 10,000 permutation fits, seeded.
      "code/tables and figures/randomization_inference.R"
    )
  ),

  appendix = list(
    label = "Stage 3 -- appendix tables and figures",
    scripts = c(
      "code/tables and figures/create_alt_outcome_result_table.R",
      "code/tables and figures/create_alt_outcome_result_coef_plot.R",
      "code/tables and figures/create_EPU_model_table.R",
      "code/appendix/create_interpolation_tables.R",
      "code/appendix/create_result_excluding_covid_table.R",
      "code/appendix/create_result_excluding_UKBE_table.R",
      "code/appendix/create_bonds_analysis_table.R",
      "code/appendix/representation_grid_plot.R"
    )
  ),

  exploratory = list(
    label = "Optional -- console-only analyses",
    scripts = c(
      # Prints the three Wald tests quoted in the Model section (pooling across
      # indicators and across forecast horizons). Nothing else reports them.
      "code/tables and figures/het_effects_by_indicator_and_forecast.R",
      "code/tables and figures/create_table_directional_error_analysis.R",
      # Writes gdp_quartile_plot.pdf and gdppc_quartile_plot.pdf, neither of
      # which is included by any .tex file.
      "code/tables and figures/create_interaction_plots.R"
    )
  ),

  fetch = list(
    label = "Optional -- re-download raw EPU spreadsheets",
    scripts = c("code/creating dataset/get_epu.R")
  )
)

DEFAULT_STAGES <- c("data", "paper", "appendix")

# ---- Environment checks ----------------------------------------------------

check_root <- function() {
  expanded <- path.expand(ROOT)
  if (!dir.exists(file.path(expanded, "code"))) {
    stop(
      "\n  '", ROOT, "' does not resolve to this project.\n",
      "  '~' currently expands to: ", path.expand("~"), "\n\n",
      "  Set R_USER to the directory *containing* EU_Capacity and re-run:\n",
      '    $env:R_USER = "C:/Users/adamd/Documents"\n',
      call. = FALSE
    )
  }
  normalizePath(expanded, winslash = "/", mustWork = TRUE)
}

# Packages the pipeline loads. Checked up front so a missing one fails in the
# first second rather than forty minutes in.
REQUIRED_PACKAGES <- c(
  "countrycode", "data.table", "dplyr", "fixest", "ggplot2", "lubridate",
  "readxl", "robomit", "rvest", "stringr", "tidyr", "wbstats", "zoo"
)

check_packages <- function() {
  missing <- REQUIRED_PACKAGES[
    !vapply(REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "\n  Missing packages: ", paste(missing, collapse = ", "), "\n",
      '  install.packages(c("', paste(missing, collapse = '", "'), '"))\n',
      call. = FALSE
    )
  }
}

# ---- Runner ----------------------------------------------------------------

RSCRIPT <- file.path(R.home("bin"), "Rscript")

# Where each script's console output goes. Keeping it out of the terminal is
# what makes a failure visible: the summary at the end names the log to read.
LOG_DIR <- file.path(tempdir(), "eu_capacity_logs")

run_script <- function(rel_path, root) {
  full <- file.path(root, rel_path)
  if (!file.exists(full)) {
    stop("script not found: ", rel_path, call. = FALSE)
  }

  log_file <- file.path(LOG_DIR, paste0(basename(rel_path), ".log"))
  message("  -> ", basename(rel_path), appendLF = FALSE)
  started <- Sys.time()

  status <- system2(
    RSCRIPT,
    args = shQuote(full),
    stdout = log_file,
    stderr = log_file
  )

  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  ok <- identical(as.integer(status), 0L)

  if (ok) {
    message(sprintf("  (%.1fs)", elapsed))
  } else {
    message("")
    message("     FAILED (exit ", status, ") -- see ", log_file)
    tail_lines <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character())
    for (l in utils::tail(tail_lines, 8)) message("       | ", l)
  }

  list(script = rel_path, ok = ok, seconds = elapsed, log = log_file)
}

run_stage <- function(name, root) {
  stage <- STAGES[[name]]
  message("")
  message(strrep("=", 72))
  message(stage$label)
  message(strrep("=", 72))
  lapply(stage$scripts, run_script, root = root)
}

# ---- Main ------------------------------------------------------------------

main <- function(stage_names) {
  unknown <- setdiff(stage_names, names(STAGES))
  if (length(unknown) > 0) {
    stop(
      "unknown stage(s): ", paste(unknown, collapse = ", "),
      "\n  valid stages: ", paste(names(STAGES), collapse = ", "),
      call. = FALSE
    )
  }

  root <- check_root()
  check_packages()

  # Child processes expand "~" using their own R_USER. Pin it to whatever
  # resolved correctly here, so the check above holds for every subprocess.
  Sys.setenv(R_USER = dirname(path.expand("~/EU_capacity")))
  dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

  message("EU_Capacity -- full rebuild")
  message("  root:   ", root)
  message("  stages: ", paste(stage_names, collapse = ", "))
  message("  logs:   ", LOG_DIR)
  message("  start:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  started <- Sys.time()
  results <- do.call(c, lapply(stage_names, run_stage, root = root))

  failed <- Filter(function(r) !r$ok, results)
  total <- as.numeric(difftime(Sys.time(), started, units = "mins"))

  message("")
  message(strrep("=", 72))
  message(sprintf(
    "%d of %d scripts succeeded in %.1f minutes",
    length(results) - length(failed), length(results), total
  ))
  if (length(failed) > 0) {
    message("")
    message("Failed:")
    for (f in failed) message("  ", f$script, "\n    log: ", f$log)
    message(strrep("=", 72))
    quit(status = 1, save = "no")
  }
  message(strrep("=", 72))
  invisible(results)
}

args <- commandArgs(trailingOnly = TRUE)
main(if (length(args) > 0) args else DEFAULT_STAGES)
