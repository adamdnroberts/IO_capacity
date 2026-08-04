# run_all.R -- rebuild every dataset, table and figure in the paper.
# See README.md for requirements and the script-to-output map.
#
#   Rscript run_all.R               # data + paper + appendix (the default)
#   Rscript run_all.R data paper    # any combination of stage names
#
# Run from the project root; every path in the project is relative to it.
#
# Stages: `data` builds data/ from raw/; `paper` and `appendix` build the
# tables and figures from data/, so they need no raw inputs. `exploratory`
# (console-only analyses) and `fetch` (re-downloads the EPU spreadsheets) are
# off by default.
#
# Each script runs in its own R process, so results cannot depend on what ran
# before. Neither objects nor the package search path carry over.
# This is because sourcing these scripts into one session leaves plyr masking
# dplyr::mutate, which silently corrupts the interpolated ECFIN series. See
# create_ecfin_variable.R.
#
# create_population_variable.R is the only default step needing a network
# connection (World Bank API). Its estimates are revised occasionally, so skip
# it and keep the distributed data/population.csv to reproduce the published
# numbers exactly.

# ---- Configuration ---------------------------------------------------------

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

# The project root is simply the working directory, which every script's
# relative paths are resolved against. Confirm it really is the project rather
# than letting a script fail 200 lines in on a missing file.
check_root <- function() {
  root <- getwd()
  markers <- c("run_all.R", "code", "data")
  missing <- markers[!file.exists(file.path(root, markers))]
  if (length(missing) > 0) {
    stop(
      "\n  The working directory does not look like the project root.\n",
      "    working directory : ",
      root,
      "\n",
      "    not found here    : ",
      paste(missing, collapse = ", "),
      "\n\n",
      "  cd into the folder containing run_all.R and try again.\n",
      call. = FALSE
    )
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

# Packages the pipeline loads. Checked up front so a missing one fails in the
# first second rather than forty minutes in.
REQUIRED_PACKAGES <- c(
  "countrycode",
  "data.table",
  "dplyr",
  "fixest",
  "ggplot2",
  "lubridate",
  "readxl",
  "robomit",
  "rvest",
  "stringr",
  "tidyr",
  "wbstats",
  "zoo"
)

check_packages <- function() {
  missing <- REQUIRED_PACKAGES[
    !vapply(REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "\n  Missing packages: ",
      paste(missing, collapse = ", "),
      "\n",
      '  install.packages(c("',
      paste(missing, collapse = '", "'),
      '"))\n',
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
    tail_lines <- tryCatch(
      readLines(log_file, warn = FALSE),
      error = function(e) character()
    )
    for (l in utils::tail(tail_lines, 8)) {
      message("       | ", l)
    }
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
      "unknown stage(s): ",
      paste(unknown, collapse = ", "),
      "\n  valid stages: ",
      paste(names(STAGES), collapse = ", "),
      call. = FALSE
    )
  }

  root <- check_root()
  check_packages()

  # Child processes inherit this working directory, which is what makes the
  # scripts' relative paths resolve.
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
    length(results) - length(failed),
    length(results),
    total
  ))
  if (length(failed) > 0) {
    message("")
    message("Failed:")
    for (f in failed) {
      message("  ", f$script, "\n    log: ", f$log)
    }
    message(strrep("=", 72))
    quit(status = 1, save = "no")
  }
  message(strrep("=", 72))
  invisible(results)
}

args <- commandArgs(trailingOnly = TRUE)
main(if (length(args) > 0) args else DEFAULT_STAGES)
