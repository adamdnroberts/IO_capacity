# Replication package

**Geographic Representation and Information Capacity in the European Union**
Adam Roberts, *European Union Politics*

This package rebuilds every dataset, table, and figure in the article and the online appendix from the raw source data.

---

## Quick start

```
cd EU_Capacity
Rscript run_all.R
```

That is the whole procedure. All paths in the project are relative to this folder, so it can live anywhere and needs no configuration. The script checks that the working directory is the project root and that every required package is installed before it runs anything.

Individual scripts can also be run on their own. Set the working directory to the project root first, with `setwd("/path/to/EU_Capacity")` or RStudio's *Session > Set Working Directory*, since every path in the scripts is relative to it.

**Runtime:** about 15 minutes, of which roughly 10 are `randomization_inference.R` (10,000 permutation model fits, seeded).

## Requirements

R 4.5.1 or later, and the following packages:

```r
install.packages(c(
  "countrycode", "data.table", "dplyr", "fixest", "ggplot2", "lubridate",
  "readxl", "robomit", "rvest", "stringr", "tidyr", "wbstats", "zoo"
))
```

Some scripts additionally use `car`, `janitor`, `marginaleffects`, `mgcv`, `plyr`, `purrr`, `stargazer`, and `viridis`.

## Running part of the pipeline

`run_all.R` takes stage names as arguments:

```
Rscript run_all.R data              # rebuild the intermediate datasets only
Rscript run_all.R paper appendix    # rebuild tables and figures from data/
```

| Stage | What it does | Default |
|---|---|---|
| `data` | builds everything in `data/` from `raw/` | yes |
| `paper` | main-text tables and figures | yes |
| `appendix` | online appendix tables and figures | yes |
| `exploratory` | console-only analyses (see note below) | no |
| `fetch` | re-downloads the raw EPU spreadsheets | no |

The `paper` and `appendix` stages read only from `data/`, so the tables and figures can be reproduced without `raw/`.

Each script runs in its own R process, so results cannot depend on what was run before it. Console output goes to a log directory named at the start of the run; the summary at the end names the log of anything that failed.

## Network access

Two steps use the internet, and neither is needed to reproduce the published numbers:

- **`create_population_variable.R`** (in the `data` stage) queries the World Bank API through `wbstats`. World Bank population estimates are revised from time to time, so re-running it may not reproduce `data/population.csv` byte-for-byte. The file as used in the article is distributed with this package. To guarantee an exact reproduction, run the pipeline once and keep the distributed `population.csv`.
- **`get_epu.R`** (the `fetch` stage, off by default) re-downloads the Economic Policy Uncertainty spreadsheets from policyuncertainty.com. `raw/EPU/` is already populated; the site's file names change without notice.

Everything else runs offline.

---

## Pipeline

### Stage 1 — datasets (`code/creating dataset/`, `code/appendix/`)

Order matters; `run_all.R` enforces it.

| Script | Output in `data/` |
|---|---|
| `clean_data11_14.R` | `full_dataset11_14.Rdata` |
| `clean_data15_23.R` | `full_dataset15_23.Rdata` |
| `create_staff_nationality_dataset.R` | `Commission_nationalities.Rdata`, `.csv` |
| `create_ecfin_variable.R` | `staff_nat.Rdata` |
| `create_population_variable.R` | `population.csv` |
| `create_gdp_variable.R` | `gdp.csv` |
| `create_guide_rate_variable.R` | `guide_rate.Rdata` |
| `clean_and_merge_epu.R` | `epu.Rdata` |
| `create_dataset.R` | `final_dataset_euro_pooled_plus_guide.Rdata`, `final_dataset_euro.Rdata`, `EU_Capacity_dataset.csv` |
| `create_bonds_dataset.R` | `bonds.Rdata` |

`final_dataset_euro_pooled_plus_guide.Rdata` (object `dfpg`) is the estimation panel used by every table and figure below. `EU_Capacity_dataset.csv` is the same data in plain text.

### Stage 2 — tables

All tables are written to `overleaf/tables/` and read into the documents with `\input{}`, so no table is transcribed by hand.

| Output | Script |
|---|---|
| `main_table.tex` | `tables and figures/create_main_result_table.R` |
| `summary_stats.tex` | `tables and figures/create_summary_stats_table.R` |
| `oster_table.tex` | `tables and figures/create_oster_sensitivity_table.R` |
| `alt_outcome_table.tex` | `tables and figures/create_alt_outcome_result_table.R` |
| `epu_table.tex` | `tables and figures/create_EPU_model_table.R` |
| `linear_int_table.tex`, `spline_int_table.tex` | `appendix/create_interpolation_tables.R` |
| `exclude_covid_table.tex` | `appendix/create_result_excluding_covid_table.R` |
| `exclude_ukbe_table.tex` | `appendix/create_result_excluding_UKBE_table.R` |
| `bonds_table.tex` | `appendix/create_bonds_analysis_table.R` |

### Stage 3 — figures

All figures are written to `overleaf/images/`.

| Output | Script |
|---|---|
| `ECFIN_Nationality_Plot.pdf` | `tables and figures/create_ECFIN_nationality_figure.R` |
| `main_plot.pdf` | `tables and figures/create_main_result_coef_plot.R` |
| `oster_plot.pdf` | `tables and figures/create_oster_sensitivity_table.R` |
| `randomization_coefficient.pdf` | `tables and figures/randomization_inference.R` |
| `marginal_effects_by_guiderate_plot.pdf`, `gam_marginal_effects_plot.pdf` | `tables and figures/create_marginal_effects_plot.R` |
| `alt_outcome_plot.pdf` | `tables and figures/create_alt_outcome_result_coef_plot.R` |
| `Representation_Plot.png` | `appendix/representation_grid_plot.R` |

### Results reported in text only

Two sets of numbers appear in the article without a table of their own:

- **The three Wald tests** in the Model section (pooling across indicators and across forecast horizons) come from `code/tables and figures/het_effects_by_indicator_and_forecast.R`, which prints to the console. It is in the `exploratory` stage: `Rscript run_all.R exploratory`.
- **The randomization inference p-values** are printed by `randomization_inference.R`, and the 10,000 draws behind the figure are saved to `data/randomization_draws.rds`, so they can be inspected without repeating the run.

---

## Raw data

| Path | Source | Used for |
|---|---|---|
| `raw/Projections/<Season Year>/AMECO16.TXT` | AMECO, European Commission (DG ECFIN) | forecasts and outturns |
| `raw/Staff/` | European Commission HR statistical bulletins | staff nationality by DG |
| `raw/namq_10_gdp_page_linear.csv` | Eurostat | quarterly GDP |
| `raw/EPU/` | policyuncertainty.com | Economic Policy Uncertainty indices |
| `raw/Bonds/` | Investing.com | 10-year government bond yields (appendix) |
| `data/EP.csv`, `data/Council.csv` | European Parliament, Council of the EU | seats and voting weights for the guiding rate |
| World Bank API | `wbstats` | population |

Each AMECO release contains eighteen tables; this project reads only `AMECO16.TXT` from each vintage. Twenty-six vintages are used: Spring and Autumn 2011 through 2014, Spring and Autumn 2015 through 2022, Spring 2023, and Spring 2024.

### Forecast errors and benchmark vintages

Forecast error is the projected value minus the outturn, in billions of euro. Because AMECO switched from ESA 1995 to ESA 2010 with the Autumn 2014 release, two benchmark vintages are used:

- **Spring 2014** supplies the outturns for target years 2011–2013 (ESA 1995).
- **Spring 2024** supplies the outturns for every target from the Autumn 2014 release onward (ESA 2010).

The Spring 2014 forecast round is excluded from the panel. It is the last ESA 1995 vintage and its targets are 2014 and 2015, for which no ESA 1995 outturn exists, so scoring it would measure an accounting-definition change as forecast error. `clean_data11_14.R` documents this. Spring 2023 enters with current-year forecasts only, because its one-year-ahead target is 2024 and the Spring 2024 figure for 2024 is itself a forecast.

---

## Notes

- `data/` ships with the built intermediate files, so the tables and figures can be checked without rebuilding from `raw/`.
- Interpolated versions of the ECFIN staff variable (`ecfin_int`, `ecfin_spline`) are used only in the appendix robustness tables. The main specification uses the raw counts.
- `REVIEW_FIXES.md` documents a code review of this project and the changes made in response to it. It is a development record, not part of the analysis.
