# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based academic research project analyzing how European Commission ECFIN (Economic and Financial Affairs) staff nationality composition affects the accuracy of EU fiscal forecasts (2011–2023). The main outcome variable is forecast error (actual minus projected), and the key predictor is the share of ECFIN staff from each country.

## Running Scripts

All scripts are plain R files — run them from within RStudio using the project file (`EU_capacity.Rproj`) or from the command line:

```bash
Rscript "code/creating dataset/create_dataset.R"
Rscript "code/tables and figures/create_main_result_table.R"
```

Scripts use `~/EU_capacity/` as the root path for all data I/O. Ensure this resolves correctly on the machine being used.

## Data Pipeline Architecture

Scripts must be run in order. The pipeline has three stages:

### Stage 1 — Build intermediate datasets (`code/creating dataset/`)

| Script | Inputs (from `~/EU_capacity/`) | Output |
|---|---|---|
| `clean_data11_14.R` | `raw/Projections/*/AMECO16.TXT` | `data/full_dataset11_14.Rdata` |
| `clean_data15_23.R` | `raw/Projections/*/AMECO16.TXT` | `data/full_dataset15_23.Rdata` |
| `create_staff_nationality_dataset.R` | `raw/Staff/` bulletins | `data/Commission_nationalities.Rdata` |
| `create_ecfin_variable.R` | `data/Commission_nationalities.Rdata` | `data/staff_nat.Rdata` |
| `create_population_variable.R` | Eurostat population data | `data/population.csv` |
| `create_gdp_variable.R` | `raw/namq_10_gdp_page_linear.csv` | `data/gdp.csv` |
| `create_dataset.R` | All of the above + `data/guide_rate.Rdata` | `data/final_dataset_euro_pooled_plus_guide.Rdata`, `data/EU_Capacity_dataset.csv` |

### Stage 2 — Main analysis (`code/tables and figures/`)

All scripts in this folder load `final_dataset_euro_pooled_plus_guide.Rdata` and produce LaTeX tables or ggplot figures. The core regression model (in `create_main_result_table.R`) is:

```r
log(err_sq) ~ ecfin + log(pop) + log(gdp) + gdppc | country + ysp + title + py
```

Estimated via `fixest::feols()` with country, year-season (`ysp`), fiscal variable type (`title`), and projection horizon (`py`) fixed effects. Results are split by revenue (`rev==1`) and expenditure (`exp==1`).

### Stage 3 — Appendix & robustness (`code/appendix/`, `code/additional_analysis/`)

These scripts run after Stage 2 and share the same final dataset. Notable analyses:
- `did.R` — difference-in-differences using Croatia's 2013 EU accession as a natural experiment
- `croatia_synth.R` — synthetic control for the same event
- `amelia_analysis.R` / `create_amelia_dataset.R` — multiple imputation for missing ECFIN staff data

## Key Dataset Variables

| Variable | Description |
|---|---|
| `country` | EU member state |
| `ysp` | Year + forecast season (e.g., Spring 2015) |
| `title` | Fiscal aggregate being forecast |
| `py` | Projection horizon (0, 1, or 2 years ahead) |
| `ecfin` / `ecfin_int` / `ecfin_spline` | Share of ECFIN staff from that country (raw / interpolated / spline) |
| `err` / `err_sq` | Forecast error / squared error |
| `pos_err` | Direction: 1 = overprediction, 0 = underprediction |
| `rev`, `exp`, `lend` | Fiscal variable type flags |
| `aeoy` | April end-of-year forecast flag |

## Key Packages

- `fixest` — fixed-effects regression (primary estimation)
- `stargazer` — LaTeX table output
- `ggplot2` — all figures
- `zoo` — time-series interpolation of staff data
- `countrycode` — country name/code standardization
- `dplyr` / `data.table` — data manipulation
