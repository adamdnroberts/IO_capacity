# Code review fixes — running log

Findings from a Gentzkow & Shapiro–style review (code style, data/keys, project
structure) run on 2026-08-04. Ordered by severity. Update the status column as
items are addressed.

Status key: **DONE** · **IN PROGRESS** · **OPEN** · **WONTFIX**

---

## Tier 1 — could change a published number

### 1. Czech rows silently dropped from 2015–2018 vintages — **DONE**

`code/creating dataset/clean_data15_23.R`

AMECO renamed "Czech Republic" → "Czechia" in the Spring 2019 release. `country`
was part of the merge key joining each vintage to `true3` (built from Spring
2024), so for vintages s15/a15 through s18/a18 every Czech row matched nothing
and was dropped by `merge()`'s default inner join.

*Verified before fix:* Czechia had 81 rows in every ysp except 2015–2018.5,
which had zero (~648 observations). Austria, as control, had 81 throughout.

*Fix:* `standardize_country()` applied to all `*nt` frames and `true3` before
the merges, plus a post-merge assertion that no EU member state present in both
inputs vanishes. Assertion scoped to `EU_MEMBERS` because AMECO aggregates
("Euro area", "European Union") legitimately fail to match — their series `code`
encodes membership count (EA19 → EA20) and changes across vintages.

*Verified after fix:* Czechia matches Austria row-for-row in all 16 vintages.
Main results re-run: **coefficients very similar to baseline.**

Commit `a6d1839`, branch `fix/czechia-merge-drop`.

Confirmed the rename is the *only* member-state label change across all
vintages 2011–2024. Other varying labels are non-members (Macedonia FYR →
North Macedonia, Korea, Canada, New Zealand) or aggregates.

### 2. Autumn 2022 vintage contributes zero observations — **OPEN**

`code/creating dataset/create_population_variable.R:40-62`

`wb_data(..., end_date = 2022)` caps `population.csv` at ysp 2022.0, but the
projection panel runs to ysp 2022.5. 2,886 rows get `pop_int = NA`, and
`gdppc = gdp / pop_int` inherits it. Every main specification includes
`log(pop_int)` and `gdppc`, so `feols` listwise-deletes all of them — the most
recent full vintage, covering the post-COVID inflation surge where forecast
errors are largest, is absent from the published table and the notes don't say
so.

*Fix:* extend the World Bank pull and interpolation grid to cover the panel;
assert coverage at the merge in `create_dataset.R`.

### 3. Two forecast vintages built but never bound in — **OPEN**

`clean_data11_14.R:126` (`s14nt`), `clean_data15_23.R:326` (`s23nt`)

Both are constructed and used only as truth sources; neither is included in the
`bind_rows` that forms the panel. Result: **zero rows at ysp 2014 and 2023.**
The project is documented as 2011–2023; it covers 24 of 26 half-years.

*Corroborated:* Austria has 3 rows at ysp 2014 and 2023 versus 81 elsewhere —
and those 3 are the orphan rows from item 5, not real observations.

### 4. Copy-paste error in staff bulletin date stamp — **OPEN**

`code/creating dataset/create_staff_nationality_dataset.R:73`

`rbind(nat19_4, c("date", rep("2019-04-03", length(nat19_1) - 1)))` — sizes the
April 2019 date row from the **January** bulletin. Nineteen sibling blocks use
their own object. If column counts differ between the two files this recycles or
errors; `ecfin` for 2019H1 would shift for every country. Also note the file
`20190401_*.csv` is dated `2019-04-03` where every other entry uses the 1st.

### 5. 156 rows with `title = NA` enter the estimation array — **OPEN**

`code/creating dataset/create_dataset.R:105`

`full_join(df_euro, gne_merge, ...)` runs *after* the `unit` filter, so its
orphans survive into `dfpg`. They pass `!(title %in% vars_to_exclude)` because
`NA %in% x` is `FALSE`. They contribute nothing to `feols` but corrupt any
`nrow()`, `distinct()`, or summary computed on the file — see item 6.

*Fix:* `left_join(..., relationship = "many-to-one")` plus a key assertion
before `save`.

### 6. Summary statistics computed at the wrong key — **OPEN**

`code/tables and figures/create_summary_stats_table.R:32`

`distinct(country, ysp, title, .keep_all = TRUE)` keeps the *first* row per key,
and `p_list` is bound in order `p0, p1, p2` — so the published error moments are
**`py == 0` only** (nowcasts) while the regression pools all three horizons.
One- and two-year-ahead errors are substantially larger, so the table understates
the estimation sample's dispersion. `dfpg_cy` is also taken before any EU
restriction, so the GDP row includes non-members.

*Fix:* compute descriptives on the exact object passed to `feols`.

### 7. Estimation sample defined by accident — **OPEN**

`code/creating dataset/create_dataset.R:101`

The only row restriction is `unit == "Mrd ECU/EUR"`, which admits Norway,
Switzerland, Iceland, Serbia, Montenegro, Albania, Turkey, US, Japan (7,383 rows
for Norway/Switzerland/US/Japan alone). They drop out *only* because
`population.csv` lacks them, so `log(pop_int)` deletes them. Drop that covariate
in any robustness check and non-members enter an "EU member state" regression.

*Fix:* filter explicitly on `EU_MEMBERS` in the build (the vector now exists in
`clean_data15_23.R` — move it somewhere shared).

### 8. Outcome measured against three different truth vintages — **OPEN**

`clean_data11_14.R:166-185`, `clean_data15_23.R:346-360`

Errors for 2011–2013 use `true1` (Spring 2014), 2014 uses `true2` (Autumn 2017),
2015–2022 use `true3` (Spring 2024). So `err` embeds 1–3 years of national
accounts revisions in the first block and up to 9 in the second — `err_sq` is
not the same quantity across the panel. `ysp` fixed effects absorb the level
shift but not differential revision behaviour across countries.

Also breaks `create_theil_u_results.R:39`, where `true_naive = first(true)`
assumes `true` is constant within country × title × forecast year.

---

## Tier 2 — documentation and structure

### 9. CLAUDE.md contradicts the code in four places — **OPEN**

| CLAUDE.md says | Code actually does |
|---|---|
| forecast error = actual − projected | `create_dataset.R:82` — `err0 = p0 - true0`, i.e. **projected − actual**. Sign of every direction claim flips. |
| `ecfin` = share of ECFIN staff | `create_ecfin_variable.R:9` — `mean(ECFIN)`, a **headcount**. Changes the coefficient's interpretation from "per pp" to "per staff member". `pct_ecfin`/`rate_ecfin` exist and are never used. |
| model uses `log(pop)` | every script uses `log(pop_int)` |
| `aeoy` = April end-of-year flag | `create_dataset.R:198` — `py == 0 & ysp %% 1 == 0.5`, i.e. the **November/autumn** current-year forecast |

### 10. `~` does not resolve under Rscript — **OPEN**

Scripts hardcode `~/EU_capacity/`. Under `Rscript`, `~` expands to
`C:\Users\adamd`, **not** `...\Documents` — so the command-line invocation
documented in CLAUDE.md cannot work. These scripts currently run only inside
RStudio (which remaps `HOME`), or with `R_USER=C:/Users/adamd/Documents` set.

Compounding: the path case is inconsistent *within single files*
(`clean_and_merge_epu.R` uses both `~/EU_capacity/` and `~/EU_Capacity/`),
which works only on a case-insensitive filesystem.

*Fix:* one root variable, or `here::here()`.

### 11. Three datasets have no producer — **OPEN**

`data/guide_rate.Rdata` (loaded `create_dataset.R:103`, feeds the **main**
specification), `data/final_dataset_euro.Rdata`, `data/bonds_with_min.Rdata`.
No script in the repo builds any of them, so a clean rebuild errors and the
trace from main table back to raw input dead-ends.

### 12. Every table is hand-pasted into LaTeX — **IN PROGRESS**

The `writeLines(...)` call is commented out in all six table scripts; they
`cat()` to console instead, and the `.tex` files contain literal `tabular`
blocks rather than `\input{}`. Nothing forces the paper to update when a
specification changes.

**Main table done.** `create_main_result_table.R` now writes
`overleaf/tables/main_table.tex` (via a new `tablepath` variable, with
`dir.create`), and `main_sage_template.tex:262` reads it with `\input`.
Verified: generated output is structurally identical to the previously pasted
block, and `pdflatex main_sage_template.tex` compiles clean.

Two content fixes made at the same time, to match the pasted block that was
actually in the paper:
- added the `\resizebox{\linewidth}{!}{%` … `}` wrapper the script was missing
- **citation key corrected** from `baker2016measuring` to `baker_measuring_2016`.
  Only the latter exists in the `.bib`, so the script's version would have
  rendered as `[?]`. The pasted block was right and the script was wrong —
  evidence of how far the two had drifted.
- footer wording aligned to the paper: "Clustered (state)" not "(country)".

Note on repo layout: `overleaf/` is a **nested git repo** with its own remote
(`git.overleaf.com/667ef85d...`), which is why the parent `.gitignore` excludes
it. Paper-side changes are committed and pushed there, not to this repo. The
template + `tables/` change is overleaf commit `7c35113`.

`main.tex` was superseded by `main_sage_template.tex` and has been deleted in
the overleaf repo (same commit); it carried a stale hand-pasted copy of this
table. Nothing referenced it. Recoverable from history.

Still open: `create_summary_stats_table.R`, `create_oster_sensitivity_table.R`,
`create_alt_outcome_result_table.R`, `create_EPU_model_table.R`,
`create_bonds_analysis_table.R`.

**Prose not yet updated.** `main_sage_template.tex:258` still quotes the
revenue range as "between -0.033 and -0.036"; Panel B is now -0.037 after the
Czechia fix. Deferred to a full prose pass once the remaining data fixes land.
The Ireland worked example is unaffected (it uses Panel C, unchanged).

### 13. No master script — **OPEN**

No `run_all.R`, no `Makefile`, zero `source()` calls. Build order exists only in
CLAUDE.md prose, and CLAUDE.md is gitignored. A draft `run_all.R` was proposed in
the structure review.

### 14. Interpolation not grouped by country — **OPEN**

`code/creating dataset/create_ecfin_variable.R:50-51`

`na.approx`/`na.spline` with no `group_by(country)`. Works **today by luck**:
`expand.grid` orders rows country-major and every country has a non-missing
value at the first and last grid point (Croatia via the hard-coded 0 at line 48).
Breaks silently the moment a country lacks a leading or trailing observation.
`create_population_variable.R:71-72` does the same operation correctly.

Also: `countries` at line 41 excludes `"Other"`, but the `full_join` at line 46
readmits it, appending it to the same interpolated vector.

Same ungrouped pattern in `additional_analysis/wages.R:27`.

### 15. ASH analysis runs on a different sample — **OPEN**

`code/tables and figures/create_ash_analysis.R:26-55` uses its own inline
exclusion list (15 titles + `NA`) rather than the shared `overlapping_titles`
literal, so the ASH table is estimated on a different sample from the main
table with nothing in the output saying so.

Note: the named `overlapping_titles` literal itself is byte-identical everywhere
it appears — it has not drifted. The problem is that this script doesn't use it.

*Fix:* define the sample once in `create_dataset.R`.

### 16. Smaller items — **OPEN**

- `create_ash_analysis.R:119` prints `"< 50"` where the threshold at line 71 is
  `5`. Also uses `log(err_sq + 1e-6)` where every other script uses `log(err_sq)`.
- `create_EPU_model_table.R:273` duplicates `\label{tab:main_model}` from
  `create_main_result_table.R:268`.
- `create_ECFIN_nationality_figure.R:3-6` loads `Commission_nationalities.Rdata`
  (contains `staff`) then plots `staff_nat`. Errors from a clean session.
- `create_result_excluding_covid_table.R:29-36` mixes target-year and vintage
  clauses; still retains 2022-target forecasts made in 2022.
- `create_bonds_dataset.R:72-81` — all five shift column names are wrong
  (`n = -1:3` with `type = "lag"` makes `yield_lag1` a lead, etc.).
- `raw/Staff/iso_alpha2.csv:156` is unquoted `NA,"Namibia"` — read as a missing
  key, and base `merge()` matches `NA` to `NA`.
- `create_population_variable.R:74-79` — the "exclude UK and Greece after 2022"
  filter is a no-op; max ysp is already 2022.0.
- `did.R`, `croatia_synth.R`, `amelia_analysis.R` are documented in CLAUDE.md as
  pipeline analyses but contain no estimator / do not parse. `croatia_synth.R`
  and several others read from `~/ecb_project/`, a different project.
- Figures write to two divergent trees (`overleaf/images/` and a Dropbox path)
  whose contents differ by five PDFs, three of which the appendix includes.
