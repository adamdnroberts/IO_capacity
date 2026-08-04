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

### 2. Autumn 2022 vintage contributes zero observations — **DONE**

`code/creating dataset/create_population_variable.R:40-62`

`wb_data(..., end_date = 2022)` caps `population.csv` at ysp 2022.0, but the
projection panel runs to ysp 2022.5. 2,886 rows get `pop_int = NA`, and
`gdppc = gdp / pop_int` inherits it. Every main specification includes
`log(pop_int)` and `gdppc`, so `feols` listwise-deletes all of them — the most
recent full vintage, covering the post-COVID inflation surge where forecast
errors are largest, is absent from the published table and the notes don't say
so.

*Verified before fix:* all 2,886 estimation-eligible rows at ysp 2022.5 had
`pop_int = NA`.

*Fix:* `PANEL_MAX_YSP <- 2023` introduced; the World Bank fetch now runs to
`PANEL_MAX_YSP + 1` so the final half-year has a real anchor on both sides, and
the output grid is trimmed back to `PANEL_MAX_YSP` so no orphan rows are created
by the `full_join` in `create_dataset.R`. Added assertions on non-missingness,
coverage, and key uniqueness.

The dormant UK/Greece filter was **deleted**. It was a no-op (data stopped at
2022) that would have activated the moment the fetch was extended. Measured
first: it removes **0 UK rows** — the UK has no estimation-eligible observations
after ysp 2020.5, for unrelated reasons — and **50 Greek rows**. The World Bank
has both countries through 2025, so it was not about data availability.

*Verified after fix:* `population.csv` covers ysp 2011–2023 for all 28 countries
with zero missing `pop_int`. Panel orphan rows unchanged at 156; total rows
unchanged. Missingness at ysp 2022.5 fell from 2,886/2,886 to 621/2,886, the
residual being non-EU countries (item 7).

*Effect on results:* N rises to 11,089 / 8,533 / 6,199 in column (3) of Panels
A/B/C (+648 / +324 / +324). Coefficients shrink 5–8% in magnitude but gain
precision: in Panel C, columns (1), (2) and (4) move from ** to ***.

**Prose impact — needs attention in the full pass.** Beyond stale digits:
- `main_sage_template.tex:254` says "the lack of significance at $\alpha = 0.05$
  in two expenditure specifications" — the Panel C expenditure coefficient is now
  significant at 0.01, so the sentence describes a state that no longer exists.
- `main_sage_template.tex:258` quotes ranges for all three columns, all shifted,
  and the Ireland worked example derives from Panel C coefficients that moved.

### 3. Two forecast vintages built but never bound in — **DONE (Spring 2023 only)**

`clean_data11_14.R:126` (`s14nt`), `clean_data15_23.R:326` (`s23nt`)

Both were constructed and used only as truth sources; neither was included in
the `bind_rows` forming the panel, so ysp 2014 and 2023 were **empty** while the
project claimed 2011–2023 coverage (24 of 26 half-years).

**Spring 2014 — bound in, then REVERTED.** It was briefly merged on `code`
(the four-key merge fails because AMECO relabelled titles from `:- ESA 1995`
to `:- ESA 2010` at the Autumn 2014 release). But that scored **ESA 1995
forecasts against ESA 2010 actuals**, the exact mismatch the two-database
design exists to avoid. Spring 2014's targets are 2014 and 2015, for which no
ESA 1995 outturn exists: `true1` is built from the Spring 2014 file itself and
cannot contain them, and every later release is ESA 2010. There is no clean
benchmark, which is very likely the author's original reason for excluding it.
Reverted; ysp 2014 is empty **by design**, and `clean_data11_14.R` now records
why. It contributed zero estimation rows either way (no staff data at ysp
2014), and both tables were byte-identical before and after the revert.

**Spring 2023.** Bound with `py=0` only. Its `p1` targets 2024, and Spring
2024's 2024 column is itself a forecast, not an outturn — including it would
compare forecast against forecast. Documented in the script for when a vintage
carrying 2024 actuals appears.

*Verified:* ysp 2023 carries `py=0` only, as intended. ysp 2014 is empty.

*Effect on results:* N rises to 11,413 / 8,857 in column (3) of Panels A and B.
**Panel C is completely unchanged**, as Spring 2023 contributes `py=0` only.
Panel B column (2) loses significance (-0.015* to -0.013, n.s.).

**All 324 added observations come from Spring 2023; Spring 2014 added zero.**
An earlier estimate of ~1,900 recovered observations for Spring 2014 was wrong:
it checked forecast and truth availability but not staff coverage. See item 17.

### 4. Copy-paste error in staff bulletin date stamp — **DONE**

`code/creating dataset/create_staff_nationality_dataset.R:73`

`rbind(nat19_4, c("date", rep("2019-04-03", length(nat19_1) - 1)))` — sizes the
April 2019 date row from the **January** bulletin. Nineteen sibling blocks use
their own object. If column counts differ between the two files this recycles or
errors; `ecfin` for 2019H1 would shift for every country.

*Verified:* both files have 30 columns, so the bug was **latent, not live** --
the date row came out correct. Fixed to use `nat19_4`; regenerating
`Commission_nationalities.Rdata` produces a byte-identical object
(`identical() == TRUE`), confirming no result changes.

The file `20190401_*.csv` is dated `2019-04-03` where every other entry uses
the 1st. Also cosmetic: the season is derived from `month <= 6`
(`create_staff_nationality_dataset.R:239`), so both dates land in spring 2019.

Still worth doing: the twenty near-identical read-and-stamp blocks should be
one `Map()` over a file/date table. This bug was only harmless by luck, and
the same shape of error would not be visible in the output.

### 5. 156 rows with `title = NA` enter the estimation array — **DONE**

`code/creating dataset/create_dataset.R:105`

`full_join(df_euro, gne_merge, ...)` runs *after* the `unit` filter, so its
orphans survive into `dfpg`. They pass `!(title %in% vars_to_exclude)` because
`NA %in% x` is `FALSE`. They contribute nothing to `feols` but corrupt any
`nrow()`, `distinct()`, or summary computed on the file — see item 6.

*Fix:* `full_join` replaced with
`left_join(..., relationship = "many-to-one")` plus
`stopifnot(nrow(dfg) == nrow(df_euro), !anyNA(dfg$title))`.

*Verified:* orphan rows are now **0** and the panel is 54,177 rows. Both the
main table and the summary statistics table are byte-identical, confirming these
rows never entered a regression — they only corrupted counts and summaries.

### 6. Summary statistics computed at the wrong key — **DONE**

`code/tables and figures/create_summary_stats_table.R:32`

`distinct(country, ysp, title, .keep_all = TRUE)` keeps the *first* row per key,
and `p_list` is bound in order `p0, p1, p2` — so the published error moments are
**`py == 0` only** (nowcasts) while the regression pools all three horizons.
One- and two-year-ahead errors are substantially larger, so the table understates
the estimation sample's dispersion. `dfpg_cy` is also taken before any EU
restriction, so the GDP row includes non-members.

*Verified:* `distinct()` kept 22,228 rows, **every one of them `py = 0`**.

Two problems were compounding, pulling in **opposite directions**:
1. the `py` bug understated the errors (nowcasts only);
2. the sample bug overstated them far more -- `dfpg_err`/`dfpg_cy` were taken
   from the whole panel, so they included Norway, Switzerland, the US and Japan,
   which never enter any regression (item 7). Their absolute errors are enormous.

The second dominates. Published squared-error SD was 59,422 against 1,725 in the
sample actually estimated on, and the maximum 4,408,278 against 75,418 -- the SD
was 34x too large.

*Fix:* descriptives now computed on the rows entering the pooled Panel A model
(`rev|exp`, all model variables finite). Country-year covariates are one row per
`country x ysp` **within that sample**. N is now 11,413, which **exactly matches
column (3), Panel A of the main table** -- the script asserts and prints this.

Per-variable N was previously 416 / 8,364 / 8,354 / 608 / 725 / 605 / 339,
each variable carrying its own missingness across the full panel; it is now 444
for every country-year variable and 11,413 for the error variables, from one
coherent sample. Only EPU legitimately differs (212), being available for 14
member states.

Wired to file: writes `overleaf/tables/summary_stats.tex` with the
`
esizebox{	extwidth}` wrapper matching the paper, and
`main_sage_template.tex:211` now `\input`s it. Template compiles clean.

Note for the prose pass: line 207 says "the number of observations varies across
variables with data availability", which is now largely untrue -- only EPU
differs. That line also contains a typo, "a standard deviation **fo** roughly
20".

### 7. Estimation sample defined by accident — **DONE**

`code/creating dataset/create_dataset.R:101`

The only row restriction is `unit == "Mrd ECU/EUR"`, which admits Norway,
Switzerland, Iceland, Serbia, Montenegro, Albania, Turkey, US, Japan (7,383 rows
for Norway/Switzerland/US/Japan alone). They drop out *only* because
`population.csv` lacks them, so `log(pop_int)` deletes them. Drop that covariate
in any robustness check and non-members enter an "EU member state" regression.

*Fix:* filter explicitly on `EU_MEMBERS` in the build (the vector now exists in
`clean_data15_23.R` — move it somewhere shared).

### 8. Outcome measured against three different truth vintages — **DOCUMENTED / WONTFIX**

`clean_data11_14.R:166-185`, `clean_data15_23.R:346-360`

Errors for 2011–2013 use `true1` (Spring 2014), 2014 uses `true2` (Autumn 2017),
2015–2022 use `true3` (Spring 2024). So `err` embeds 1–3 years of national
accounts revisions in the first block and up to 9 in the second — `err_sq` is
not the same quantity across the panel. `ysp` fixed effects absorb the level
shift but not differential revision behaviour across countries.

**Decision: left as published.** The two-database split is deliberate and the
paper states it — an ESA 1995 forecast must be scored against an ESA 1995
actual, or an accounting-definition change is measured as forecast error. The
ESA break falls exactly between Spring and Autumn 2014, confirmed against the
raw files.

What remains, recorded rather than changed:
- There are **three** vintages, not the two the paper describes. `true2` is
  Autumn 2017, supplying the 2014 actual. It appears to be a historical artifact
  — the newest release available when that script was written. Spring 2024 *does*
  contain a 2014 column, so extracting `true14` from `s24_full` would put every
  ESA 2010 target on one benchmark.
- Because of that, **87% of country × title × target-year cells for targets
  2015–2016 carry two different `true` values** (e.g. Austria 2015: 50.19 vs
  50.24, and 0.51 vs 0.62). Target years 2015–2016 are reached both from the
  Autumn 2014 vintage (scored on Autumn 2017) and from the 2015+ vintages
  (scored on Spring 2024), so the benchmark is partly confounded with `py`.
  Roughly 2,457 of 11,413 estimation rows are affected.
- The `first(true)` bug in `create_theil_u_results.R:39` is moot — that script
  has been **deleted** (see item 18).

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

### 11. Three datasets have no producer — **DONE**

`data/guide_rate.Rdata` (loaded by `create_dataset.R`), `data/final_dataset_euro.Rdata`,
`data/bonds_with_min.Rdata`. No script in the repo built any of them, so a clean
rebuild errored and the trace back to raw input dead-ended.

**`guide_rate.Rdata` recovered.** The producer was `code/guiderate.R`, deleted in
commit `3f6709f` ("cleaning up code for submission", 2025-10-02) while the
`.Rdata` stayed in the repo. Restored as
`code/creating dataset/create_guide_rate_variable.R`, repointed from the old
`~/ec_project/data/` path to this project, with the legacy tail dropped (it
rebuilt `final_dataset_euro_plus_guide` and wrote presentation figures, both
obsolete now that `create_dataset.R` does the pooling). Inputs `EP.csv` and
`Council.csv` were located in `data/old_data/`. Added a key-uniqueness assertion,
since `create_dataset.R` now joins this many-to-one.

The guiding rate is the average of a state's Council voting weight share, EP seat
share, and population share; `diff_iv` is its Commission share minus that
benchmark, plus four lags.

*Regeneration is faithful but not byte-identical*, for two traceable reasons,
neither a restoration error:
- Greece now has ysp 2022.5 and 2023 rows. The UK/Greece filter removed in item 2
  **was** live when `population.csv` still ran past 2022; it only became a no-op
  later when the fetch was narrowed to `end_date = 2022`.
- 36 rows shift by exactly 0.1–0.3 because the World Bank has revised its
  historical population estimates since the original fetch, and `round(rate, 1)`
  turns a sub-0.05% share change into a visible jump. **The rounding makes this
  variable brittle to trivial input changes** — worth knowing.

Main table and summary statistics are byte-identical (neither uses `diff_iv`).
The two marginal-effects figures do depend on it and were regenerated.

**`final_dataset_euro.Rdata` restored.** It holds the unpooled euro panel, which
the bonds appendix needs with `p0`/`p1`/`p2` side by side rather than stacked by
horizon. `create_dataset.R` now saves `df_euro` under the name `df`, which is
what the bonds script loads.

**`bonds_with_min.Rdata` eliminated rather than recreated.** Only its `min`
column was ever used -- `min_abs`, the `_lead` columns and `date_lead1` are
vestigial, and the analysis recomputes `yield_lag1` itself. `min` is the signed
number of days to the nearest forecast release (so `post` is `min > 0`), and it
is now computed in the analysis script, vectorised, replacing a row-by-row loop
that had been commented out before its script was deleted.

*Verified:* the reconstruction reproduces the stored `bonds_with_min$min`
**exactly on all 81,339 rows** (using the full release set -- the event window
separately drops the first release, as the original did), and the refactor is
behaviour-neutral: original and new code both give N of 292/292/292/290 on the
stored inputs.

**Every dataset in `data/` now has a producer.**

*Effect on the bonds appendix:* N becomes 294 across all four models. Two claims
in the text needed correcting, for different reasons:
- Revenue updates remain jointly significant in model 2 at p = 0.07 on the old
  data but **p = 0.14** on the corrected panel, so "the significance for revenue
  holds when I remove expenditure updates" no longer stands. **Broke today.**
- Lending updates were described as jointly significant at p = 0.001; they are
  **p = 0.06**, and were already 0.06 against the stored data files. **Already
  stale**, as was the reported N of 338.

Unchanged: only the expenditure 1-year update is individually significant, all
updates are jointly significant at p = 0.00, revenue joint in model 1 is
p = 0.00, and expenditure significance does not survive removing revenue
(p = 0.32). Expenditure joint in model 1 is now significant at 0.05 rather than
the 0.1 claimed.

### 12. Every table is hand-pasted into LaTeX — **DONE (paper tables)**

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

**All four tables that appear in the paper or appendix are now generated**:
main results, summary statistics, Oster sensitivity, and alternative outcomes.
Each is written to `overleaf/tables/` and read with `\input`.

The alternative-outcomes table (appendix) also had two labelling problems fixed
at the same time: its caption was byte-identical to the main results table's, so
two different tables shared a caption (labels differed, so cross-references
resolved -- it now reads "...Under Alternative Error Measures"); and its footnote
said "Clustered (country)" where every other table says "Clustered (state)".
Its pasted numbers were stale, e.g. N of 8,089 against a current 8,857.

Still open, both unused by the paper: `create_EPU_model_table.R` (largely
subsumed by column 4 of the main table, and carrying a duplicate
`\label{tab:main_model}` -- item 16) and `create_bonds_analysis_table.R`.

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

### 19. Two paper figures were never committed to Overleaf — **DONE**

Found while regenerating guide-rate figures. `overleaf/.gitignore:2` ignores
`*.pdf` as a build artifact, which also caught the figures. Ten images predate
the rule and are tracked; four newer ones were not, and **two of those are
`\includegraphics`'d by the paper** — `gam_marginal_effects_plot.pdf` and
`alt_outcome_plot.pdf`, each with **zero commits** in the Overleaf repo. They
were therefore absent from the project on Overleaf while present on this machine.

The same rule was hiding uncommitted modifications to `main_plot.pdf` and
`oster_plot.pdf`, which never showed up in `git status`.

*Fix:* `!images/*.pdf` exception added, all figures committed, and the
data-dependent ones regenerated first so the committed versions match the
rebuilt panel.

*Randomization figure refreshed.* Re-ran the 10,000 fits (4.7 min, seed 42).
`randomization_inference.R` now also **saves its draws** to
`data/randomization_draws.rds` and **prints its p-values**, which were
previously computed and then discarded when the session ended — so the numbers
behind the published figure were not recoverable without repeating the run.

| | paper | now |
|---|---|---|
| one-sided p (t-stat) | 0.004 | **0.0082** |
| two-sided p (t-stat) | — | 0.0177 |
| one-sided p (raw coef) | — | 0.0162 |
| true t | — | -4.617 |

The paper's claim that the main t-statistic is "more negative than more than
99\%" of simulated ones **still holds** (99.18%). Only the parenthetical
`p = 0.004` needs updating to 0.008.

### 18. Theil's U analysis deleted — **DONE**

`create_theil_u_results.R` computed Theil's U (forecast error relative to a
naive no-change benchmark) and re-ran the `ecfin` models on it. Grepping all six
`.tex` files found **no reference to it anywhere** — not the paper, not the
appendix, not the response to reviewers — and nothing `\includegraphics`ed
`theil_u_plot.pdf`. It was unpublished exploratory work carrying a real bug
(`first(true)`, item 8), so it was deleted at the author's direction along with
its two orphaned PDFs. Recoverable from git history.

### 17. Raw `ecfin` is missing for a third of the panel's vintages — **WONTFIX**

Found while investigating item 3. Countries with a non-missing raw `ecfin`:

| ysp | 2012 | 2013 | 2014 | 2014.5 | 2015 | 2015.5 | 2016.5 | 2017.5 | 2019.5 | 2020 |
|---|---|---|---|---|---|---|---|---|---|---|
| n | 1 | 1 | **0** | 29 | 29 | **0** | **0** | **0** | **0** | **0** |

The main model uses raw `ecfin`, not `ecfin_int`, so **ysp 2014, 2015.5, 2016.5,
2017.5, 2019.5 and 2020 contribute nothing to any regression**, and 2012 and
2013 contribute a single country each. The effective vintage coverage of the
published table is roughly half the panel, and neither the table notes nor the
text says so.

This is presumably why `ecfin_int` / `ecfin_spline` exist as robustness checks
(item 14 concerns how they are built). But the main specification's sample is
being defined by staff-bulletin availability in a way no reader could infer.

**Decision: no change.** Coverage is already visible in the paper. Figure
`fig:ecfin_nat` plots `geom_col(ysp, ecfin)` faceted by country, so periods
without staff data appear as absent bars, and the surrounding text describes the
2011-2014 versus 2015-2023 collection difference. Recorded here so the gap in
the estimation sample is documented for anyone reading the code.

### 16. Smaller items — **DONE**

- `create_ash_analysis.R` printed `"< 50"` where the threshold was `5`. Both the
  condition and the two messages now derive from one `MIN_TITLE_OBS` constant.
  ASH appears in no `.tex` file, so this was cosmetic. Its `log(err_sq + 1e-6)`
  is left alone -- also unpublished.
- **`create_EPU_model_table.R` duplicated `\label{tab:main_model}`.** This one
  mattered: the EPU table *is* published (appendix `tab:epu_model`), and the
  appendix had been hand-corrected while the script had not. Regenerating would
  have produced two tables with the same label, so every `ef{tab:main_model}`
  would have resolved to whichever came second. Label and caption fixed, table
  wired to `overleaf/tables/epu_table.tex`, appendix now `\input`s it. Its
  pasted numbers were stale (N 5,038 against a current 5,434). Its commented-out
  `writeLines` also pointed at **`main_table.tex`** -- uncommenting it would have
  overwritten the main results table.
- `create_ECFIN_nationality_figure.R` loaded `Commission_nationalities.Rdata`
  (which holds `staff` and has no `ysp` column) then plotted `staff_nat`.
  **Confirmed**: the old version fails with `object 'staff_nat' not found` from a
  clean session, so the published figure was reproducible only by accident of run
  order. Now loads `staff_nat.Rdata`; the 2014.75 magic number is named
  `CONTRACT_STAFF_ADDED`.
- `create_result_excluding_covid_table.R:29-36` mixes target-year and vintage
  clauses; still retains 2022-target forecasts made in 2022.
- `create_bonds_dataset.R` — all five shift column names were wrong (`n = -1:3`
  with `type = "lag"` made `yield_lag1` a lead, `change_pct_lag1` plain `yield`,
  and the "lead" columns lags). Each now computes what its name says. Verified
  the bonds table is unchanged, confirming nothing downstream read them.
- `raw/Staff/iso_alpha2.csv:156` is unquoted `NA,"Namibia"` -- Namibia's real
  ISO alpha-2 code. Now read with `na.strings = ""` plus assertions on key
  non-missingness and uniqueness, so an unparsed staff code falls through to
  "Other" instead of being matched to Namibia. Verified behaviour-neutral: the
  regenerated `staff` object is `identical()` to the previous one.
- `create_population_variable.R` -- the no-op UK/Greece filter: resolved in
  item 2.
- `did.R`, `croatia_synth.R`, `amelia_analysis.R` are documented in CLAUDE.md as
  pipeline analyses but contain no estimator / do not parse. `croatia_synth.R`
  and several others read from `~/ecb_project/`, a different project.
- Figures wrote to two divergent trees. **No Dropbox path remains in `code/`**:
  `create_ECFIN_nationality_figure.R`, `representation_grid_plot.R` and
  `create_interaction_plots.R` now write to `overleaf/images/`. The two published
  figures affected (`ECFIN_Nationality_Plot.pdf`, `Representation_Plot.png`) were
  regenerated to the correct location.
