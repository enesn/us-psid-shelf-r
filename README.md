# PSID-SHELF R pipeline

R pipeline that turns the raw PSID extract (`psid_abridged`, built by
`01-ingest.R`) into the clean **PSID_SHELF_R_\<fromyear>_\<toyear>_LONG** file. All
construction parameters (value labels, year→variable maps, publish lists) live
in `spec/`, so the pipeline reads only `spec/` and the raw PSID data.

## Run it

```sh
Rscript 00-run-all.R            # ingest -> collect -> generate -> revise -> publish
Rscript 08-validate-output.R    # compare LONG output to a reference release
```

Outputs land in `output/`:
- `PSID_SHELF_R_1968_2021_LONG.parquet` (185 MB) and `…_LONG.dta` (9.9 GB, single
  file, with variable + value labels via `haven`) — 3,533,040 rows × 552 cols
- `PSID_SHELF_R_1968_2021_WIDE.parquet` (196 MB)

The LONG table (~15 GB in memory) is built one column at a time and the wide
tables are freed (`rm` + `gc`) first, so `07-publish.R` stays well within RAM
rather than materialising the multi-copy `pivot_longer` that previously OOM'd.
Before writing, whole-valued columns that fit a 32-bit int are downcast to
`integer` (355 / 552 cols) so Stata stores them compactly; fractional columns
(weights) and out-of-range columns (inflated real-dollar amounts) stay `double`.

## Architecture

| Stage | File(s) | Role |
|------|---------|------|
| Load parameters/spec | `03-shelf-parameters.R` | construction parameters & value labels |
| Ingest raw extract | `01-ingest.R` | raw PSID -> `psid_abridged` |
| Shared helpers | `R/programs.R` | recode / label / cross-year helpers |
| Collect inputs | `04-collect-inputs.R` → `R/collect/<domain>.R` | input variables |
| Generate variables | `05-generate-variables.R` → `R/generate/<domain>.R` | derived variables |
| Revise variables | `06-revise-variables.R` → `R/revise/<part>.R` | not-in-FU / family-size / inflation |
| Publish (reshape) | `07-publish.R` | wide -> long, write parquet + dta |
| Orchestrator | `00-run-all.R` | runs the whole pipeline |

`spec/` (machine-generated, safe to regenerate):
`parameters.json`, `value_labels.csv`, `var_labels.csv`, `input_var_single.csv`,
`input_var_map.csv`, `var_value_label_map.csv`, `publish_vars.csv`,
`time_invariant_vars.txt`, `metadata.csv`, `unlabeled_sets.txt`.

## How a domain is built (the pattern)

Each collected variable starts at the sentinel `-1`, then recode rules overwrite
it by value (and, for time-varying variables, by wave), then value labels are
attached. In R, in `R/collect/<domain>.R`:

```r
# time-invariant (single input var):
psid_abridged <- collect_inv(psid_abridged, "demo_sex", function(x) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, inlist(x, 2), 2)
  out <- rc(out, inlist(x, 9), NA)
  out
})

# time-varying (per-wave input vars; fn receives the input column x and wave y):
psid_abridged <- collect_tv(psid_abridged, "demo_age_rep", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1),       1)
  out <- rc(out, inrange(x, 2, 110), x)   # passthrough
  out <- rc(out, inlist(x, 999),     NA)
  out
})
```

Helpers (`R/programs.R`):
`rc(out, cond, val)` is a NA-safe conditional assignment (`out[cond] <- val`);
`inrange`/`inlist`; `collect_tv`/`collect_inv` build the columns, pull the
year→input-var map from `spec/`, and attach variable + value labels
automatically; `set_value_labels` silently skips dynamic/optional label sets.

Generate files derive cross-year summaries (e.g. last-reported birth year) — plain
dplyr/vector R, added as new columns to `psid_abridged`. Revise files do the
not-in-FU recode, family-size and PCEPI inflation adjustments.

The collect/generate/revise drivers source whatever `R/<stage>/*.R` files exist,
so the pipeline runs and produces a valid (partial-coverage) LONG file as
domains are added — coverage grows monotonically.

## Validation against a reference release

`08-validate-output.R` compares the LONG output (auto-discovered from
`output/PSID_SHELF_R_<fromyear>_<toyear>_LONG.parquet`) against a reference release —
`raw-data/psid-shelf-original/PSIDSHELF_1968_2021_LONG.dta` by default, or any
path passed via `PSIDSHELF_REF` or the first CLI argument (3,533,082 rows ×
593 cols):

- **Coverage**: 552 / 593 reference variables reproduced; the 41 not-yet-produced
  are the `_rp`/`_sp` and combined variants in the most complex generators
  (education, race-ethnicity, COVID majority vars).
- **Rows**: ours 3,533,040 = 84,120 persons × 42 waves; reference has 42 more
  (84,121 persons) — a one-person difference that originates in **ingestion**
  (`01-ingest.R`), not the construction logic.
- **Value agreement** (40-variable sample, spread across domains): **mean 99.18%**,
  ≥99% on 34/40, 100% on 22/40. Lowest: `EDU_LEVEL_MAX` 88%, ADL/IADL items
  ~93% (the best-effort education + disability generators).

Run it with:  `Rscript 08-validate-output.R [n_vars]`

## Status

Complete and verified end-to-end: extraction, helpers, all domain builders,
reshape, labelled `.dta` + parquet output, orchestrator, and validation.

- **Collect** (`R/collect/`) — **23 / 23 domains**: survey (survey_identifiers,
  panel_status, sample_design); social (demographics, education, family_type,
  geography, race_ethnicity, time_use); health (chronic_conditions, covid_19,
  dementia, depression, disability, general_wellbeing); economic (employment,
  occupations, family_income, earnings, expenditures, primary_home, wealth);
  relationship_id (from the MAR*_MH* / CHI*_CAH* supplement columns).
- **Generate** (`R/generate/`) — **19 / 19** generating domains (the other 4 are
  pass-through with no derived variables).
- **Revise** (`R/revise/`) — recode_not_in_fu, family_size, inflation.

Known residual gaps (≈1%): the `_rp`/`_sp` and combined-majority variants in the
most complex generators (education levels, race-ethnicity, COVID) and the
one-person ingestion discrepancy.
