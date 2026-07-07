# PSID-SHELF-R pipeline

R version of the PSID-SHELF pipeline, originally developed in Pfeffer, Fabian T., Daumler, Davis, and Friedman, Esther. PSID-SHELF, 1968–2021: The PSID’s Social, Health, and Economic Longitudinal File (PSID-SHELF), Beta Release. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2025-02-24. https://doi.org/10.3886/E194322V2. 

The pipeline turns the raw PSID extract into the clean
**PSID_SHELF_R_\<fromyear>_\<toyear>_LONG** file. All construction parameters
(value labels, year→variable maps, publish lists) live in `spec/`, so the
pipeline reads only `spec/` and the raw PSID data.

> **To add variables, waves, or domains — start in [`spec/`](spec/README.md).**
> Everything the pipeline needs to know about *what* to build (which raw PSID
> variable feeds each SHELF variable, what the codes mean, what gets published)
> is a CSV or JSON file there. The R scripts contain only *logic*. The
> [`spec/README.md`](spec/README.md) has step-by-step recipes for all three
> extension types.

## Run it

```sh
Rscript 00-run-all.R            # ingest -> collect -> generate -> revise -> publish -> metadata
Rscript 08-validate-output.R    # compare LONG output to a reference release
```

Outputs land in `output/` (3,533,040 rows × 552 cols):
- `PSID_SHELF_R_1968_2021_LONG.parquet` (185 MB) and `…_LONG.dta` (9.9 GB, with
  variable + value labels via `haven`)
- `PSID_SHELF_R_1968_2021_WIDE.parquet` (196 MB)

The build also writes, to `metadata/`, a YAML run manifest `<version>.yaml`
(provenance, output schema, quality notes) and an Excel codebook
`codebook_<version>.xlsx` — sheets for every variable + its labels, the value
codes, the cross-year-index recoding (which raw PSID variable builds each SHELF
variable in each wave), and a per-domain summary, all derived from `spec/` + the
R recode files. Regenerate both with `Rscript 09-metadata.R`
(`PSID_META_HASH_BIG=0` skips hashing the ~10 GB `.dta`).

## Architecture

| Stage | File(s) | Role |
|------|---------|------|
| Ingest raw extract | `01-ingest.R` | raw PSID -> `psid_abridged` |
| Load parameters/spec | `03-shelf-parameters.R` | construction parameters & value labels |
| Shared helpers | `R/programs.R` | recode / label / cross-year helpers |
| Collect inputs | `04-collect-inputs.R` → `R/collect/<domain>.R` | input variables |
| Generate variables | `05-generate-variables.R` → `R/generate/<domain>.R` | derived variables |
| Revise variables | `06-revise-variables.R` → `R/revise/<part>.R` | not-in-FU / family-size / inflation |
| Publish (reshape) | `07-publish.R` | wide -> long, write parquet + dta |
| Metadata + codebook | `09-metadata.R` | write `metadata/<version>.yaml` manifest + `codebook_<version>.xlsx` |
| Orchestrator | `00-run-all.R` | runs the whole pipeline |

## How a domain is built

Each collected variable starts at the sentinel `-1`, then `recode()` rules
overwrite it by value (and, for time-varying variables, by wave), then value
labels are attached. In `R/collect/<domain>.R`:

```r
# time-invariant (single input var):
psid_abridged <- collect_inv(psid_abridged, "demo_sex", function(x) recode(x,
  1 ~ 1,
  2 ~ 2,
  9 ~ NA))

# time-varying (per-wave input vars; fn receives input column x and wave y):
psid_abridged <- collect_tv(psid_abridged, "demo_age_rep", function(x, y) recode(x,
  1          ~ 1,
  2 %..% 110 ~ keep,   # passthrough (keep the raw value)
  c(999, 0)  ~ NA))
```

Rules apply top to bottom, last match wins. See
[`spec/README.md`](spec/README.md#recoding-cheatsheet-recode-rules) for the full
cheatsheet (`k ~ v`, `c(a,b) ~ v`, `lo %..% hi ~ v`, `keep`, `NA`).

Generate files derive cross-year summaries (plain dplyr/vector R). Revise files
do the not-in-FU recode, family-size and PCEPI inflation adjustments. The
drivers source whatever `R/<stage>/*.R` files exist, so the pipeline always
produces a valid (partial-coverage) LONG file as domains are added.

## Validation

`08-validate-output.R` compares the LONG output against a reference release
(`raw-data/psid-shelf-original/PSIDSHELF_1968_2021_LONG.dta` by default, or a
path via `PSIDSHELF_REF` / first CLI argument):

- **Coverage**: 552 / 593 reference variables reproduced (the 41 gaps are
  `_rp`/`_sp` and combined variants in the most complex generators).
- **Rows**: ours 3,533,040 = 84,120 persons × 42 waves; reference has one more
  person — a difference that originates in ingestion (`01-ingest.R`).
- **Value agreement** (all 550 shared variables, every row): mean **99.943%**,
  exact-100% on 404, ≥99% on 545, none below 90%. The few remaining sub-99%
  variables are extract-vintage differences (our CAH/MH supplements are newer
  than the Feb-2025 reference, so `REL_CHI_*`/`REL_MAR_*` counts differ slightly)
  and `REL_CHI*_TYPE` rows for old child records that carry no birth year or id
  (their slot order follows the reference's native CAH file sequence).

Results print to the console and save to `log/validate-output_<timestamp>.txt`
(latest copied to `log/validate-output_latest.txt`).

## Release notes

### v1 — PSID-SHELF-R 1968–2021 (initial release)
- R port of the Stata PSID-SHELF construction covering 42 waves (1968–2021).
- 552 / 593 reference variables reproduced; 99.943% mean value agreement across all shared variables and rows.
- Outputs: `PSID_SHELF_R_1968_2021_LONG.parquet` / `.dta`, `_WIDE.parquet`; YAML manifest + Excel codebook in `metadata/`.

### 2023 wave extension 

Preparatory work to extend the file from 2021 to 2023. 

**Spec / pipeline changes (committed):**

| Commit | Change |
|--------|--------|
| `515b654` | Added `spec/update_input_var_map.R` — reusable script to bulk-populate `input_var_map.csv` for any new wave using the cross-year index |
| `aa7270e` | Added 423 rows to `spec/input_var_map.csv` for the 2023 wave (30 COVID supplement variables had no 2023 counterpart and were skipped) |
| `c04ef9e` | Added `spec/psid-cross-year-index.xlsx`; updated `spec/README.md` with new-wave workflow |
| `927c1f6` | Fixed `R/collect/covid_19.R`: 5 recode harmonization issues identified by comparing 2021 vs 2023 codebooks — `bin9→bin5` for `hosp_any`, `8→NA` added to `sev_fn`, new code `2` ("Not sure") handled for `test_ling_any`, code `98` added to NA catch for `hosp_num` |
| `c68ef68` | Removed stale `spec/metadata.csv` |
| `7aaa6a7` | Updated `spec/parameters.json`: `psid_lastwave`, `year[]`, `wlthyear[]` extended to 2023 |

---

## EconOps Call
This repository follows the [EconOps](https://github.com/enesn/EconOps) workflow. If you have the required authentication credentials, use [this script](https://github.com/enesn/EconOps/blob/main/implementation/step6.md#user-script-to-read-data) to query the dataset.