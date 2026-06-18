# `raw-data/` — the PSID inputs (download these yourself)

The pipeline reads its inputs from this folder. **Nothing here is committed to git**
(`raw-data/*` is in [`.gitignore`](../.gitignore)) — the files are large and come
straight from the [PSID Data Center](https://psidonline.isr.umich.edu/), which
requires a (free) account and agreeing to PSID's conditions of use. You download
them once into the layout below, then run `Rscript 00-run-all.R`.

```
raw-data/
├── downloaded-from-psid/     ← REQUIRED — the pipeline reads these (01-ingest.R)
│   ├── ascii/                  custom main extract (your variable cart)
│   ├── mh85_23/                Marriage History supplement
│   ├── cah85_23/               Childbirth & Adoption History supplement
│   └── pid23/                  Parent Identification File supplement
├── codebooks/                ← OPTIONAL — family/individual codebook PDFs for reference
└── psid-shelf-original/      ← OPTIONAL — reference release for 08-validate-output.R
```

Only `downloaded-from-psid/` is needed to build the output. `codebooks/` and
`psid-shelf-original/` are reference material (see [the bottom](#optional-folders)).

---

## What the pipeline expects in `downloaded-from-psid/`

[`01-ingest.R`](../01-ingest.R) reads four PSID products. Each PSID download is a
zip that unpacks to a data file plus SAS/SPSS/Stata setup files and a codebook; the
pipeline uses the **fixed-width `.txt` data** and the **`.sas` setup file** (it
parses column positions and variable labels out of the SAS `INPUT`/`ATTRIB`
blocks). Put each product in its own subfolder with these exact names:

| Subfolder | PSID product | Files `01-ingest.R` reads | What it is |
|-----------|--------------|---------------------------|------------|
| `ascii/` | your custom main extract | `J######.txt` + `J######.sas` | the main 1968–2021 individual+family panel (84,120 persons × ~6,880 vars) — the bulk of the data |
| `mh85_23/` | Marriage History 1985–2023 | `MH85_23.txt` + `MH85_23.sas` | one row per marriage; pivoted to `MAR{n}_MH{col}` and joined on person |
| `cah85_23/` | Childbirth & Adoption History 1985–2023 | `CAH85_23.txt` + `CAH85_23.sas` | one row per child; pivoted to `CHI{n}_CAH{col}` and joined on parent |
| `pid23/` | Parent Identification File 2023 | `PID23.txt` + `PID23.sas` | one row per individual; joined on person |

> **⚠ The main extract's job number.** When PSID builds your cart it names the files
> after a job number — `J362500.txt`/`J362500.sas` in the reference build. **Your
> cart will have a different number.** `01-ingest.R` currently hard-codes
> `J362500.*` ([lines 37–38](../01-ingest.R#L37-L38)), so either rename your files
> to `J362500.txt`/`J362500.sas`, or edit those two paths to match. The supplement
> file names (`MH85_23`, `CAH85_23`, `PID23`) are stable and don't change.

The supplements join to the main extract on the 1968 person key
(`ER30001` = 1968 interview number, `ER30002` = person number), so all four come
from the same PSID sample and need no manual alignment.

---

## How to download each one

All four start at <https://psidonline.isr.umich.edu/> → **Data**. Log in first.

### 1. `ascii/` — the custom main extract (variable cart)

This is the one extract you assemble yourself; it must contain every raw variable
the pipeline maps in [`spec/input_var_map.csv`](../spec/input_var_map.csv) /
[`spec/input_var_single.csv`](../spec/input_var_single.csv).

1. **Data** → **Data Center**
2. **Variable list** (Cross-Year Variable Index) → tick the variables you need and
   **Add to cart**, then **generate** the data cart.
3. Choose the **ASCII Data With SAS Statements** download format.
4. Unzip into `raw-data/downloaded-from-psid/ascii/` and rename the `J######.txt`
   / `J######.sas` to `J362500.*` (or update the paths in `01-ingest.R` — see the
   note above).

> Use the **PSID Cross-Year Index** to find the per-wave variable name for each
> concept (a concept like "age" is `ER30004` in 1968, `ER34904` in 2021, …). The
> cart must include all of those. See
> [`spec/README.md`](../spec/README.md#finding-the-raw-variable-names-the-psid-cross-year-index)
> for how the cart connects to the spec, and how to add variables or a new wave.

### 2. `mh85_23/` — Marriage History

1. **Data** → **Packaged Data** → **Main and Supplemental Studies**
2. Select **Marriage History File (1985–2023)** and download the **ASCII + SAS**
   package.
3. Unzip into `raw-data/downloaded-from-psid/mh85_23/` (keep the `MH85_23.*` names).

### 3. `cah85_23/` — Childbirth & Adoption History

1. **Data** → **Packaged Data** → **Main and Supplemental Studies**
2. Select **Childbirth and Adoption History File (1985–2023)** and download the
   **ASCII + SAS** package.
3. Unzip into `raw-data/downloaded-from-psid/cah85_23/` (keep the `CAH85_23.*` names).

### 4. `pid23/` — Parent Identification File

1. **Data** → **Packaged Data** → **Main and Supplemental Studies**
2. Select the **Parent Identification File (2023)** and download the **ASCII + SAS**
   package.
3. Unzip into `raw-data/downloaded-from-psid/pid23/` (keep the `PID23.*` names).

A short copy of these steps also lives in
[`downloaded-from-psid/download_instructions.txt`](downloaded-from-psid/download_instructions.txt).

---

## Verify the layout

Before running the pipeline, confirm the four data files exist where `01-ingest.R`
looks for them:

```sh
cd raw-data/downloaded-from-psid
ls ascii/J362500.txt ascii/J362500.sas \
   mh85_23/MH85_23.txt mh85_23/MH85_23.sas \
   cah85_23/CAH85_23.txt cah85_23/CAH85_23.sas \
   pid23/PID23.txt pid23/PID23.sas
```

`01-ingest.R` `stopifnot()`s on these folders/files and will tell you immediately if
one is missing. Then:

```sh
Rscript 00-run-all.R        # ingest → collect → generate → revise → publish
```

---

## Optional folders

- **`codebooks/`** — PSID family-file codebook PDFs (`FAM####_codebook.pdf`, plus
  `IND2023ER`). The pipeline does **not** read these; they're for looking up a
  variable's codes when you write or check a recode in `R/collect/`. Download the
  per-year codebooks from the same Data Center pages.
- **`psid-shelf-original/`** — the published PSID-SHELF reference release
  (`PSIDSHELF_1968_2021_LONG.dta`) used by
  [`08-validate-output.R`](../08-validate-output.R) to compare this pipeline's
  output against the original, value-for-value. Only needed if you want to run that
  validation; point it elsewhere with the `PSIDSHELF_REF` env var or a CLI argument.
