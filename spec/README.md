# `spec/` — how to extend the PSID-SHELF pipeline

This folder holds **all the construction data** the R pipeline needs: which raw
PSID variable feeds each SHELF variable in each wave, the human labels, the value
codes, and the publish order. The R scripts contain only *logic* (recodes,
derivations); everything that is just *specification* lives here. That separation is what
lets you add **new waves, new variables, and new domains by editing these files**
(plus a small amount of recode R), without touching the entire engine.

---

## The files

| File | Schema | Role |
|------|--------|------|
| `parameters.json` | `psid_lastwave`, `year[]`, `wlthyear[]`, `inflate_year`, `pcepi{}` | the **wave list** and the PCEPI inflation index |
| `input_var_map.csv` | `newvar, year, input_var` | for a **time-varying** SHELF var, the raw PSID variable in each wave |
| `input_var_single.csv` | `newvar, input_var` | for a **time-invariant** SHELF var, its one raw PSID variable |
| `var_labels.csv` | `newvar, label` | the variable description written to the `.dta` |
| `var_value_label_map.csv` | `newvar, label_set` | which value-label set a SHELF var uses |
| `value_labels.csv` | `label_set, value, label` | the code → text definitions (e.g. `1 = "Male"`) |
| `publish_vars.csv` | `domain, token, order` | which variables are published, in which order (`token` may contain `*`) |
| `time_invariant_vars.txt` | one stub per line | variables that carry **no** `_<year>` suffix (constant across waves) |
| `metadata.csv` | `key, value` | *optional* upstream Stata-release strings (release no. / retrieved / compiler), folded into the generated `metadata/<version>.yaml` provenance |
| `unlabeled_sets.txt` | one set name per line | label sets defined dynamically in R, so the loader skips them silently |

A SHELF variable named `foo`:
- is **time-varying** if it appears in `input_var_map.csv` → the pipeline builds
  `foo_1968`, `foo_1969`, … one column per listed wave, and the publish step
  reshapes those into long rows keyed by `(ID, YEAR)`.
- is **time-invariant** if it appears in `input_var_single.csv` and is listed in
  `time_invariant_vars.txt` → one column `foo`, repeated across all waves in long.

---

## Finding the raw variable names: the PSID Cross-Year Index

A single concept (say "age") is a **different** raw variable in every wave:
`ER30004` in 1968, `ER30023` in 1969, … `ER34904` in 2021. The map from concept
to per-wave variable name is exactly the
[**PSID Cross-Year Index**](https://psidonline.isr.umich.edu) (Data Center →
"Variable Cross-Year Index", or the per-variable "cross-year" links in the
codebook). Use it to fill the `input_var` column of `input_var_map.csv`.

When you build a raw extract in the PSID Data Center, **add the new variables /
new wave to the cart** so they land in the downloaded fixed-width file.
`01-ingest.R` reads **every** column in that file, so once a raw variable is in
the extract it is already in `psid_abridged` and available to the pipeline —
ingestion needs no per-variable change (only a new wave needs new files; see
below).

---

## Recipe 1 — Add a new variable from an existing wave

You want to surface a raw PSID variable that the pipeline currently ignores
(e.g. a new health item present from 1999 on). Say the new SHELF variable is
`health_newitem`, in the `chronic_conditions` domain, time-varying.

1. **Raw data**: make sure the per-wave raw variables are in your extract
   (Cross-Year Index → add to cart → re-download). Confirm they appear as
   columns after `01-ingest.R`.

2. **`input_var_map.csv`** — add one row per wave it exists in:
   ```csv
   health_newitem,1999,ER15553
   health_newitem,2001,ER19612
   …
   ```
   (Omit waves where it was not asked; the pipeline simply won't build those
   `_<year>` columns, and they become `NA` in long.)

3. **`var_labels.csv`** — one row:
   ```csv
   health_newitem,Newly added health item
   ```

4. **Value labels** (optional, if it's categorical):
   - add the set to `value_labels.csv`:
     ```csv
     healthnew_3cat,1,Yes
     healthnew_3cat,2,No
     healthnew_3cat,9,Don't know
     ```
   - point the variable at it in `var_value_label_map.csv`:
     ```csv
     health_newitem,healthnew_3cat
     ```

5. **`publish_vars.csv`** — add it so it reaches the output (pick the domain and
   an `order` near its neighbours):
   ```csv
   chronic_conditions,health_newitem,57
   ```

6. **Recode logic** — in `R/collect/chronic_conditions.R`, add a `collect_tv`
   block. Map each raw code with `recode()`, which reads like the codebook (see
   [the demographics example](../R/collect/demographics.R)):
   ```r
   psid_abridged <- collect_tv(psid_abridged, "health_newitem", function(x, y) recode(x,
     1        ~ 1,    # Yes
     5        ~ 2,    # No  (PSID often codes No as 5)
     c(8, 9)  ~ 9))   # DK/NA
   ```
   How the two pieces work:
   - **`collect_tv(df, newvar, fn)`** is the per-wave loop. It reads the
     `(year, input_var)` rows for `newvar` from `input_var_map.csv`, and for each
     wave calls your `fn` on that wave's raw column, stores the result as
     `newvar_<year>` (e.g. `health_newitem_1999`), and attaches the variable +
     value labels. So **the map says *where* the data comes from each wave; your
     `fn` says *how* to recode it.** The same `fn` is replayed for every wave,
     which is why adding a wave is usually just new rows in the map.
   - **`recode(x, code ~ value, …)`** is the recoding front-end. Each rule maps
     input codes (left of `~`) to an output value (right of `~`); rules are applied
     **top to bottom and the last matching rule wins**, exactly like a Stata
     `gen out = -1` followed by a series of `replace out = value if …`. Any
     position no rule touches keeps the sentinel `-1`, flagging an unhandled code.
     It mirrors the PSID codebook directly — see the
     [**Recoding cheatsheet**](#recoding-cheatsheet-recode-rules) section below.

   For a **time-invariant** variable use `input_var_single.csv` +
   `time_invariant_vars.txt` + `collect_inv(...)` (one input, one output column,
   `fn` is just `function(x) recode(x, …)`) instead.

7. **Run** `Rscript 00-run-all.R`. The new column flows through collect → revise →
   publish into both `output/…_LONG.parquet` and `…_LONG.dta`.

> Sentinel check: any `-1` left after your recode means a raw code you didn't
> handle. `check_unassigned()` (in `R/programs.R`) flags these — cover every code
> the codebook lists, or map the leftovers to `NA`/a DK value explicitly.

---

## Recoding cheatsheet (`recode` rules)

`recode(x, code ~ value, …)` is how every collect domain maps raw PSID codes to
SHELF values. It reads like the codebook: each rule sends some input **codes** (the
left of `~`) to one output **value** (the right). The most common rules:

| Codebook intent | Rule |
|-----------------|------|
| a single code `k` becomes `v` | `k ~ v` |
| several codes become `v` | `c(a, b, c) ~ v` |
| a whole range `lo`…`hi` (inclusive) becomes `v` | `lo %..% hi ~ v` |
| **keep the raw value unchanged** (passthrough) | `… ~ keep` &nbsp;e.g. `2 %..% 110 ~ keep` |
| a code (or codes) means missing | `c(8, 9) ~ NA` |
| blank / `.` (an `NA` *input*) should be `NA` | put `NA` in the code set: `c(8, 9, NA) ~ NA`, or `NA ~ NA` on its own |
| everything not matched | stays `-1`; pass `.default = 0` to start from another value |

Example (PSID "age reported", 1968–present): code `1` → `1`, actual ages `2..110`
pass through, `999`/`0` → missing:

```r
recode(x,
  1          ~ 1,
  2 %..% 110 ~ keep,
  c(999, 0)  ~ NA)
```

Semantics that keep the translation faithful to Stata:

- **Order matters, last match wins.** Later rules overwrite earlier ones on the
  same positions, so list narrowing/override rules after the broad ones — exactly
  like a chain of `replace out = value if …`.
- **An `NA` *input* never matches a code rule** (just as Stata's `if` never matches
  a missing comparison). To send blanks to `NA` you must say so — add `NA` to the
  code set, or a `NA ~ NA` rule. Omit it and blanks stay `-1` (occasionally
  intended; see `fuid` / `rindiv` in
  [`survey_identifiers.R`](../R/collect/survey_identifiers.R)).
- **`keep` passes the raw value through** unchanged; a plain value on the right
  replaces it.

**When `recode` isn't enough.** `recode` covers per-value/per-range maps of a
single column. For a rule that depends on *another* column or wave (e.g. a
sample-conditional wild code, or an inches value that's only valid when the feet
value is real), do the bulk with `recode`, then refine with the underlying
primitive **`rc(out, cond, val)`** — Stata's `replace out = val if cond`, NA-safe
like the `if`. See [`earnings.R`](../R/collect/earnings.R) (sample-conditional) and
[`geography.R`](../R/collect/geography.R) (FIPS lookup). For coding that changed
across eras, branch on the wave `y` and return a `recode(…)` per era, as in
[`family_income.R`](../R/collect/family_income.R).

---

## Recipe 2 — Add a new wave (year) of PSID

When PSID releases the next wave (e.g. **2023**):

1. **Raw data**: download the new wave's extract files into
   `raw-data/downloaded-from-psid/…` and extend `01-ingest.R` to read them
   (mirror how the existing fixed-width / supplement files are read and joined on
   `ER30001`/`ER30002`). The new wave's raw variables must end up as columns in
   `psid_abridged`.

2. **`parameters.json`** — add the wave to `year` (and to `wlthyear` if it's a
   wealth wave), and bump `psid_lastwave`:
   ```json
   "psid_lastwave": 2023,
   "year": [ …, 2021, 2023 ],
   ```
   Add the new year's **PCEPI** value to `pcepi` (and, if a newer base year is
   used, update `inflate_year`) so inflation adjustment covers the wave.

3. **`input_var_map.csv`** — for **every** time-varying SHELF variable that was
   asked in the new wave, add a row with that wave's raw variable name (look each
   one up in the **Cross-Year Index**):
   ```csv
   demo_age_rep,2023,ER36017
   finc_tot_nd,2023,ER36...
   …
   ```
   This is the bulk of the work for a new wave. A variable with no 2023 row simply
   has no `demo_age_rep_2023` column (→ `NA` for 2023 in long).

4. **Run** `Rscript 00-run-all.R`. Because the wave list drives the build and the
   reshape, the long file automatically gains the new `(ID, 2023)` rows.

No recode R changes are needed for a new wave **unless** PSID changed a variable's
coding scheme that wave — then adjust the relevant `collect_*` block with a year
gate that returns a `recode(…)` per era, e.g.
`if (y >= 2023) recode(x, …) else recode(x, …)` (see the era branches in
[`family_income.R`](../R/collect/family_income.R) / [`geography.R`](../R/collect/geography.R)).

---

## Recipe 3 — Add a whole new domain

To group a set of related new variables under their own file (e.g. `housing`):

1. **Create `R/collect/housing.R`** (and `R/generate/housing.R` if it has derived
   cross-year variables) following the existing domain files.

2. **Register the domain name** in the driver vectors so it gets sourced:
   - `collect_domains` in [`04-collect-inputs.R`](../04-collect-inputs.R)
   - `generate_domains` in [`05-generate-variables.R`](../05-generate-variables.R)
     (only if you added an `R/generate/housing.R`)
   - `dom_order` in [`07-publish.R`](../07-publish.R) — controls where the
     domain's variables sit in the published column order.

3. **Add the domain's variables** to `input_var_map.csv` / `input_var_single.csv`,
   `var_labels.csv`, value-label files, and `publish_vars.csv` (with
   `domain = housing`) exactly as in Recipe 1.

4. **Run** `Rscript 00-run-all.R`.

The drivers source whatever domain files exist, so a new domain is picked up
automatically once its name is in the vectors above.

---

## After any change: verify

```sh
Rscript 00-run-all.R                       # rebuild output/
Rscript 08-validate-output.R [n_vars]      # compare against a reference .dta
```

`08-validate-output.R` reports column coverage, the `(ID, YEAR)` key, and
per-variable value agreement, so you can confirm new variables landed and existing
ones are unchanged. It also saves each run as a human-readable report under
`log/` (`validate-output_<timestamp>.txt`, plus a `…_latest.txt` copy), so you can
diff successive validation runs. Keep a copy of the prior `output/…_LONG.parquet`
as a regression baseline if you want to diff before/after for the variables you
didn't touch.

## Conventions to respect

- **Sentinel `-1`** marks "not yet assigned" during collect/generate (the default
  start value of every `recode`); never let it reach the output — cover every raw
  code or map leftovers to `NA`.
- **Missing is `NA`** (Stata `.`), distinct from a real "Don't know" code.
- **Time-varying** variables are named `<stub>` here and become `<stub>_<year>`
  columns internally; the publish step strips the suffix and reshapes to long.
- **`input_var` names** must match the raw columns in `psid_abridged` exactly
  (PSID `ER#####`, or the supplement names like `MH#`/`CAH#`/`PID#`).
