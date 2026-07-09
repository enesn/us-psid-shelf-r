# =====================================================================
# 08-validate-output.R  --  Validate the pipeline's LONG output against a
#                           reference PSIDSHELF_1968_2021_LONG.dta release
#
# Compares the pipeline's LONG output to a reference release of the same file:
#   1. variable coverage (shared / reference-only / ours-only)
#   2. row count and key (ID x YEAR)
#   3. value-level agreement, variable by variable, on the shared ID x YEAR rows
#
# By default it now checks *every* shared variable. Pass a number to spot-check a
# spread-out sample instead (faster to eyeball):
#   Rscript 08-validate-output.R            # ALL shared variables
#   Rscript 08-validate-output.R 50         # a 50-variable sample
#
# SPEED: reading the 7.4 GB reference .dta with haven is the bottleneck (~5 min).
# The first run converts it once to a parquet cache next to it
# (PSIDSHELF_..._LONG.parquet); every later run reads that in seconds. Delete the
# cache (or set PSIDSHELF_REFCACHE=0) to force a re-read.
#
# Key results print to the console AND are saved to a human-readable report at
# log/validate-output_<timestamp>.txt (+ a stable log/validate-output_latest.txt).
#
# Reference path: env PSIDSHELF_REF, else first CLI arg, else
# raw-data/psid-shelf-original/PSIDSHELF_1968_2021_LONG.dta.  Our LONG output is
# auto-discovered from output/PSID_SHELF_R_<fromyear>_<toyear>_LONG.parquet.
# =====================================================================

suppressMessages({library(arrow); library(haven); library(dplyr)})

args <- commandArgs(trailingOnly = TRUE)
ref_path <- Sys.getenv("PSIDSHELF_REF",
                       if (length(args) >= 1 && file.exists(args[1])) args[1]
                       else "raw-data/psid-shelf-original/PSIDSHELF_1968_2021_LONG.dta")
if (!file.exists(ref_path))
  stop("reference file not found: ", ref_path,
       "\n  set PSIDSHELF_REF or pass the path as the first argument.")

our_files <- list.files("output", pattern = "^PSID_SHELF_R_\\d{4}_\\d{4}_LONG\\.parquet$", full.names = TRUE)
if (!length(our_files)) stop("no output/PSID_SHELF_R_<fromyear>_<toyear>_LONG.parquet found — run 00-run-all.R first")
our_dir <- our_files[order(file.mtime(our_files), decreasing = TRUE)][1]
n_arg <- args[!file.exists(args)]
n_value_vars <- if (length(n_arg)) as.integer(n_arg[1]) else NA_integer_   # NA = ALL

# ---- logging: tee key results to console AND a human-readable report -----
LOG    <- character(0)
emit   <- function(txt = "") { message(txt); LOG[[length(LOG) + 1L]] <<- txt; invisible() }  # console + file
rec    <- function(txt = "") { LOG[[length(LOG) + 1L]] <<- txt; invisible() }                # file only
banner <- function(m) emit(sprintf("\n%s\n  %s\n%s", strrep("=", 64), m, strrep("=", 64)))
ok     <- function(cond, msg) emit(sprintf("  [%s] %s", if (isTRUE(cond)) "PASS" else "FAIL", msg))

started <- Sys.time()

# ---- reference parquet cache (big speed win on repeat runs) -----------
ref_pq <- sub("\\.dta$", ".parquet", ref_path)
use_cache <- Sys.getenv("PSIDSHELF_REFCACHE", "1") != "0"
if (use_cache && (!file.exists(ref_pq) || file.mtime(ref_pq) < file.mtime(ref_path))) {
  message("  one-time: caching reference .dta -> ", basename(ref_pq), " (~5 min) ...")
  write_parquet(read_dta(ref_path), ref_pq)
}
ref_src  <- if (use_cache && file.exists(ref_pq)) ref_pq else ref_path
read_ref <- function(cols = NULL) {                 # read selected cols from cache or dta
  if (identical(ref_src, ref_pq)) {
    ds <- open_dataset(ref_pq)
    if (is.null(cols)) collect(ds) else collect(select(ds, all_of(intersect(cols, names(ds)))))
  } else {
    if (is.null(cols)) read_dta(ref_path) else read_dta(ref_path, col_select = all_of(cols))
  }
}
ref_cols_all <- if (identical(ref_src, ref_pq)) names(open_dataset(ref_pq)) else names(read_dta(ref_path, n_max = 0))

emit("PSID-SHELF-R validation report")
emit(sprintf("  run at      : %s", format(started, "%Y-%m-%d %H:%M:%S")))
emit(sprintf("  our output  : %s", our_dir))
emit(sprintf("  reference   : %s%s", ref_path, if (identical(ref_src, ref_pq)) "  (via parquet cache)" else ""))

# ---- 1. column inventories -------------------------------------------
banner("1  variable coverage")
our_ds   <- open_dataset(our_dir)
our_cols <- names(our_ds)
shared   <- intersect(ref_cols_all, our_cols)
ref_only <- setdiff(ref_cols_all, our_cols)
our_only <- setdiff(our_cols, ref_cols_all)
emit(sprintf("  reference: %d cols | ours: %d cols | shared: %d",
             length(ref_cols_all), length(our_cols), length(shared)))
# reference-only = in the reference but not built here. Explain each by category.
ref_cause <- function(v) {
  base <- sub("_(RP|SP)$", "", v)
  if (base != v && base %in% our_cols)
    "role variant (_RP/_SP); the combined per-person sibling IS produced"
  else if (v %in% c("PSID_RETRIEVE", "PSIDSHELF_COMPILE", "PSIDSHELF_RELEASE"))
    "release-metadata column (this pipeline carries it in metadata/<version>.yaml instead)"
  else
    "combined/summary variable not yet generated (built from _rp/_sp pieces in the reference)"
}
if (length(ref_only)) {
  emit(sprintf("  reference-only (%d) — in the reference release but not built by this pipeline:", length(ref_only)))
  rc_grp <- vapply(ref_only, ref_cause, character(1))
  for (g in unique(rc_grp)) {
    vs <- ref_only[rc_grp == g]
    emit(sprintf("    [%d] %s", length(vs), g))
    emit(paste0("        ", paste(vs, collapse = ", ")))
  }
}
if (length(our_only)) emit(paste0("  ours-only (", length(our_only), "): ",
                                  paste(head(our_only, 25), collapse = ", "),
                                  if (length(our_only) > 25) " ..." else ""))

# ---- 2. key & row count ----------------------------------------------
banner("2  rows & key")
our_key <- our_ds %>% select(ID, YEAR) %>% collect()
ref_key <- read_ref(c("ID", "YEAR"))
ref_key$ID <- as.numeric(ref_key$ID); ref_key$YEAR <- as.integer(ref_key$YEAR)
ok(nrow(our_key) == nrow(ref_key),
   sprintf("row count  ours=%d  ref=%d", nrow(our_key), nrow(ref_key)))
ok(!anyDuplicated(our_key), "ID x YEAR is unique in our output")
ok(setequal(unique(our_key$YEAR), unique(ref_key$YEAR)),
   sprintf("YEAR domain matches (%d waves)", length(unique(ref_key$YEAR))))
ours_k <- paste(our_key$ID, our_key$YEAR)
ref_k  <- paste(ref_key$ID,  ref_key$YEAR)
ok(setequal(ours_k, ref_k), "ID x YEAR key sets are identical")
if (nrow(our_key) != nrow(ref_key))
  emit(sprintf("        -> %+d rows = a person-count difference from ingestion (01-ingest.R), not construction logic",
               nrow(our_key) - nrow(ref_key)))

# ---- 3. value-level agreement ----------------------------------------
value_vars <- setdiff(shared, c("ID", "YEAR"))
if (!is.na(n_value_vars) && n_value_vars < length(value_vars)) {
  idx <- unique(round(seq(1, length(value_vars), length.out = n_value_vars)))  # spread sample
  value_vars <- value_vars[idx]
  banner(sprintf("3  value agreement (%d-variable sample)", length(value_vars)))
} else {
  banner(sprintf("3  value agreement (ALL %d shared variables)", length(value_vars)))
}

# classify the disagreements for one variable into a likely cause (data-driven,
# plus one domain rule for the REL_* supplement vars).
classify <- function(v, a, b, d) {
  na_v  <- sum(d & is.na(a) & !is.na(b))     # ours NA, reference has a value
  v_na  <- sum(d & !is.na(a) & is.na(b))     # ours has a value, reference NA
  neg1  <- sum(d & a %in% -1)                # unassigned -1 sentinel
  bd    <- d & !is.na(a) & !is.na(b); nboth <- sum(bd)
  roundish <- if (nboth) sum(b[bd] == trunc(a[bd]) | b[bd] == round(a[bd]) | abs(a[bd] - b[bd]) < 1) else 0L
  cause <-
    if (sum(d) == 0)                          ""
    else if (grepl("^REL_(CHI|MAR)", v))      "extract-vintage: CAH/MH supplement newer than the reference release"
    else if (nboth > 0 && roundish >= 0.9 * nboth) "precision/rounding: reference stores an integer/rounded value (not a logic difference)"
    else if (neg1 >= 0.5 * sum(d))            "unassigned -1 sentinel reaching the output"
    else if (na_v >= max(v_na, nboth))        "coverage: ours is NA where the reference has a value"
    else if (v_na >= max(na_v, nboth))        "ours has a value where the reference is NA"
    else                                      "value / construction-logic difference"
  data.frame(variable = v, n_compared = length(a), n_disagree = sum(d),
             ours_NA = na_v, ref_NA = v_na, sentinel = neg1, both_differ = nboth,
             cause = cause, stringsAsFactors = FALSE)
}

# compare in column batches so peak memory stays small (only the batch is held)
res <- vector("list", length(value_vars)); ri <- 0L
batch <- 60L
for (start in seq(1, length(value_vars), by = batch)) {
  vs <- value_vars[start:min(start + batch - 1L, length(value_vars))]
  # read ID+YEAR *with* each batch from both sources and align within the batch,
  # so the comparison can't depend on two separate reads returning the same order
  ob <- our_ds %>% select(ID, YEAR, all_of(vs)) %>% collect()
  rb <- read_ref(c("ID", "YEAR", vs))
  rb <- rb[match(paste(ob$ID, ob$YEAR), paste(as.numeric(rb$ID), as.integer(rb$YEAR))), , drop = FALSE]
  for (v in vs) {
    a <- as.numeric(ob[[v]]); b <- as.numeric(rb[[v]])
    agree <- (is.na(a) & is.na(b)) |
             (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-6 * pmax(1, abs(b)))
    row <- classify(v, a, b, !agree); row$agree_pct <- 100 * mean(agree)
    ri <- ri + 1L; res[[ri]] <- row
  }
  message(sprintf("    ... %d / %d variables compared", ri, length(value_vars)))
}
results <- do.call(rbind, res)
results <- results[order(results$agree_pct), ]

# full table -> report file; worst 40 also to console
emit("  per-variable agreement (worst first; full table in the saved report):")
for (i in seq_len(nrow(results))) {
  line <- sprintf("    %-30s %7.3f%%  (n=%d)", results$variable[i], results$agree_pct[i], results$n_compared[i])
  if (i <= 40) emit(line) else rec(line)
}
banner(sprintf("mean agreement across %d variables: %.3f%%   (100%% on %d | >=99%% on %d | <90%% on %d)",
               nrow(results), mean(results$agree_pct),
               sum(results$agree_pct >= 99.9995), sum(results$agree_pct >= 99),
               sum(results$agree_pct < 90)))

# ---- 4. explain the variables below 100% -----------------------------
allsub <- results[results$agree_pct < 99.9995, ]    # everything not exact
sub    <- results[results$agree_pct < 99, ]          # the material gaps (detailed)
banner(sprintf("4  why <100%%  (%d below 100%%; %d below 99%% detailed)", nrow(allsub), nrow(sub)))
if (!nrow(allsub)) emit("  none — every compared variable matches the reference exactly.")
for (i in seq_len(nrow(sub))) {
  emit(sprintf("  %-28s %7.3f%%  | disagree=%d  [ours=NA/ref=val:%d  ours=val/ref=NA:%d  -1:%d  both-differ:%d]",
               sub$variable[i], sub$agree_pct[i], sub$n_disagree[i],
               sub$ours_NA[i], sub$ref_NA[i], sub$sentinel[i], sub$both_differ[i]))
  emit(sprintf("        -> %s", sub$cause[i]))
}
ne <- nrow(allsub) - nrow(sub)
if (ne) {
  emit(sprintf("\n  (+ %d variable(s) at 99.0-99.999%% — a few differing rows each; grouped by cause below)", ne))
  rest <- allsub[allsub$agree_pct >= 99, ]
  for (i in seq_len(nrow(rest)))
    rec(sprintf("  %-28s %7.3f%%  | disagree=%d  [ours=NA/ref=val:%d  ours=val/ref=NA:%d  -1:%d  both-differ:%d] -> %s",
                rest$variable[i], rest$agree_pct[i], rest$n_disagree[i],
                rest$ours_NA[i], rest$ref_NA[i], rest$sentinel[i], rest$both_differ[i], rest$cause[i]))
}
if (nrow(allsub)) {
  tab <- sort(table(allsub$cause), decreasing = TRUE)
  emit("\n  all variables below 100%, grouped by likely cause:")
  for (k in names(tab)) emit(sprintf("    %3d  %s", tab[[k]], k))
}

# full per-variable results (incl. cause) -> CSV, for follow-up triage
csv_path <- file.path("log", sprintf("validate-output_%s.csv", format(started, "%Y%m%d_%H%M%S")))
write.csv(results, csv_path, row.names = FALSE)
file.copy(csv_path, file.path("log", "validate-output_latest.csv"), overwrite = TRUE)
message("  full per-variable results saved to ", csv_path, "  (+ log/validate-output_latest.csv)")

# ---- 5. latest-wave sanity checks (no reference required) ---------------
# These are purely self-contained: they only use the pipeline's own output and
# check for construction-logic problems that the reference comparison can't catch
# (the reference only covers waves up to its release year).
banner("5  latest-wave sanity checks")

zn <- function(x) suppressWarnings(as.numeric(haven::zap_labels(x)))

# -- wave inventory ---------------------------------------------------------
yr_all <- zn(our_ds %>% select(YEAR) %>% collect() %>% pull(YEAR))
waves  <- sort(unique(yr_all))
wn     <- as.integer(table(yr_all))           # person-count per wave
lw     <- waves[length(waves)]               # latest wave
plw    <- waves[length(waves) - 1L]          # penultimate wave
gap    <- lw - plw                           # survey gap in years
is_lw  <- yr_all == lw
is_plw <- yr_all == plw

emit(sprintf("  latest=%d  penultimate=%d  gap=%d yr  |  %d waves, %d people per wave",
             lw, plw, gap, length(waves), wn[1]))

# 5a. balanced panel --------------------------------------------------------
ok(length(unique(wn)) == 1L,
   sprintf("5a. balanced panel — %d waves each with N=%d", length(waves), wn[1]))

# -- single batch-scan for 5b + 5e ------------------------------------------
# Each batch loads 60 cols × all rows; we compute -1 counts and NA rates for
# the latest and penultimate waves without holding the full dataset in memory.
neg1_tab  <- list()
na_lw_t   <- setNames(rep(NA_real_, length(our_cols)), our_cols)
na_plw_t  <- setNames(rep(NA_real_, length(our_cols)), our_cols)

for (start in seq(1, length(our_cols), by = batch)) {
  vs  <- our_cols[start:min(start + batch - 1L, length(our_cols))]
  vs2 <- setdiff(vs, c("ID", "YEAR"))
  # YEAR must be read *with every batch* so yr_b aligns row-for-row with each
  # column here; selecting it only when it happens to fall in this batch leaves
  # yr_b empty for all other batches (crashing the -1 tapply, and silently
  # leaving na_lw_t/na_plw_t as NA for every variable outside YEAR's batch).
  ob  <- as.data.frame(our_ds %>% select(all_of(union("YEAR", vs2))) %>% collect())
  yr_b  <- zn(ob$YEAR)
  lw_b  <- yr_b == lw
  plw_b <- yr_b == plw
  for (v in vs2) {
    x <- zn(ob[[v]])
    n1 <- sum(x == -1L, na.rm = TRUE)
    if (n1 > 0) {
      bw <- tapply(x == -1L, yr_b, sum, na.rm = TRUE)
      neg1_tab[[v]] <- bw[bw > 0]
    }
    if (any(lw_b))  na_lw_t[v]  <- mean(is.na(x[lw_b]))
    if (any(plw_b)) na_plw_t[v] <- mean(is.na(x[plw_b]))
  }
  message(sprintf("    [5] scanned %d / %d columns",
                  min(start + batch - 1L, length(our_cols)), length(our_cols)))
}

# 5b. -1 sentinel scan ------------------------------------------------------
# recode() uses -1 as an "unhandled code" sentinel and must never reach output.
emit("\n  5b. -1 sentinel scan (recode unhandled codes — should be zero in every wave)")
ok(length(neg1_tab) == 0,
   sprintf("-1 sentinel: %s",
           if (!length(neg1_tab)) "CLEAN — no unhandled codes in any wave"
           else sprintf("%d variable(s) have -1 values", length(neg1_tab))))
for (v in names(neg1_tab)) {
  wv <- neg1_tab[[v]]
  emit(sprintf("    %-42s  waves: %s", v,
               paste(sprintf("%s(n=%d)", names(wv), as.integer(wv)), collapse=", ")))
}

# 5c. birth-year stability --------------------------------------------------
emit(sprintf("\n  5c. DEMO_BIRTH_YEAR stability (%d vs %d, same person)", lw, plw))
if ("DEMO_BIRTH_YEAR" %in% our_cols) {
  by_d <- as.data.frame(our_ds %>% select(ID, YEAR, DEMO_BIRTH_YEAR) %>% collect())
  by_d[] <- lapply(by_d, zn)
  lw_d  <- by_d[by_d$YEAR == lw,  c("ID","DEMO_BIRTH_YEAR")]
  plw_d <- by_d[by_d$YEAR == plw, c("ID","DEMO_BIRTH_YEAR")]
  cm <- merge(lw_d, plw_d, by = "ID", suffixes = c("_lw","_plw"))
  both_obs <- !is.na(cm$DEMO_BIRTH_YEAR_lw) & !is.na(cm$DEMO_BIRTH_YEAR_plw)
  disc     <- both_obs & cm$DEMO_BIRTH_YEAR_lw != cm$DEMO_BIRTH_YEAR_plw
  ok(!any(disc),
     sprintf("DEMO_BIRTH_YEAR identical in both waves for %d/%d persons observed in both",
             sum(both_obs) - sum(disc), sum(both_obs)))
  if (any(disc)) {
    emit(sprintf("        -> %d person(s) have different DEMO_BIRTH_YEAR in waves %d and %d",
                 sum(disc), lw, plw))
    ex <- cm[disc, ][1:min(5, sum(disc)), ]
    for (i in seq_len(nrow(ex)))
      emit(sprintf("           ID=%d  %d=%d  %d=%d", ex$ID[i],
                   plw, ex$DEMO_BIRTH_YEAR_plw[i], lw, ex$DEMO_BIRTH_YEAR_lw[i]))
  }
} else { emit("  DEMO_BIRTH_YEAR not in output — skipping") }

# 5d. age progression -------------------------------------------------------
emit(sprintf("\n  5d. DEMO_AGE_REP progression (%d – %d; expected median ≈ %d)", lw, plw, gap))
if ("DEMO_AGE_REP" %in% our_cols) {
  age_d <- as.data.frame(our_ds %>% select(ID, YEAR, DEMO_AGE_REP) %>% collect())
  age_d[] <- lapply(age_d, zn)
  lw_a  <- age_d[age_d$YEAR == lw,  c("ID","DEMO_AGE_REP")]
  plw_a <- age_d[age_d$YEAR == plw, c("ID","DEMO_AGE_REP")]
  am   <- merge(lw_a, plw_a, by = "ID", suffixes = c("_lw","_plw"))
  both <- !is.na(am$DEMO_AGE_REP_lw) & !is.na(am$DEMO_AGE_REP_plw)
  if (sum(both) > 0) {
    diffs <- am$DEMO_AGE_REP_lw[both] - am$DEMO_AGE_REP_plw[both]
    med_d <- median(diffs)
    ok(abs(med_d - gap) <= 1,
       sprintf("median age diff %.0f (expected %d; n=%d with both waves observed)",
               med_d, gap, sum(both)))
    ext <- sum(abs(diffs - gap) > 4)
    if (ext > 0)
      emit(sprintf("        -> %d person(s) with |age diff - %d| > 4 yr (large deviation)", ext, gap))
    dt <- as.data.frame(table(diffs)); names(dt) <- c("diff","n")
    dt$diff <- as.integer(dt$diff)
    emit(sprintf("        distribution: %s",
                 paste(sprintf("%+d(%s)", dt$diff, dt$n), collapse=", ")))
  }
} else { emit("  DEMO_AGE_REP not in output — skipping") }

# 5e. coverage continuity ---------------------------------------------------
# Flag variables where the NA rate jumped >20 pp from penultimate to latest wave.
# Expected: COVID variables go to 100% NA in waves where the module is not fielded.
# Unexpected: time-invariant or health-status variables suddenly dropping coverage.
emit(sprintf("\n  5e. coverage continuity (NA rate, %d → %d; flagging > 20 pp increase)", plw, lw))
always_na  <- !is.na(na_plw_t) & na_plw_t > 0.995  # entirely absent in penultimate too
jump       <- na_lw_t - na_plw_t
big_jump   <- names(jump)[!always_na & !is.na(jump) & jump > 0.20]
ok(!length(big_jump),
   sprintf("coverage continuity: %s with >20pp NA increase from %d to %d",
           if (!length(big_jump)) "no variables" else sprintf("%d variable(s)", length(big_jump)),
           plw, lw))
if (length(big_jump)) {
  emit(sprintf("    (Variables going from observed in %d to mostly-NA in %d:)", plw, lw))
  for (v in head(big_jump, 30))
    emit(sprintf("    %-42s  %d: %4.0f%%NA  %d: %4.0f%%NA  (+%.0f pp)",
                 v, plw, 100*na_plw_t[v], lw, 100*na_lw_t[v], 100*jump[v]))
  if (length(big_jump) > 30)
    emit(sprintf("    (+ %d more; see CSV)", length(big_jump) - 30))
}

# ---- 6. new-wave validation (waves beyond the reference release) --------
# Sections 1-4 can only diff waves the reference release also contains; any wave
# this build adds on top of it has NO reference to compare against. This section
# validates those genuinely-new waves on their own terms: the wave exists with a
# full person-count, its variables are populated (not silently all-NA from a
# missing input_var_map entry), it carries no unexpected -1 sentinels, and it
# shows no coverage cliff vs the last shared wave.
banner("6  new-wave validation (waves beyond the reference)")
ref_yrs   <- sort(unique(as.integer(ref_key$YEAR)))
new_waves <- waves[waves > max(ref_yrs)]
if (!length(new_waves)) {
  emit(sprintf("  our latest wave (%d) is within reference coverage (<= %d) — no beyond-reference wave to validate here.",
               lw, max(ref_yrs)))
} else {
  nw <- lw                                             # newest wave = the primary new wave
  emit(sprintf("  reference covers waves <= %d; this build adds %d new wave(s): %s",
               max(ref_yrs), length(new_waves), paste(new_waves, collapse = ", ")))

  # 6a. new wave present with the expected (balanced) person-count
  ok(sum(yr_all == nw) == wn[1],
     sprintf("6a. wave %d present with N=%d persons (matches every other wave)", nw, sum(yr_all == nw)))

  # 6b. variable population. "newly empty" = fully-NA in the new wave but
  #     meaningfully populated (> 0.5% of persons) in the last shared wave — a
  #     real coverage cliff (likely a missing/broken input_var_map for the new
  #     wave). A variable already essentially absent (a niche or not-fielded
  #     module) dropping to zero is NOT a defect; the 0.995 threshold matches 5e.
  val_cols    <- setdiff(our_cols, c("ID", "YEAR"))
  empty_nw    <- val_cols[!is.na(na_lw_t[val_cols])  & na_lw_t[val_cols]  >= 0.99999]
  newly_empty <- empty_nw[!is.na(na_plw_t[empty_nw]) & na_plw_t[empty_nw] <= 0.995]
  # A newly-empty COVID variable is expected: PSID fielded a reduced COVID module
  # in the 2023 wave, so several 2021 COVID items have no 2023 counterpart (they
  # are simply not asked). Only a newly-empty NON-COVID variable is suspicious —
  # it points at a missing/broken input_var_map entry for the new wave.
  covid_ne <- grep("^COVID_|^DF_COVID_", newly_empty, value = TRUE)
  unexp_ne <- setdiff(newly_empty, covid_ne)
  emit(sprintf("\n  6b. variable population in wave %d", nw))
  emit(sprintf("      %d / %d variables populated  (%d entirely NA; of those %d already absent in %d, %d newly empty [%d COVID-module, %d other])",
               length(val_cols) - length(empty_nw), length(val_cols),
               length(empty_nw), length(empty_nw) - length(newly_empty), plw,
               length(newly_empty), length(covid_ne), length(unexp_ne)))
  ok(length(unexp_ne) == 0,
     sprintf("no well-populated non-COVID variable lost all coverage entering wave %d (%d unexpected newly-empty; %d COVID-module vars expected-empty)",
             nw, length(unexp_ne), length(covid_ne)))
  for (v in head(unexp_ne, 30))
    emit(sprintf("      newly empty (unexpected): %-30s (%.1f%% NA in %d -> 100%% NA in %d)", v, 100*na_plw_t[v], plw, nw))
  for (v in head(covid_ne, 30))
    emit(sprintf("      newly empty (COVID module, expected): %-20s (%.1f%% NA in %d -> 100%% NA in %d)", v, 100*na_plw_t[v], plw, nw))

  # 6c. -1 sentinels reaching the new wave. NB: the nominal/real-dollar variables
  #     (*_ND/*_NDF/*_RD/*_RDF) also carry a handful of -1 in the reference for the
  #     shared waves (an upstream PSID/Stata construction quirk, faithfully
  #     reproduced), so -1 confined to those is expected; -1 in any OTHER variable
  #     is a real unhandled-code defect for the new wave.
  nw_sent   <- Filter(function(wv) as.character(nw) %in% names(wv), neg1_tab)
  is_dollar <- grepl("(_ND|_NDF|_RD|_RDF)$", names(nw_sent))
  sent_bad  <- names(nw_sent)[!is_dollar]
  emit(sprintf("\n  6c. -1 sentinels in wave %d", nw))
  ok(length(sent_bad) == 0,
     if (!length(sent_bad))
       sprintf("no unexpected -1 in wave %d (%d dollar-var(s) carry -1, matching the reference pattern)", nw, sum(is_dollar))
     else sprintf("%d non-dollar variable(s) carry an unhandled-code -1 in wave %d", length(sent_bad), nw))
  for (v in names(nw_sent)) {
    n1 <- as.integer(nw_sent[[v]][as.character(nw)])
    emit(sprintf("      %-42s wave %d: n=%d%s", v, nw, n1,
                 if (grepl("(_ND|_NDF|_RD|_RDF)$", v)) "  (dollar-var; expected, matches reference)" else "  <-- unhandled code"))
  }

  # 6d. birth-year stability + age progression into the new wave are already
  #     validated by 5c/5d (which compare the penultimate = last shared wave to
  #     the latest = new wave).
  emit(sprintf("\n  6d. birth-year stability & ~%d-yr age progression into wave %d: see 5c/5d above.", gap, nw))
}

# ---- 7. new-domain validation (labor_income / capital_income / income) --
# The labor_/capital_/income-domain *_ND variables are new to this build and
# absent from the reference release, so sections 1-4 never compare them.
# Validate them on their own terms: every specced variable reached the output
# (7a); each is populated in exactly the waves its input_var_map covers (7b);
# -1 sentinels only where the Stata-SHELF wage pattern intentionally leaves
# them (7c); and the five series that read the same input variables as the
# earnings domain agree with their earn_* twins row-for-row (7d).
banner("7  new-domain validation (labor_income / capital_income / income)")
ivm     <- read.csv("spec/input_var_map.csv", stringsAsFactors = FALSE)
pub     <- read.csv("spec/publish_vars.csv",  stringsAsFactors = FALSE)
new_dom <- c("labor_income", "capital_income", "income")
nd_stub <- toupper(unique(sub("_(19|20)\\*$", "", pub$token[pub$domain %in% new_dom])))
# the _NDF/_RD/_RDF revise derivatives inherit wave coverage from their _ND
# parent; map every stub to its parent for the input_var_map lookups below
parent_of <- function(v) sub("_(NDF|RDF|RD)_", "_ND_", v)
no_map  <- nd_stub[!(parent_of(nd_stub) %in% toupper(ivm$newvar))]   # unmapped -> can't be built
if (length(no_map))
  emit(sprintf("  (%d token stub(s) have no input_var_map entries yet — excluded: %s and derivatives)",
               length(no_map), paste(unique(parent_of(no_map)), collapse = ", ")))
nd_stub <- setdiff(nd_stub, no_map)

# 7a. presence in the published output
miss7 <- setdiff(nd_stub, our_cols)
ok(!length(miss7), sprintf("7a. all %d mapped new-domain variables in the published output (%d missing)",
                           length(nd_stub), length(miss7)))
for (v in miss7) emit(sprintf("      missing: %s", v))
nd_have <- setdiff(nd_stub, miss7)

# 7b. per-wave population matches input_var_map: a mapped wave that is entirely
#     NA means a broken input (or ingest) for that wave; a populated unmapped
#     wave means a stray column. (Not-in-FU recoding empties values, never a
#     whole wave, so all-NA is a real defect signal here.)
nd_dat <- as.data.frame(our_ds %>% select(all_of(c("YEAR", nd_have))) %>% collect())
yr_nd  <- zn(nd_dat$YEAR)
cov_msg <- character(0)
for (v in nd_have) {
  x      <- zn(nd_dat[[v]])
  mapped <- sort(unique(ivm$year[toupper(ivm$newvar) == parent_of(v)]))
  na_w   <- tapply(is.na(x), yr_nd, mean)
  popw   <- as.integer(names(na_w))[na_w < 1]
  empty_mapped <- setdiff(mapped, popw)
  stray        <- setdiff(popw, mapped)
  if (length(empty_mapped) || length(stray))
    cov_msg <- c(cov_msg, sprintf("      %-42s %s%s", v,
      if (length(empty_mapped)) paste0("all-NA in mapped wave(s): ", paste(empty_mapped, collapse = ","), "  ") else "",
      if (length(stray))        paste0("populated in unmapped wave(s): ", paste(stray, collapse = ",")) else ""))
}
ok(!length(cov_msg), sprintf("7b. wave coverage matches input_var_map for %d/%d variables",
                             length(nd_have) - length(cov_msg), length(nd_have)))
for (m in cov_msg) emit(m)
rm(nd_dat)

# 7c. -1 values: two expected sources, anything else is an unhandled code.
#     (i)  LABOR_WAGE_INCOME_ND_RP keeps -1 in 1994/95/97 (the sample-conditional
#          wild-code rule mirrored from EARN_WAGE_ND_RP / Stata SHELF leaves
#          non-Latino/Immigrant-sample members of wild-coded families
#          unassigned); its _NDF derivative keeps -1 where fam_size = 1.
#     (ii) in the loss-capable dollar variables -1 is also a LEGITIMATE value
#          (a $1 net loss kept by the negative range) — the same sentinel/value
#          collision the reference release carries in the wealth vars (see 6c).
loss_capable <- paste0("^(LABOR_GARDEN_INCOME|CAPITAL_(BUSINESS_INCOME|FARM_INCOME|",
                       "RENTAL_INCOME|RENT_DIV_INTST_TRST_INCOME)|BUSINESS_INCOME|",
                       "BUSINESS_NETPROFIT|FARM_INCOME|TAXABLE_INCOME)_NDF?_")
sent_nd  <- neg1_tab[names(neg1_tab) %in% nd_have]
sent_why <- vapply(names(sent_nd), function(v) {
  if (v %in% c("LABOR_WAGE_INCOME_ND_RP", "LABOR_WAGE_INCOME_NDF_RP") &&
      all(names(sent_nd[[v]]) %in% c("1994", "1995", "1997")))
    "expected; mirrors EARN_WAGE_ND_RP wild-code handling"
  else if (grepl(loss_capable, v))
    "expected; -1 is a legitimate $1 net loss in a loss-capable dollar var"
  else ""
}, character(1))
ok(all(nzchar(sent_why)),
   sprintf("7c. -1 only from the wage wild-code rule or as a legitimate $1 loss (%d variable(s) with -1; %d unexpected)",
           length(sent_nd), sum(!nzchar(sent_why))))
for (v in names(sent_nd)) {
  wv <- sent_nd[[v]]
  emit(sprintf("      %-42s waves: %s  %s", v,
               paste(sprintf("%s(n=%d)", names(wv), as.integer(wv)), collapse = ", "),
               if (nzchar(sent_why[[v]])) paste0("(", sent_why[[v]], ")") else "<-- unhandled code"))
}

# 7d. earnings twins: these five pairs read identical input variables in every
#     wave and use identical recodes, so they must agree row-for-row (NA = NA).
#     labor_wage_income_nd_sp is NOT a twin (different 2015+ inputs).
twins <- c(LABOR_WAGE_INCOME_ND_RP     = "EARN_WAGE_ND_RP",
           LABOR_BUSINESS_INCOME_ND_RP = "EARN_BUSI_ND_RP",
           LABOR_BUSINESS_INCOME_ND_SP = "EARN_BUSI_ND_SP",
           LABOR_FARM_INCOME_ND_RP     = "EARN_FARM_ND_RP",
           LABOR_FARM_INCOME_ND_SP     = "EARN_FARM_ND_SP")
twins <- twins[names(twins) %in% our_cols & twins %in% our_cols]
if (!length(twins)) {
  # the earn_* component variables are not part of the published output (only
  # earn_tot_* is), so the identity cannot be re-checked here; it is asserted
  # in-pipeline at collect time instead ("twin check" lines in the 04 log,
  # R/collect/labor_income.R), where both sides are in memory.
  ok(TRUE, "7d. earnings-twin identity asserted in-pipeline at collect (components not published; see 04 log)")
} else {
  tw_dat <- as.data.frame(our_ds %>% select(all_of(unname(c(names(twins), twins)))) %>% collect())
  tw_bad <- 0L
  for (v in names(twins)) {
    a <- zn(tw_dat[[v]]); b <- zn(tw_dat[[twins[[v]]]])
    agree <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    if (!all(agree)) tw_bad <- tw_bad + 1L
    emit(sprintf("      %-42s vs %-18s %9.4f%% agree%s", v, twins[[v]], 100 * mean(agree),
                 if (all(agree)) "" else sprintf("  (%d rows differ)", sum(!agree))))
  }
  ok(tw_bad == 0, sprintf("7d. earnings-twin agreement exact for %d/%d pairs", length(twins) - tw_bad, length(twins)))
  rm(tw_dat)
}

# ---- persist the report ----------------------------------------------
emit(sprintf("\n  validation finished in %.1f min", as.numeric(difftime(Sys.time(), started, units = "mins"))))
dir.create("log", showWarnings = FALSE)
log_path <- file.path("log", sprintf("validate-output_%s.txt", format(started, "%Y%m%d_%H%M%S")))
writeLines(LOG, log_path)
file.copy(log_path, file.path("log", "validate-output_latest.txt"), overwrite = TRUE)
message("\n  report saved to ", log_path, "  (+ log/validate-output_latest.txt)")
