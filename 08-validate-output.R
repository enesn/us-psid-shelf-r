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
if (ne) emit(sprintf("\n  (+ %d variable(s) at 99.0-99.999%% — a few differing rows each; see the full table above)", ne))
if (nrow(allsub)) {
  tab <- sort(table(allsub$cause), decreasing = TRUE)
  emit("\n  all variables below 100%, grouped by likely cause:")
  for (k in names(tab)) emit(sprintf("    %3d  %s", tab[[k]], k))
}

# ---- persist the report ----------------------------------------------
emit(sprintf("\n  validation finished in %.1f min", as.numeric(difftime(Sys.time(), started, units = "mins"))))
dir.create("log", showWarnings = FALSE)
log_path <- file.path("log", sprintf("validate-output_%s.txt", format(started, "%Y%m%d_%H%M%S")))
writeLines(LOG, log_path)
file.copy(log_path, file.path("log", "validate-output_latest.txt"), overwrite = TRUE)
message("\n  report saved to ", log_path, "  (+ log/validate-output_latest.txt)")
