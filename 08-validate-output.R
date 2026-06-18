# =====================================================================
# 08-validate-output.R  --  Validate the pipeline's LONG output against a
#                           reference PSIDSHELF_1968_2021_LONG.dta release
#
# Compares the pipeline's LONG output to a reference release of the same file:
#   1. row count and key (ID x YEAR)
#   2. variable coverage (shared / reference-only / ours-only)
#   3. value-level agreement, variable by variable, on the shared ID x YEAR rows
#
# Both files are large, so columns are streamed one (or a few) at a time.
# The reference path is taken from the env var PSIDSHELF_REF, else the first
# CLI argument, else raw-data/psid-shelf-original/PSIDSHELF_1968_2021_LONG.dta.
# The number of variables to value-check is the second CLI argument (default 40).
# Our LONG output is auto-discovered from
# output/PSID_SHELF_R_<fromyear>_<toyear>_LONG.parquet (most recently modified,
# if more than one exists).
#
# Usage:  PSIDSHELF_REF=/path/to/ref.dta Rscript 08-validate-output.R [n_vars]
#         Rscript 08-validate-output.R /path/to/ref.dta [n_vars]
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
message("  validating: ", our_dir)
n_arg <- args[!file.exists(args)]
n_value_vars <- if (length(n_arg)) as.integer(n_arg[1]) else 40L

banner <- function(m) message(sprintf("\n%s\n  %s\n%s", strrep("=", 64), m, strrep("=", 64)))
ok <- function(cond, msg) message(sprintf("  [%s] %s", if (isTRUE(cond)) "PASS" else "FAIL", msg))

# ---- column inventories ----------------------------------------------
banner("1  variable coverage")
ref_cols <- names(read_dta(ref_path, n_max = 0))
our_ds   <- open_dataset(our_dir)
our_cols <- names(our_ds)

shared   <- intersect(ref_cols, our_cols)
ref_only <- setdiff(ref_cols, our_cols)
our_only <- setdiff(our_cols, ref_cols)
message(sprintf("  reference: %d cols | ours: %d cols | shared: %d",
                length(ref_cols), length(our_cols), length(shared)))
if (length(ref_only)) message("  reference-only (", length(ref_only), "): ",
                              paste(head(ref_only, 25), collapse = ", "),
                              if (length(ref_only) > 25) " ..." else "")
if (length(our_only)) message("  ours-only (", length(our_only), "): ",
                              paste(head(our_only, 25), collapse = ", "),
                              if (length(our_only) > 25) " ..." else "")

# ---- key & row count --------------------------------------------------
banner("2  rows & key")
our_key <- our_ds %>% select(ID, YEAR) %>% collect()
ref_key <- read_dta(ref_path, col_select = c("ID", "YEAR"))
ref_key$ID <- as.numeric(ref_key$ID); ref_key$YEAR <- as.integer(ref_key$YEAR)
ok(nrow(our_key) == nrow(ref_key),
   sprintf("row count  ours=%d  ref=%d", nrow(our_key), nrow(ref_key)))
ok(!anyDuplicated(our_key), "ID x YEAR is unique in our output")
ok(setequal(unique(our_key$YEAR), unique(ref_key$YEAR)),
   sprintf("YEAR domain matches (%d waves)", length(unique(ref_key$YEAR))))
ours_keyset <- paste(our_key$ID, our_key$YEAR)
ref_keyset  <- paste(ref_key$ID,  ref_key$YEAR)
ok(setequal(ours_keyset, ref_keyset), "ID x YEAR key sets are identical")

# ---- value-level agreement (sample of shared variables) ---------------
banner(sprintf("3  value agreement (%d shared variables)", n_value_vars))
value_vars <- setdiff(shared, c("ID", "YEAR"))
# spread the sample across the column order (domains) for breadth
idx <- unique(round(seq(1, length(value_vars), length.out = min(n_value_vars, length(value_vars)))))
sample_vars <- value_vars[idx]

# read all sampled columns in ONE pass from each source (the .dta is 7.4 GB)
ours <- our_ds %>% select(ID, YEAR, all_of(sample_vars)) %>% collect()
ref  <- read_dta(ref_path, col_select = all_of(c("ID", "YEAR", sample_vars)))
ours$.k <- paste(ours$ID, ours$YEAR)
ref$.k  <- paste(as.numeric(ref$ID), as.integer(ref$YEAR))
ref <- ref[match(ours$.k, ref$.k), ]                 # align ref rows to ours
results <- data.frame(variable = sample_vars, agree_pct = NA_real_,
                      n_compared = NA_integer_, stringsAsFactors = FALSE)
for (i in seq_along(sample_vars)) {
  v <- sample_vars[i]
  a <- as.numeric(ours[[v]]); b <- as.numeric(ref[[v]])
  agree <- (is.na(a) & is.na(b)) |
           (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-6 * pmax(1, abs(b)))
  results$agree_pct[i]  <- 100 * mean(agree)
  results$n_compared[i] <- length(agree)
}
results <- results[order(results$agree_pct), ]
message("  per-variable agreement (worst first):")
for (i in seq_len(nrow(results)))
  message(sprintf("    %-28s %6.2f%%  (n=%d)", results$variable[i], results$agree_pct[i], results$n_compared[i]))
banner(sprintf("mean agreement across %d variables: %.2f%%   (>=99%% on %d / %d)",
               nrow(results), mean(results$agree_pct),
               sum(results$agree_pct >= 99), nrow(results)))
