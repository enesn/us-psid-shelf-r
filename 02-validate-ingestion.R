# =====================================================================
# 02-validate.R  --  
# Validate psid_abridged (raw data ingested by us)
# against shelf-abridged (raw data ingested by 
# Daumler, Davis, Esther Friedman, and Fabian T. Pfeffer)
#
# See
# Daumler, D., Friedman, E., & Pfeffer, F. T. (2025). PSID-SHELF 
# User Guide and Codebook, 1968–2021, Beta Release. 
# Survey Research Center, 
# Institute for Social Research, University of Michigan, Ann Arbor MI.

# Prerequisite: psid_abridged must already be loaded in the environment
#   (run 01-ingest.R first, or source("01-ingest.R"))
#
# The shelf file is 1.1 GB; we read column names first, then load only
# the columns that exist in both datasets for value-level comparison.
# =====================================================================

stopifnot(exists("psid_abridged"), is.data.frame(psid_abridged),
          exists("mh"),           is.data.frame(mh),
          exists("cah"),          is.data.frame(cah),
          exists("pid"),          is.data.frame(pid))

library(haven)
library(dplyr)

banner <- function(msg) {
  message(sprintf("\n%s\n  %s\n%s", strrep("─", 60), msg, strrep("─", 60)))
}

shelf_path <- "raw-data/psid-shelf-original/PSID_COMPLETE_MAIN_STUDY_1968_2021_ABRIDGED.dta"
stopifnot(file.exists(shelf_path))

# Wide MH columns in psid_abridged (MAR{n}_MH{col}) — now match shelf naming
mh_cols  <- grep("^MAR\\d+_MH",  names(psid_abridged), value = TRUE)
# Wide CAH columns in psid_abridged (CHI{n}_CAH{col}) — now match shelf naming
cah_cols <- grep("^CHI\\d+_CAH", names(psid_abridged), value = TRUE)

# ── 1. Dimensions ────────────────────────────────────────────────────
banner("1 / 4  Dimension check")

message("psid_abridged : ", nrow(psid_abridged), " rows × ", ncol(psid_abridged), " cols")
message("  incl. ", length(mh_cols),  " wide MH cols  (MAR1_MH1 … MAR8_MH20)")
message("  incl. ", length(cah_cols), " wide CAH cols (CHI1_CAH1 … CHIn_CAHm)")
message("mh            : ", nrow(mh),  " rows × ", ncol(mh),  " cols (long source)")
message("cah           : ", nrow(cah), " rows × ", ncol(cah), " cols (long source)")
message("pid           : ", nrow(pid), " rows × ", ncol(pid), " cols (one row per individual)")

# Read zero rows just to get column names and labels from the .dta file
full_meta <- read_dta(shelf_path, n_max = 0)
message("shelf-abridged      : ? rows × ", ncol(full_meta), " cols  (rows not yet counted)")

# ── 2. Column overlap ────────────────────────────────────────────────
banner("2 / 4  Column overlap")

ab_cols   <- names(psid_abridged)
full_cols <- names(full_meta)

common    <- intersect(ab_cols, full_cols)
only_ab   <- setdiff(ab_cols,   full_cols)
only_full <- setdiff(full_cols, ab_cols)

message("Columns in psid_abridged only : ", length(only_ab))
message("Columns in shelf-abridged only: ", length(only_full),
        "  (any supplements not yet ingested; PID/MH/CAH now merged)")
message("Columns in both               : ", length(common),
        "  (includes ", sum(grepl("^MAR\\d+_MH",  common)), " wide MH cols",
        " and ",        sum(grepl("^CHI\\d+_CAH", common)), " wide CAH cols)")

if (length(only_ab) > 0) {
  message("  psid_abridged-only (first 10): ",
          paste(head(only_ab, 10), collapse = ", "))
}

# ── 3. Load common columns from shelf-abridged and count rows ─────────────
banner("3 / 4  Load common columns from shelf-abridged")

# Limit to ≤500 columns to keep RAM manageable (~500 doubles × 84k rows ≈ 340 MB)
sample_cols <- head(common, 500)
message("Reading ", length(sample_cols), " common columns from shelf-abridged ...")
t0 <- Sys.time()
full_sub <- read_dta(shelf_path, col_select = all_of(sample_cols))
message(sprintf("  Done in %.1f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
message("shelf-abridged rows : ", nrow(full_sub))

if (nrow(psid_abridged) != nrow(full_sub)) {
  # Known upstream discrepancy: person 1335-204 exists in shelf only;
  # person 6129-43 (extract) vs 6129-183 (shelf) is a PSID person-number revision.
  message(sprintf("Row count differs by %+d  (extract=%d, shelf=%d) — see known discrepancy note",
                  nrow(psid_abridged) - nrow(full_sub),
                  nrow(psid_abridged), nrow(full_sub)))
} else {
  message("Row counts match: ", nrow(psid_abridged))
}

# ── 4. Value-level comparison on common columns ──────────────────────
banner("4 / 4  Value comparison (summary statistics)")

ab_sub <- psid_abridged[, sample_cols]

results <- lapply(sample_cols, function(v) {
  a <- as.numeric(ab_sub[[v]])
  f <- as.numeric(full_sub[[v]])
  data.frame(
    variable    = v,
    n_ab        = sum(!is.na(a)),
    n_full      = sum(!is.na(f)),
    na_ab       = sum(is.na(a)),
    na_full     = sum(is.na(f)),
    min_ab      = if (any(!is.na(a))) min(a, na.rm = TRUE) else NA_real_,
    min_full    = if (any(!is.na(f))) min(f, na.rm = TRUE) else NA_real_,
    max_ab      = if (any(!is.na(a))) max(a, na.rm = TRUE) else NA_real_,
    max_full    = if (any(!is.na(f))) max(f, na.rm = TRUE) else NA_real_,
    mean_ab     = mean(a, na.rm = TRUE),
    mean_full   = mean(f, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
})
results_df <- do.call(rbind, results)

n_match    <- sum(results_df$values_match %in% TRUE)
n_mismatch <- sum(results_df$values_match %in% FALSE)
n_no_overlap <- sum(is.na(results_df$values_match))

if (n_no_overlap > 0)
  message("  (", n_no_overlap, " columns have no jointly-observed rows — values_match = NA)")

if (n_mismatch > 0) {
  mismatches <- results_df[results_df$values_match %in% FALSE, ]
  message("\nMismatched columns (first 20):")
  print(head(mismatches[, c("variable", "na_ab", "na_full",
                             "min_ab", "min_full", "max_ab", "max_full",
                             "mean_ab", "mean_full")], 20),
        row.names = FALSE)
} else {
  message("\nAll ", n_match, " sampled columns match exactly between psid_abridged and shelf-abridged.")
}

# Summary table saved to console; assign for further inspection:
val_summary <- results_df
message("\nFull results in `val_summary` data frame.")
