# =====================================================================
# 03-shelf-parameters.R  --  Load extracted spec/ into memory
#
# Reads everything 000-extract-specs.R wrote into a single `SPEC` list and the
# key parameters (`year`, `pcepi`, ...) into the session, so the rest of the
# pipeline reads only spec/ (no dependency on the original construction files).
#
# Prerequisite: psid_abridged loaded (run 01-ingest.R first).
# =====================================================================

library(readr)
library(jsonlite)

stopifnot(dir.exists("spec"))

# ---- key parameters --------------------------------------------------
params        <- fromJSON("spec/parameters.json")
psid_lastwave <- as.integer(params$psid_lastwave)
year          <- as.integer(params$year)     # 42 PSID waves, 1968..2021
n_year        <- length(year)
wlthyear      <- as.integer(params$wlthyear)
inflate_year  <- as.integer(params$inflate_year)
pcepi         <- unlist(params$pcepi)         # named numeric, "1959".."2024"

# ---- spec tables (bundled in SPEC, consumed by R/programs.R) ----------
SPEC <- list(
  value_labels        = read_csv("spec/value_labels.csv",        show_col_types = FALSE),
  var_labels          = read_csv("spec/var_labels.csv",          show_col_types = FALSE),
  input_var_single    = read_csv("spec/input_var_single.csv",    show_col_types = FALSE),
  input_var_map       = read_csv("spec/input_var_map.csv",       show_col_types = FALSE),
  var_value_label_map = read_csv("spec/var_value_label_map.csv", show_col_types = FALSE),
  publish_vars        = read_csv("spec/publish_vars.csv",        show_col_types = FALSE),
  time_invariant      = readLines("spec/time_invariant_vars.txt"))

# ---- shared helpers --------------------------------------------------
source("R/programs.R")

# ---- key variable: lowercase `id` (Stata: rename ID, lower) -----------
stopifnot(exists("psid_abridged"), !is.null(psid_abridged$ID))
psid_abridged$id <- psid_abridged$ID

message(sprintf("Parameters loaded: %d waves (%d-%d), inflate year %d, %d label sets",
                n_year, min(year), max(year), inflate_year,
                length(unique(SPEC$value_labels$label_set))))
