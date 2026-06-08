# =====================================================================
# 01-ingest.R  --  Read PSID main extract + Marriage History supplement
#
# Inputs:
#   raw-data/downloaded-from-psid/ascii/J362500.{txt,sas}
#     fixed-width ASCII data (84,120 x 6,880, LRECL 15713)
#   raw-data/downloaded-from-psid/mh85_23/MH85_23.{txt,sas}
#     Marriage History 1985-2023 (65,226 x 20, one row per marriage)
#
# Outputs:
#   psid_abridged  -- main extract (84,120 rows)
#   mh             -- marriage history long file (65,226 rows)
#   psid_mh        -- psid_abridged left-joined with mh on ER30001+ER30002
#                     (persons with multiple marriages expand to multiple rows)
# =====================================================================

# ---- 0. packages ----------------------------------------------------
# install.packages(c("readr", "stringr", "fst", "arrow"))  # run once
library(readr)
library(stringr)
library(dplyr)
library(tidyr)

banner <- function(msg) {
  message(sprintf("\n%s\n  %s\n%s", strrep("─", 60), msg, strrep("─", 60)))
}
elapsed <- function(t) sprintf("  [%.1f s]", as.numeric(difftime(Sys.time(), t, units = "secs")))

t_total <- Sys.time()

banner("1 / 4  Paths & validation")
t1 <- Sys.time()
# ---- 1. paths -------------------------------------------------------
# Folder that contains the ./ascii subfolder. Edit if you move the script.
base_dir  <- "raw-data/downloaded-from-psid"
ascii_dir <- file.path(base_dir, "ascii")
mh_dir    <- file.path(base_dir, "mh85_23")
stopifnot(dir.exists(ascii_dir), dir.exists(mh_dir))

sas_file  <- file.path(ascii_dir, "J362500.sas")
dat_file  <- file.path(ascii_dir, "J362500.txt")
mh_sas    <- file.path(mh_dir,   "MH85_23.sas")
mh_dat    <- file.path(mh_dir,   "MH85_23.txt")

message(elapsed(t1))
banner("2 / 4  Parse SAS setup file")
t2 <- Sys.time()
# ---- 2. parse the SAS setup file -----------------------------------
sas <- paste(readLines(sas_file, warn = FALSE), collapse = "\n")

# (a) column positions from the INPUT ... ; block:  NAME  start - end
input_block <- str_match(sas, "(?s)\\bINPUT\\b(.*?);")[, 2]
pos <- str_match_all(input_block, "([A-Za-z_]\\w*)\\s+(\\d+)\\s*-\\s*(\\d+)")[[1]]
positions <- data.frame(
  name  = pos[, 2],
  begin = as.integer(pos[, 3]),
  end   = as.integer(pos[, 4]),
  stringsAsFactors = FALSE
)
stopifnot(nrow(positions) == 6880L, max(positions$end) == 15713L)

# (b) variable labels from the ATTRIB block:  NAME  LABEL="..."  FORMAT=Fx.
lab <- str_match_all(sas, '([A-Za-z_]\\w*)\\s+LABEL="([^"]*)"')[[1]]
labels <- setNames(str_squish(lab[, 3]), lab[, 2])

message(elapsed(t2))
banner("3 / 4  Read main extract (J362500)")
t3 <- Sys.time()
# ---- 3. read ALL columns -------------------------------------------
# All PSID vars in this extract are numeric; read as double (177 vars are
# 10 digits wide and would overflow 32-bit integers).
col_pos <- fwf_positions(positions$begin, positions$end, positions$name)

message("Reading ", nrow(positions), " columns from ", basename(dat_file), " ...")
psid_abridged <- vroom::vroom_fwf(          # ~5-10x faster than read_fwf on low-clock CPUs
  dat_file,                        # (ALTREP: columns parsed lazily on first access)
  col_positions = col_pos,
  col_types     = cols(.default = col_double()),
  progress      = TRUE
)

# attach variable labels as a column attribute (visible via attr(psid$VAR,"label"))
#for (nm in names(psid)) attr(psid[[nm]], "label") <- unname(labels[nm])

psid_abridged$ID <- psid_abridged$ER30001 * 1000 + psid_abridged$ER30002

message("Loaded: ", nrow(psid_abridged), " rows x ", ncol(psid_abridged), " columns")
message(elapsed(t3))

# ── 4. Marriage History supplement ───────────────────────────────────
banner("4 / 4  Read & merge Marriage History (MH85_23)")
t4 <- Sys.time()

sas_mh      <- paste(readLines(mh_sas, warn = FALSE), collapse = "\n")
ib_mh       <- str_match(sas_mh, "(?s)\\bINPUT\\b(.*?);")[, 2]
pos_mh      <- str_match_all(ib_mh, "([A-Za-z_]\\w*)\\s+(\\d+)\\s*-\\s*(\\d+)")[[1]]
positions_mh <- data.frame(
  name  = pos_mh[, 2],
  begin = as.integer(pos_mh[, 3]),
  end   = as.integer(pos_mh[, 4]),
  stringsAsFactors = FALSE
)
lab_mh   <- str_match_all(sas_mh, '([A-Za-z_]\\w*)\\s+LABEL="([^"]*)"')[[1]]
labels_mh <- setNames(str_squish(lab_mh[, 3]), lab_mh[, 2])

mh <- vroom::vroom_fwf(
  mh_dat,
  col_positions = fwf_positions(positions_mh$begin, positions_mh$end, positions_mh$name),
  col_types     = cols(.default = col_double()),
  progress      = TRUE
)
mh[] <- Map(\(col, lbl) `attr<-`(col, "label", lbl), mh, unname(labels_mh[names(mh)]))

# Pivot MH to wide: one row per person, columns named MAR{n}_MH{col}
# matching the shelf-abridged naming convention (MAR1–MAR8 only).
# MH9 values 98/99 are PSID missing codes (DK / not ascertained);
# orders >8 are 3 individuals — dropped to match shelf.
mh_wide <- pivot_wider(
  filter(mh, MH9 %in% 1:8),
  id_cols     = c(MH2, MH3),
  names_from  = MH9,
  values_from = setdiff(names(mh), c("MH2", "MH3", "MH9")),
  names_glue  = "MAR{MH9}_{.value}"
)

# MH2, MH3, MH9 were used as pivot keys so they were dropped from values.
# Re-derive them per marriage slot to match shelf (NA where that marriage doesn't exist).
for (n in 1:8) {
  has_n <- !is.na(mh_wide[[sprintf("MAR%d_MH1", n)]])
  mh_wide[[sprintf("MAR%d_MH2", n)]] <- ifelse(has_n, mh_wide$MH2,     NA_real_)
  mh_wide[[sprintf("MAR%d_MH3", n)]] <- ifelse(has_n, mh_wide$MH3,     NA_real_)
  mh_wide[[sprintf("MAR%d_MH9", n)]] <- ifelse(has_n, as.double(n),    NA_real_)
}

# Join key: ER30001 = 1968 interview number, ER30002 = person number
#           MH2     = 1968 interview number, MH3     = person number
psid_abridged <- left_join(
  psid_abridged,
  rename(mh_wide, ER30001 = MH2, ER30002 = MH3),
  by = c("ER30001", "ER30002")
)

message(sprintf("  mh       : %d rows × %d cols (one row per marriage)",
                nrow(mh), ncol(mh)))
message(sprintf("  mh_wide  : %d rows × %d cols (pivoted MAR{n}_MH{col})",
                nrow(mh_wide), ncol(mh_wide)))
message(sprintf("  persons with MH records: %d / %d",
                sum(psid_abridged$ER30001 %in% mh$MH2), nrow(psid_abridged)))
message(elapsed(t4))

message(sprintf("\n  Total elapsed: %.1f s",
                as.numeric(difftime(Sys.time(), t_total, units = "secs"))))

rm(col_pos, lab, pos, positions, input_block,
   sas_mh, ib_mh, pos_mh, positions_mh, lab_mh)
