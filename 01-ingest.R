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

banner("1 / 6   Paths & validation")
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
banner("2 / 6  Parse SAS setup file")
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
banner("3 / 6  Read main extract (J362500)")
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
banner("4 / 6  Read & merge Marriage History (MH85_23)")
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

rm(col_pos, lab, pos, positions, input_block,
   sas_mh, ib_mh, pos_mh, positions_mh, lab_mh)


# ── 5. Child Assessment History (CAH85_23) ──────────────────────────────────
banner("5 / 6  Read & merge Child Assessment History (CAH85_23)")
t5 <- Sys.time()

cah_dir <- file.path(base_dir, "cah85_23")
stopifnot(dir.exists(cah_dir))
cah_sas <- file.path(cah_dir, "CAH85_23.sas")
cah_dat <- file.path(cah_dir, "CAH85_23.txt")

sas_cah       <- paste(readLines(cah_sas, warn = FALSE), collapse = "\n")
ib_cah        <- str_match(sas_cah, "(?s)\\bINPUT\\b(.*?);")[, 2]
pos_cah       <- str_match_all(ib_cah, "([A-Za-z_]\\w*)\\s+(\\d+)\\s*-\\s*(\\d+)")[[1]]
positions_cah <- data.frame(
  name  = pos_cah[, 2],
  begin = as.integer(pos_cah[, 3]),
  end   = as.integer(pos_cah[, 4]),
  stringsAsFactors = FALSE
)
lab_cah    <- str_match_all(sas_cah, '([A-Za-z_]\\w*)\\s+LABEL="([^"]*)"')[[1]]
labels_cah <- setNames(str_squish(lab_cah[, 3]), lab_cah[, 2])

cah <- vroom::vroom_fwf(
  cah_dat,
  col_positions = fwf_positions(positions_cah$begin, positions_cah$end, positions_cah$name),
  col_types     = cols(.default = col_double()),
  progress      = TRUE
)
cah[] <- Map(\(col, lbl) `attr<-`(col, "label", lbl), cah, unname(labels_cah[names(cah)]))

# Pivot CAH to wide: one row per parent, columns named CHI{n}_CAH{col}
# CAH3 = 1968 interview number of parent, CAH4 = person number of parent
# CAH9 = birth order; 98/99 are PSID missing codes (DK / not ascertained)
cah_wide <- pivot_wider(
  filter(cah, !CAH9 %in% c(98, 99)),
  id_cols     = c(CAH3, CAH4),
  names_from  = CAH9,
  values_from = setdiff(names(cah), c("CAH3", "CAH4", "CAH9")),
  names_glue  = "CHI{CAH9}_{.value}"
)

# CAH3, CAH4, CAH9 were used as pivot keys so they were dropped from values.
# Re-derive them per child slot to match shelf naming convention.
birth_orders <- sort(unique(filter(cah, !CAH9 %in% c(98, 99))$CAH9))
for (n in birth_orders) {
  has_n <- !is.na(cah_wide[[sprintf("CHI%d_CAH1", n)]])
  cah_wide[[sprintf("CHI%d_CAH3", n)]] <- ifelse(has_n, cah_wide$CAH3, NA_real_)
  cah_wide[[sprintf("CHI%d_CAH4", n)]] <- ifelse(has_n, cah_wide$CAH4, NA_real_)
  cah_wide[[sprintf("CHI%d_CAH9", n)]] <- ifelse(has_n, as.double(n),  NA_real_)
}

# Join key: ER30001 = 1968 interview number, ER30002 = person number of parent
#           CAH3    = 1968 interview number, CAH4    = person number of parent
psid_abridged <- left_join(
  psid_abridged,
  rename(cah_wide, ER30001 = CAH3, ER30002 = CAH4),
  by = c("ER30001", "ER30002")
)

message(sprintf("  cah      : %d rows × %d cols (one row per child)",
                nrow(cah), ncol(cah)))
message(sprintf("  cah_wide : %d rows × %d cols (pivoted CHI{n}_CAH{col})",
                nrow(cah_wide), ncol(cah_wide)))
message(sprintf("  parents with CAH records: %d / %d",
                sum(psid_abridged$ER30001 %in% cah$CAH3), nrow(psid_abridged)))
message(elapsed(t5))

rm(sas_cah, ib_cah, pos_cah, positions_cah, lab_cah, birth_orders)


# ── 6. Parent Identification (PID23) ────────────────────────────────────────
banner("6 / 6  Read & merge Parent Identification (PID23)")
t6 <- Sys.time()

pid_dir <- file.path(base_dir, "pid23")
stopifnot(dir.exists(pid_dir))
pid_sas <- file.path(pid_dir, "PID23.sas")
pid_dat <- file.path(pid_dir, "PID23.txt")

sas_pid       <- paste(readLines(pid_sas, warn = FALSE), collapse = "\n")
ib_pid        <- str_match(sas_pid, "(?s)\\bINPUT\\b(.*?);")[, 2]
pos_pid       <- str_match_all(ib_pid, "([A-Za-z_]\\w*)\\s+(\\d+)\\s*-\\s*(\\d+)")[[1]]
positions_pid <- data.frame(
  name  = pos_pid[, 2],
  begin = as.integer(pos_pid[, 3]),
  end   = as.integer(pos_pid[, 4]),
  stringsAsFactors = FALSE
)
lab_pid    <- str_match_all(sas_pid, '([A-Za-z_]\\w*)\\s+LABEL="([^"]*)"')[[1]]
labels_pid <- setNames(str_squish(lab_pid[, 3]), lab_pid[, 2])

# PID is already one row per individual — no pivot needed.
# Join key: ER30001 = 1968 interview number, ER30002 = person number
#           PID2    = 1968 interview number, PID3    = person number
pid <- vroom::vroom_fwf(
  pid_dat,
  col_positions = fwf_positions(positions_pid$begin, positions_pid$end, positions_pid$name),
  col_types     = cols(.default = col_double()),
  progress      = TRUE
)
pid[] <- Map(\(col, lbl) `attr<-`(col, "label", lbl), pid, unname(labels_pid[names(pid)]))

psid_abridged <- left_join(
  psid_abridged,
  rename(pid, ER30001 = PID2, ER30002 = PID3),
  by = c("ER30001", "ER30002")
)

message(sprintf("  pid      : %d rows × %d cols (one row per individual)",
                nrow(pid), ncol(pid)))
message(sprintf("  individuals with PID records: %d / %d",
                sum(psid_abridged$ER30001 %in% pid$PID2), nrow(psid_abridged)))
message(elapsed(t6))

# ── Materialise lazy vroom (ALTREP) columns ─────────────────────────────────
# vroom returns a spec_tbl_df whose columns are read lazily. A later full gc()
# (in 07-publish, to free the wide table before building the long one) tries to
# finalise those lazy columns and errors — "object 'psid_abridged' not found" —
# so the ~15 GB wide table is never reclaimed and publish gets OOM-killed.
# Reading every column now (x[]) turns them into plain vectors and drops the
# spec_tbl_df class, so gc() behaves and memory is freed as intended.
banner("7 / 7  Materialise columns (drop lazy vroom ALTREP)")
t7 <- Sys.time()
psid_abridged <- as.data.frame(lapply(psid_abridged, function(x) x[]),
                               stringsAsFactors = FALSE, check.names = FALSE)
message(sprintf("  materialised %d columns", ncol(psid_abridged))); message(elapsed(t7))

message(sprintf("\n  Total elapsed: %.1f s",
                as.numeric(difftime(Sys.time(), t_total, units = "secs"))))

rm(sas_pid, ib_pid, pos_pid, positions_pid, lab_pid)
