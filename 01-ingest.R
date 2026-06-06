# =====================================================================
# 01-ingest.R  --  Read a full PSID Data Center extract (ALL columns)
#
# Inputs (in ./ascii):
#   J362500.txt           fixed-width ASCII data (84,120 x 6,880, LRECL 15713)
#   J362500.sas           SAS setup: ATTRIB labels + INPUT column positions
#   J362500_formats.sas   value labels (optional, not applied here)
#
# Strategy: parse the SAS file once for column positions + labels, read the
# whole thing with readr::read_fwf, then cache to .fst / .parquet so future
# loads take seconds. Expect ~4-5 GB RAM while reading all columns.
# =====================================================================

# ---- 0. packages ----------------------------------------------------
# install.packages(c("readr", "stringr", "fst", "arrow"))  # run once
library(readr)
library(stringr)

banner <- function(msg) {
  message(sprintf("\n%s\n  %s\n%s", strrep("─", 60), msg, strrep("─", 60)))
}
elapsed <- function(t) sprintf("  [%.1f s]", as.numeric(difftime(Sys.time(), t, units = "secs")))

t_total <- Sys.time()

banner("1 / 4  Paths & validation")
t1 <- Sys.time()
# ---- 1. paths -------------------------------------------------------
# Folder that contains the ./ascii subfolder. Edit if you move the script.
base_dir <- "raw-data/downloaded-from-psid"
ascii_dir <- file.path(base_dir, "ascii")
stopifnot(dir.exists(ascii_dir))  # fails fast if the path is wrong

sas_file  <- file.path(ascii_dir, "J362500.sas")
dat_file  <- file.path(ascii_dir, "J362500.txt")
fst_out   <- file.path(base_dir, "J362500.fst")
pq_out    <- file.path(base_dir, "J362500.parquet")

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
banner("3 / 4  Read fixed-width ASCII data")
t3 <- Sys.time()
# ---- 3. read ALL columns -------------------------------------------
# All PSID vars in this extract are numeric; read as double (177 vars are
# 10 digits wide and would overflow 32-bit integers).
col_pos <- fwf_positions(positions$begin, positions$end, positions$name)

message("Reading ", nrow(positions), " columns from ", basename(dat_file), " ...")
psid <- vroom::vroom_fwf(          # ~5-10x faster than read_fwf on low-clock CPUs
  dat_file,                        # (ALTREP: columns parsed lazily on first access)
  col_positions = col_pos,
  col_types     = cols(.default = col_double()),
  progress      = TRUE
)

# attach variable labels as a column attribute (visible via attr(psid$VAR,"label"))
#for (nm in names(psid)) attr(psid[[nm]], "label") <- unname(labels[nm])

message("Loaded: ", nrow(psid), " rows x ", ncol(psid), " columns")
message(elapsed(t3))
banner("4 / 4  Cache to binary formats")
t4 <- Sys.time()
# ---- 4. cache to a fast binary format ------------------------------

# .parquet -> portable (Python/Stata/Spark) and columnar
if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(psid, pq_out)
  message("Wrote ", pq_out)
  # Lazy, out-of-memory access to selected columns:
  #   library(arrow)
  #   ds <- open_dataset("J362500.parquet")
  #   df <- dplyr::select(ds, ER30001, ER30002) |> dplyr::collect()
}

message(elapsed(t4))
message(sprintf("\n  Total elapsed: %.1f s", as.numeric(difftime(Sys.time(), t_total, units = "secs"))))

# `psid` is now in memory. Variable labels: attr(psid$ER30001, "label")

# =====================================================================
# LOW-RAM ALTERNATIVE (no full load): the LaF package memory-maps the
# fixed-width file and reads columns on demand.
#   install.packages("LaF")
#   library(LaF)
#   d <- laf_open_fwf(dat_file,
#                     column_types  = rep("double", nrow(positions)),
#                     column_names  = positions$name,
#                     column_widths = positions$end - positions$begin + 1L)
#   wt  <- d[, "ER34902"]          # pull one column
#   blk <- d[1:1000, ]            # or a block of rows
#
# ONE-LINER WITH LABELS (slower, heavier): asciiSetupReader reads the .txt
# + .sas pair directly and applies value labels from the formats file too.
#   install.packages("asciiSetupReader")
#   psid <- asciiSetupReader::read_ascii_setup(dat_file, sas_file)
# =====================================================================
