# =====================================================================
# 09-metadata.R  --  Generate the run manifest (metadata/<version>.yaml)
#
# Writes a YAML provenance + schema manifest describing one build of the
# PSID-SHELF-R output: who/when, the exact raw inputs (sha256), the pipeline
# (git commit, scripts, runtime), the output schema, and known quality notes.
#
# Runs at the end of 00-run-all.R, but is also standalone — it derives
# everything from spec/ + output/ on disk (it does NOT need psid_abridged in
# memory), so you can regenerate the manifest for an existing build:
#     Rscript 09-metadata.R
#
# Hashing the large files (the ~10 GB .dta) can take a minute; set
#   PSID_META_HASH_BIG=0   to record size-only for files over 2 GB.
# =====================================================================

suppressMessages({library(jsonlite); library(yaml)})

banner <- function(m) message(sprintf("\n%s\n  %s\n%s", strrep("-", 60), m, strrep("-", 60)))
banner("09  generate metadata/<version>.yaml")

# ---- helpers ---------------------------------------------------------
BIG <- 2e9
hash_big <- Sys.getenv("PSID_META_HASH_BIG", "1") != "0"
sha256 <- function(path) {                       # NULL if absent / skipped
  if (!file.exists(path)) return(NULL)
  if (!hash_big && file.size(path) > BIG) return(sprintf("(skipped: %.1f GB)", file.size(path) / 1e9))
  message("    sha256 ", path, " (", format(structure(file.size(path), class = "object_size"), units = "auto"), ") ...")
  digest::digest(file = path, algo = "sha256")
}
files_sha256 <- function(dir, files) {           # named list file -> sha256, for files that exist
  files <- files[file.exists(file.path(dir, files))]
  setNames(lapply(files, function(f) sha256(file.path(dir, f))), files)
}
git <- function(args, default = NA_character_) {
  v <- tryCatch(suppressWarnings(system2("git", args, stdout = TRUE, stderr = FALSE)),
                error = function(e) character(0))
  if (length(v)) trimws(v[1]) else default
}
pkg_ver <- function(p) tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_)

# ---- parameters / version -------------------------------------------
params <- fromJSON("spec/parameters.json")
yr     <- as.integer(params$year)
fw     <- min(yr); lw <- as.integer(params$psid_lastwave)
stub   <- sprintf("PSID_SHELF_R_%d_%d", fw, lw)
out    <- "output"

# upstream Stata release this R port reproduces (optional provenance input)
src_release <- NULL
if (file.exists("spec/metadata.csv")) {
  m <- read.csv("spec/metadata.csv", stringsAsFactors = FALSE)
  kv <- setNames(as.list(m$value), m$key)
  src_release <- list(release_num = kv[["release_num"]],
                      retrieved   = kv[["retrieve_date"]],
                      compiled_by = kv[["compile_name"]])
}

# ---- schema (read cheaply from the published LONG parquet) ----------
long_pq <- file.path(out, paste0(stub, "_LONG.parquet"))
n_rows <- NA_integer_; n_cols <- NA_integer_; completeness <- NULL
if (file.exists(long_pq)) {
  suppressMessages(library(arrow))
  ds     <- open_dataset(long_pq)
  n_rows <- ds$num_rows
  schema_names <- ds$schema$names
  n_cols <- length(schema_names)
  keys   <- intersect(c("ID", "YEAR"), schema_names)
  if (length(keys)) {
    kt <- read_parquet(long_pq, col_select = all_of(keys))
    completeness <- setNames(lapply(keys, function(k) {
      nnull <- sum(is.na(kt[[k]]))
      if (nnull == 0) "OK (0 nulls)" else sprintf("FLAG — %d nulls", nnull)
    }), keys)
  }
} else {
  message("  NOTE: ", long_pq, " not found — schema fields left null. Run 00-run-all.R first.")
}

# ---- assemble the manifest ------------------------------------------
raw_base <- "raw-data/downloaded-from-psid"
manifest <- list(
  dataset = list(
    name             = "psid-shelf-r",
    version          = stub,
    previous_version = "",
    release_date     = as.character(Sys.Date()),
    created_by        = git("config --get user.name", default = unname(Sys.info()[["user"]]))
  ),
  provenance = list(
    source_release = src_release,
    raw_sources = list(
      list(name = "main_extract", folder = file.path(raw_base, "ascii"),
           description = "PSID main 1968-2023 individual+family fixed-width extract (variable cart)",
           files_sha256 = files_sha256(file.path(raw_base, "ascii"), c("J362500.txt", "J362500.sas"))),
      list(name = "marriage_history", folder = file.path(raw_base, "mh85_23"),
           description = "Marriage History 1985-2023 supplement",
           files_sha256 = files_sha256(file.path(raw_base, "mh85_23"), c("MH85_23.txt", "MH85_23.sas"))),
      list(name = "childbirth_adoption_history", folder = file.path(raw_base, "cah85_23"),
           description = "Childbirth & Adoption History 1985-2023 supplement",
           files_sha256 = files_sha256(file.path(raw_base, "cah85_23"), c("CAH85_23.txt", "CAH85_23.sas"))),
      list(name = "parent_identification", folder = file.path(raw_base, "pid23"),
           description = "Parent Identification File 2023 supplement",
           files_sha256 = files_sha256(file.path(raw_base, "pid23"), c("PID23.txt", "PID23.sas")))
    ),
    pipeline = list(
      repo        = sub("^https://", "", sub("\\.git$", "", git("config --get remote.origin.url", ""))),
      branch      = git("rev-parse --abbrev-ref HEAD", ""),
      commit      = git("rev-parse HEAD", ""),
      entry_point = "00-run-all.R",
      scripts_executed = c("01-ingest.R", "03-shelf-parameters.R", "04-collect-inputs.R",
                           "05-generate-variables.R", "06-revise-variables.R", "07-publish.R"),
      outputs_sha256 = files_sha256(out, paste0(stub, c("_LONG.parquet", "_LONG.dta", "_WIDE.parquet"))),
      runtime = list(
        R = paste(R.version$major, R.version$minor, sep = "."),
        packages = Filter(Negate(is.na), lapply(
          setNames(nm = c("readr","vroom","stringr","dplyr","tidyr","haven","arrow","jsonlite","digest","yaml")),
          pkg_ver))
      ),
      environment = list(
        image = sub("^FROM\\s+", "", grep("^FROM", readLines("Dockerfile", warn = FALSE), value = TRUE)[1])
      )
    )
  ),
  schema = list(
    codebook_ref       = "spec/psid-cross-year-index.xlsx",
    n_rows             = n_rows,
    n_cols             = n_cols,
    unit_of_observation = "Individual × wave (person-year), keyed by (ID, YEAR)",
    panel_waves        = yr,
    key_variables = list(
      ID   = "Individual identifier = ER30001 * 1000 + ER30002 (1968 interview no. + person no.)",
      YEAR = "Survey wave / calendar year of interview"
    ),
    weight_variables = list(
      FW = "Family-level longitudinal expansion weight",
      IW = "Individual-level longitudinal expansion weight"
    )
  ),
  changelog = list(
    list(type = "initial",
         description = paste("Initial PSID-SHELF-R release — R port of the Stata PSID-SHELF construction:",
                             "ingest main extract + MH/CAH/PID supplements, collect input variables,",
                             "generate derived variables, revise (not-in-FU / family-size / PCEPI inflation),",
                             "and publish a labelled LONG .dta + parquet."),
         variables_affected = "all",
         rationale = "First version of the R reproduction of the longitudinal file.")
  ),
  quality = list(
    completeness = completeness,
    known_issues = list(
      ingestion_one_person = paste("Output has 3,533,040 rows (84,120 persons × 42 waves);",
                                   "the reference release has 42 more (84,121 persons) — a one-person",
                                   "difference originating in 01-ingest.R, not the construction logic."),
      coverage_gaps = paste("552 / 593 reference variables reproduced; the unreproduced ~41 are the",
                            "_rp/_sp and combined-majority variants in the most complex generators",
                            "(education levels, race-ethnicity, COVID).")
    ),
    validation = list(
      reference = "raw-data/psid-shelf-original/PSIDSHELF_1968_2021_LONG.dta",
      how       = "Rscript 08-validate-output.R [n_vars]  (value-level agreement vs the reference release)"
    )
  )
)

# ---- write -----------------------------------------------------------
dir.create("metadata", showWarnings = FALSE)
out_yaml <- file.path("metadata", paste0(stub, ".yaml"))
write_yaml(manifest, out_yaml)
message("  wrote ", out_yaml, " (", n_rows, " rows × ", n_cols, " cols)")
