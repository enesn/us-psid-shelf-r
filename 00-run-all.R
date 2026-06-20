# =====================================================================
# 00-run-all.R  --  PSID-SHELF R pipeline orchestrator
#
# Builds the PSID-SHELF, 1968-2021 (Social, Health & Economic Longitudinal File):
#   01-ingest.R             raw PSID extract  -> psid_abridged (wide, keyed by id)
#   03-shelf-parameters.R   load spec/  (construction parameters & value labels)
#   04-collect-inputs.R     collect & merge input variables
#   05-generate-variables.R generated (derived) variables
#   06-revise-variables.R   not-in-FU recode, family-size, inflation adjustments
#   07-publish.R            reshape wide->long, write parquet + dta
#   09-metadata.R           write metadata/<version>.yaml run manifest
#
# The spec/ folder (parameters, value labels, variable maps, publish lists) must
# already exist; the pipeline reads only spec/ and the raw PSID data.
#
# Usage:   Rscript 00-run-all.R
# =====================================================================

t_all <- Sys.time()

# ---- packages --------------------------------------------------------
need <- c("readr", "vroom", "stringr", "dplyr", "tidyr", "haven", "arrow", "jsonlite",
          "yaml", "digest", "writexl")   # yaml/digest/writexl: 09-metadata.R (manifest + codebook)
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  message("Installing missing packages: ", paste(miss, collapse = ", "))
  install.packages(miss, repos = "https://cloud.r-project.org")
}

stopifnot(dir.exists("spec"))   # run 000-extract-specs.R first

banner <- function(m) message(sprintf("\n%s\n### %s\n%s", strrep("=", 64), m, strrep("=", 64)))

banner("01  ingest raw PSID extract")
if (!exists("psid_abridged")) source("01-ingest.R") else
  message("  psid_abridged already in memory — skipping ingest")

banner("03  load parameters / spec")
source("03-shelf-parameters.R")

banner("04  collect input variables")
source("04-collect-inputs.R")

banner("05  generate variables")
source("05-generate-variables.R")

banner("06  revise variables")
source("06-revise-variables.R")

banner("07  publish (reshape + write)")
source("07-publish.R")

banner("09  generate metadata manifest")
source("09-metadata.R")

message(sprintf("\n[00-run-all] DONE in %.1f min",
                as.numeric(difftime(Sys.time(), t_all, units = "mins"))))
