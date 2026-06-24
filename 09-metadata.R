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

# ---- write manifest --------------------------------------------------
dir.create("metadata", showWarnings = FALSE)
out_yaml <- file.path("metadata", paste0(stub, ".yaml"))
write_yaml(manifest, out_yaml)
message("  wrote ", out_yaml, " (", n_rows, " rows × ", n_cols, " cols)")

# =====================================================================
# Excel codebook (metadata/codebook_<version>.xlsx) — built entirely from
# spec/ (+ the R recode files for the source-file pointer). Lets a user:
#  * understand every variable + value label,
#  * see which raw (cross-year-index) variable constructs each SHELF variable
#    in each wave (the "recoding across waves"),
#  * and which raw inputs feed each domain.
# =====================================================================
banner("09  generate metadata/codebook_<version>.xlsx")
if (!requireNamespace("writexl", quietly = TRUE)) {
  message("  (writexl not installed — skipping Excel codebook; install.packages('writexl'))")
} else {
  rd <- function(f) read.csv(file.path("spec", f), stringsAsFactors = FALSE)
  vlab <- rd("var_labels.csv"); vals <- rd("value_labels.csv"); vmap <- rd("var_value_label_map.csv")
  imap <- rd("input_var_map.csv"); isin <- rd("input_var_single.csv"); pubv <- rd("publish_vars.csv")
  tinv <- readLines("spec/time_invariant_vars.txt")

  # stub -> domain & publish order. Publish tokens are <stub> + a wave suffix
  # (_19*, _20*, or a literal year like _2021); strip that to recover the stub.
  pubv$stub <- sub("_(19|20)[0-9]{0,2}\\*?$", "", pubv$token)
  stubdom <- tapply(pubv$domain, pubv$stub, function(z) z[1])
  stubord <- tapply(pubv$order,  pubv$stub, function(z) z[1])
  # prefix -> domain (majority vote) on the first 1 and 2 name segments, to place
  # intermediates whose family has no published representative (covid_*_rp,
  # race_eth_1m_ext_rp) or whose 1st segment is a generic flag (if_expn, df_covid).
  maj <- function(z) names(sort(table(z), decreasing = TRUE))[1]
  seg1dom <- tapply(unname(stubdom), sub("_.*$", "", names(stubdom)), maj)
  seg2dom <- tapply(unname(stubdom), sub("^([^_]+_[^_]+).*$", "\\1", names(stubdom)), maj)
  # R-file scan: a var quoted literally in R/<stage>/<domain>.R belongs to <domain>
  # (resolves intermediates with no published representative, e.g. if_wlth_* flags).
  rfile_dom <- character(0)
  for (f in list.files(c("R/collect", "R/generate", "R/revise"), pattern = "\\.R$", full.names = TRUE)) {
    dom <- sub("\\.R$", "", basename(f)); ct <- paste(readLines(f, warn = FALSE), collapse = "\n")
    toks  <- gsub('"', '', unlist(regmatches(ct, gregexpr('"[a-z][a-z0-9_]*"', ct))))    # exact names
    for (v in setdiff(intersect(toks, vlab$newvar), names(rfile_dom))) rfile_dom[v] <- dom
    prefs <- gsub('"', '', unlist(regmatches(ct, gregexpr('"[a-z][a-z0-9_]*_"', ct))))   # paste0/sprintf bases
    for (p in prefs) for (v in setdiff(vlab$newvar[startsWith(vlab$newvar, p)], names(rfile_dom))) rfile_dom[v] <- dom
  }
  dom_of <- function(v) {                              # match var (or its stub) to a domain
    for (cand in c(v, sub("_(rp|sp|ind)$", "", v), sub("_(1m|2m|3m|4m)(_(rp|sp|ind))?$", "", v)))
      if (cand %in% names(stubdom)) return(unname(stubdom[cand]))
    hits <- names(stubdom)[startsWith(v, paste0(names(stubdom), "_"))]  # longest stub that v extends
    if (length(hits)) return(unname(stubdom[hits[which.max(nchar(hits))]]))
    if (v %in% names(rfile_dom)) return(unname(rfile_dom[v]))
    s2 <- sub("^([^_]+_[^_]+).*$", "\\1", v)
    if (s2 %in% names(seg2dom)) return(unname(seg2dom[s2]))
    s1 <- sub("_.*$", "", v)
    if (s1 %in% names(seg1dom)) return(unname(seg1dom[s1]))
    NA_character_
  }
  by_var <- split(imap$year, imap$newvar)              # waves available per (time-varying) var

  # --- Variables sheet ---
  V <- data.frame(variable = vlab$newvar, label = vlab$label, stringsAsFactors = FALSE)
  V$domain    <- vapply(V$variable, dom_of, character(1))
  V$domain[is.na(V$domain)] <- "(parameter/metadata)"   # pcepi index, release strings, etc.
  V$value_set <- vmap$label_set[match(V$variable, vmap$newvar)]
  V$scope     <- ifelse(V$variable %in% names(by_var), "time-varying",
                 ifelse(V$variable %in% isin$newvar | V$variable %in% tinv, "time-invariant", "derived"))
  V$n_waves    <- vapply(V$variable, function(v) if (v %in% names(by_var)) length(by_var[[v]]) else NA_integer_, integer(1))
  V$first_wave <- vapply(V$variable, function(v) if (v %in% names(by_var)) min(by_var[[v]])  else NA_integer_, integer(1))
  V$last_wave  <- vapply(V$variable, function(v) if (v %in% names(by_var)) max(by_var[[v]])  else NA_integer_, integer(1))
  V$published  <- V$variable %in% vmap$newvar | V$variable %in% names(stubdom)
  V$stage      <- ifelse(V$variable %in% names(by_var) | V$variable %in% isin$newvar, "collect", "generate")
  V$source_R   <- ifelse(is.na(V$domain), NA_character_, sprintf("R/%s/%s.R", V$stage, V$domain))
  V$publish_order <- unname(stubord[match(V$variable, names(stubord))])
  V <- V[order(is.na(V$domain), V$domain, !V$published, V$publish_order, V$variable),
         c("domain","variable","label","scope","value_set","n_waves","first_wave","last_wave",
           "published","stage","source_R","publish_order")]

  # --- Value labels sheet (one row per variable x value) ---
  VL <- merge(vmap, vals, by = "label_set")
  VL$domain <- vapply(VL$newvar, dom_of, character(1))
  VL <- VL[order(VL$domain, VL$newvar, suppressWarnings(as.numeric(VL$value))),
           c("domain","newvar","label_set","value","label")]
  names(VL) <- c("domain","variable","value_set","value","value_label")

  # --- Cross-year map sheet (which raw var builds each SHELF var, per wave) ---
  # Wide: one row per SHELF variable, one column per wave (time-invariant vars
  # carry their single raw input in `time_invariant_input_var` instead).
  CYl <- rbind(imap[, c("newvar","year","input_var")],
               if (nrow(isin)) data.frame(newvar = isin$newvar, year = NA_integer_,
                                          input_var = isin$input_var) else NULL)
  CYl$domain <- vapply(CYl$newvar, dom_of, character(1))
  years <- sort(unique(stats::na.omit(CYl$year)))
  vars  <- sort(unique(vlab$newvar))    # every SHELF variable, not just collected ones
  wide  <- matrix(NA_character_, nrow = length(vars), ncol = length(years),
                  dimnames = list(vars, as.character(years)))
  tv    <- CYl[!is.na(CYl$year), ]
  wide[cbind(match(tv$newvar, vars), match(as.character(tv$year), colnames(wide)))] <- tv$input_var
  inv   <- setNames(CYl$input_var[is.na(CYl$year)], CYl$newvar[is.na(CYl$year)])
  CY <- data.frame(variable = vars, stringsAsFactors = FALSE, check.names = FALSE)
  CY$domain <- vapply(CY$variable, dom_of, character(1))
  CY$label  <- vlab$label[match(CY$variable, vlab$newvar)]
  CY$scope  <- V$scope[match(CY$variable, V$variable)]
  CY$time_invariant_input_var <- unname(inv[CY$variable])
  CY <- cbind(CY, as.data.frame(wide, stringsAsFactors = FALSE, check.names = FALSE))
  CY <- CY[order(CY$domain, CY$variable),
           c("domain","variable","label","scope","time_invariant_input_var", as.character(years))]

  # --- Domain summary sheet ---
  doms <- sort(unique(stats::na.omit(V$domain)))
  DS <- data.frame(
    domain       = doms,
    n_variables  = vapply(doms, function(d) sum(V$domain == d, na.rm = TRUE), integer(1)),
    n_published  = vapply(doms, function(d) sum(V$domain == d & V$published, na.rm = TRUE), integer(1)),
    n_raw_inputs = vapply(doms, function(d) length(unique(CYl$input_var[CYl$domain %in% d])), integer(1)),
    stringsAsFactors = FALSE)

  overview <- data.frame(sheet = c("Variables","Value_labels","Cross_year_map","Domain_summary"),
    description = c(
      "Every SHELF variable: domain, label, scope (time-varying/invariant/derived), value-label set, wave coverage, and the R file that holds its recode.",
      "Code -> text for every variable's value labels (one row per variable x value).",
      "The cross-year-index recoding across waves, wide: one row per SHELF variable (with its label and scope), one column per wave giving the raw PSID variable that constructs it that wave (time-invariant variables instead populate time_invariant_input_var; derived/generated variables with no raw input have these left blank).",
      "Per-domain counts of variables, published variables, and distinct raw inputs used."),
    stringsAsFactors = FALSE)
  meta <- data.frame(field = c("dataset","version","generated","spec_dir","n_variables","n_value_labels"),
    value = c("psid-shelf-r", stub, as.character(Sys.Date()), "spec/",
              nrow(V), nrow(VL)), stringsAsFactors = FALSE)

  out_xlsx <- file.path("metadata", paste0("codebook_", stub, ".xlsx"))
  writexl::write_xlsx(list(Overview = rbind(setNames(meta, c("sheet","description")), overview),
                           Variables = V, Value_labels = VL,
                           Cross_year_map = CY, Domain_summary = DS), out_xlsx)
  message(sprintf("  wrote %s  (%d variables, %d value labels, %d cross-year rows, %d domains)",
                  out_xlsx, nrow(V), nrow(VL), nrow(CY), nrow(DS)))
}
