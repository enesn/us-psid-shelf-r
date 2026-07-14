#!/usr/bin/env Rscript
# =====================================================================
# label_data.R  --  Reattach PSID-SHELF variable + value labels to
#                   unlabelled data.
#
# The published parquet files carry haven-style labels only when read
# back through R/arrow; anything that went through DuckDB, Python, CSV,
# or attribute-dropping subsetting comes back unlabelled. This script
# reattaches labels from the spec/ CSVs, mirroring 07-publish.R step 5.
#
# Works on both layouts:
#   * LONG columns (ID, YEAR, LINEAGE, ...)          -> stub label
#   * WIDE columns with a wave suffix (FUID_1985, OCC_1970C_HD_2003, ...)
#     -> stub label with ", <wave>" appended (matches the build's labels)
#
# As a CLI:
#   Rscript label_data.R <input> [output]
#     input : .parquet | .dta | .rds | .csv
#     output: .rds | .dta | .parquet   (default: <input>_labelled.rds;
#             .csv is refused -- writing CSV would drop the labels again)
#
# From R:
#   source("label_data.R")
#   df <- attach_shelf_labels(df, spec_dir = "spec")
# =====================================================================

# attach_shelf_labels(df, spec_dir): return a labelled data.frame — every
# recognised column becomes a haven_labelled vector carrying its variable
# label, plus value labels where the variable has a label set assigned
# (spec/var_value_label_map.csv). Accepts anything coercible to a data.frame
# (tibble, arrow Table). Unrecognised columns are left untouched and reported.
#
# values = "factor" (default): columns with value labels are returned as
#   factors showing the label TEXT ("Not employed: Looking for work"), with
#   unlabelled codes kept as their number; the variable label is preserved.
# values = "codes": keep haven_labelled numeric codes (required for .dta
#   output — factor levels renumber to 1..k and would corrupt the codes).
attach_shelf_labels <- function(df, spec_dir = "spec", verbose = TRUE,
                                values = c("factor", "codes")) {
  values <- match.arg(values)
  df    <- as.data.frame(df)
  vlab  <- utils::read.csv(file.path(spec_dir, "var_labels.csv"))
  vals  <- utils::read.csv(file.path(spec_dir, "value_labels.csv"))
  vmap  <- utils::read.csv(file.path(spec_dir, "var_value_label_map.csv"))
  years <- jsonlite::fromJSON(file.path(spec_dir, "parameters.json"))$year
  wave_rx <- paste0("_(", paste(years, collapse = "|"), ")$")

  lab_of <- setNames(vlab$label, vlab$newvar)
  vmap   <- vmap[!is.na(vmap$label_set), ]
  set_of <- setNames(vmap$label_set, vmap$newvar)

  n_lab <- 0L; n_val <- 0L; unmatched <- character(0)
  for (k in seq_along(df)) {
    nmk <- names(df)[k]
    v   <- tolower(nmk)
    x   <- df[[k]]
    if (v == "year") {
      df[[k]] <- haven::labelled(x, label = "Survey year")
      n_lab <- n_lab + 1L
      next
    }
    # wide layout: strip a trailing _<wave> if the stub is a known variable
    # (years inside a name, e.g. OCC_1970C, never match the $-anchored regex)
    wave <- NULL
    if (!v %in% names(lab_of) && grepl(wave_rx, v)) {
      stub <- sub(wave_rx, "", v)
      if (stub %in% names(lab_of) || stub %in% names(set_of)) {
        wave <- sub(paste0("^.*", wave_rx), "\\1", v)
        v    <- stub
      }
    }
    if (!(v %in% names(lab_of) || v %in% names(set_of))) {
      unmatched <- c(unmatched, nmk)
      next
    }
    lab <- if (v %in% names(lab_of)) unname(lab_of[[v]]) else v
    if (!is.null(wave)) lab <- paste0(lab, ", ", wave)

    labs <- NULL
    if (v %in% names(set_of) && is.numeric(x)) {
      vl <- vals[vals$label_set == set_of[[v]], ]
      if (nrow(vl)) {
        labs <- setNames(as.numeric(vl$value), vl$label)
        labs <- labs[!duplicated(labs)]
        labs <- labs[!duplicated(names(labs))]
      }
    }
    if (!is.null(labs)) {
      if (is.integer(x)) labs <- setNames(as.integer(labs), names(labs))
      x <- haven::labelled(x, labels = labs, label = lab)
      if (values == "factor") {
        x <- haven::as_factor(x, levels = "default")   # label text; bare codes kept as-is
        attr(x, "label") <- lab                        # as_factor drops the variable label
      }
      df[[k]] <- x
      n_val <- n_val + 1L
    } else if (is.numeric(x) || is.character(x)) {
      # haven_labelled even without value labels, so the whole frame is
      # uniformly labelled (print/as_factor/look_for see every column)
      df[[k]] <- haven::labelled(x, label = lab)
    } else {
      df[[k]] <- structure(x, label = lab)   # labelled() rejects other types
    }
    n_lab <- n_lab + 1L
  }
  if (verbose) {
    message(sprintf("  labelled %d/%d columns (%d with value labels)",
                    n_lab, ncol(df), n_val))
    if (length(unmatched))
      message("  no label found for: ", paste(unmatched, collapse = ", "))
  }
  df
}

# ---- CLI ---------------------------------------------------------------
# runs only under `Rscript label_data.R ...`; source() just defines the fn
.this_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (length(.this_file) && basename(.this_file) == "label_data.R") {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1)
    stop("usage: Rscript label_data.R <input.{parquet,dta,rds,csv}> [output.{rds,dta,parquet}]")
  infile  <- args[1]
  outfile <- if (length(args) >= 2) args[2]
             else paste0(sub("\\.[A-Za-z]+$", "", infile), "_labelled.rds")
  spec_dir <- file.path(dirname(sub("^--file=", "", .this_file[1])), "spec")

  ext <- function(p) tolower(sub(".*\\.", "", p))
  df <- switch(ext(infile),
    parquet = arrow::read_parquet(infile),
    dta     = haven::read_dta(infile),
    rds     = readRDS(infile),
    csv     = utils::read.csv(infile),
    stop("unsupported input format: .", ext(infile)))
  message(sprintf("read %s: %d rows x %d cols", infile, nrow(df), ncol(df)))

  # .dta keeps numeric codes (Stata shows the value labels itself; factor
  # conversion would renumber codes to 1..k); .rds/.parquet get label text
  df <- attach_shelf_labels(df, spec_dir = spec_dir,
                            values = if (ext(outfile) == "dta") "codes" else "factor")

  switch(ext(outfile),
    rds     = saveRDS(df, outfile),
    dta     = haven::write_dta(df, outfile),
    parquet = arrow::write_parquet(df, outfile),
    stop("unsupported output format: .", ext(outfile),
         " (labels survive only in .rds, .dta, or R-written .parquet)"))
  message("wrote ", outfile)
}
