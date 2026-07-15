# =====================================================================
# 07-publish.R  --  Finalize order, reshape wide -> long, publish
#
# Finalize variable order and publish the PSID-SHELF data:
#   * keep & order the published variables (spec/publish_vars.csv),
#   * uppercase names,
#   * reshape long on time-varying stubs (those with a _<wave> suffix),
#   * reattach variable + value labels (also embedded as a portable JSON
#     dictionary in the parquet schema metadata, key "shelf_labels", so any
#     reader — R, Python, DuckDB — can recover them from the schema alone),
#   * write a single PSID_SHELF_R_<fromyear>_<toyear>_LONG.{parquet,dta}  (+ _WIDE.parquet;
#     set PSIDSHELF_WRITE_WIDE=0 to skip the WIDE file on dev iterations).
#
# The long table (~3.7M x ~744) is built ONE COLUMN AT A TIME (not via a
# multi-copy pivot) so peak memory stays well within RAM; the wide tables are
# freed before the long table is materialised. Leftover objects leaked into
# .GlobalEnv by earlier stages (domain files are sourced with local = FALSE)
# are dropped up front, and both parquet files are streamed in row chunks so
# the Arrow conversion never holds a full second copy of a table.
#
# Prerequisites: 01/03/04/05/06 already sourced.
# =====================================================================

suppressMessages({library(haven); library(arrow); library(stringr)})

stopifnot(exists("psid_abridged"), exists("SPEC"), exists("year"))
out_dir <- "output"; dir.create(out_dir, showWarnings = FALSE)
fw <- min(year); lw <- psid_lastwave
# gc() here is only a memory optimisation; never let a gc-internal hiccup halt publish
.safe_gc <- function() tryCatch(gc(FALSE), error = function(e) message("  (gc skipped: ", conditionMessage(e), ")"))
banner <- function(m) message(sprintf("\n%s\n  %s\n%s", strrep("-", 60), m, strrep("-", 60)))

# Stream a data.frame to parquet in row chunks. write_parquet(df) converts the
# whole table to an Arrow copy at once (~doubles peak memory, the OOM point on
# large builds); this caps the extra copy at one chunk. Base `[` drops the
# label attribute on plain vectors (haven_labelled keeps its attrs), so it is
# re-attached per chunk.
write_parquet_chunked <- function(df, path, chunk_rows = 250000L,
                                  extra_metadata = NULL) {
  n <- nrow(df)
  chunk_df <- function(i) {
    out <- lapply(df, function(col) {
      x <- col[i]
      lb <- attr(col, "label", exact = TRUE)
      if (!is.null(lb) && is.null(attr(x, "label", exact = TRUE))) attr(x, "label") <- lb
      x
    })
    class(out) <- "data.frame"
    attr(out, "row.names") <- .set_row_names(length(i))
    out
  }
  starts <- seq(1L, n, by = chunk_rows)
  first  <- arrow::as_arrow_table(chunk_df(starts[1]:min(starts[1] + chunk_rows - 1L, n)))
  if (!is.null(extra_metadata))
    first <- first$ReplaceSchemaMetadata(c(first$schema$metadata, extra_metadata))
  sink   <- arrow::FileOutputStream$create(path)
  writer <- arrow::ParquetFileWriter$create(
    schema = first$schema, sink = sink,
    properties = arrow::ParquetWriterProperties$create(
      column_names = names(df), compression = "snappy"))
  writer$WriteTable(first, chunk_size = chunk_rows)
  rm(first)
  for (k in seq_along(starts)[-1]) {
    writer$WriteTable(
      arrow::as_arrow_table(chunk_df(starts[k]:min(starts[k] + chunk_rows - 1L, n))),
      chunk_size = chunk_rows)
    if (k %% 4L == 0L) .safe_gc()
  }
  writer$Close()
  sink$close()
  invisible(path)
}

# Compact JSON dictionary of every column's variable label + value labels,
# embedded in the parquet schema metadata under the "shelf_labels" key. Unlike
# the R-serialized "r" blob arrow adds on its own, this is readable from ANY
# parquet client without touching the data:
#   R:      jsonlite::fromJSON(arrow::ParquetFileReader$create(f)$GetSchema()$metadata$shelf_labels)
#   Python: json.loads(pyarrow.parquet.read_schema(f).metadata[b"shelf_labels"])
# Shape: {"COL": {"label": "...", "values": {"<code>": "<label text>", ...}}, ...}
shelf_labels_json <- function(df) {
  labs <- lapply(df, function(col) {
    out <- list()
    lb <- attr(col, "label", exact = TRUE)
    if (!is.null(lb)) out$label <- as.character(lb)[1]
    vl <- attr(col, "labels", exact = TRUE)
    if (!is.null(vl) && length(vl))
      out$values <- setNames(as.list(names(vl)), as.character(unname(vl)))
    out
  })
  labs <- labs[vapply(labs, length, integer(1)) > 0L]
  as.character(jsonlite::toJSON(labs, auto_unbox = TRUE))
}

# ---- 1. ordered publish list (expand Stata varlist wildcards) ---------
banner("publish: select & order finalized variables")
dom_order <- c("survey_identifiers","panel_status","sample_design","demographics",
               "education","family_type","geography","race_ethnicity","time_use",
               "chronic_conditions","covid_19","dementia","depression","disability",
               "general_wellbeing","earnings","employment","expenditures",
               "family_income","occupations","primary_home","wealth","relationship_id",
               "labor_income","capital_income","income")
pv <- SPEC$publish_vars
pv$dom_rank <- match(pv$domain, dom_order)
pv <- pv[order(pv$dom_rank, pv$order), ]

cols_now <- names(psid_abridged)
expand_token <- function(tok) {
  if (str_detect(tok, "\\*")) cols_now[str_detect(cols_now, paste0("^", str_replace_all(tok, "\\*", ".*"), "$"))]
  else if (tok %in% cols_now) tok else character(0)
}
publish <- union("id", intersect(unique(unlist(lapply(pv$token, expand_token))), cols_now))
message(sprintf("  %d published columns (%d publish tokens unresolved)",
                length(publish),
                length(unique(pv$token[!str_detect(pv$token, "\\*") & !(pv$token %in% cols_now)]))))

shelf_wide <- psid_abridged[, publish, drop = FALSE]
.safe_gc()                                          # drain ALTREP finalizer queue while psid_abridged still exists
if (exists("psid_abridged")) rm(psid_abridged)
.safe_gc()                                          # free the full wide table now

# ---- 1b. drop leftovers leaked by earlier stages ----------------------
# R/collect + R/generate + R/revise files are sourced with local = FALSE and
# leak their intermediates into .GlobalEnv (can be GBs). Publish needs only
# shelf_wide + the spec objects/helpers below; functions are always kept.
# 09-metadata reads everything from spec/ + output/ on disk.
.keep <- c("shelf_wide", "SPEC", "year", "psid_lastwave", "pcepi", "params",
           "n_year", "wlthyear", "inflate_year", "out_dir", "fw", "lw",
           "publish", "keep", "t_all", "t0", "need", "miss")
.drop <- setdiff(ls(.GlobalEnv), .keep)
.drop <- .drop[!vapply(.drop, function(nm) is.function(.GlobalEnv[[nm]]), logical(1))]
if (length(.drop)) {
  message(sprintf("  freeing %d leftover objects from earlier stages (e.g. %s)",
                  length(.drop), paste(head(.drop, 8), collapse = ", ")))
  rm(list = .drop, envir = .GlobalEnv)
}
.safe_gc()

names(shelf_wide) <- toupper(names(shelf_wide))

# OCC_*1970C* carry a year *inside* the name; shield from the wave-suffix parse
occ_shield <- c("1970C" = "XXXXC", "2000C" = "YYYYC", "2010C" = "ZZZZC")
for (k in names(occ_shield))
  names(shelf_wide) <- str_replace(names(shelf_wide), paste0("OCC_", k), paste0("OCC_", occ_shield[k]))
restore_occ <- function(x) { for (k in names(occ_shield)) x <- str_replace(x, paste0("OCC_", occ_shield[k]), paste0("OCC_", k)); x }

# ---- 2. write WIDE (parquet) ------------------------------------------
# PSIDSHELF_WRITE_WIDE=0 skips this write on dev iterations (08-validate reads
# only the LONG file); leave unset for release builds so the WIDE artifact
# stays in sync with the LONG one.
if (Sys.getenv("PSIDSHELF_WRITE_WIDE", "1") != "0") {
  banner("publish: write WIDE parquet")
  write_parquet_chunked(shelf_wide,
                        file.path(out_dir, sprintf("PSID_SHELF_R_%d_%d_WIDE.parquet", fw, lw)),
                        chunk_rows = 16000L,   # ~11.7k cols: keep each Arrow chunk ~1.5 GB
                        extra_metadata = c(shelf_labels = shelf_labels_json(shelf_wide)))
} else {
  banner("publish: SKIP WIDE parquet (PSIDSHELF_WRITE_WIDE=0)")
}

# ---- 3. identify time-varying stubs ----------------------------------
wave_rx <- paste0("_(", paste(year, collapse = "|"), ")$")
tv_cols <- grep(wave_rx, names(shelf_wide), value = TRUE)
ti_cols <- setdiff(names(shelf_wide), c(tv_cols, "ID"))
stubs   <- unique(str_replace(tv_cols, wave_rx, ""))
stubs_final <- restore_occ(stubs)
message(sprintf("  %d time-varying stubs, %d time-invariant columns", length(stubs), length(ti_cols)))

# ---- 4. build LONG one column at a time (wave-major), then sort -------
banner("publish: reshape wide -> long")
# int_ok is also used here (not just in step 5): whole-valued columns are
# downcast to integer AS they are built, halving the long table's footprint.
int_ok <- function(x) {
  if (is.integer(x)) return(TRUE)
  if (!is.double(x)) return(FALSE)
  r <- suppressWarnings(range(x, na.rm = TRUE))     # suppress the empty-range warning on all-NA cols
  if (!is.finite(r[1])) return(TRUE)                # all NA (range -> Inf/-Inf) -> castable to integer NA
  r[1] >= -2147483647 && r[2] <= 2147483647 && !any(x != floor(x), na.rm = TRUE)
}
n <- nrow(shelf_wide); ny <- length(year); na_dbl <- rep(NA_real_, n)
long <- vector("list", 2 + length(ti_cols) + length(stubs))
nm   <- character(length(long))
idv <- rep(shelf_wide$ID, ny)
long[[1]] <- if (int_ok(idv)) as.integer(idv) else idv; nm[1] <- "ID"; rm(idv)
long[[2]] <- rep(as.integer(year), each = n);   nm[2] <- "YEAR"
# consume shelf_wide column-by-column, freeing each source column once copied,
# so the wide and long tables never fully coexist (peak memory ~ max, not sum)
shelf_wide <- as.list(shelf_wide)
p <- 2L
for (c in ti_cols) {
  p <- p + 1L; x <- rep(as.vector(shelf_wide[[c]]), ny)
  long[[p]] <- if (int_ok(x)) as.integer(x) else x; nm[p] <- restore_occ(c)
  shelf_wide[[c]] <- NULL
}
for (j in seq_along(stubs)) {
  p <- p + 1L
  x <- unlist(lapply(year, function(y) {
    col <- shelf_wide[[paste0(stubs[j], "_", y)]]; if (is.null(col)) na_dbl else as.vector(col)
  }), use.names = FALSE)
  long[[p]] <- if (int_ok(x)) as.integer(x) else x
  nm[p] <- stubs_final[j]
  for (y in year) shelf_wide[[paste0(stubs[j], "_", y)]] <- NULL
  if (j %% 50L == 0L) .safe_gc()
}
rm(x)
names(long) <- nm
rm(shelf_wide); .safe_gc()
ord <- order(long$ID, long$YEAR)                 # sort by ID, YEAR (matches Stata)
for (k in seq_along(long)) long[[k]] <- long[[k]][ord]
rm(ord); .safe_gc()
# (ID, YEAR) duplicates must now be adjacent — O(n) check; anyDuplicated on the
# full data.frame pastes every row into a string (slow + large temporaries)
.nl <- length(long$ID)
stopifnot(!any(long$ID[-1L] == long$ID[-.nl] & long$YEAR[-1L] == long$YEAR[-.nl]))
rm(.nl)
message(sprintf("  LONG: %d rows x %d cols", length(long[[1]]), length(long)))

# ---- 5. downcast to integer where possible, then reattach labels ------
# Columns that are whole-valued and fit in a 32-bit int are stored as integer so
# Stata writes them as byte/int/long (≈ half the .dta size); fractional columns
# (weights) and out-of-range columns (inflated real-dollar amounts) stay double.
banner("publish: attach labels + write")
# every long column was already int-downcast as it was built in step 4
.safe_gc()                                           # reclaim memory before peak allocation
for (k in seq_along(long)) {
  v <- names(long)[k]
  x <- long[[k]]
  if (k %% 100L == 0L) .safe_gc()
  if (v == "YEAR") { long[[k]] <- haven::labelled(x, label = "Survey year"); next }
  lab <- var_label(tolower(v)); s <- set_for(tolower(v)); labs <- NULL
  if (!is.null(s)) {
    vl <- SPEC$value_labels[which(SPEC$value_labels$label_set == s), ]
    if (nrow(vl)) {
      labs <- setNames(as.numeric(vl$value), vl$label)
      labs <- labs[!duplicated(labs)]; labs <- labs[!duplicated(names(labs))]
    }
  }
  if (!is.null(labs)) {
    if (is.integer(x)) labs <- setNames(as.integer(labs), names(labs)) else x <- as.numeric(x)
    long[[k]] <- haven::labelled(x, labels = labs, label = lab)
  } else {
    long[[k]] <- structure(x, label = lab)
  }
}
class(long) <- "data.frame"; attr(long, "row.names") <- .set_row_names(length(long[[1]]))

write_parquet_chunked(long, file.path(out_dir, sprintf("PSID_SHELF_R_%d_%d_LONG.parquet", fw, lw)),
                      extra_metadata = c(shelf_labels = shelf_labels_json(long)))
#write_dta(long,     file.path(out_dir, sprintf("PSID_SHELF_R_%d_%d_LONG.dta", fw, lw)))

banner(sprintf("[07-publish] wrote PSID_SHELF_R_%d_%d_LONG  (%d x %d)  parquet",
               fw, lw, nrow(long), ncol(long)))
