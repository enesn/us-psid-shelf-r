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
# The long table (~3.7M x ~800) is never fully materialised: it is emitted in
# ID-chunks, each already in final (ID, YEAR) order, and streamed straight to
# parquet, so peak memory is one chunk + the resident wide table (~10-12 GB) and
# stays flat as new domains add columns. Leftover objects leaked into .GlobalEnv
# by earlier stages (domain files are sourced with local = FALSE) are dropped up
# front, and the WIDE parquet is likewise streamed in row chunks so the Arrow
# conversion never holds a full second copy of a table.
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
               "general_wellbeing","earnings","employment","work_history","expenditures",
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
ti_final    <- restore_occ(ti_cols)
message(sprintf("  %d time-varying stubs, %d time-invariant columns", length(stubs), length(ti_cols)))

# ---- 4. reshape wide -> long, STREAMED in ID-chunks -------------------
# The wide table holds one row per ID and every wave block shares that same ID
# ordering, so the final (ID, YEAR) sort is a deterministic interleave: for each
# ID we emit its waves in year order, IDs ascending. We therefore build the long
# table directly IN sorted order, one ID-chunk at a time, and stream each chunk
# straight to parquet. The full long table (~20 GB and growing with every new
# domain) is never materialised — peak memory is one chunk + the resident wide
# table (~10-12 GB total), independent of the published column count. Equivalence
# to the former unlist()+global-sort build is regression-tested against the WIDE
# parquet (see scratchpad verify_reshape.R in the memory notes).
banner("publish: reshape wide -> long (streamed)")
int_ok <- function(x) {                              # whole-valued & 32-bit-safe -> integer
  if (is.integer(x)) return(TRUE)
  if (!is.double(x)) return(FALSE)
  r <- suppressWarnings(range(x, na.rm = TRUE))      # suppress empty-range warning on all-NA cols
  if (!is.finite(r[1])) return(TRUE)                 # all NA (range -> Inf/-Inf) -> castable to integer NA
  r[1] >= -2147483647 && r[2] <= 2147483647 && !any(x != floor(x), na.rm = TRUE)
}
n  <- nrow(shelf_wide); ny <- length(year)
ys <- year[order(year)]                              # years ascending (Stata (ID,YEAR) order)
shelf_wide <- as.list(shelf_wide)
id <- shelf_wide$ID
# (ID, YEAR) uniqueness reduces to unique wide IDs (each ID emits one row/year);
# O(n) on the wide key, far cheaper than an adjacency scan of the n*ny long table
stopifnot(!anyDuplicated(id))
id_perm <- order(id)                                 # wide-row positions in ascending-ID order

# Integer-castability is decided ONCE per output column so every streamed
# row-group shares a schema. A stub is int-castable iff every PRESENT wave is
# (the union of in-range whole values stays in-range and whole).
id_int   <- int_ok(id)
ti_int   <- vapply(ti_cols, function(c) int_ok(shelf_wide[[c]]), logical(1))
stub_int <- vapply(stubs, function(s) all(vapply(ys, function(y) {
  col <- shelf_wide[[paste0(s, "_", y)]]; is.null(col) || int_ok(col) }, logical(1))), logical(1))

out_names <- c("ID", "YEAR", ti_final, stubs_final)

# attach labels exactly as the former step 5 did (var label always; value-label
# set when one is assigned; integer stays integer, else promoted to double)
label_col <- function(v, x) {
  if (v == "YEAR") return(haven::labelled(x, label = "Survey year"))
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
    haven::labelled(x, labels = labs, label = lab)
  } else structure(x, label = lab)
}

# Build one (ID,YEAR)-sorted long block for a chunk of wide-row positions `rows`.
# ti value -> repeated over the ny waves; stub -> the ny wave values interleaved
# per ID (t(matrix) flatten = ID-major then year). Types match the global decision.
build_chunk <- function(rows) {
  m <- length(rows)
  cast <- function(x, as_int) if (as_int) as.integer(x) else x
  cols <- vector("list", length(out_names)); names(cols) <- out_names
  cols[["ID"]]   <- cast(rep(id[rows], each = ny), id_int)
  cols[["YEAR"]] <- rep(as.integer(ys), times = m)
  for (i in seq_along(ti_cols))
    cols[[ti_final[i]]] <- cast(rep(as.vector(shelf_wide[[ti_cols[i]]])[rows], each = ny), ti_int[[i]])
  for (j in seq_along(stubs)) {
    M <- vapply(ys, function(y) {
      col <- shelf_wide[[paste0(stubs[j], "_", y)]]
      if (is.null(col)) rep(NA_real_, m) else as.vector(col)[rows]
    }, numeric(m))
    if (!is.matrix(M)) M <- matrix(M, nrow = m)      # m == 1 guard
    cols[[stubs_final[j]]] <- cast(as.vector(t(M)), stub_int[[j]])
  }
  for (v in out_names) cols[[v]] <- label_col(v, cols[[v]])
  class(cols) <- "data.frame"; attr(cols, "row.names") <- .set_row_names(m * ny)
  cols
}

# ---- 5. stream the long blocks to parquet -----------------------------
# Each chunk is written as its own row-group; the schema + shelf_labels metadata
# come from the first block (its label attrs are identical across blocks).
banner("publish: attach labels + write (streamed)")
chunk_ids <- as.integer(Sys.getenv("PSIDSHELF_LONG_CHUNK_IDS", "6000"))  # ~6000*ny rows/block
starts <- seq(1L, length(id_perm), by = chunk_ids)
message(sprintf("  LONG: %d rows x %d cols, streamed in %d ID-chunk(s) of <=%d IDs",
                n * ny, length(out_names), length(starts), chunk_ids))

long_path <- file.path(out_dir, sprintf("PSID_SHELF_R_%d_%d_LONG.parquet", fw, lw))
first_df  <- build_chunk(id_perm[starts[1]:min(starts[1] + chunk_ids - 1L, length(id_perm))])
first     <- arrow::as_arrow_table(first_df)
first     <- first$ReplaceSchemaMetadata(
  c(first$schema$metadata, shelf_labels = shelf_labels_json(first_df)))
sink   <- arrow::FileOutputStream$create(long_path)
writer <- arrow::ParquetFileWriter$create(
  schema = first$schema, sink = sink,
  properties = arrow::ParquetWriterProperties$create(
    column_names = out_names, compression = "snappy"))
writer$WriteTable(first, chunk_size = nrow(first_df))
rm(first, first_df); .safe_gc()
for (k in seq_along(starts)[-1]) {
  df <- build_chunk(id_perm[starts[k]:min(starts[k] + chunk_ids - 1L, length(id_perm))])
  writer$WriteTable(arrow::as_arrow_table(df), chunk_size = nrow(df))
  rm(df); if (k %% 4L == 0L) .safe_gc()
}
writer$Close(); sink$close()
#write_dta(long,     file.path(out_dir, sprintf("PSID_SHELF_R_%d_%d_LONG.dta", fw, lw)))

banner(sprintf("[07-publish] wrote PSID_SHELF_R_%d_%d_LONG  (%d x %d)  parquet",
               fw, lw, n * ny, length(out_names)))
