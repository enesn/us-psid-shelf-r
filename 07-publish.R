# =====================================================================
# 07-publish.R  --  Finalize order, reshape wide -> long, publish
#
# Finalize variable order and publish the PSID-SHELF data:
#   * keep & order the published variables (spec/publish_vars.csv),
#   * uppercase names,
#   * reshape long on time-varying stubs (those with a _<wave> suffix),
#   * reattach variable + value labels,
#   * write a single PSID_SHELF_R_<fromyear>_<toyear>_LONG.{parquet,dta}  (+ _WIDE.parquet).
#
# The long table (~3.5M x ~552) is built ONE COLUMN AT A TIME (not via a
# multi-copy pivot) so peak memory stays well within RAM; the wide tables are
# freed before the long table is materialised.
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

# ---- 1. ordered publish list (expand Stata varlist wildcards) ---------
banner("publish: select & order finalized variables")
dom_order <- c("survey_identifiers","panel_status","sample_design","demographics",
               "education","family_type","geography","race_ethnicity","time_use",
               "chronic_conditions","covid_19","dementia","depression","disability",
               "general_wellbeing","earnings","employment","expenditures",
               "family_income","occupations","primary_home","wealth","relationship_id")
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
names(shelf_wide) <- toupper(names(shelf_wide))

# OCC_*1970C* carry a year *inside* the name; shield from the wave-suffix parse
occ_shield <- c("1970C" = "XXXXC", "2000C" = "YYYYC", "2010C" = "ZZZZC")
for (k in names(occ_shield))
  names(shelf_wide) <- str_replace(names(shelf_wide), paste0("OCC_", k), paste0("OCC_", occ_shield[k]))
restore_occ <- function(x) { for (k in names(occ_shield)) x <- str_replace(x, paste0("OCC_", occ_shield[k]), paste0("OCC_", k)); x }

# ---- 2. write WIDE (parquet) ------------------------------------------
banner("publish: write WIDE parquet")
write_parquet(shelf_wide, file.path(out_dir, sprintf("PSID_SHELF_R_%d_%d_WIDE.parquet", fw, lw)))

# ---- 3. identify time-varying stubs ----------------------------------
wave_rx <- paste0("_(", paste(year, collapse = "|"), ")$")
tv_cols <- grep(wave_rx, names(shelf_wide), value = TRUE)
ti_cols <- setdiff(names(shelf_wide), c(tv_cols, "ID"))
stubs   <- unique(str_replace(tv_cols, wave_rx, ""))
stubs_final <- restore_occ(stubs)
message(sprintf("  %d time-varying stubs, %d time-invariant columns", length(stubs), length(ti_cols)))

# ---- 4. build LONG one column at a time (wave-major), then sort -------
banner("publish: reshape wide -> long")
n <- nrow(shelf_wide); ny <- length(year); na_dbl <- rep(NA_real_, n)
long <- vector("list", 2 + length(ti_cols) + length(stubs))
nm   <- character(length(long))
long[[1]] <- rep(shelf_wide$ID, ny);            nm[1] <- "ID"
long[[2]] <- rep(as.integer(year), each = n);   nm[2] <- "YEAR"
p <- 2L
for (c in ti_cols) { p <- p + 1L; long[[p]] <- rep(as.vector(shelf_wide[[c]]), ny); nm[p] <- restore_occ(c) }
for (j in seq_along(stubs)) {
  p <- p + 1L
  long[[p]] <- unlist(lapply(year, function(y) {
    col <- shelf_wide[[paste0(stubs[j], "_", y)]]; if (is.null(col)) na_dbl else as.vector(col)
  }), use.names = FALSE)
  nm[p] <- stubs_final[j]
}
names(long) <- nm
rm(shelf_wide); .safe_gc()
ord <- order(long$ID, long$YEAR)                 # sort by ID, YEAR (matches Stata)
for (k in seq_along(long)) long[[k]] <- long[[k]][ord]
rm(ord); .safe_gc()
message(sprintf("  LONG: %d rows x %d cols", length(long[[1]]), length(long)))

# ---- 5. downcast to integer where possible, then reattach labels ------
# Columns that are whole-valued and fit in a 32-bit int are stored as integer so
# Stata writes them as byte/int/long (≈ half the .dta size); fractional columns
# (weights) and out-of-range columns (inflated real-dollar amounts) stay double.
banner("publish: downcast + attach labels + write")
.safe_gc()                                           # reclaim memory before peak allocation
int_ok <- function(x) {
  if (is.integer(x)) return(TRUE)
  if (!is.double(x)) return(FALSE)
  r <- suppressWarnings(range(x, na.rm = TRUE))     # suppress the empty-range warning on all-NA cols
  if (!is.finite(r[1])) return(TRUE)                # all NA (range -> Inf/-Inf) -> castable to integer NA
  r[1] >= -2147483647 && r[2] <= 2147483647 && !any(x != floor(x), na.rm = TRUE)
}
for (k in seq_along(long)) {
  v <- names(long)[k]
  x <- long[[k]]
  if (int_ok(x)) x <- as.integer(x)
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

stopifnot(!anyDuplicated(long[c("ID", "YEAR")]))
write_parquet(long, file.path(out_dir, sprintf("PSID_SHELF_R_%d_%d_LONG.parquet", fw, lw)))
#write_dta(long,     file.path(out_dir, sprintf("PSID_SHELF_R_%d_%d_LONG.dta", fw, lw)))

banner(sprintf("[07-publish] wrote PSID_SHELF_R_%d_%d_LONG  (%d x %d)  parquet + dta",
               fw, lw, nrow(long), ncol(long)))
