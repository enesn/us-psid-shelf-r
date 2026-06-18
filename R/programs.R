# =====================================================================
# R/programs.R  --  Shared helpers (R ports of the Stata "programs" and the
#                   collect/label scaffolding used throughout the pipeline)
#
# These implement the PSID-SHELF construction "programs" that affect
# the saved data, plus the per-variable "gen newvar=-1 / replace ... if ..."
# scaffolding that every collect & generate domain repeats.
#
# Conventions (mirror the Stata pipeline):
#   * A freshly built SHELF variable starts at the sentinel -1; recode rules
#     overwrite it. Any -1 left at the end means a value code was not assigned
#     (flagged by check_unassigned(), exactly like Stata's progmissval).
#   * Missing is NA (Stata `.`).
# =====================================================================

# ---- recode primitive -------------------------------------------------
# rc(out, cond, val): out[cond] <- val   (Stata: replace out = val if cond)
#   * cond may contain NA (from NA inputs); NA positions are treated as FALSE,
#     exactly like Stata, which never matches `if` on a missing comparison.
#   * val is a scalar (recycled) or a full-length vector (used element-wise).
rc <- function(out, cond, val) {
  i <- which(cond)                      # which() drops NA -> FALSE semantics
  if (length(i)) out[i] <- if (length(val) == 1L) val else val[i]
  out
}

# inrange / inlist convenience (closed interval; NA-safe via rc's which())
inrange <- function(x, lo, hi) x >= lo & x <= hi
inlist  <- function(x, ...)    x %in% c(...)

# dollar block (used by the nominal-dollar economic domains): passthrough the
# valid range [lo,hi] and any `pass` codes, then map the era top-code `tc` to
# `tcout` (the standardized 9999999, or NA in the PSID "wild-code" years).
blk <- function(out, x, lo, hi, pass = NULL, tc = NULL, tcout = 9999999) {
  out <- rc(out, inrange(x, lo, hi), x)
  if (length(pass)) out <- rc(out, x %in% pass, x)
  if (length(tc))   out <- rc(out, x %in% tc, tcout)
  out
}

# ---- progsubsetpsid: year -> input-variable map for one SHELF variable ----
# Returns a data.frame(year, input_var) for the waves in which `newvar` has an
# input variable (i.e. the non-"." grid entries extracted into input_var_map).
subset_psid <- function(newvar) {
  m <- SPEC$input_var_map
  out <- m[m$newvar == newvar, c("year", "input_var")]
  out[order(out$year), ]
}
single_input <- function(newvar) {
  s <- SPEC$input_var_single
  v <- s$input_var[s$newvar == newvar]
  if (length(v) != 1L) stop("no single input var for ", newvar)
  v
}

# ---- collect scaffolding ---------------------------------------------
# collect_tv(df, newvar, fn): build time-varying columns newvar_<year> for every
# available wave by applying fn(input_values, year). fn returns the recoded
# vector (typically starting from rep(-1, n); see the per-domain definitions).
collect_tv <- function(df, newvar, fn) {
  map <- subset_psid(newvar)
  use_df <- length(formals(fn)) >= 3L     # fn(x, y, df) may read sibling columns
  for (i in seq_len(nrow(map))) {
    y  <- map$year[i]
    iv <- map$input_var[i]
    if (is.null(df[[iv]]))
      stop(sprintf("input var %s (for %s_%d) missing from data", iv, newvar, y))
    col <- paste0(newvar, "_", y)
    df[[col]] <- if (use_df) fn(df[[iv]], y, df) else fn(df[[iv]], y)
    df[[col]] <- set_value_labels(set_label(df[[col]], var_label(newvar, y)), newvar)
  }
  df
}

# collect_inv(df, newvar, fn): build a single time-invariant column from the
# variable's one input variable (inputvar1_).
collect_inv <- function(df, newvar, fn) {
  iv <- single_input(newvar)
  if (is.null(df[[iv]])) stop(sprintf("input var %s (for %s) missing", iv, newvar))
  df[[newvar]] <- fn(df[[iv]])
  df[[newvar]] <- set_value_labels(set_label(df[[newvar]], var_label(newvar)), newvar)
  df
}

# ---- variable / value labels (haven-style attributes) -----------------
# var_label(newvar[, year]): the variable's description from spec/var_labels.csv,
# with the wave appended for time-varying variables (matches the Stata labels,
# which the reshape step truncates back to the stub label).
var_label <- function(newvar, year = NULL) {
  lab <- SPEC$var_labels$label[SPEC$var_labels$newvar == newvar]
  lab <- if (length(lab)) lab[1] else newvar
  if (is.null(year)) lab else paste0(lab, ", ", year)
}
set_label <- function(x, label) { attr(x, "label") <- label; x }

# set_value_labels(x, newvar): attach the value-label set assigned to newvar.
# Mirrors Stata's `capture noisily lab val` — silently does nothing if the set
# is undefined/dynamic (see spec/unlabeled_sets.txt).
# attach the codes/labels of a single label set (size- and NA-robust); the
# tibble must be indexed via which() so a 0-length/NA logical can't error.
.attach_vl <- function(x, set) {
  if (is.null(set) || length(set) != 1L || is.na(set)) return(x)
  vl <- SPEC$value_labels[which(SPEC$value_labels$label_set == set), ]
  if (nrow(vl)) attr(x, "labels") <- setNames(as.numeric(vl$value), vl$label)
  x
}
# the value-label set assigned to a variable (NULL if none / dynamic)
set_for <- function(newvar) {
  s <- SPEC$var_value_label_map$label_set[SPEC$var_value_label_map$newvar == newvar]
  s <- s[!is.na(s)]
  if (length(s)) s[1] else NULL
}

set_value_labels <- function(x, newvar) .attach_vl(x, set_for(newvar))

# ---- generate-stage helpers (cross-year derivations) ------------------
# attach a generated variable's label (+ optional value-label set)
g_label <- function(x, newvar, year = NULL, set = NULL) {
  .attach_vl(set_label(x, var_label(newvar, year)), set)
}
# build newvar_<y> for each wave from fn(y); fn returns a full-length vector and
# reads sibling columns from the global psid_abridged.
gen_tv <- function(newvar, fn, set = NULL, years = year) {
  for (y in years) {
    v <- fn(y)
    if (is.null(v)) next                 # module not fielded this wave -> no column
    .GlobalEnv$psid_abridged[[paste0(newvar, "_", y)]] <- g_label(v, newvar, y, set)
  }
  invisible(NULL)
}
# within-group maximum (na.rm), assigned to every row; NA groups handled.
fu_max <- function(val, grp) {
  ave(val, ifelse(is.na(grp), -1L, grp),
      FUN = function(z) { m <- max(z, na.rm = TRUE); if (is.infinite(m)) NA_real_ else m })
}
# row-wise max across a list of equal-length vectors (Stata egen rowmax)
rowmax <- function(lst) {
  if (!length(lst)) return(rep(NA_real_, nrow(psid_abridged)))
  m <- do.call(pmax, c(lst, list(na.rm = TRUE))); m[is.infinite(m)] <- NA; m
}
# most recent non-NA value of <prefix>_<y> across waves (reverse-chronological)
latest <- function(prefix, years. = year) {
  out <- rep(NA_real_, nrow(psid_abridged))
  for (y in rev(years.)) {
    c <- psid_abridged[[paste0(prefix, "_", y)]]
    if (!is.null(c)) out <- ifelse(is.na(out) & !is.na(c), c, out)
  }
  out
}

# assign a "<measure>_resp" answer to the person who was the FU respondent:
# the RP answering as RP, or the SP answering as SP (and a current member).
role_map <- function(measure) {
  set <- set_for(measure)
  for (y in year) {
    src <- psid_abridged[[paste0(measure, "_resp_", y)]]
    if (is.null(src)) next
    rex <- psid_abridged[[paste0("respondent_ext_", y)]]
    rel <- psid_abridged[[paste0("rel_ext_", y)]]
    sq  <- psid_abridged[[paste0("seqnum_", y)]]
    cond <- (inrange(rex, 100, 199) & inrange(rel, 100, 199) & inrange(sq, 1, 20)) |
            (inrange(rex, 200, 299) & inrange(rel, 200, 299) & inrange(sq, 1, 20))
    out <- ifelse(cond %in% TRUE, src, NA_real_)
    .GlobalEnv$psid_abridged[[paste0(measure, "_", y)]] <- g_label(out, measure, y, set)
  }
}

# combine a measure's _ind / _rp / _sp versions into one per-person variable:
# the individual's own answer by default, overridden by the RP answer for the
# RP and the SP answer for the SP (current members).
combine_roles <- function(measure, set = NULL) {
  if (is.null(set)) set <- set_for(measure)
  for (y in year) {
    ind <- psid_abridged[[paste0(measure, "_ind_", y)]]
    rp  <- psid_abridged[[paste0(measure, "_rp_", y)]]
    sp  <- psid_abridged[[paste0(measure, "_sp_", y)]]
    if (is.null(ind) && is.null(rp) && is.null(sp)) next
    rel <- psid_abridged[[paste0("rel_ext_", y)]]; rex <- psid_abridged[[paste0("response_ext_", y)]]
    out <- rep(-1, nrow(psid_abridged))
    if (!is.null(ind)) out <- rc(out, TRUE, ind)
    if (!is.null(rp))  out <- rc(out, inrange(rel, 100, 199) & rex %in% 0, rp)
    if (!is.null(sp))  out <- rc(out, inrange(rel, 200, 299) & rex %in% 0, sp)
    .GlobalEnv$psid_abridged[[paste0(measure, "_", y)]] <- g_label(out, measure, y, set)
  }
}

# combine a measure's _rp / _sp versions into one per-person variable: the RP
# answer for the RP, the SP answer for the SP, NA for everyone else. (No _ind
# fallback — used where only RP/SP report, e.g. chronic conditions.)
combine_rpsp <- function(measure) {
  set <- set_for(measure)
  for (y in year) {
    rp <- psid_abridged[[paste0(measure, "_rp_", y)]]
    sp <- psid_abridged[[paste0(measure, "_sp_", y)]]
    if (is.null(rp) && is.null(sp)) next
    rel <- psid_abridged[[paste0("rel_ext_", y)]]; rex <- psid_abridged[[paste0("response_ext_", y)]]
    out <- rep(NA_real_, nrow(psid_abridged))
    if (!is.null(rp)) out <- rc(out, inrange(rel, 100, 199) & rex %in% 0, rp)
    if (!is.null(sp)) out <- rc(out, inrange(rel, 200, 299) & rex %in% 0, sp)
    .GlobalEnv$psid_abridged[[paste0(measure, "_", y)]] <- g_label(out, measure, y, set)
  }
}

# most-frequently-reported value of <measure>_rp/_sp across waves (the person's
# own value as RP/SP), ties broken by the most recent wave. Returns a per-person
# vector; NA where the value was never reported. (Stata "modal, recency" pattern.)
modal_recent <- function(measure) {
  n <- nrow(psid_abridged); cols <- list(); yrs <- integer(0)
  for (y in year) {
    rp <- psid_abridged[[paste0(measure, "_rp_", y)]]
    sp <- psid_abridged[[paste0(measure, "_sp_", y)]]
    if (is.null(rp) && is.null(sp)) next
    rel <- psid_abridged[[paste0("rel_ext_", y)]]; rex <- psid_abridged[[paste0("response_ext_", y)]]
    t <- rep(NA_real_, n)
    if (!is.null(rp)) t <- rc(t, inrange(rel, 100, 199) & rex %in% 0, rp)
    if (!is.null(sp)) t <- rc(t, inrange(rel, 200, 299) & rex %in% 0, sp)
    cols[[length(cols) + 1]] <- t; yrs <- c(yrs, y)
  }
  if (!length(cols)) return(rep(NA_real_, n))
  M <- do.call(cbind, cols)
  vals <- sort(unique(M[!is.na(M)]))
  best_val <- rep(NA_real_, n); best_cnt <- rep(0L, n); best_yr <- rep(-Inf, n)
  for (x in vals) {
    isx <- !is.na(M) & M == x
    cnt <- rowSums(isx)
    ym <- isx * matrix(yrs, n, length(yrs), byrow = TRUE); ym[ym == 0] <- NA
    lasty <- do.call(pmax, c(lapply(seq_len(ncol(ym)), function(j) ym[, j]), list(na.rm = TRUE)))
    lasty[is.infinite(lasty)] <- NA
    better <- cnt > best_cnt | (cnt == best_cnt & cnt > 0 & !is.na(lasty) & lasty > best_yr)
    best_val[better] <- x; best_cnt[better] <- cnt[better]
    best_yr[better & !is.na(lasty)] <- lasty[better & !is.na(lasty)]
  }
  ifelse(best_cnt == 0, NA_real_, best_val)
}

# ---- revise stage: nominal-dollar top-codes to preserve (not rescale) -------
# Values that are PSID sentinels (top-code / "1-or-less") and must pass through
# the family-size and inflation adjustments unchanged. Mirrors the per-category
# lists in Step_09 files 14 & 15.
dollar_topcodes <- function(varcat, y) {
  tc <- numeric(0)
  if (varcat %in% c("earn", "finc")) {
    if (y >= 1968 && y <= 1982) tc <- c(tc, 99999)
    if (y >= 1983 && y <= 1992) tc <- c(tc, 999999)
    if (y >= 1993 && y <= 2009) tc <- c(tc, 9999999)
    if (y >= 2011)              tc <- c(tc, 9999997)
    if (y >= 1970 && y <= 1993) tc <- c(tc, 1)
    tc <- c(tc, 9999997, 9999999)
  } else if (varcat == "home") {
    if (y >= 1968 && y <= 1974) tc <- c(tc, 99999)
    if (y >= 1975 && y <= 1993) tc <- c(tc, 999999)
    if (y >= 1994 && y <= 2019) tc <- c(tc, 9999997)
    if (y >= 2021)              tc <- c(tc, 99999997)
  } else if (varcat == "wlth") {
    tc <- c(999999, 9999997, 9999999, 999999997, 999999999)
  }
  unique(tc)
}
# columns that are nominal-dollar variables (stub ends in _nd / _ndf [+_rp/_sp]),
# returned as a data.frame(col, stub, varcat, year).
dollar_cols <- function(pattern = "_nd") {
  yr <- paste0("(", paste(year, collapse = "|"), ")")
  rx <- paste0(pattern, "[f]?(_rp|_sp)?_", yr, "$")
  cols <- grep(rx, names(psid_abridged), value = TRUE)
  if (!length(cols)) return(NULL)
  y  <- as.integer(sub(paste0(".*_(", paste(year, collapse = "|"), ")$"), "\\1", cols))
  data.frame(col = cols, stub = sub(paste0("_", yr, "$"), "", cols),
             varcat = substr(cols, 1, 4), year = y, stringsAsFactors = FALSE)
}

# ---- progemptydrop: drop variables that are NA for every observation ----
drop_empty <- function(df, vars = names(df)) {
  empties <- vars[vapply(df[vars], function(c) all(is.na(c)), logical(1))]
  if (length(empties)) {
    message("  drop_empty: dropping ", length(empties), " empty variables")
    df[empties] <- NULL
  }
  df
}

# ---- progmissval: warn about leftover unassigned sentinels --------------
check_unassigned <- function(df, vars = names(df),
                             errvals = c(-1, -111111111)) {
  bad <- vars[vapply(df[vars], function(c) any(c %in% errvals, na.rm = TRUE),
                     logical(1))]
  if (length(bad))
    warning("check_unassigned: ", length(bad),
            " variable(s) contain unassigned sentinels (-1/-111111111): ",
            paste(utils::head(bad, 20), collapse = ", "),
            if (length(bad) > 20) " ...")
  invisible(bad)
}
