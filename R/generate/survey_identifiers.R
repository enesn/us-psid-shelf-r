# =====================================================================
# R/generate/survey_identifiers.R
# Derives: respondent_*, rel_*, refcouple_* (from *_ext), sampstat (from sampstat_ext)
# =====================================================================

.gn <- nrow(psid_abridged)
.gset <- function(x, nv, y = NULL, set = NULL) {
  x <- set_label(x, var_label(nv, y))
  if (!is.null(set)) { vl <- SPEC$value_labels[which(SPEC$value_labels$label_set %in% set), ]
    if (nrow(vl)) attr(x, "labels") <- setNames(as.numeric(vl$value), vl$label) }
  x
}
# build newvar_<y> from src_<y> for every wave the source exists
gen_src <- function(newvar, src, fn, set = NULL) {
  for (y in year) {
    s <- psid_abridged[[paste0(src, "_", y)]]
    if (is.null(s)) next
    psid_abridged[[paste0(newvar, "_", y)]] <<- .gset(fn(s), newvar, y, set)
  }
}

gen_src("respondent", "respondent_ext", function(s) {
  out <- rep(-1, .gn)
  out <- rc(out, inrange(s, 100, 199), 1); out <- rc(out, inrange(s, 200, 299), 2)
  out <- rc(out, inrange(s, 800, 999), 3); rc(out, is.na(s), NA)
}, "respondent_3cat")

gen_src("rel", "rel_ext", function(s) {
  out <- rep(-1, .gn)
  out <- rc(out, inrange(s, 100, 199), 1); out <- rc(out, inrange(s, 200, 299), 2)
  out <- rc(out, inrange(s, 300, 399), 3); out <- rc(out, inrange(s, 400, 499), 4)
  rc(out, is.na(s), NA)
}, "rel_4cat")

gen_src("refcouple", "rel_ext", function(s) {
  out <- rep(-1, .gn)
  out <- rc(out, inrange(s, 300, 499), 0); out <- rc(out, inrange(s, 100, 299), 1)
  rc(out, is.na(s), NA)
}, "refcouple_2cat")

# sampstat (time-invariant) from sampstat_ext
se <- psid_abridged$sampstat_ext
ss <- rep(-1, .gn)
ss <- rc(ss, inlist(se, 1, 11, 12), 0); ss <- rc(ss, inlist(se, 100), 1)
ss <- rc(ss, inlist(se, 200), 2); ss <- rc(ss, inlist(se, 300), 3); ss <- rc(ss, inlist(se, 400), 4)
ss <- rc(ss, is.na(se), NA)
psid_abridged$sampstat <- .gset(ss, "sampstat", NULL, "sampstat_5cat")
