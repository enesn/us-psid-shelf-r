# =====================================================================
# R/generate/family_type.R
# Derives: fam_partnered_* (all waves), fam_married_* (1978+ only)
# =====================================================================

.gn <- nrow(psid_abridged)
.gset <- function(x, nv, y, set) {
  x <- set_label(x, var_label(nv, y))
  vl <- SPEC$value_labels[which(SPEC$value_labels$label_set %in% set), ]
  if (nrow(vl)) attr(x, "labels") <- setNames(as.numeric(vl$value), vl$label)
  x
}
ps <- function(y) psid_abridged[[paste0("fam_parstat_", y)]]
pt <- function(y) psid_abridged[[paste0("fam_partype_", y)]]
ms <- function(y) psid_abridged[[paste0("fam_marstat_", y)]]

# fam_partnered — RP currently has a spouse/partner in FU?
for (y in year) {
  out <- rep(-1, .gn)
  if (y == 1968) {
    out <- rc(out, inrange(ps(y), 2, 5) | inlist(ps(y), 80), 0); out <- rc(out, inlist(ps(y), 1), 1); out <- rc(out, is.na(ps(y)), NA)
  } else if (y <= 1982) {
    out <- rc(out, inrange(ps(y), 2, 5), 0); out <- rc(out, inlist(ps(y), 1), 1); out <- rc(out, is.na(ps(y)), NA)
  } else {
    out <- rc(out, inlist(pt(y), 0, 4), 0); out <- rc(out, inrange(pt(y), 1, 3), 1); out <- rc(out, is.na(pt(y)), NA)
  }
  psid_abridged[[paste0("fam_partnered_", y)]] <- .gset(out, "fam_partnered", y, "fampartner_2cat")
}

# fam_married — RP legally married to a co-resident spouse? (1978 onward)
for (y in year[year >= 1978]) {
  out <- rep(-1, .gn)
  if (y <= 1982) {
    out <- rc(out, inrange(ps(y), 2, 5) | inlist(ms(y), 3, 4, 5), 0)
    out <- rc(out, inlist(ps(y), 1) & inlist(ms(y), 1, 2), 1)
    out <- rc(out, is.na(ps(y)) | is.na(ms(y)), NA)
  } else {
    out <- rc(out, inlist(pt(y), 0, 4) | inlist(ms(y), 3, 4, 5), 0)
    out <- rc(out, inrange(pt(y), 1, 3) & inlist(ms(y), 1, 2), 1)
    out <- rc(out, is.na(pt(y)) | is.na(ms(y)), NA)
  }
  psid_abridged[[paste0("fam_married_", y)]] <- .gset(out, "fam_married", y, "fammarried_2cat")
}
