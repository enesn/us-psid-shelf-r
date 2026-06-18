# =====================================================================
# R/collect/family_type.R
# Builds: fam_size_*, fam_size_chi_*, fam_parstat_*, fam_partype_*,
#         fam_marstat_*, fam_young_*
# =====================================================================

# fam_size — number of people in FU
psid_abridged <- collect_tv(psid_abridged, "fam_size", function(x, y) recode(x,
  1 %..% 20 ~ keep,
  c(99, NA) ~ NA))

# fam_size_chi — number of children in FU
psid_abridged <- collect_tv(psid_abridged, "fam_size_chi", function(x, y) recode(x,
  0 %..% 20 ~ keep,
  NA ~ NA))

# fam_parstat — RP partnership status
psid_abridged <- collect_tv(psid_abridged, "fam_parstat", function(x, y) recode(x,
  1 ~ 1, 5 ~ 2, 4 ~ 3, 3 ~ 4, 2 ~ 5, 8 ~ 80,
  c(9, NA) ~ NA))

# fam_partype — RP partner type
psid_abridged <- collect_tv(psid_abridged, "fam_partype", function(x, y) recode(x,
  5 ~ 0, 1 ~ 1, 2 ~ 2, 3 ~ 3, 4 ~ 4,
  NA ~ NA))

# fam_marstat — RP legal marital status
psid_abridged <- collect_tv(psid_abridged, "fam_marstat", function(x, y) recode(x,
  1 ~ 1, 5 ~ 2, 4 ~ 3, 3 ~ 4, 2 ~ 5,
  c(8, 9, 0, NA) ~ NA))

# fam_young — age of youngest child in FU
psid_abridged <- collect_tv(psid_abridged, "fam_young", function(x, y) {
  out <- rep(-1, length(x))
  if (y >= 1968 && y <= 1969) {
    out <- rc(out, inlist(x, 0),       0)
    out <- rc(out, inrange(x, 1, 5),   x)
    out <- rc(out, inlist(x, 6),       806)
    out <- rc(out, inlist(x, 7),       809)
    out <- rc(out, inlist(x, 8),       814)
    out <- rc(out, inlist(x, 9) | is.na(x), NA)
  } else {
    out <- rc(out, inlist(x, 0),       0)
    out <- rc(out, inrange(x, 1, 17),  x)
    out <- rc(out, is.na(x),           NA)
  }
  out
})
