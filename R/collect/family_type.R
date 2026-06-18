# =====================================================================
# R/collect/family_type.R
# Builds: fam_size_*, fam_size_chi_*, fam_parstat_*, fam_partype_*,
#         fam_marstat_*, fam_young_*
# =====================================================================

# fam_size — number of people in FU
psid_abridged <- collect_tv(psid_abridged, "fam_size", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 20), x)
  out <- rc(out, inlist(x, 99) | is.na(x), NA)
  out
})

# fam_size_chi — number of children in FU
psid_abridged <- collect_tv(psid_abridged, "fam_size_chi", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 0, 20), x)
  out <- rc(out, is.na(x), NA)
  out
})

# fam_parstat — RP partnership status
psid_abridged <- collect_tv(psid_abridged, "fam_parstat", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 5), 2)
  out <- rc(out, inlist(x, 4), 3); out <- rc(out, inlist(x, 3), 4)
  out <- rc(out, inlist(x, 2), 5); out <- rc(out, inlist(x, 8), 80)
  out <- rc(out, inlist(x, 9) | is.na(x), NA)
  out
})

# fam_partype — RP partner type
psid_abridged <- collect_tv(psid_abridged, "fam_partype", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, inlist(x, 2), 2); out <- rc(out, inlist(x, 3), 3)
  out <- rc(out, inlist(x, 4), 4); out <- rc(out, is.na(x), NA)
  out
})

# fam_marstat — RP legal marital status
psid_abridged <- collect_tv(psid_abridged, "fam_marstat", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 5), 2)
  out <- rc(out, inlist(x, 4), 3); out <- rc(out, inlist(x, 3), 4)
  out <- rc(out, inlist(x, 2), 5)
  out <- rc(out, inlist(x, 8, 9), NA); out <- rc(out, inlist(x, 0), NA)
  out <- rc(out, is.na(x), NA)
  out
})

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
