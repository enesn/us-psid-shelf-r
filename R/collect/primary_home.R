# =====================================================================
# R/collect/primary_home.R
# Builds: home_stat_*, home_own_val_nd_*, home_own_mor_any_{1m,2m,3m}_*,
#         home_own_mor_val_{1m,2m}_nd_*
# =====================================================================

# home_stat — own / rent / neither
psid_abridged <- collect_tv(psid_abridged, "home_stat", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 5), 2); out <- rc(out, inlist(x, 8), 3)
  out <- rc(out, is.na(x), NA)
  if (y == 1994) out <- rc(out, inlist(x, 0), NA)
  if (y == 1994 || y == 2007) out <- rc(out, inlist(x, 9), NA)
  out
})

# home value (nominal dollars). The valid range grows by era; 0 and the era's
# "max valid" code pass through, DK/refused (top sentinels) -> missing.
psid_abridged <- collect_tv(psid_abridged, "home_own_val_nd", function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 1974)        out <- blk(out, x, 1, 99998,    pass = c(0, 99999))
  else if (y <= 1993)   out <- blk(out, x, 1, 999998,   pass = c(0, 999999))
  else if (y <= 2003)   { out <- blk(out, x, 1, 9999996, pass = c(0, 9999997)); out <- rc(out, inlist(x, 9999998, 9999999), NA) }
  else if (y <= 2017)   { out <- blk(out, x, 1, 9999997, pass = 0);             out <- rc(out, inlist(x, 9999998, 9999999), NA) }
  else if (y == 2019)   { out <- blk(out, x, 1, 9999996, pass = c(0, 9999997)); out <- rc(out, inlist(x, 9999998, 9999999), NA) }
  else                  { out <- blk(out, x, 1, 99999996, pass = c(0, 99999997)); out <- rc(out, inlist(x, 99999998, 99999999), NA) }
  rc(out, is.na(x), NA)
})

# mortgage value (1m / 2m) — identical era blocks
mor_val <- function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 1972)        out <- blk(out, x, 1, 99998,   pass = c(0, 99999))
  else if (y <= 1993)   out <- blk(out, x, 1, 999998,  pass = c(0, 999999))
  else                  { out <- blk(out, x, 1, 9999996, pass = c(0, 9999997)); out <- rc(out, inlist(x, 9999998, 9999999), NA) }
  rc(out, is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_val_1m_nd", mor_val)
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_val_2m_nd", mor_val)

# any mortgage? (1m has the 1968 "1 or 2" coding; 2m starts 1969; 3m single era)
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_any_1m", function(x, y) {
  out <- rep(-1, length(x))
  if (y == 1968)       { out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1, 2), 1); out <- rc(out, inlist(x, 9, 0) | is.na(x), NA) }
  else if (y <= 1993)  { out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1);    out <- rc(out, inlist(x, 9, 0) | is.na(x), NA) }
  else                 { out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1);    out <- rc(out, inlist(x, 8, 9, 0) | is.na(x), NA) }
  out
})
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_any_2m", function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 1993)       { out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 9, 0) | is.na(x), NA) }
  else                 { out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 8, 9, 0) | is.na(x), NA) }
  out
})
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_any_3m", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})
