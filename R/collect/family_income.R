# =====================================================================
# R/collect/family_income.R
# Builds: finc_tot_nd_*  (total family income, nominal dollars). The PSID
# top-code grows across eras; 9999999 is the standardized top-code.
# =====================================================================

psid_abridged <- collect_tv(psid_abridged, "finc_tot_nd", function(x, y, df) {
  out <- rep(-1, length(x))
  if (y >= 1968 && y <= 1969) {
    out <- rc(out, inrange(x, 1, 99998), x); out <- rc(out, inlist(x, 0), 0)
  } else if (y >= 1970 && y <= 1979) {
    out <- rc(out, inrange(x, 2, 99998), x); out <- rc(out, inlist(x, 1), 1)
    out <- rc(out, inlist(x, 99999), 9999999)
  } else if (y == 1980) {
    out <- rc(out, inrange(x, 2, 999998), x); out <- rc(out, inlist(x, 1), 1)
    out <- rc(out, inlist(x, 999999), 9999999)
  } else if (y >= 1981 && y <= 1993) {
    out <- rc(out, inrange(x, 2, 9999998), x); out <- rc(out, inlist(x, 1), 1)
    out <- rc(out, inlist(x, 9999999), 9999999)
  } else if (y >= 1994 && y <= 1995) {
    out <- rc(out, inrange(x, -999998, 9999998), x); out <- rc(out, inlist(x, -999999), -999999)
    out <- rc(out, inlist(x, 9999999) & inlist(df[[paste0("sample")]], 3), NA)
  } else if (y >= 1996 && y <= 2009) {
    out <- rc(out, inrange(x, -999998, 9999998), x); out <- rc(out, inlist(x, -999999), -999999)
    out <- rc(out, inlist(x, 9999999), 9999999)
  } else {  # 2011+
    out <- rc(out, inrange(x, -999996, 9999996), x); out <- rc(out, inlist(x, -999997), -999997)
    out <- rc(out, inlist(x, 9999997), 9999999)
  }
  rc(out, is.na(x), NA)
})
