# =====================================================================
# R/collect/family_income.R
# Builds: finc_tot_nd_*  (total family income, nominal dollars). The PSID
# top-code grows across eras; 9999999 is the standardized top-code.
# =====================================================================

psid_abridged <- collect_tv(psid_abridged, "finc_tot_nd", function(x, y, df) {
  out <-
    if (y >= 1968 && y <= 1969)
      recode(x, 1 %..% 99998 ~ keep, 0 ~ keep)
    else if (y >= 1970 && y <= 1979)
      recode(x, 2 %..% 99998 ~ keep, 1 ~ keep, 99999 ~ 9999999)
    else if (y == 1980)
      recode(x, 2 %..% 999998 ~ keep, 1 ~ keep, 999999 ~ 9999999)
    else if (y >= 1981 && y <= 1993)
      recode(x, 2 %..% 9999998 ~ keep, 1 ~ keep, 9999999 ~ 9999999)
    else if (y >= 1994 && y <= 1995) {
      # sample-conditional wild code: the top-code is missing for the immigrant
      # refresher (sample 3), so it can't be a pure recode rule.
      o <- recode(x, -999998 %..% 9999998 ~ keep, -999999 ~ keep)
      rc(o, x %in% 9999999 & df[["sample"]] %in% 3, NA)
    }
    else if (y >= 1996 && y <= 2009)
      recode(x, -999998 %..% 9999998 ~ keep, -999999 ~ keep, 9999999 ~ 9999999)
    else  # 2011+
      recode(x, -999996 %..% 9999996 ~ keep, -999997 ~ keep, 9999997 ~ 9999999)
  rc(out, is.na(x), NA)
})
