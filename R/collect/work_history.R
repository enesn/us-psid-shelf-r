# =====================================================================
# R/collect/work_history.R
# Builds: total_years_worked_{rp,sp}_*      (# years worked full time since age 18)
#         last_year_working_hour_{rp,sp}_*  (total annual work hours, prior year)
#
# Both are fully-assigned constructed PSID variables (all missing data imputed),
# so the only recodes needed are (a) passing the actual counts through and
# (b) mapping the genuine DK/NA/refused sentinels to missing.
#
# total_years_worked  (V3621-era "YRS WKD FULL-TIME", L58/L71):
#   0        Inap.: never worked / never worked full time  (a valid 0)
#   1        one year or less
#   2..97    actual number of years
#   95/96    (2013+) "less than one year, age <=18" / "all years since 18"
#   97/98    "N or more" top-code (98 in the V-coded 1968-1993 frame; 97 in 1994)
#   98       DK       (1994+ frame only)            -> NA
#   99       NA; DK; refused (every wave)           -> NA
#   Frame boundary: the V-coded waves (<=1993) top-code at 98 and use 99 as the
#   only missing code; the ER waves (>=1994) top-code at 97 and add 98 = DK. The
#   modern 95/96 special codes fall inside the kept 0..97 range, so a single
#   two-branch recode covers all waves.
#
# last_year_working_hour ("ANN WRK HRS" / "TOTAL HOURS OF WORK", head/wife):
#   0          none; did not work                    (a valid 0)
#   1..9998    actual number of hours
#   9999       "9,999 hours or more" top-code (never observed) ................ keep
#              EXCEPT 1994-1995, where 9999 = "Latino sample family" (amounts
#              not calculated for that sample) ....................... -> NA
# =====================================================================

# --- total years worked full time (rp 1974+, sp 1974+) ---
mk_years <- function(x, y) {
  out <- if (y <= 1993) recode(x, 0 %..% 98 ~ keep, 99 ~ NA)          # 98 = "98 or more"; 99 = NA/DK
         else           recode(x, 0 %..% 97 ~ keep, c(98, 99) ~ NA)   # 97 = "97+" / 95,96 specials; 98 = DK, 99 = NA
  rc(out, is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "total_years_worked_rp", mk_years)
psid_abridged <- collect_tv(psid_abridged, "total_years_worked_sp", mk_years)

# --- total annual work hours, prior year (rp 1968+, sp 1968+) ---
mk_hours <- function(x, y) {
  out <- if (y %in% c(1994, 1995)) recode(x, 0 %..% 9998 ~ keep, 9999 ~ NA)  # 9999 = Latino sample, not calculated
         else                      recode(x, 0 %..% 9999 ~ keep)             # 9999 = "9999+ hours" (unobserved top-code)
  rc(out, is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "last_year_working_hour_rp", mk_hours)
psid_abridged <- collect_tv(psid_abridged, "last_year_working_hour_sp", mk_hours)

rm(mk_years, mk_hours)
