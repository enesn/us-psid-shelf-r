# =====================================================================
# R/collect/primary_home.R
# Builds: home_stat_*, home_own_val_nd_*, home_own_mor_any_{1m,2m,3m}_*,
#         home_own_mor_val_{1m,2m}_nd_*
# =====================================================================

# home_stat — own / rent / neither
psid_abridged <- collect_tv(psid_abridged, "home_stat", function(x, y) {
  out <- recode(x, 1 ~ 1, 5 ~ 2, 8 ~ 3, NA ~ NA)
  if (y == 1994) out <- rc(out, x %in% 0, NA)
  if (y == 1994 || y == 2007) out <- rc(out, x %in% 9, NA)
  out
})

# home value (nominal dollars). The valid range grows by era; 0 and the era's
# "max valid" code pass through, DK/refused (top sentinels) -> missing.
psid_abridged <- collect_tv(psid_abridged, "home_own_val_nd", function(x, y) {
  out <-
    if (y <= 1974)      recode(x, 1 %..% 99998 ~ keep,    c(0, 99999) ~ keep)
    else if (y <= 1993) recode(x, 1 %..% 999998 ~ keep,   c(0, 999999) ~ keep)
    else if (y <= 2003) recode(x, 1 %..% 9999996 ~ keep,  c(0, 9999997) ~ keep, c(9999998, 9999999) ~ NA)
    else if (y <= 2017) recode(x, 1 %..% 9999997 ~ keep,  0 ~ keep,             c(9999998, 9999999) ~ NA)
    else if (y == 2019) recode(x, 1 %..% 9999996 ~ keep,  c(0, 9999997) ~ keep, c(9999998, 9999999) ~ NA)
    else                recode(x, 1 %..% 99999996 ~ keep, c(0, 99999997) ~ keep, c(99999998, 99999999) ~ NA)
  rc(out, is.na(x), NA)
})

# mortgage value (1m / 2m) — identical era blocks
mor_val <- function(x, y) {
  out <-
    if (y <= 1972)      recode(x, 1 %..% 99998 ~ keep,   c(0, 99999) ~ keep)
    else if (y <= 1993) recode(x, 1 %..% 999998 ~ keep,  c(0, 999999) ~ keep)
    else                recode(x, 1 %..% 9999996 ~ keep, c(0, 9999997) ~ keep, c(9999998, 9999999) ~ NA)
  rc(out, is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_val_1m_nd", mor_val)
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_val_2m_nd", mor_val)

# any mortgage? (1m has the 1968 "1 or 2" coding; 2m starts 1969; 3m single era)
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_any_1m", function(x, y) {
  if (y == 1968)      recode(x, 5 ~ 0, c(1, 2) ~ 1, c(9, 0, NA) ~ NA)
  else if (y <= 1993) recode(x, 5 ~ 0, 1 ~ 1, c(9, 0, NA) ~ NA)
  else                recode(x, 5 ~ 0, 1 ~ 1, c(8, 9, 0, NA) ~ NA)
})
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_any_2m", function(x, y) {
  if (y <= 1993) recode(x, 5 ~ 0, 1 ~ 1, c(9, 0, NA) ~ NA)
  else           recode(x, 5 ~ 0, 1 ~ 1, c(8, 9, 0, NA) ~ NA)
})
psid_abridged <- collect_tv(psid_abridged, "home_own_mor_any_3m", function(x, y) recode(x,
  5 ~ 0, 1 ~ 1, c(8, 9, 0, NA) ~ NA))
