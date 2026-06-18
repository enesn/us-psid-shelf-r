# =====================================================================
# R/collect/earnings.R
# Builds: earn_{busi,farm,uni,wage}_nd_{rp,sp}_*  (nominal-dollar earnings).
# PSID dollar top-codes grow across eras; mapped to standard 9999999 (or, in the
# 1994-95 / 1997 "wild-code" years, to missing for the affected sample).
# Each era keeps its valid range, passes 0 (and sometimes 1) through, and maps
# the era top-code to the standardized 9999999 (or NA in the wild-code years).
# =====================================================================

# --- business & farm earnings (rp have the early categorical years) ---
psid_abridged <- collect_tv(psid_abridged, "earn_busi_nd_rp", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1992) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else if (y == 1993) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y == 1997) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y <= 2003) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})

psid_abridged <- collect_tv(psid_abridged, "earn_busi_nd_sp", function(x, y) {
  out <-
    if (y == 1993)      recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y == 1997) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y <= 2003) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "earn_farm_nd_rp", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1992) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else                recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)  # 1993
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "earn_farm_nd_sp", function(x, y) {  # 1993 only
  rc(recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999), is.na(x), NA)
})

# --- unified labor (non-wage) earnings ---
psid_abridged <- collect_tv(psid_abridged, "earn_uni_nd_rp", function(x, y) {
  out <-
    if (y <= 1974)      recode(x, 1 %..% 99998 ~ keep,   0 ~ keep,        99999 ~ 9999999)
    else if (y <= 1977) recode(x, 2 %..% 99998 ~ keep,   c(0, 1) ~ keep,  99999 ~ 9999999)
    else if (y <= 1982) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep,        99999 ~ 9999999)
    else if (y <= 1992) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep,        999999 ~ 9999999)
    else                recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep,        9999999 ~ 9999999)  # 1993
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "earn_uni_nd_sp", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 1 %..% 99998 ~ keep,   0 ~ keep,       99999 ~ 9999999)
    else if (y <= 1982) recode(x, 2 %..% 99998 ~ keep,   c(0, 1) ~ keep, 99999 ~ 9999999)
    else if (y == 1983) recode(x, 2 %..% 999998 ~ keep,  c(0, 1) ~ keep, 99999 ~ 9999999)
    else if (y <= 1992) recode(x, 2 %..% 999998 ~ keep,  c(0, 1) ~ keep, 999999 ~ 9999999)
    else                recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep,       9999999 ~ 9999999)  # 1993
  rc(out, is.na(x), NA)
})

# --- wage earnings (sample-conditional wild codes in 1994-95 / 1997) ---
psid_abridged <- collect_tv(psid_abridged, "earn_wage_nd_rp", function(x, y, df) {
  out <-
    if (y <= 1969)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1982) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else if (y <= 1992) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y == 1993) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) { o <- recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep)
                          rc(o, x %in% 9999999 & df$sample %in% 3, NA) }
    else if (y == 1996) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y == 1997) { o <- recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep)
                          rc(o, x %in% 9999999 & df$sample %in% 4, NA) }
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "earn_wage_nd_sp", function(x, y, df) {
  out <-
    if (y == 1993)      recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) { o <- recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep)
                          rc(o, x %in% 9999999 & df$sample %in% 3, NA) }
    else if (y == 1996) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y == 1997) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
