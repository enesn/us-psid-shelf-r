# =====================================================================
# R/collect/earnings.R
# Builds: earn_{busi,farm,uni,wage}_nd_{rp,sp}_*  (nominal-dollar earnings).
# PSID dollar top-codes grow across eras; mapped to standard 9999999 (or, in the
# 1994-95 / 1997 "wild-code" years, to missing for the affected sample).
# =====================================================================

# (blk() — the dollar-block helper — lives in R/programs.R)

# --- business & farm earnings (rp have the early categorical years) ---
psid_abridged <- collect_tv(psid_abridged, "earn_busi_nd_rp", function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 1975)        { out <- blk(out, x, 0, 8);                 out <- rc(out, inlist(x, 9), NA) }
  else if (y <= 1992)   out <- blk(out, x, 1, 99998,   pass = 0, tc = 99999)
  else if (y == 1993)   out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else if (y <= 1995)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999, tcout = NA)
  else if (y == 1996)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999)
  else if (y == 1997)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999, tcout = NA)
  else if (y <= 2003)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999)
  else if (y <= 2009)   out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else                  out <- blk(out, x, 1, 9999997, pass = 0)
  rc(out, is.na(x), NA)
})

psid_abridged <- collect_tv(psid_abridged, "earn_busi_nd_sp", function(x, y) {
  out <- rep(-1, length(x))
  if (y == 1993)        out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else if (y <= 1995)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999, tcout = NA)
  else if (y == 1996)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999)
  else if (y == 1997)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999, tcout = NA)
  else if (y <= 2003)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999)
  else if (y <= 2009)   out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else                  out <- blk(out, x, 1, 9999997, pass = 0)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "earn_farm_nd_rp", function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 1975)        { out <- blk(out, x, 0, 8);                 out <- rc(out, inlist(x, 9), NA) }
  else if (y <= 1992)   out <- blk(out, x, 1, 99998,   pass = 0, tc = 99999)
  else                  out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)   # 1993
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "earn_farm_nd_sp", function(x, y) {
  out <- rep(-1, length(x))
  out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)   # 1993 only
  rc(out, is.na(x), NA)
})

# --- unified labor (non-wage) earnings ---
psid_abridged <- collect_tv(psid_abridged, "earn_uni_nd_rp", function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 1974)        out <- blk(out, x, 1, 99998,   pass = 0,      tc = 99999)
  else if (y <= 1977)   out <- blk(out, x, 2, 99998,   pass = c(0, 1), tc = 99999)
  else if (y <= 1982)   out <- blk(out, x, 1, 99998,   pass = 0,      tc = 99999)
  else if (y <= 1992)   out <- blk(out, x, 1, 999998,  pass = 0,      tc = 999999)
  else                  out <- blk(out, x, 1, 9999998, pass = 0,      tc = 9999999)  # 1993
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "earn_uni_nd_sp", function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 1975)        out <- blk(out, x, 1, 99998,   pass = 0,      tc = 99999)
  else if (y <= 1982)   out <- blk(out, x, 2, 99998,   pass = c(0, 1), tc = 99999)
  else if (y == 1983)   out <- blk(out, x, 2, 999998,  pass = c(0, 1), tc = 99999)
  else if (y <= 1992)   out <- blk(out, x, 2, 999998,  pass = c(0, 1), tc = 999999)
  else                  out <- blk(out, x, 1, 9999998, pass = 0,      tc = 9999999)  # 1993
  rc(out, is.na(x), NA)
})

# --- wage earnings (sample-conditional wild codes in 1994-95 / 1997) ---
psid_abridged <- collect_tv(psid_abridged, "earn_wage_nd_rp", function(x, y, df) {
  out <- rep(-1, length(x))
  if (y <= 1969)        { out <- blk(out, x, 0, 8);                 out <- rc(out, inlist(x, 9), NA) }
  else if (y <= 1982)   out <- blk(out, x, 1, 99998,   pass = 0, tc = 99999)
  else if (y <= 1992)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999)
  else if (y == 1993)   out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else if (y <= 1995) { out <- blk(out, x, 1, 9999998, pass = 0)
                        out <- rc(out, inlist(x, 9999999) & inlist(df$sample, 3), NA) }
  else if (y == 1996)   out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else if (y == 1997) { out <- blk(out, x, 1, 9999998, pass = 0)
                        out <- rc(out, inlist(x, 9999999) & inlist(df$sample, 4), NA) }
  else if (y <= 2009)   out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else                  out <- blk(out, x, 1, 9999997, pass = 0)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "earn_wage_nd_sp", function(x, y, df) {
  out <- rep(-1, length(x))
  if (y == 1993)        out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else if (y <= 1995) { out <- blk(out, x, 1, 9999998, pass = 0)
                        out <- rc(out, inlist(x, 9999999) & inlist(df$sample, 3), NA) }
  else if (y == 1996)   out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else if (y == 1997)   out <- blk(out, x, 1, 999998,  pass = 0, tc = 999999)
  else if (y <= 2009)   out <- blk(out, x, 1, 9999998, pass = 0, tc = 9999999)
  else                  out <- blk(out, x, 1, 9999997, pass = 0)
  rc(out, is.na(x), NA)
})
