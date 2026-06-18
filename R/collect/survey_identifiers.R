# =====================================================================
# R/collect/survey_identifiers.R
# Builds: id, lineage, pnum, fuid_*, hhdid_*, respondent_ext_*, rindiv_*,
#         rel_ext_*, sample, sampstat_ext
# =====================================================================

# (1.1) id — already = ER30001*1000 + ER30002 from 01-ingest / 03-parameters.
psid_abridged$id <- set_label(psid_abridged$id, var_label("id"))

# (1.2) lineage — 1968 family-unit ID  [ER30001]
psid_abridged <- collect_inv(psid_abridged, "lineage", function(x) recode(x,
  1 %..% 10000 ~ keep))

# (1.3) pnum — person number  [ER30002]
psid_abridged <- collect_inv(psid_abridged, "pnum", function(x) recode(x,
  1 %..% 500 ~ keep))

# (1.4) fuid — family-unit ID, wave-specific  (0 -> NA; . left unassigned, as in source)
psid_abridged <- collect_tv(psid_abridged, "fuid", function(x, y) recode(x,
  1 %..% 19999 ~ keep, 0 ~ NA))

# (1.5) hhdid — household dwelling ID, wave-specific
psid_abridged <- collect_tv(psid_abridged, "hhdid", function(x, y) recode(x,
  1 %..% 19999 ~ keep, NA ~ NA))

# (1.6) respondent_ext — respondent who completed FU interview, extended
psid_abridged <- collect_tv(psid_abridged, "respondent_ext", function(x, y) {
  if (y == 1968)                  recode(x, 1 ~ 101, 2 ~ 201, 8 ~ 251, 3 ~ 801, 9 ~ 999, NA ~ NA)
  else if (y == 1969)             recode(x, 1 ~ 101, 2 ~ 202, 3 ~ 251, 7 ~ 802, 9 ~ 999, NA ~ NA)
  else if (y >= 1970 && y <= 1972) recode(x, 1 ~ 101, 2 ~ 201, 3 ~ 251, 7 ~ 803, 9 ~ 999, NA ~ NA)
  else if (y >= 1973 && y <= 1982) recode(x, 1 ~ 101, 2 ~ 203, 7 ~ 803, 9 ~ 999, NA ~ NA)
  else if (y >= 1983 && y <= 1993) recode(x, 1 ~ 101, 2 ~ 204, 7 ~ 804, 9 ~ 999, NA ~ NA)
  else if (y >= 1994 && y <= 1997) recode(x, 1 ~ 102, 2 ~ 205, 3 ~ 206, 4 ~ 851, 7 ~ 861, NA ~ NA)
  else if (y >= 1999 && y <= 2013) recode(x, 1 ~ 102, 2 ~ 205, 3 ~ 206, 4 ~ 851, 7 ~ 861, 9 ~ 999, NA ~ NA)
  else if (y == 2015)             recode(x, 1 ~ 102, 2 ~ 205, 3 ~ 207, 4 ~ 851, 7 ~ 861, NA ~ NA)
  else if (y >= 2017)             recode(x, 1 ~ 103, 3 ~ 207, 2 ~ 208, 4 ~ 851, 7 ~ 861, NA ~ NA)
  else rep(-1, length(x))
})

# (1.7) rindiv — Ind is current respondent?  (9/0 -> NA; . left unassigned, as in source)
psid_abridged <- collect_tv(psid_abridged, "rindiv", function(x, y) recode(x,
  5 ~ 0, 1 ~ 1, c(9, 0) ~ NA))

# (1.8) rel_ext — relationship to reference person, extended (1968–present)
psid_abridged <- collect_tv(psid_abridged, "rel_ext", function(x, y) {
  out <- recode(x,
    c(1, 10) ~ 100,
    c(2, 20) ~ 200, 22 ~ 201,
    c(9, 90) ~ 208, 92 ~ 209,
    c(3, 30) ~ 300, 33 ~ 301, 35 ~ 302,
    88 ~ 401, 38 ~ 402,
    83 ~ 410, 37 ~ 411,
    c(5, 50) ~ 420, 57 ~ 421, 58 ~ 422,
    c(6, 60) ~ 430, 65 ~ 431,
    66 ~ 440, 67 ~ 441, 68 ~ 442, 69 ~ 443,
    c(4, 40) ~ 450, 47 ~ 451, 48 ~ 452, 70 ~ 453, 71 ~ 454, 74 ~ 455, 75 ~ 456, 72 ~ 457, 73 ~ 458,
    7 ~ 480, 95 ~ 481, 96 ~ 482, 97 ~ 483,
    c(8, 98) ~ 490,
    0 ~ 999)
  # hand-code inconsistent 1968 values
  if (y == 1968) { out <- rc(out, x %in% 8, 208); out <- rc(out, x %in% 9, 490) }
  # 999 (N/A) -> missing
  rc(out, out %in% 999, NA)
})

# (1.9) sample — sample membership (by 1968 family lineage)  [ER30001]
psid_abridged <- collect_inv(psid_abridged, "sample", function(x) recode(x,
  1 %..% 2930 ~ 1,
  5001 %..% 6872 ~ 2,
  7001 %..% 9308 ~ 3,
  3001 %..% 3511 ~ 4,
  4001 %..% 4851 ~ 5))

# (1.10) sampstat_ext — sample person status, extended  [ER32006]
psid_abridged <- collect_inv(psid_abridged, "sampstat_ext", function(x) recode(x,
  0 ~ 1, 6 ~ 11, 5 ~ 12, 1 ~ 100, 2 ~ 200, 3 ~ 300, 4 ~ 400))
