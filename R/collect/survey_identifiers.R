# =====================================================================
# R/collect/survey_identifiers.R
# Builds: id, lineage, pnum, fuid_*, hhdid_*, respondent_ext_*, rindiv_*,
#         rel_ext_*, sample, sampstat_ext
# =====================================================================

# (1.1) id — already = ER30001*1000 + ER30002 from 01-ingest / 03-parameters.
psid_abridged$id <- set_label(psid_abridged$id, var_label("id"))

# (1.2) lineage — 1968 family-unit ID  [ER30001]
psid_abridged <- collect_inv(psid_abridged, "lineage", function(x) {
  out <- rep(-1, length(x)); rc(out, inrange(x, 1, 10000), x)
})

# (1.3) pnum — person number  [ER30002]
psid_abridged <- collect_inv(psid_abridged, "pnum", function(x) {
  out <- rep(-1, length(x)); rc(out, inrange(x, 1, 500), x)
})

# (1.4) fuid — family-unit ID, wave-specific
psid_abridged <- collect_tv(psid_abridged, "fuid", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 19999), x)
  out <- rc(out, inlist(x, 0), NA)
  out
})

# (1.5) hhdid — household dwelling ID, wave-specific
psid_abridged <- collect_tv(psid_abridged, "hhdid", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 19999), x)
  out <- rc(out, is.na(x), NA)
  out
})

# (1.6) respondent_ext — respondent who completed FU interview, extended
psid_abridged <- collect_tv(psid_abridged, "respondent_ext", function(x, y) {
  out <- rep(-1, length(x))
  if (y == 1968) {
    out <- rc(out, inlist(x, 1), 101); out <- rc(out, inlist(x, 2), 201)
    out <- rc(out, inlist(x, 8), 251); out <- rc(out, inlist(x, 3), 801)
    out <- rc(out, inlist(x, 9), 999); out <- rc(out, is.na(x), NA)
  } else if (y == 1969) {
    out <- rc(out, inlist(x, 1), 101); out <- rc(out, inlist(x, 2), 202)
    out <- rc(out, inlist(x, 3), 251); out <- rc(out, inlist(x, 7), 802)
    out <- rc(out, inlist(x, 9), 999); out <- rc(out, is.na(x), NA)
  } else if (y >= 1970 && y <= 1972) {
    out <- rc(out, inlist(x, 1), 101); out <- rc(out, inlist(x, 2), 201)
    out <- rc(out, inlist(x, 3), 251); out <- rc(out, inlist(x, 7), 803)
    out <- rc(out, inlist(x, 9), 999); out <- rc(out, is.na(x), NA)
  } else if (y >= 1973 && y <= 1982) {
    out <- rc(out, inlist(x, 1), 101); out <- rc(out, inlist(x, 2), 203)
    out <- rc(out, inlist(x, 7), 803); out <- rc(out, inlist(x, 9), 999)
    out <- rc(out, is.na(x), NA)
  } else if (y >= 1983 && y <= 1993) {
    out <- rc(out, inlist(x, 1), 101); out <- rc(out, inlist(x, 2), 204)
    out <- rc(out, inlist(x, 7), 804); out <- rc(out, inlist(x, 9), 999)
    out <- rc(out, is.na(x), NA)
  } else if (y >= 1994 && y <= 1997) {
    out <- rc(out, inlist(x, 1), 102); out <- rc(out, inlist(x, 2), 205)
    out <- rc(out, inlist(x, 3), 206); out <- rc(out, inlist(x, 4), 851)
    out <- rc(out, inlist(x, 7), 861); out <- rc(out, is.na(x), NA)
  } else if (y >= 1999 && y <= 2013) {
    out <- rc(out, inlist(x, 1), 102); out <- rc(out, inlist(x, 2), 205)
    out <- rc(out, inlist(x, 3), 206); out <- rc(out, inlist(x, 4), 851)
    out <- rc(out, inlist(x, 7), 861); out <- rc(out, inlist(x, 9), 999)
    out <- rc(out, is.na(x), NA)
  } else if (y == 2015) {
    out <- rc(out, inlist(x, 1), 102); out <- rc(out, inlist(x, 2), 205)
    out <- rc(out, inlist(x, 3), 207); out <- rc(out, inlist(x, 4), 851)
    out <- rc(out, inlist(x, 7), 861); out <- rc(out, is.na(x), NA)
  } else if (y >= 2017) {
    out <- rc(out, inlist(x, 1), 103); out <- rc(out, inlist(x, 3), 207)
    out <- rc(out, inlist(x, 2), 208); out <- rc(out, inlist(x, 4), 851)
    out <- rc(out, inlist(x, 7), 861); out <- rc(out, is.na(x), NA)
  }
  out
})

# (1.7) rindiv — Ind is current respondent?
psid_abridged <- collect_tv(psid_abridged, "rindiv", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0)
  out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, inlist(x, 9), NA)
  out <- rc(out, inlist(x, 0), NA)
  out
})

# (1.8) rel_ext — relationship to reference person, extended (1968–present)
psid_abridged <- collect_tv(psid_abridged, "rel_ext", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1, 10),  100)
  out <- rc(out, inlist(x, 2, 20),  200); out <- rc(out, inlist(x, 22), 201)
  out <- rc(out, inlist(x, 9, 90),  208); out <- rc(out, inlist(x, 92), 209)
  out <- rc(out, inlist(x, 3, 30),  300); out <- rc(out, inlist(x, 33), 301)
  out <- rc(out, inlist(x, 35),     302)
  out <- rc(out, inlist(x, 88),     401); out <- rc(out, inlist(x, 38), 402)
  out <- rc(out, inlist(x, 83),     410); out <- rc(out, inlist(x, 37), 411)
  out <- rc(out, inlist(x, 5, 50),  420); out <- rc(out, inlist(x, 57), 421)
  out <- rc(out, inlist(x, 58),     422)
  out <- rc(out, inlist(x, 6, 60),  430); out <- rc(out, inlist(x, 65), 431)
  out <- rc(out, inlist(x, 66),     440); out <- rc(out, inlist(x, 67), 441)
  out <- rc(out, inlist(x, 68),     442); out <- rc(out, inlist(x, 69), 443)
  out <- rc(out, inlist(x, 4, 40),  450); out <- rc(out, inlist(x, 47), 451)
  out <- rc(out, inlist(x, 48),     452); out <- rc(out, inlist(x, 70), 453)
  out <- rc(out, inlist(x, 71),     454); out <- rc(out, inlist(x, 74), 455)
  out <- rc(out, inlist(x, 75),     456); out <- rc(out, inlist(x, 72), 457)
  out <- rc(out, inlist(x, 73),     458)
  out <- rc(out, inlist(x, 7),      480); out <- rc(out, inlist(x, 95), 481)
  out <- rc(out, inlist(x, 96),     482); out <- rc(out, inlist(x, 97), 483)
  out <- rc(out, inlist(x, 8, 98),  490)
  out <- rc(out, inlist(x, 0),      999)
  # hand-code inconsistent 1968 values
  if (y == 1968) {
    out <- rc(out, inlist(x, 8), 208)
    out <- rc(out, inlist(x, 9), 490)
  }
  # 999 (N/A) -> missing
  out <- rc(out, inlist(out, 999), NA)
  out
})

# (1.9) sample — sample membership (by 1968 family lineage)  [ER30001]
psid_abridged <- collect_inv(psid_abridged, "sample", function(x) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x,    1, 2930), 1)
  out <- rc(out, inrange(x, 5001, 6872), 2)
  out <- rc(out, inrange(x, 7001, 9308), 3)
  out <- rc(out, inrange(x, 3001, 3511), 4)
  out <- rc(out, inrange(x, 4001, 4851), 5)
  out
})

# (1.10) sampstat_ext — sample person status, extended  [ER32006]
psid_abridged <- collect_inv(psid_abridged, "sampstat_ext", function(x) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 0), 1)
  out <- rc(out, inlist(x, 6), 11)
  out <- rc(out, inlist(x, 5), 12)
  out <- rc(out, inlist(x, 1), 100)
  out <- rc(out, inlist(x, 2), 200)
  out <- rc(out, inlist(x, 3), 300)
  out <- rc(out, inlist(x, 4), 400)
  out
})
