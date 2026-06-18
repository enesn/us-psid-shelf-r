# =====================================================================
# R/collect/panel_status.R
# Builds: seqnum_*, response_ext_*, panel_drop_rein_*, panel_rein_elig_ind
# =====================================================================

# (2.1) seqnum — sequence number in FU interview
psid_abridged <- collect_tv(psid_abridged, "seqnum", function(x, y) {
  out <- rep(-1, length(x)); rc(out, inrange(x, 0, 89), x)
})

# (2.2) response_ext — response / reason for nonresponse, extended
psid_abridged <- collect_tv(psid_abridged, "response_ext", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 0),  0)
  out <- rc(out, inlist(x, 11), 101); out <- rc(out, inlist(x, 12), 102)
  out <- rc(out, inlist(x, 13), 103); out <- rc(out, inlist(x, 14), 104)
  out <- rc(out, inlist(x, 19), 105)
  out <- rc(out, inlist(x, 91), 200); out <- rc(out, inlist(x, 92), 201)
  out <- rc(out, inlist(x, 45), 210); out <- rc(out, inlist(x, 31), 211)
  out <- rc(out, inlist(x, 41), 300)
  out <- rc(out, inlist(x, 1),  400); out <- rc(out, inlist(x, 10), 401)
  out <- rc(out, inlist(x, 15), 410); out <- rc(out, inlist(x, 99), 420)
  out <- rc(out, inlist(x, 2),  500); out <- rc(out, inlist(x, 5),  501)
  out <- rc(out, inlist(x, 3),  502); out <- rc(out, inlist(x, 4),  520)
  out <- rc(out, inlist(x, 57), 521); out <- rc(out, inlist(x, 52), 530)
  out <- rc(out, inlist(x, 51), 540); out <- rc(out, inlist(x, 53), 541)
  out <- rc(out, inlist(x, 54), 542); out <- rc(out, inlist(x, 59), 543)
  out <- rc(out, inlist(x, 58), 550); out <- rc(out, inlist(x, 94), 551)
  out <- rc(out, inlist(x, 56), 552)
  out <- rc(out, inlist(x, 80), 600)
  out <- rc(out, inlist(x, 60), 700); out <- rc(out, inlist(x, 21), 711)
  out <- rc(out, inlist(x, 22), 712); out <- rc(out, inlist(x, 23), 713)
  out <- rc(out, inlist(x, 24), 714); out <- rc(out, inlist(x, 29), 715)
  out <- rc(out, inlist(x, 93), 720); out <- rc(out, inlist(x, 32), 721)
  out <- rc(out, inlist(x, 42), 730); out <- rc(out, inlist(x, 25), 740)
  out <- rc(out, inlist(x, 61), 750); out <- rc(out, inlist(x, 81), 751)
  out <- rc(out, inlist(x, 98), 800)
  out <- rc(out, inlist(x, 97), 900)
  out
})
# Hand-recode the two erroneous "active response" cases (reference seqnum and
# other-year response_ext columns; applied after all year columns exist).
with(psid_abridged, {
  i1 <- id == 1290003 & response_ext_1969 %in% 0 & seqnum_1969 %in% 52 &
        response_ext_1970 %in% 101
  i2 <- id == 2411175 & response_ext_2009 %in% 0 & seqnum_2009 %in% 71 &
        response_ext_2011 %in% 800
  psid_abridged$response_ext_1969[which(i1)] <<- 101
  psid_abridged$response_ext_2009[which(i2)] <<- 800
})

# (2.3) panel_drop_rein — FU SEO sample-drop reinstatement flag (1997–present)
psid_abridged <- collect_tv(psid_abridged, "panel_drop_rein", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0)
  out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, is.na(x),     NA)
  out
})

# (2.4) panel_rein_elig_ind — Ind was 1997 CDS-eligible child?  [ER33418]
psid_abridged <- collect_inv(psid_abridged, "panel_rein_elig_ind", function(x) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 0, 5), 0)
  out <- rc(out, inlist(x, 1),    1)
  out <- rc(out, is.na(x),        NA)
  out
})
