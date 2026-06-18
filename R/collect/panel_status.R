# =====================================================================
# R/collect/panel_status.R
# Builds: seqnum_*, response_ext_*, panel_drop_rein_*, panel_rein_elig_ind
# =====================================================================

# (2.1) seqnum — sequence number in FU interview
psid_abridged <- collect_tv(psid_abridged, "seqnum", function(x, y) recode(x,
  0 %..% 89 ~ keep))

# (2.2) response_ext — response / reason for nonresponse, extended
psid_abridged <- collect_tv(psid_abridged, "response_ext", function(x, y) recode(x,
  0  ~ 0,
  11 ~ 101, 12 ~ 102, 13 ~ 103, 14 ~ 104, 19 ~ 105,
  91 ~ 200, 92 ~ 201, 45 ~ 210, 31 ~ 211,
  41 ~ 300,
  1  ~ 400, 10 ~ 401, 15 ~ 410, 99 ~ 420,
  2  ~ 500, 5  ~ 501, 3  ~ 502, 4  ~ 520, 57 ~ 521, 52 ~ 530,
  51 ~ 540, 53 ~ 541, 54 ~ 542, 59 ~ 543, 58 ~ 550, 94 ~ 551, 56 ~ 552,
  80 ~ 600,
  60 ~ 700, 21 ~ 711, 22 ~ 712, 23 ~ 713, 24 ~ 714, 29 ~ 715,
  93 ~ 720, 32 ~ 721, 42 ~ 730, 25 ~ 740, 61 ~ 750, 81 ~ 751,
  98 ~ 800,
  97 ~ 900))
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
psid_abridged <- collect_tv(psid_abridged, "panel_drop_rein", function(x, y) recode(x,
  5 ~ 0, 1 ~ 1, NA ~ NA))

# (2.4) panel_rein_elig_ind — Ind was 1997 CDS-eligible child?  [ER33418]
psid_abridged <- collect_inv(psid_abridged, "panel_rein_elig_ind", function(x) recode(x,
  c(0, 5) ~ 0, 1 ~ 1, NA ~ NA))
