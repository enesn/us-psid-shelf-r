# =====================================================================
# R/collect/depression.R
# Builds: dep_chng_{less,more}_resp_*, dep_chng_resp_*, dep_degr_resp_*,
#         dep_q{1..6}_freq_resp_*, dep_score_tot_resp_*  (K6 depression battery)
# =====================================================================

psid_abridged <- collect_tv(psid_abridged, "dep_chng_less_resp", function(x, y) recode(x,
  2 ~ 1, 3 ~ 2, 1 ~ 3,
  c(8, 9, 0, NA) ~ NA))

dep_chng_more <- function(x, y) recode(x,   # dep_chng_more_resp & dep_chng_resp share this
  3 ~ 1, 2 ~ 2, 1 ~ 3,
  c(8, 9, 0, NA) ~ NA)
psid_abridged <- collect_tv(psid_abridged, "dep_chng_more_resp", dep_chng_more)
psid_abridged <- collect_tv(psid_abridged, "dep_chng_resp",      dep_chng_more)

psid_abridged <- collect_tv(psid_abridged, "dep_degr_resp", function(x, y) recode(x,
  5 ~ 0, 3 ~ 1, 2 ~ 2, 1 ~ 3,
  c(8, 9, 0, NA) ~ NA))

# K6 frequency items (5->0 .. 1->4)
dep_freq <- function(x, y) recode(x,
  5 ~ 0, 4 ~ 1, 3 ~ 2, 2 ~ 3, 1 ~ 4,
  c(8, 9, 0, NA) ~ NA)
for (q in 1:6)
  psid_abridged <- collect_tv(psid_abridged, sprintf("dep_q%d_freq_resp", q), dep_freq)

# total K6 score (0-24)
psid_abridged <- collect_tv(psid_abridged, "dep_score_tot_resp", function(x, y) recode(x,
  0 %..% 24 ~ keep,
  c(99, NA) ~ NA))
