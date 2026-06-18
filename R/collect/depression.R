# =====================================================================
# R/collect/depression.R
# Builds: dep_chng_{less,more}_resp_*, dep_chng_resp_*, dep_degr_resp_*,
#         dep_q{1..6}_freq_resp_*, dep_score_tot_resp_*  (K6 depression battery)
# =====================================================================

psid_abridged <- collect_tv(psid_abridged, "dep_chng_less_resp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 2), 1); out <- rc(out, inlist(x, 3), 2)
  out <- rc(out, inlist(x, 1), 3)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})

dep_chng_more <- function(x, y) {   # dep_chng_more_resp & dep_chng_resp share this
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 3), 1); out <- rc(out, inlist(x, 2), 2)
  out <- rc(out, inlist(x, 1), 3)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "dep_chng_more_resp", dep_chng_more)
psid_abridged <- collect_tv(psid_abridged, "dep_chng_resp",      dep_chng_more)

psid_abridged <- collect_tv(psid_abridged, "dep_degr_resp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 3), 1)
  out <- rc(out, inlist(x, 2), 2); out <- rc(out, inlist(x, 1), 3)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})

# K6 frequency items (5->0 .. 1->4)
dep_freq <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 4), 1)
  out <- rc(out, inlist(x, 3), 2); out <- rc(out, inlist(x, 2), 3)
  out <- rc(out, inlist(x, 1), 4)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
}
for (q in 1:6)
  psid_abridged <- collect_tv(psid_abridged, sprintf("dep_q%d_freq_resp", q), dep_freq)

# total K6 score (0-24)
psid_abridged <- collect_tv(psid_abridged, "dep_score_tot_resp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 0, 24), x)
  rc(out, inlist(x, 99) | is.na(x), NA)
})
