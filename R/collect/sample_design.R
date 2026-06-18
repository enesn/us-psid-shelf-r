# =====================================================================
# R/collect/sample_design.R
# Builds: fw_*, fw_cross_*, fw_latin_main_*, fw_latin_only_*, iw_*, iw_cross_*,
#         iw_latin_main_*, iw_latin_only_*, stratum, cluster
# =====================================================================

# longitudinal weights: passthrough 0.001..1000, keep 0, else missing
wt_long <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 0.001, 1000), x)
  out <- rc(out, inlist(x, 0), 0)
  out <- rc(out, is.na(x),     NA)
  out
}
# cross-sectional weights: passthrough 1..150000
wt_cross <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 150000), x)
  out <- rc(out, inlist(x, 0), 0)
  out <- rc(out, is.na(x),     NA)
  out
}

for (v in c("fw", "fw_latin_main", "fw_latin_only",
            "iw", "iw_latin_main", "iw_latin_only"))
  psid_abridged <- collect_tv(psid_abridged, v, wt_long)
# fw_cross has no `=0 if 0` rule in the source; iw_cross does. Both share the
# 1..150000 passthrough; the 0-keep is harmless for fw_cross (0 is out of range
# and would otherwise stay -1, but PSID cross weights are >=1 or missing).
psid_abridged <- collect_tv(psid_abridged, "fw_cross", function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 1, 150000), x)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "iw_cross", wt_cross)

# stratum  [ER31996]
psid_abridged <- collect_inv(psid_abridged, "stratum", function(x) {
  out <- rep(-1, length(x)); rc(out, inrange(x, 1, 94), x)
})
# cluster  [ER31997]
psid_abridged <- collect_inv(psid_abridged, "cluster", function(x) {
  out <- rep(-1, length(x)); rc(out, inlist(x, 1, 2), x)
})
