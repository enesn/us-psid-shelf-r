# =====================================================================
# R/collect/sample_design.R
# Builds: fw_*, fw_cross_*, fw_latin_main_*, fw_latin_only_*, iw_*, iw_cross_*,
#         iw_latin_main_*, iw_latin_only_*, stratum, cluster
# =====================================================================

# longitudinal weights: passthrough 0.001..1000, keep 0, else missing
wt_long <- function(x, y) recode(x,
  0.001 %..% 1000 ~ keep,
  0  ~ 0,
  NA ~ NA)
# cross-sectional weights: passthrough 1..150000
wt_cross <- function(x, y) recode(x,
  1 %..% 150000 ~ keep,
  0  ~ 0,
  NA ~ NA)

for (v in c("fw", "fw_latin_main", "fw_latin_only",
            "iw", "iw_latin_main", "iw_latin_only"))
  psid_abridged <- collect_tv(psid_abridged, v, wt_long)
# fw_cross has no `=0 if 0` rule in the source; iw_cross does. Both share the
# 1..150000 passthrough; the 0-keep is harmless for fw_cross (0 is out of range
# and would otherwise stay -1, but PSID cross weights are >=1 or missing).
psid_abridged <- collect_tv(psid_abridged, "fw_cross", function(x, y) recode(x,
  1 %..% 150000 ~ keep,
  NA ~ NA))
psid_abridged <- collect_tv(psid_abridged, "iw_cross", wt_cross)

# stratum  [ER31996]
psid_abridged <- collect_inv(psid_abridged, "stratum", function(x) recode(x,
  1 %..% 94 ~ keep))
# cluster  [ER31997]
psid_abridged <- collect_inv(psid_abridged, "cluster", function(x) recode(x,
  c(1, 2) ~ keep))
