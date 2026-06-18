# =====================================================================
# R/collect/occupations.R
# Builds: occ_1970c_{rp,sp}_*, occ_2000c_{1m..4m}_{rp,sp}_*,
#         occ_2010c_{1m..4m}_{rp,sp}_*   (census occupation codes, passthrough)
# =====================================================================

occ_1970 <- function(x, y) recode(x,          # 1970 census codes
  1 %..% 809 ~ keep, 811 %..% 984 ~ keep,
  c(810, 0, 997, 998, 999, NA) ~ NA)
occ_2000 <- function(x, y) recode(x,          # 2000 census codes
  1 %..% 614 ~ keep, 616 %..% 983 ~ keep,
  c(615, 0, 999, NA) ~ NA)
occ_2010 <- function(x, y) recode(x,          # 2010 census codes
  10 %..% 9830 ~ keep,
  c(0, 9999, NA) ~ NA)

psid_abridged <- collect_tv(psid_abridged, "occ_1970c_rp", occ_1970)
psid_abridged <- collect_tv(psid_abridged, "occ_1970c_sp", occ_1970)
for (slot in c("1m","2m","3m","4m")) for (who in c("rp","sp")) {
  psid_abridged <- collect_tv(psid_abridged, sprintf("occ_2000c_%s_%s", slot, who), occ_2000)
  psid_abridged <- collect_tv(psid_abridged, sprintf("occ_2010c_%s_%s", slot, who), occ_2010)
}
