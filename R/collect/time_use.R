# =====================================================================
# R/collect/time_use.R
# Builds: time_{acar,ccar,educ,hous,leis,pers,shop,volu,work}_{rp,sp}_*,
#         if_time_hous_{rp,sp}_*
# =====================================================================

# care hours: passthrough 1..168, 0 -> 0, 998/999/. -> NA
time_care <- function(x, y) recode(x,
  1 %..% 168 ~ keep, 0 ~ keep, c(998, 999, NA) ~ NA)
# generic hours with 112 top-code: passthrough 1..111, 0 -> 0, 112 -> 112
time_hrs <- function(x, y) recode(x,
  1 %..% 111 ~ keep, 0 ~ keep, 112 ~ keep, c(998, 999, NA) ~ NA)
# housework flag
if_hous <- function(x, y) recode(x, c(0, 1) ~ keep, NA ~ NA)
# housework hours (coding scheme changed across eras)
time_hous_fn <- function(x, y) {
  if (y == 1976)
    recode(x, 0 ~ keep, 1 ~ keep, 2 %..% 97 ~ keep, 98 ~ keep, c(99, NA) ~ NA)
  else if (y >= 1977 && y <= 1981)
    recode(x, 0 ~ keep, 1 %..% 97 ~ keep, 98 ~ keep, c(99, NA) ~ NA)
  else if (y >= 1983 && y <= 1993)
    recode(x, 0 ~ keep, 1 ~ keep, 2 %..% 97 ~ keep, 98 ~ keep, c(99, NA) ~ NA)
  else if (y == 1994)
    recode(x, 0 ~ keep, 1 ~ keep, 2 %..% 111 ~ keep, 112 ~ keep, c(998, 999, NA) ~ NA)
  else if (y >= 1995 && y <= 2009)
    recode(x, 0 ~ keep, 0.1 %..% 111 ~ keep, 112 ~ keep, c(998, 999, NA) ~ NA)
  else  # 2011+
    recode(x, 0 ~ keep, 1 %..% 111 ~ keep, 112 ~ keep, c(998, 999, NA) ~ NA)
}

for (who in c("rp", "sp")) {
  for (v in c("time_acar", "time_ccar"))
    psid_abridged <- collect_tv(psid_abridged, paste0(v, "_", who), time_care)
  for (v in c("time_educ", "time_leis", "time_pers", "time_shop",
              "time_volu", "time_work"))
    psid_abridged <- collect_tv(psid_abridged, paste0(v, "_", who), time_hrs)
  psid_abridged <- collect_tv(psid_abridged, paste0("if_time_hous_", who), if_hous)
  psid_abridged <- collect_tv(psid_abridged, paste0("time_hous_", who), time_hous_fn)
}
