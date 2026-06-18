# =====================================================================
# R/collect/general_wellbeing.R
# Builds: nhlth_birth_wght, body_hght_*, body_wght_*, ghlth_*, hosp_*, life_stat_resp
# =====================================================================

# nhlth_birth_wght — birth weight (oz)  [single input]
psid_abridged <- collect_inv(psid_abridged, "nhlth_birth_wght", function(x) recode(x,
  16 %..% 224 ~ keep,
  991 ~ 980, 995 ~ 981,
  c(998, 999) ~ NA))

# height: feet of parent (rp/sp)
body_ft <- function(x, y) recode(x, 2 %..% 7 ~ keep, c(8, 9, 0, NA) ~ NA)
psid_abridged <- collect_tv(psid_abridged, "body_hght_par_ft_rp", body_ft)
psid_abridged <- collect_tv(psid_abridged, "body_hght_par_ft_sp", body_ft)

# height: inches of parent — 0 inches is valid only if the feet sibling is a
# real height (2..7); otherwise 0 is missing. (rp 1999-2009 keeps 0 outright.)
# Cross-column + era logic, so kept on the explicit rc() chain.
psid_abridged <- collect_tv(psid_abridged, "body_hght_par_in_rp", function(x, y, df) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 1, 11), x)
  ft <- df[[paste0("body_hght_par_ft_rp_", y)]]
  if (y >= 1999 && y <= 2009) {
    out <- rc(out, inlist(x, 0), 0)
  } else {
    out <- rc(out, inlist(x, 0) &  inrange(ft, 2, 7), 0)
    out <- rc(out, inlist(x, 0) & !inrange(ft, 2, 7), NA)
  }
  rc(out, inlist(x, 98, 99) | is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "body_hght_par_in_sp", function(x, y, df) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 1, 11), x)
  ft <- df[[paste0("body_hght_par_ft_sp_", y)]]
  out <- rc(out, inlist(x, 0) &  inrange(ft, 2, 7), 0)
  out <- rc(out, inlist(x, 0) & !inrange(ft, 2, 7), NA)
  rc(out, inlist(x, 98, 99) | is.na(x), NA)
})

# height: unified inches
psid_abridged <- collect_tv(psid_abridged, "body_hght_uni_in_rp", function(x, y) recode(x,
  48 %..% 90 ~ keep,
  c(99, NA) ~ NA))
psid_abridged <- collect_tv(psid_abridged, "body_hght_uni_in_sp", function(x, y) recode(x,
  48 %..% 85 ~ keep,
  c(99, 0, NA) ~ NA))
# height: unified metres
psid_abridged <- collect_tv(psid_abridged, "body_hght_uni_me_rp", function(x, y) recode(x,
  0.61 %..% 2.09 ~ keep,
  0.60 ~ 0, 2.10 ~ 997,
  c(8, 9, 0, NA) ~ NA))
psid_abridged <- collect_tv(psid_abridged, "body_hght_uni_me_sp", function(x, y) recode(x,
  0.61 %..% 2.09 ~ keep,
  0.5999 %..% 0.6001 ~ 0, 2.10 ~ 997,
  c(8, 9, 0, NA) ~ NA))
# weight: unified kg
body_kg <- function(x, y) recode(x,
  36.1 %..% 179.9 ~ keep,
  36.0 ~ 0, 180.0 ~ 997,
  c(998, 999, 0, NA) ~ NA)
psid_abridged <- collect_tv(psid_abridged, "body_wght_uni_kg_rp", body_kg)
psid_abridged <- collect_tv(psid_abridged, "body_wght_uni_kg_sp", body_kg)

# weight: unified lb (coding changed across eras; kept on the explicit chain)
psid_abridged <- collect_tv(psid_abridged, "body_wght_uni_lb_rp", function(x, y) {
  out <- rep(-1, length(x))
  if (y == 1986) { out <- rc(out, inrange(x, 80, 450), x); out <- rc(out, inlist(x, 999) | is.na(x), NA) }
  else if (y >= 1999 && y <= 2003) { out <- rc(out, inrange(x, 50, 500), x); out <- rc(out, inlist(x, 998, 999) | is.na(x), NA) }
  else if (y >= 2005 && y <= 2009) { out <- rc(out, inrange(x, 51, 399), x); out <- rc(out, inlist(x, 50), 0); out <- rc(out, inlist(x, 400), 997); out <- rc(out, inlist(x, 998, 999) | is.na(x), NA) }
  else { out <- rc(out, inrange(x, 51, 399), x); out <- rc(out, inlist(x, 50), 0); out <- rc(out, inlist(x, 400), 997); out <- rc(out, inlist(x, 998, 999, 0) | is.na(x), NA) }
  if (y == 1999) out <- rc(out, inlist(x, 0, 6), NA)
  if (y == 2007) out <- rc(out, inlist(x, 0), NA)
  out
})
psid_abridged <- collect_tv(psid_abridged, "body_wght_uni_lb_sp", function(x, y) {
  out <- rep(-1, length(x))
  if (y == 1986) { out <- rc(out, inrange(x, 75, 400), x); out <- rc(out, inlist(x, 999, 0) | is.na(x), NA) }
  else if (y >= 1999 && y <= 2003) { out <- rc(out, inrange(x, 50, 500), x); out <- rc(out, inlist(x, 998, 999, 0) | is.na(x), NA) }
  else if (y >= 2005 && y <= 2009) { out <- rc(out, inrange(x, 51, 399), x); out <- rc(out, inlist(x, 50), 0); out <- rc(out, inlist(x, 400), 997); out <- rc(out, inlist(x, 998, 999, 0) | is.na(x), NA) }
  else { out <- rc(out, inrange(x, 51, 399), x); out <- rc(out, inlist(x, 50), 0); out <- rc(out, inlist(x, 400), 997); out <- rc(out, inlist(x, 998, 999, 0) | is.na(x), NA) }
  if (y == 1999) out <- rc(out, inlist(x, 18), NA)
  out
})

# general health change
psid_abridged <- collect_tv(psid_abridged, "ghlth_chng_rp", function(x, y) recode(x,
  5 ~ 1, 3 ~ 2, 1 ~ 3, c(8, 9, NA) ~ NA))
psid_abridged <- collect_tv(psid_abridged, "ghlth_chng_sp", function(x, y) recode(x,
  5 ~ 1, 3 ~ 2, 1 ~ 3, c(8, 9, 0, NA) ~ NA))
# good/poor health indicators
psid_abridged <- collect_tv(psid_abridged, "ghlth_good_ind", function(x, y) recode(x,
  1 ~ 0, 5 ~ 1, c(9, 0, NA) ~ NA))
psid_abridged <- collect_tv(psid_abridged, "ghlth_poor_ind", function(x, y) recode(x,
  5 ~ 0, 1 ~ 1, c(9, 0, NA) ~ NA))
# self-rated health status (5-cat reversed)
ghlth_stat <- function(x, y) recode(x,
  5 ~ 1, 4 ~ 2, 3 ~ 3, 2 ~ 4, 1 ~ 5, c(8, 9, 0, NA) ~ NA)
psid_abridged <- collect_tv(psid_abridged, "ghlth_stat_ind", ghlth_stat)
psid_abridged <- collect_tv(psid_abridged, "ghlth_stat_rp",  ghlth_stat)
psid_abridged <- collect_tv(psid_abridged, "ghlth_stat_sp",  ghlth_stat)  # sp: . ,0 -> NA (same set)

# hospitalization
hosp_any <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, c(8, 9, 0, NA) ~ NA)
psid_abridged <- collect_tv(psid_abridged, "hosp_any_rp", hosp_any)
psid_abridged <- collect_tv(psid_abridged, "hosp_any_sp", hosp_any)
hosp_nt <- function(x, y) recode(x, 1 %..% 365 ~ keep, c(998, 999, 0, NA) ~ NA)
psid_abridged <- collect_tv(psid_abridged, "hosp_num_nt_rp", hosp_nt)
psid_abridged <- collect_tv(psid_abridged, "hosp_num_nt_sp", hosp_nt)
hosp_wk <- function(x, y) recode(x, 1 %..% 52 ~ keep, c(98, 99, 0, NA) ~ NA)
psid_abridged <- collect_tv(psid_abridged, "hosp_num_wk_rp", hosp_wk)
psid_abridged <- collect_tv(psid_abridged, "hosp_num_wk_sp", hosp_wk)

# life satisfaction (5-cat reversed)
psid_abridged <- collect_tv(psid_abridged, "life_stat_resp", function(x, y) recode(x,
  5 ~ 0, 4 ~ 1, 3 ~ 2, 2 ~ 3, 1 ~ 4, c(8, 9, 0, NA) ~ NA))
