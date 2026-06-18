# =====================================================================
# R/collect/general_wellbeing.R
# Builds: nhlth_birth_wght, body_hght_*, body_wght_*, ghlth_*, hosp_*, life_stat_resp
# =====================================================================

# nhlth_birth_wght — birth weight (oz)  [single input]
psid_abridged <- collect_inv(psid_abridged, "nhlth_birth_wght", function(x) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 16, 224), x)
  out <- rc(out, inlist(x, 991), 980); out <- rc(out, inlist(x, 995), 981)
  rc(out, inlist(x, 998, 999), NA)
})

# height: feet of parent (rp/sp)
body_ft <- function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 2, 7), x)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "body_hght_par_ft_rp", body_ft)
psid_abridged <- collect_tv(psid_abridged, "body_hght_par_ft_sp", body_ft)

# height: inches of parent — 0 inches is valid only if the feet sibling is a
# real height (2..7); otherwise 0 is missing. (rp 1999-2009 keeps 0 outright.)
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
psid_abridged <- collect_tv(psid_abridged, "body_hght_uni_in_rp", function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 48, 90), x)
  rc(out, inlist(x, 99) | is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "body_hght_uni_in_sp", function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 48, 85), x)
  rc(out, inlist(x, 99, 0) | is.na(x), NA)
})
# height: unified metres
psid_abridged <- collect_tv(psid_abridged, "body_hght_uni_me_rp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 0.61, 2.09), x); out <- rc(out, inlist(x, 0.60), 0)
  out <- rc(out, inlist(x, 2.10), 997)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "body_hght_uni_me_sp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 0.61, 2.09), x)
  out <- rc(out, inrange(x, 0.5999, 0.6001), 0); out <- rc(out, inlist(x, 2.10), 997)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})
# weight: unified kg
body_kg <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 36.1, 179.9), x); out <- rc(out, inlist(x, 36.0), 0)
  out <- rc(out, inlist(x, 180.0), 997)
  rc(out, inlist(x, 998, 999, 0) | is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "body_wght_uni_kg_rp", body_kg)
psid_abridged <- collect_tv(psid_abridged, "body_wght_uni_kg_sp", body_kg)

# weight: unified lb (coding changed across eras)
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
psid_abridged <- collect_tv(psid_abridged, "ghlth_chng_rp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 1); out <- rc(out, inlist(x, 3), 2); out <- rc(out, inlist(x, 1), 3)
  rc(out, inlist(x, 8, 9) | is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "ghlth_chng_sp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 1); out <- rc(out, inlist(x, 3), 2); out <- rc(out, inlist(x, 1), 3)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})
# good/poor health indicators
psid_abridged <- collect_tv(psid_abridged, "ghlth_good_ind", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1), 0); out <- rc(out, inlist(x, 5), 1)
  rc(out, inlist(x, 9, 0) | is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "ghlth_poor_ind", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, inlist(x, 9, 0) | is.na(x), NA)
})
# self-rated health status (5-cat reversed)
ghlth_stat <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 1); out <- rc(out, inlist(x, 4), 2); out <- rc(out, inlist(x, 3), 3)
  out <- rc(out, inlist(x, 2), 4); out <- rc(out, inlist(x, 1), 5)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "ghlth_stat_ind", ghlth_stat)
psid_abridged <- collect_tv(psid_abridged, "ghlth_stat_rp",  ghlth_stat)
psid_abridged <- collect_tv(psid_abridged, "ghlth_stat_sp",  ghlth_stat)  # sp: . ,0 -> NA (same set)

# hospitalization
hosp_any <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "hosp_any_rp", hosp_any)
psid_abridged <- collect_tv(psid_abridged, "hosp_any_sp", hosp_any)
hosp_nt <- function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 1, 365), x)
  rc(out, inlist(x, 998, 999, 0) | is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "hosp_num_nt_rp", hosp_nt)
psid_abridged <- collect_tv(psid_abridged, "hosp_num_nt_sp", hosp_nt)
hosp_wk <- function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 1, 52), x)
  rc(out, inlist(x, 98, 99, 0) | is.na(x), NA)
}
psid_abridged <- collect_tv(psid_abridged, "hosp_num_wk_rp", hosp_wk)
psid_abridged <- collect_tv(psid_abridged, "hosp_num_wk_sp", hosp_wk)

# life satisfaction (5-cat reversed)
psid_abridged <- collect_tv(psid_abridged, "life_stat_resp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 4), 1); out <- rc(out, inlist(x, 3), 2)
  out <- rc(out, inlist(x, 2), 3); out <- rc(out, inlist(x, 1), 4)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})
