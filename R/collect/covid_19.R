# =====================================================================
# R/collect/covid_19.R
# Builds the 57 COVID-19 module variables (2021 wave). All share a small set of
# recode shapes; each is applied to its variable(s) below.
# =====================================================================

# binary yes/no: 5->0, 1->1, 8/9->NA, 0/.->NA
bin5 <- function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
}
# yes/no with 9-only DK
bin9 <- function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, inlist(x, 9, 0) | is.na(x), NA)
}
# "ever reported COVID" flag: 0 if 0/., 1 if 5/1/8/9
df_flag <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 0) | is.na(x), 0)
  rc(out, inlist(x, 5, 1, 8, 9), 1)
}
# month (calendar 1-12 or 21-24 = seasons)
mo_fn <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 12), x); out <- rc(out, inrange(x, 21, 24), x)
  rc(out, inlist(x, 98, 99, 0) | is.na(x), NA)
}
# year (2019+)
yr_fn <- function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 2019, 2099), x)
  rc(out, inlist(x, 9998, 9999, 0) | is.na(x), NA)
}
# 5-cat opinion reversed (5->1 .. 1->5)
opi_fn <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 1); out <- rc(out, inlist(x, 4), 2); out <- rc(out, inlist(x, 3), 3)
  out <- rc(out, inlist(x, 2), 4); out <- rc(out, inlist(x, 1), 5)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
}
# 4-cat severity (1..4, 9->NA)
sev_fn <- function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 1, 4), x)
  rc(out, inlist(x, 9, 0) | is.na(x), NA)
}

apply_fn <- function(vars, fn)
  for (v in vars) psid_abridged <<- collect_tv(psid_abridged, v, fn)

apply_fn(c("df_covid_rep_ind","df_covid_rep_rp","df_covid_rep_sp"), df_flag)

apply_fn(c("covid_vacc_ind","covid_vacc_rp","covid_vacc_sp",
           "covid_test_rp","covid_test_sp",
           "covid_medi_talk_any_rp","covid_medi_talk_any_sp",
           "covid_medi_nodi_sym_rp","covid_medi_nodi_sym_sp",
           "covid_test_ling_any_rp","covid_test_ling_any_sp",
           "covid_diag_hosp_oxy_rp","covid_diag_hosp_oxy_sp",
           "covid_diag_hosp_icu_rp","covid_diag_hosp_icu_sp",
           "covid_diag_hosp_ven_rp","covid_diag_hosp_ven_sp",
           "covid_diag_hosp_oth_rp","covid_diag_hosp_oth_sp",
           "covid_diag_noho_sym_rp","covid_diag_noho_sym_sp"), bin5)

# covid_posi_ind: like bin5 but only 0 (not .) recoded to missing
psid_abridged <- collect_tv(psid_abridged, "covid_posi_ind", function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, inlist(x, 8, 9, 0), NA)
})

apply_fn(c("covid_medi_talk_opi_rp","covid_medi_talk_opi_sp"), opi_fn)
apply_fn(c("covid_medi_diag_mo_rp","covid_medi_diag_mo_sp",
           "covid_medi_nodi_mo_rp","covid_medi_nodi_mo_sp",
           "covid_test_rece_mo_rp","covid_test_rece_mo_sp"), mo_fn)
apply_fn(c("covid_medi_diag_yr_rp","covid_medi_diag_yr_sp",
           "covid_medi_nodi_yr_rp","covid_medi_nodi_yr_sp",
           "covid_test_rece_yr_rp","covid_test_rece_yr_sp"), yr_fn)

# covid_check_test: did you get tested? (rp has no 0 category; sp does)
psid_abridged <- collect_tv(psid_abridged, "covid_check_test_rp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 1); out <- rc(out, inlist(x, 3), 2); out <- rc(out, inlist(x, 1), 3)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "covid_check_test_sp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 0), 0)
  out <- rc(out, inlist(x, 5), 1); out <- rc(out, inlist(x, 3), 2); out <- rc(out, inlist(x, 1), 3)
  rc(out, is.na(x), NA)
})

apply_fn(c("covid_test_rece_typ_rp","covid_test_rece_typ_sp"), function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 2), 2)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})
apply_fn(c("covid_test_rece_res_rp","covid_test_rece_res_sp"), function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, inlist(x, 7), 9)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})
apply_fn(c("covid_test_ling_typ_rp","covid_test_ling_typ_sp"), function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 1, 3), x)
  rc(out, inlist(x, 8, 9, 0) | is.na(x), NA)
})
apply_fn(c("covid_test_ling_sev_rp","covid_test_ling_sev_sp",
           "covid_diag_noho_sev_rp","covid_diag_noho_sev_sp"), sev_fn)

# covid_check_diag: 0->1, 5->2, 1->3
apply_fn(c("covid_check_diag_rp","covid_check_diag_sp"), function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 0), 1); out <- rc(out, inlist(x, 5), 2); out <- rc(out, inlist(x, 1), 3)
  rc(out, is.na(x), NA)
})
apply_fn(c("covid_diag_hosp_any_rp","covid_diag_hosp_any_sp"), bin9)
apply_fn(c("covid_diag_hosp_num_rp","covid_diag_hosp_num_sp"), function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, 1, 60), x)
  rc(out, inlist(x, 99, 0) | is.na(x), NA)
})
