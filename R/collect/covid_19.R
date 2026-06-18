# =====================================================================
# R/collect/covid_19.R
# Builds the 57 COVID-19 module variables (2021 wave). All share a small set of
# recode shapes; each is applied to its variable(s) below.
# =====================================================================

# binary yes/no: 5->0, 1->1, 8/9->NA, 0/.->NA
bin5 <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, c(8, 9, 0, NA) ~ NA)
# yes/no with 9-only DK
bin9 <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, c(9, 0, NA) ~ NA)
# "ever reported COVID" flag: 0 if 0/., 1 if 5/1/8/9
df_flag <- function(x, y) recode(x, c(0, NA) ~ 0, c(5, 1, 8, 9) ~ 1)
# month (calendar 1-12 or 21-24 = seasons)
mo_fn <- function(x, y) recode(x, 1 %..% 12 ~ keep, 21 %..% 24 ~ keep, c(98, 99, 0, NA) ~ NA)
# year (2019+)
yr_fn <- function(x, y) recode(x, 2019 %..% 2099 ~ keep, c(9998, 9999, 0, NA) ~ NA)
# 5-cat opinion reversed (5->1 .. 1->5)
opi_fn <- function(x, y) recode(x, 5 ~ 1, 4 ~ 2, 3 ~ 3, 2 ~ 4, 1 ~ 5, c(8, 9, 0, NA) ~ NA)
# 4-cat severity (1..4, 9->NA)
sev_fn <- function(x, y) recode(x, 1 %..% 4 ~ keep, c(9, 0, NA) ~ NA)

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
psid_abridged <- collect_tv(psid_abridged, "covid_posi_ind", function(x, y) recode(x,
  5 ~ 0, 1 ~ 1, c(8, 9, 0) ~ NA))

apply_fn(c("covid_medi_talk_opi_rp","covid_medi_talk_opi_sp"), opi_fn)
apply_fn(c("covid_medi_diag_mo_rp","covid_medi_diag_mo_sp",
           "covid_medi_nodi_mo_rp","covid_medi_nodi_mo_sp",
           "covid_test_rece_mo_rp","covid_test_rece_mo_sp"), mo_fn)
apply_fn(c("covid_medi_diag_yr_rp","covid_medi_diag_yr_sp",
           "covid_medi_nodi_yr_rp","covid_medi_nodi_yr_sp",
           "covid_test_rece_yr_rp","covid_test_rece_yr_sp"), yr_fn)

# covid_check_test: did you get tested? (rp has no 0 category; sp does)
psid_abridged <- collect_tv(psid_abridged, "covid_check_test_rp", function(x, y) recode(x,
  5 ~ 1, 3 ~ 2, 1 ~ 3, NA ~ NA))
psid_abridged <- collect_tv(psid_abridged, "covid_check_test_sp", function(x, y) recode(x,
  0 ~ 0, 5 ~ 1, 3 ~ 2, 1 ~ 3, NA ~ NA))

apply_fn(c("covid_test_rece_typ_rp","covid_test_rece_typ_sp"), function(x, y) recode(x,
  1 ~ 1, 2 ~ 2, c(8, 9, 0, NA) ~ NA))
apply_fn(c("covid_test_rece_res_rp","covid_test_rece_res_sp"), function(x, y) recode(x,
  5 ~ 0, 1 ~ 1, 7 ~ 9, c(8, 9, 0, NA) ~ NA))
apply_fn(c("covid_test_ling_typ_rp","covid_test_ling_typ_sp"), function(x, y) recode(x,
  1 %..% 3 ~ keep, c(8, 9, 0, NA) ~ NA))
apply_fn(c("covid_test_ling_sev_rp","covid_test_ling_sev_sp",
           "covid_diag_noho_sev_rp","covid_diag_noho_sev_sp"), sev_fn)

# covid_check_diag: 0->1, 5->2, 1->3
apply_fn(c("covid_check_diag_rp","covid_check_diag_sp"), function(x, y) recode(x,
  0 ~ 1, 5 ~ 2, 1 ~ 3, NA ~ NA))
apply_fn(c("covid_diag_hosp_any_rp","covid_diag_hosp_any_sp"), bin9)
apply_fn(c("covid_diag_hosp_num_rp","covid_diag_hosp_num_sp"), function(x, y) recode(x,
  1 %..% 60 ~ keep, c(99, 0, NA) ~ NA))
