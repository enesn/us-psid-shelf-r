# =====================================================================
# R/collect/education.R
# Builds: edu_coll_att_{rp,sp}_*, edu_coll_deg_*, edu_coll_gra_*, edu_coll_num_*,
#         edu_grde_*, edu_hsch_*, edu_icol_att_*, edu_icol_deg_*, edu_year_ind_*
# =====================================================================

edu_coll_att <- function(x, y) recode(x,          # any college attendance?
  5 ~ 0, 1 ~ 1,
  c(0, 9, NA) ~ NA)
edu_coll_deg <- function(x, y) recode(x,          # highest college degree
  1 %..% 6 ~ keep,
  8 ~ 7, 97 ~ 8,
  c(0, 98, 99, NA) ~ NA)
edu_coll_gra <- function(x, y) recode(x,          # bachelor's degree?
  5 ~ 0, 1 ~ 1,
  c(9, 0, NA) ~ NA)
edu_coll_num <- function(x, y) recode(x,          # years of college
  1 %..% 5 ~ keep,
  c(9, 0, NA) ~ NA)
edu_grde <- function(x, y) recode(x,              # highest grade completed (0-8 scale)
  0 %..% 8 ~ keep,
  c(9, NA) ~ NA)
edu_hsch <- function(x, y) recode(x,              # high-school graduate type
  3 ~ 0, 1 ~ 1, 2 ~ 2,
  c(0, 9, NA) ~ NA)
edu_icol_deg <- function(x, y) recode(x,          # institution/degree level
  1 %..% 7 ~ keep,
  c(0, 9, NA) ~ NA)
edu_year <- function(x, y) recode(x,              # years of education
  1 %..% 20 ~ keep,
  c(0, 98, 99, NA) ~ NA)

for (v in c("edu_coll_att_rp","edu_coll_att_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_coll_att)
for (v in c("edu_coll_deg_rp","edu_coll_deg_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_coll_deg)
for (v in c("edu_coll_gra_rp","edu_coll_gra_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_coll_gra)
for (v in c("edu_coll_num_rp","edu_coll_num_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_coll_num)
for (v in c("edu_grde_rp","edu_grde_sp"))         psid_abridged <- collect_tv(psid_abridged, v, edu_grde)
for (v in c("edu_hsch_rp","edu_hsch_sp"))         psid_abridged <- collect_tv(psid_abridged, v, edu_hsch)
for (v in c("edu_icol_deg_rp","edu_icol_deg_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_icol_deg)
psid_abridged <- collect_tv(psid_abridged, "edu_year_ind", edu_year)

# edu_icol_att — institution attendance (rp/sp differ slightly on missing codes)
psid_abridged <- collect_tv(psid_abridged, "edu_icol_att_rp", function(x, y) recode(x,
  1 %..% 3 ~ keep, 5 ~ 4,
  c(4, 8, 9, NA) ~ NA))
psid_abridged <- collect_tv(psid_abridged, "edu_icol_att_sp", function(x, y) recode(x,
  1 %..% 3 ~ keep, 5 ~ 4,
  c(0, 4, 8, 9, NA) ~ NA))
