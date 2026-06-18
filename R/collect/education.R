# =====================================================================
# R/collect/education.R
# Builds: edu_coll_att_{rp,sp}_*, edu_coll_deg_*, edu_coll_gra_*, edu_coll_num_*,
#         edu_grde_*, edu_hsch_*, edu_icol_att_*, edu_icol_deg_*, edu_year_ind_*
# =====================================================================

edu_coll_att <- function(x, y) {            # any college attendance?
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, inlist(x, 0, 9) | is.na(x), NA)
}
edu_coll_deg <- function(x, y) {            # highest college degree
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 6), x)
  out <- rc(out, inlist(x, 8), 7); out <- rc(out, inlist(x, 97), 8)
  rc(out, inlist(x, 0, 98, 99) | is.na(x), NA)
}
edu_coll_gra <- function(x, y) {            # bachelor's degree?
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 5), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, inlist(x, 9, 0) | is.na(x), NA)
}
edu_coll_num <- function(x, y) {            # years of college
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 5), x)
  rc(out, inlist(x, 9, 0) | is.na(x), NA)
}
edu_grde <- function(x, y) {                # highest grade completed (0-8 scale)
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 0, 8), x)
  rc(out, inlist(x, 9) | is.na(x), NA)
}
edu_hsch <- function(x, y) {                # high-school graduate type
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 3), 0); out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, inlist(x, 2), 2)
  rc(out, inlist(x, 0, 9) | is.na(x), NA)
}
edu_icol_deg <- function(x, y) {            # institution/degree level
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 7), x)
  rc(out, inlist(x, 0, 9) | is.na(x), NA)
}
edu_year <- function(x, y) {                # years of education
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 20), x)
  rc(out, inlist(x, 0, 98, 99) | is.na(x), NA)
}

for (v in c("edu_coll_att_rp","edu_coll_att_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_coll_att)
for (v in c("edu_coll_deg_rp","edu_coll_deg_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_coll_deg)
for (v in c("edu_coll_gra_rp","edu_coll_gra_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_coll_gra)
for (v in c("edu_coll_num_rp","edu_coll_num_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_coll_num)
for (v in c("edu_grde_rp","edu_grde_sp"))         psid_abridged <- collect_tv(psid_abridged, v, edu_grde)
for (v in c("edu_hsch_rp","edu_hsch_sp"))         psid_abridged <- collect_tv(psid_abridged, v, edu_hsch)
for (v in c("edu_icol_deg_rp","edu_icol_deg_sp")) psid_abridged <- collect_tv(psid_abridged, v, edu_icol_deg)
psid_abridged <- collect_tv(psid_abridged, "edu_year_ind", edu_year)

# edu_icol_att — institution attendance (rp/sp differ slightly on missing codes)
psid_abridged <- collect_tv(psid_abridged, "edu_icol_att_rp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 3), x); out <- rc(out, inlist(x, 5), 4)
  rc(out, inlist(x, 4, 8, 9) | is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "edu_icol_att_sp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 3), x); out <- rc(out, inlist(x, 5), 4)
  rc(out, inlist(x, 0, 4, 8, 9) | is.na(x), NA)
})
