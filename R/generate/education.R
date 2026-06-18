# =====================================================================
# R/generate/education.R
# Derives: the combined per-person education measures, plus edu_year /
# edu_year_max (years of schooling, carried forward across waves) and
# edu_level / edu_level_max (5-category attainment).
# =====================================================================

.n <- nrow(psid_abridged)
ec <- function(stub, y) psid_abridged[[paste0(stub, "_", y)]]

# combined per-person versions of the collected RP/SP education measures
for (m in c("edu_grde", "edu_hsch", "edu_coll_att", "edu_coll_num", "edu_coll_gra",
            "edu_coll_deg", "edu_icol_att", "edu_icol_deg"))
  combine_rpsp(m)

# edu_year — years of schooling, carried forward across waves for current members
prev <- rep(NA_real_, .n)
for (y in year) {
  ind <- ec("edu_year_ind", y)
  cur <- if (is.null(ind)) rep(NA_real_, .n) else ind
  rex <- ec("response_ext", y); member <- rex %in% 0
  cur <- ifelse(is.na(cur) & member & !is.na(prev), prev, cur)   # carry forward
  psid_abridged[[paste0("edu_year_", y)]] <- g_label(cur, "edu_year", y)
  prev <- ifelse(!is.na(cur), cur, prev)
}

# edu_year_max — highest years of schooling observed so far
yr_cols <- paste0("edu_year_", year)
edu_year_max <- rowmax(lapply(yr_cols, function(c) psid_abridged[[c]]))
psid_abridged$edu_year_max <- g_label(edu_year_max, "edu_year_max")

# edu_level — 5-category attainment from years of schooling
to_level <- function(yrs) case_when(
  inrange(yrs, 1, 11) ~ 0, yrs %in% 12 ~ 1, inrange(yrs, 13, 15) ~ 2,
  yrs %in% 16 ~ 3, inrange(yrs, 17, 99) ~ 4, .default = NA_real_)
gen_tv("edu_level", function(y) {
  yrs <- ec("edu_year", y); if (is.null(yrs)) return(NULL); to_level(yrs)
}, "edulevel_5cat")
psid_abridged$edu_level_max <- .attach_vl(
  set_label(to_level(edu_year_max), var_label("edu_level_max")), set_for("edu_level_max"))
