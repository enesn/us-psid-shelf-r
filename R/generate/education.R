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

# edu_level — 5-category attainment, harmonized from three Stata "versions" and
# carried forward across waves (Stata Step_06 file 09):
#   v1 (1985+): HS/college milestones (edu_hsch/coll_att/coll_gra/coll_deg)
#   v2 (1968-1990): grade categories (edu_grde)
#   v3 (1968,1970+): years of schooling (edu_year)
# Per-era priority (v1>v2>v3, with v2 dropped from 1991+ and only v2 in 1969),
# then carry the last known value forward for current FU members.
# Stata-style missing semantics (Stata treats . as larger-than-any & inrange/inlist(.)=0):
s_in    <- function(x, ...) x %in% c(...)                       # inlist (NA -> FALSE)
s_inna  <- function(x, ...) x %in% c(...) | is.na(x)            # inlist incl. .
s_notin <- function(x, ...) !(x %in% c(...))                    # !inlist (NA -> TRUE)
s_inr   <- function(x, lo, hi) !is.na(x) & x >= lo & x <= hi    # inrange
s_notinr<- function(x, lo, hi) is.na(x) | x < lo | x > hi       # !inrange (NA -> TRUE)
ecol    <- function(stub, y) { v <- psid_abridged[[paste0(stub, "_", y)]]; if (is.null(v)) rep(NA_real_, .n) else v }

lvl_grde <- function(g) { o <- rep(NA_real_, .n)               # v2
  o[s_inr(g,0,3)] <- 0; o[s_inr(g,4,5)] <- 1; o[s_in(g,6)] <- 2; o[s_in(g,7)] <- 3; o[s_in(g,8)] <- 4; o }
lvl_year <- function(yr) { o <- rep(NA_real_, .n)              # v3
  o[s_inr(yr,1,11)] <- 0; o[s_in(yr,12)] <- 1; o[s_inr(yr,13,15)] <- 2; o[s_in(yr,16)] <- 3; o[s_in(yr,17)] <- 4; o }
lvl_mile <- function(hsch, catt, cgra, cdeg) {                 # v1
  o <- rep(NA_real_, .n)
  o[s_in(hsch,0)   & s_notin(catt,1) & s_notin(cgra,1) & s_notinr(cdeg,1,8)] <- 0
  o[s_in(hsch,1,2) & s_notin(catt,1) & s_notin(cgra,1) & s_notinr(cdeg,1,8)] <- 1
  o[s_inna(hsch,0,1,2) & ((s_in(catt,1)     & s_notin(cgra,1)   & s_notinr(cdeg,2,8)) |
                          (s_inna(catt,0,1)  & s_notin(cgra,1)   & s_in(cdeg,1)))] <- 2
  o[s_inna(hsch,0,1,2) & ((s_inna(catt,0,1) & s_in(cgra,1)      & s_notinr(cdeg,3,6)) |
                          (s_inna(catt,0,1)  & s_inna(cgra,0,1)  & s_in(cdeg,2)))] <- 3
  o[s_inna(hsch,0,1,2) &   s_inna(catt,0,1) & s_inna(cgra,0,1)  & s_inr(cdeg,3,6)] <- 4
  o[is.na(hsch) & is.na(catt) & s_inna(cgra,0) & s_notinr(cdeg,1,8)] <- NA
  o }

tprev <- rep(NA_real_, .n)                                     # temp_new carried from prior wave
for (y in year) {
  member <- ecol("response_ext", y) %in% 0
  v1 <- if (y >= 1985)              lvl_mile(ecol("edu_hsch",y), ecol("edu_coll_att",y), ecol("edu_coll_gra",y), ecol("edu_coll_deg",y)) else rep(NA_real_, .n)
  v2 <- if (y >= 1968 && y <= 1990) lvl_grde(ecol("edu_grde", y)) else rep(NA_real_, .n)
  v3 <- if (y == 1968 || y >= 1970) lvl_year(ecol("edu_year", y)) else rep(NA_real_, .n)
  if (y == 1968) {
    out <- rep(-1, .n)
    out <- rc(out, !is.na(v2), v2)
    out <- rc(out, out %in% -1 & !is.na(v3), v3)
    out <- rc(out, is.na(ecol("edu_grde", y)) & is.na(ecol("edu_year", y)), NA)
    tnew <- out
  } else {
    cur <- if (y == 1969)      v2
           else if (y <= 1984) ifelse(!is.na(v2), v2, v3)
           else if (y <= 1990) ifelse(!is.na(v1), v1, ifelse(!is.na(v2), v2, v3))
           else                ifelse(!is.na(v1), v1, v3)
    out  <- ifelse(!is.na(cur), cur, -1)
    tnew <- cur
    carry <- !is.na(tprev) & out %in% -1
    tnew <- ifelse(carry, tprev, tnew)
    out  <- ifelse(carry & member, tprev, out)
    out  <- ifelse((is.na(tprev) & is.na(cur)) | !member, NA, out)
  }
  psid_abridged[[paste0("edu_level_", y)]] <- g_label(out, "edu_level", y, "edulevel_5cat")
  tprev <- tnew
}

# edu_level_max — highest edu_level observed across waves
lvl_cols <- paste0("edu_level_", year)
edu_level_max <- rowmax(lapply(lvl_cols, function(c) psid_abridged[[c]]))
psid_abridged$edu_level_max <- .attach_vl(
  set_label(edu_level_max, var_label("edu_level_max")), set_for("edu_level_max"))
