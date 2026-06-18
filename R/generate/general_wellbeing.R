# =====================================================================
# R/generate/general_wellbeing.R
# Derives: ghlth_stat/ghlth_chng/hosp_any/life_stat (role-mapped), ghlth_poor
# (+_rp/_sp), hosp_num, body_hght_{cm,in}, body_wght_{lb,kg}.
# =====================================================================

.n <- nrow(psid_abridged)
g_col <- function(stub, y) psid_abridged[[paste0(stub, "_", y)]]
# assign per-role values to the RP / SP person (NA for everyone else)
assign_rpsp <- function(y, vrp, vsp) {
  rel <- g_col("rel_ext", y); rex <- g_col("response_ext", y)
  out <- rep(NA_real_, .n)
  if (!is.null(vrp)) out <- rc(out, inrange(rel, 100, 199) & rex %in% 0, vrp)
  if (!is.null(vsp)) out <- rc(out, inrange(rel, 200, 299) & rex %in% 0, vsp)
  out
}

# ghlth_poor per role: fair/poor self-rated health (stat 1,2 on the reversed scale)
for (role in c("rp", "sp"))
  gen_tv(paste0("ghlth_poor_", role), function(y) {
    s <- g_col(paste0("ghlth_stat_", role), y); if (is.null(s)) return(NULL)
    case_when(s %in% c(3, 4, 5) ~ 0, s %in% c(1, 2) ~ 1, .default = NA_real_)
  })

# ghlth_poor (combined, from the individual's own measure; era-specific source)
gen_tv("ghlth_poor", function(y) {
  if (y == 1986)            { s <- g_col("ghlth_stat_ind", y); if (is.null(s)) return(NULL); case_when(s %in% c(3,4,5) ~ 0, s %in% c(1,2) ~ 1, .default = NA_real_) }
  else if (y >= 1988 && y <= 1993) { s <- g_col("ghlth_good_ind", y); if (is.null(s)) return(NULL); case_when(s %in% 1 ~ 0, s %in% 0 ~ 1, .default = NA_real_) }
  else if (y >= 1994)       { s <- g_col("ghlth_poor_ind", y); if (is.null(s)) return(NULL); case_when(s %in% 0 ~ 0, s %in% 1 ~ 1, .default = NA_real_) }
  else return(NULL)
})

# role-mapped person-level measures
for (m in c("ghlth_stat", "ghlth_chng", "hosp_any", "life_stat")) role_map(m)

# hosp_num — nights (preferred) or weeks of hospitalization, assigned to RP/SP
gen_tv("hosp_num", function(y) {
  tr <- ts <- NULL
  for (role in c("rp", "sp")) {
    nt <- g_col(paste0("hosp_num_nt_", role), y); wk <- g_col(paste0("hosp_num_wk_", role), y)
    v <- if (!is.null(nt)) nt else wk
    if (role == "rp") tr <- v else ts <- v
  }
  if (is.null(tr) && is.null(ts)) return(NULL)
  assign_rpsp(y, tr, ts)
})

# body height (cm then inches) -------------------------------------------------
ht_temp <- function(role, y) {
  uin <- g_col(paste0("body_hght_uni_in_", role), y); ume <- g_col(paste0("body_hght_uni_me_", role), y)
  pft <- g_col(paste0("body_hght_par_ft_", role), y); pin <- g_col(paste0("body_hght_par_in_", role), y)
  if (is.null(uin) && is.null(ume) && is.null(pft)) return(NULL)
  t <- rep(NA_real_, .n)
  if (!is.null(uin)) t <- rc(t, !is.na(uin), uin * 2.54)
  if (!is.null(ume)) { t <- rc(t, !(ume %in% c(0, 997)) & !is.na(ume), ume * 100); t <- rc(t, ume %in% c(0, 997), ume) }
  if (!is.null(pft) && !is.null(pin)) t <- rc(t, !is.na(pft) & !is.na(pin), pft * 2.54 * 12 + pin * 2.54)
  t
}
r5 <- function(x) round(x * 2) / 2                     # round to nearest 0.5
gen_tv("body_hght_cm", function(y) {
  tr <- ht_temp("rp", y); ts <- ht_temp("sp", y)
  if (is.null(tr) && is.null(ts)) return(NULL)
  r5(assign_rpsp(y, tr, ts))
})
gen_tv("body_hght_in", function(y) {
  cm <- g_col("body_hght_cm", y); if (is.null(cm)) return(NULL)
  r5(case_when(cm %in% c(0, 997) ~ cm, !is.na(cm) ~ cm / 2.54, .default = NA_real_))
})

# body weight (pounds then kilograms) ------------------------------------------
wt_temp <- function(role, y) {
  lb <- g_col(paste0("body_wght_uni_lb_", role), y); kg <- g_col(paste0("body_wght_uni_kg_", role), y)
  if (is.null(lb) && is.null(kg)) return(NULL)
  t <- rep(NA_real_, .n)
  if (!is.null(lb)) { t <- rc(t, !(lb %in% c(0, 997)) & !is.na(lb), lb); t <- rc(t, lb %in% c(0, 997), lb) }
  if (!is.null(kg)) { t <- rc(t, !(kg %in% c(0, 997)) & !is.na(kg), kg * 2.20462262185); t <- rc(t, kg %in% c(0, 997), kg) }
  t
}
gen_tv("body_wght_lb", function(y) {
  tr <- wt_temp("rp", y); ts <- wt_temp("sp", y)
  if (is.null(tr) && is.null(ts)) return(NULL)
  assign_rpsp(y, tr, ts)
})
gen_tv("body_wght_kg", function(y) {
  lb <- g_col("body_wght_lb", y); if (is.null(lb)) return(NULL)
  case_when(lb %in% c(0, 997) ~ lb, !is.na(lb) ~ lb / 2.20462262185, .default = NA_real_)
})
