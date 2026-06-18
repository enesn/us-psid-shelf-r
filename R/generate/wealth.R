# =====================================================================
# R/generate/wealth.R
# Derives net-worth components and totals from the collected wealth pieces:
#   per-category net = unified measure (<=2011) or assets - debts (2013+);
#   home assets/debts; total assets/debts/net (incl. excl-home [xh] and
#   excl-home-&-real [xhr] variants). Missing components are skipped in sums.
# =====================================================================

.n <- nrow(psid_abridged)
wc <- function(stub, y) psid_abridged[[paste0(stub, "_", y)]]
# net of two components a (+ op*b); missing component contributes its partner
net2 <- function(a, b, op) {
  if (is.null(a)) a <- rep(NA_real_, .n)
  if (is.null(b)) b <- rep(NA_real_, .n)
  case_when(
  !is.na(a) & !is.na(b) ~ a + op * b,
  !is.na(a) &  is.na(b) ~ a,
  is.na(a) & !is.na(b) ~ op * b,
  .default = NA_real_)
}
# sum of the present components that fall in [0, 999999999]
sum_in_range <- function(comps) {
  if (!length(comps)) return(rep(NA_real_, .n))
  any_present <- Reduce(`|`, lapply(comps, function(z) !is.na(z)))
  tot <- rep(0, .n)
  for (z in comps) tot <- tot + ifelse(!is.na(z) & z >= 0 & z <= 999999999, z, 0)
  ifelse(any_present, tot, NA_real_)
}

# per-category net worth (uni where present, else asset/debt combination)
net_cat <- function(cat, comps_fn) gen_tv(paste0("wlth_", cat, "_net_nd"), function(y) {
  uni <- wc(paste0("wlth_", cat, "_uni_nd"), y)
  if (!is.null(uni)) return(ifelse(!is.na(uni), uni, NA_real_))
  comps_fn(y)
})
net_cat("real", function(y) net2(wc("wlth_real_ass_nd", y), wc("wlth_real_deb_nd", y), -1))
net_cat("fbus", function(y) net2(wc("wlth_fbus_ass_nd", y), wc("wlth_fbus_deb_nd", y), -1))
net_cat("savi", function(y) net2(wc("wlth_savi_bnk_nd", y), wc("wlth_savi_bnd_nd", y), +1))
net_cat("inve", function(y) net2(wc("wlth_inve_stk_nd", y), wc("wlth_inve_ira_nd", y), +1))
net_cat("odeb", function(y) {
  comps <- Filter(Negate(is.null), lapply(c("cre","stu","med","leg","fam","rem"),
                  function(k) wc(paste0("wlth_odeb_", k, "_nd"), y)))
  if (!length(comps)) return(NULL)
  sum_in_range(comps)
})

# home assets (value) and debts (mortgages, 1m + 2m)
gen_tv("wlth_home_ass_nd", function(y) { v <- wc("wlth_home_net_nd", y); if (is.null(v)) return(NULL)
  ifelse(is.na(v), NA_real_, wc("home_own_val_nd", y)) })
gen_tv("wlth_home_deb_nd", function(y) {
  m1 <- wc("home_own_mor_val_1m_nd", y); m2 <- wc("home_own_mor_val_2m_nd", y)
  if (is.null(m1) && is.null(m2)) return(NULL)
  sum_in_range(Filter(Negate(is.null), list(m1, m2)))
})

# total assets / debts (sum of present in-range components), and net worth
asset_comps <- function(y) Filter(Negate(is.null), list(
  wc("wlth_home_ass_nd", y), wc("wlth_real_net_nd", y), wc("wlth_fbus_net_nd", y),
  wc("wlth_savi_net_nd", y), wc("wlth_inve_net_nd", y), wc("wlth_vehi_net_nd", y),
  wc("wlth_oass_net_nd", y)))
debt_comps  <- function(y) Filter(Negate(is.null), list(
  wc("wlth_home_deb_nd", y), wc("wlth_odeb_net_nd", y)))

gen_tv("wlth_tot_ass_nd", function(y) sum_in_range(asset_comps(y)))
gen_tv("wlth_tot_deb_nd", function(y) sum_in_range(debt_comps(y)))
# excl-home and excl-home-&-real variants
gen_tv("wlth_tot_ass_xh_nd",  function(y) sum_in_range(Filter(Negate(is.null), list(
  wc("wlth_real_net_nd", y), wc("wlth_fbus_net_nd", y), wc("wlth_savi_net_nd", y),
  wc("wlth_inve_net_nd", y), wc("wlth_vehi_net_nd", y), wc("wlth_oass_net_nd", y)))))
gen_tv("wlth_tot_ass_xhr_nd", function(y) sum_in_range(Filter(Negate(is.null), list(
  wc("wlth_fbus_net_nd", y), wc("wlth_savi_net_nd", y), wc("wlth_inve_net_nd", y),
  wc("wlth_vehi_net_nd", y), wc("wlth_oass_net_nd", y)))))
gen_tv("wlth_tot_deb_xh_nd",  function(y) { v <- wc("wlth_odeb_net_nd", y); if (is.null(v)) return(NULL); sum_in_range(list(v)) })
gen_tv("wlth_tot_deb_xhr_nd", function(y) { v <- wc("wlth_odeb_net_nd", y); if (is.null(v)) return(NULL); sum_in_range(list(v)) })

for (suf in c("", "_xh", "_xhr"))
  gen_tv(paste0("wlth_tot_net", suf, "_nd"), function(y) {
    a <- wc(paste0("wlth_tot_ass", suf, "_nd"), y); d <- wc(paste0("wlth_tot_deb", suf, "_nd"), y)
    if (is.null(a) || is.null(d)) return(NULL)
    ifelse(!is.na(a) & !is.na(d), a - d, NA_real_)
  })
