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

# home assets (value) & debts (mortgages): decompose home equity (wlth_home_net)
# into assets and debts. The reference imputes from home value & mortgages, then
# (where the reported equity disagrees with value-minus-mortgage) splits with a
# multi-branch rule. Faithful port of Stata Step_06 file 09 (wlth_home_ass/deb).
for (y in wlthyear) {
  hn <- wc("wlth_home_net_nd", y); if (is.null(hn)) next
  hv <- wc("home_own_val_nd", y);       if (is.null(hv)) hv <- rep(NA_real_, .n)
  m1 <- wc("home_own_mor_val_1m_nd", y); if (is.null(m1)) m1 <- rep(NA_real_, .n)
  m2 <- wc("home_own_mor_val_2m_nd", y); if (is.null(m2)) m2 <- rep(NA_real_, .n)
  # total mortgage: 1m only (<=1989); 1m+2m (1994+) but only when 1m present and
  # 2m is a real non-zero value, else missing (matches the Stata egen-if).
  tmor <- if (y <= 1989) m1 else ifelse(!is.na(m1) & !is.na(m2) & !(m2 %in% 0), m1 + m2, NA_real_)
  # value-implied equity, its difference from reported equity, imputation flag
  tv <- rep(NA_real_, .n)
  tv <- rc(tv, !is.na(hn) & !is.na(hv) & !is.na(tmor), hv - tmor)
  tv <- rc(tv, !is.na(hn) & !is.na(hv) &  is.na(tmor), hv)
  tv <- rc(tv, !is.na(hn) &  is.na(hv) & !is.na(tmor), -tmor)
  td <- ifelse(!is.na(hn) & !is.na(tv), hn - tv, NA_real_)
  fl <- rep(NA_real_, .n)                       # if_wlth_home_ass/deb
  fl <- rc(fl, !is.na(hn) &   td %in% 0,  0)
  fl <- rc(fl, !is.na(hn) & !(td %in% 0), 1)    # td != 0 OR td missing -> 1 (Stata . > 0)
  ha <- rep(-1, .n); hd <- rep(-1, .n)
  ha <- rc(ha, is.na(hn), NA); hd <- rc(hd, is.na(hn), NA)
  P <- !is.na(hn)
  # flag 0: equity == value - mortgage
  ha <- rc(ha, P & fl %in% 0 & !is.na(hv),   hv);   ha <- rc(ha, P & fl %in% 0 & is.na(hv),   0)
  hd <- rc(hd, P & fl %in% 0 & !is.na(tmor), tmor); hd <- rc(hd, P & fl %in% 0 & is.na(tmor), 0)
  # flag 1, group A: value & mortgage both present
  gA <- P & fl %in% 1 & !is.na(hv) & !is.na(tmor); d <- hv - hn
  ha <- rc(ha, gA & d >= 0,                hv);        hd <- rc(hd, gA & d >= 0,                hv - hn)
  ha <- rc(ha, gA & d < 0 & tmor %in% 0,   hn);        hd <- rc(hd, gA & d < 0 & tmor %in% 0,   0)
  ha <- rc(ha, gA & d < 0 & tmor > 0,      hn + tmor); hd <- rc(hd, gA & d < 0 & tmor > 0,      tmor)
  # group B: value present, mortgage missing
  gB <- P & fl %in% 1 & !is.na(hv) & is.na(tmor); d <- hv - hn
  ha <- rc(ha, gB & d >= 0, hv); hd <- rc(hd, gB & d >= 0, hv - hn)
  ha <- rc(ha, gB & d < 0,  hn); hd <- rc(hd, gB & d < 0,  0)
  # group C: value missing, mortgage present (permits negative assets, per Stata)
  gC <- P & fl %in% 1 & is.na(hv) & !is.na(tmor)
  ha <- rc(ha, gC, hn + tmor); hd <- rc(hd, gC, tmor)
  # group D: both missing
  gD <- P & fl %in% 1 & is.na(hv) & is.na(tmor)
  ha <- rc(ha, gD & hn >= 0, hn); hd <- rc(hd, gD & hn >= 0, 0)
  ha <- rc(ha, gD & hn < 0,  0);  hd <- rc(hd, gD & hn < 0,  -hn)
  psid_abridged[[paste0("wlth_home_ass_nd_", y)]] <- g_label(ha, "wlth_home_ass_nd", y)
  psid_abridged[[paste0("wlth_home_deb_nd_", y)]] <- g_label(hd, "wlth_home_deb_nd", y)
}

# total assets / debts (sum of present in-range components), and net worth.
# Real estate is special: pre-2013 only a single unified net report exists
# (wlth_real_uni_nd, already what net_cat folds into wlth_real_net_nd), but for
# 2013+ Stata adds the *gross* wlth_real_ass_nd/wlth_real_deb_nd to assets/debts
# separately -- like home -- rather than netting them (Step_06 file 09 lines
# 507/518 add real_ass; line 623 adds real_deb). Using the netted wlth_real_net_nd
# as a single asset-side component for 2013+ (as this used to) double-nets it
# and undercounts debt for anyone with real-estate debt that wave.
real_ass_comp <- function(y) { v <- wc("wlth_real_ass_nd", y); if (is.null(v)) wc("wlth_real_net_nd", y) else v }
real_deb_comp <- function(y) wc("wlth_real_deb_nd", y)   # only present 2013+; NULL (omitted) pre-2013
# farm/business gets the same gross-asset/gross-debt split as real estate for
# 2013+ (Stata Step_06 file 09 lines 505/515 add fbus_ass; lines 622 adds
# fbus_deb) -- using the netted wlth_fbus_net_nd as a single asset-side
# component double-nets it and undercounts debt for anyone with business debt.
fbus_ass_comp <- function(y) { v <- wc("wlth_fbus_ass_nd", y); if (is.null(v)) wc("wlth_fbus_net_nd", y) else v }
fbus_deb_comp <- function(y) wc("wlth_fbus_deb_nd", y)   # only present 2013+; NULL (omitted) pre-2013
# investments are reported as separate stock/IRA components from 1999+ (Stata
# Step_06 file 09 lines 498-499/508-509/519-520 add inve_stk/inve_ira to assets;
# lines 645-646/655-656/666-667 add their *individual* negative values to debt)
# -- 1984-1994 has only a unified report (wlth_inve_uni_nd, what wlth_inve_net_nd
# reduces to that era). The [0,999999999] inclusion test and the negative-
# component-as-debt spillover are applied to stk and ira separately in Stata, so
# netting them first (as wlth_inve_net_nd does) hides an individually-negative
# leg inside an overall-positive net and drops it from both sides.
inve_comps <- function(y) {
  parts <- Filter(Negate(is.null), list(wc("wlth_inve_stk_nd", y), wc("wlth_inve_ira_nd", y)))
  if (length(parts)) parts else Filter(Negate(is.null), list(wc("wlth_inve_net_nd", y)))
}
# savings are reported as separate bank/bond components from 2019+ only (Stata
# lines 516-517 add savi_bnk/savi_bnd to assets; lines 663-664 add their
# individual negative values to debt); 1984-2017 has only a unified report
# (wlth_savi_uni_nd, what wlth_savi_net_nd reduces to those years). Same
# netting hazard as investments above.
savi_comps <- function(y) {
  parts <- Filter(Negate(is.null), list(wc("wlth_savi_bnk_nd", y), wc("wlth_savi_bnd_nd", y)))
  if (length(parts)) parts else Filter(Negate(is.null), list(wc("wlth_savi_net_nd", y)))
}

asset_comps <- function(y) Filter(Negate(is.null), c(
  list(wc("wlth_home_ass_nd", y), real_ass_comp(y), fbus_ass_comp(y)),
  savi_comps(y), inve_comps(y),
  list(wc("wlth_vehi_net_nd", y), wc("wlth_oass_net_nd", y))))
debt_comps  <- function(y) Filter(Negate(is.null), list(
  wc("wlth_home_deb_nd", y), real_deb_comp(y), fbus_deb_comp(y), wc("wlth_odeb_net_nd", y)))
ass_comps_xh  <- function(y) Filter(Negate(is.null), c(
  list(real_ass_comp(y), fbus_ass_comp(y)),
  savi_comps(y), inve_comps(y),
  list(wc("wlth_vehi_net_nd", y), wc("wlth_oass_net_nd", y))))
ass_comps_xhr <- function(y) Filter(Negate(is.null), c(
  list(fbus_ass_comp(y)),
  savi_comps(y), inve_comps(y),
  list(wc("wlth_vehi_net_nd", y), wc("wlth_oass_net_nd", y))))

# negative-valued asset components count toward debt, at their absolute value
# (Stata Step_06 file 09, wlth_tot_deb_nd lines 631-669: "add in asset
# components with negative values"). any_present mirrors sum_in_range's NA gate
# but spans both the positive debt comps and the negative side of the assets.
neg_as_debt <- function(z) ifelse(!is.na(z) & z < 0 & z >= -999999999, -z, 0)
deb_with_spillover <- function(ac, dc) {
  if (!length(ac) && !length(dc)) return(NULL)
  pos_present <- if (length(dc)) Reduce(`|`, lapply(dc, function(z) !is.na(z))) else rep(FALSE, .n)
  neg_present <- if (length(ac)) Reduce(`|`, lapply(ac, function(z) !is.na(z) & z < 0)) else rep(FALSE, .n)
  pos <- if (length(dc)) { p <- sum_in_range(dc); ifelse(is.na(p), 0, p) } else rep(0, .n)
  neg <- if (length(ac)) Reduce(`+`, lapply(ac, neg_as_debt)) else rep(0, .n)
  ifelse(pos_present | neg_present, pos + neg, NA_real_)
}

gen_tv("wlth_tot_ass_nd", function(y) sum_in_range(asset_comps(y)))
gen_tv("wlth_tot_deb_nd", function(y) deb_with_spillover(asset_comps(y), debt_comps(y)))
# excl-home and excl-home-&-real variants
gen_tv("wlth_tot_ass_xh_nd",  function(y) sum_in_range(ass_comps_xh(y)))
gen_tv("wlth_tot_ass_xhr_nd", function(y) sum_in_range(ass_comps_xhr(y)))
gen_tv("wlth_tot_deb_xh_nd",  function(y) deb_with_spillover(ass_comps_xh(y),  Filter(Negate(is.null), list(real_deb_comp(y), fbus_deb_comp(y), wc("wlth_odeb_net_nd", y)))))
gen_tv("wlth_tot_deb_xhr_nd", function(y) deb_with_spillover(ass_comps_xhr(y), Filter(Negate(is.null), list(fbus_deb_comp(y), wc("wlth_odeb_net_nd", y)))))

for (suf in c("", "_xh", "_xhr"))
  gen_tv(paste0("wlth_tot_net", suf, "_nd"), function(y) {
    a <- wc(paste0("wlth_tot_ass", suf, "_nd"), y); d <- wc(paste0("wlth_tot_deb", suf, "_nd"), y)
    if (is.null(a) || is.null(d)) return(NULL)
    ifelse(!is.na(a) & !is.na(d), a - d, NA_real_)
  })
