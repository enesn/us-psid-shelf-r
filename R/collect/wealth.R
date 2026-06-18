# =====================================================================
# R/collect/wealth.R
# Builds: 24 wlth_*_nd (nominal-dollar wealth components; passthrough, the era
# sentinels pass through as themselves) + 24 if_wlth_* flags (0/1).
# Wealth questionnaire eras: 1984-2005, 2007-2009, 2011+.
# =====================================================================

# 3-era net (can be negative)
w3 <- function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 2005)      out <- blk(out, x, -99999998, 999999998, pass = c(-99999999, 0, 999999999))
  else if (y <= 2009) out <- blk(out, x, -99999998, 999999996, pass = c(0, 999999997))
  else                out <- blk(out, x, -99999997, 999999997, pass = 0)
  rc(out, is.na(x), NA)
}
# 3-era positive-only
wp3 <- function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 2005)      out <- blk(out, x, 1, 999999998, pass = c(0, 999999999))
  else if (y <= 2009) out <- blk(out, x, 1, 999999996, pass = c(0, 999999997))
  else                out <- blk(out, x, 1, 999999997, pass = 0)
  rc(out, is.na(x), NA)
}
# 2-era net (home equity)
w2 <- function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 2009)      out <- blk(out, x, -99999998, 999999996, pass = c(0, 999999997))
  else                out <- blk(out, x, -99999997, 999999997, pass = 0)
  rc(out, is.na(x), NA)
}
# single-era net
w1 <- function(x, y) { out <- rep(-1, length(x)); out <- blk(out, x, -99999997, 999999997, pass = 0); rc(out, is.na(x), NA) }
# single-era positive
wp1 <- function(x, y) { out <- rep(-1, length(x)); out <- blk(out, x, 1, 999999997, pass = 0); rc(out, is.na(x), NA) }

w3_vars  <- c("wlth_tot_net_nd","wlth_tot_net_xh_nd","wlth_inve_stk_nd","wlth_vehi_net_nd",
              "wlth_oass_net_nd","wlth_real_uni_nd","wlth_fbus_uni_nd","wlth_savi_uni_nd",
              "wlth_inve_uni_nd")
wp3_vars <- c("wlth_inve_ira_nd","wlth_odeb_fam_nd","wlth_odeb_rem_nd")
w1_vars  <- c("wlth_real_ass_nd","wlth_real_deb_nd","wlth_fbus_ass_nd","wlth_fbus_deb_nd",
              "wlth_savi_bnk_nd","wlth_savi_bnd_nd")
wp1_vars <- c("wlth_odeb_cre_nd","wlth_odeb_leg_nd","wlth_odeb_med_nd","wlth_odeb_stu_nd")

for (v in w3_vars)  psid_abridged <- collect_tv(psid_abridged, v, w3)
for (v in wp3_vars) psid_abridged <- collect_tv(psid_abridged, v, wp3)
for (v in w1_vars)  psid_abridged <- collect_tv(psid_abridged, v, w1)
for (v in wp1_vars) psid_abridged <- collect_tv(psid_abridged, v, wp1)
psid_abridged <- collect_tv(psid_abridged, "wlth_home_net_nd", w2)
# wlth_odeb_uni_nd: positive, eras 1984-2005 & 2007-2009 only (no 2011+ in source)
psid_abridged <- collect_tv(psid_abridged, "wlth_odeb_uni_nd", function(x, y) {
  out <- rep(-1, length(x))
  if (y <= 2005)      out <- blk(out, x, 1, 999999998, pass = c(0, 999999999))
  else if (y <= 2009) out <- blk(out, x, 1, 999999996, pass = c(0, 999999997))
  rc(out, is.na(x), NA)
})

# if_wlth_* flags: 0/1 passthrough
flag01 <- function(x, y) { out <- rep(-1, length(x)); out <- rc(out, inlist(x, 0, 1), x); rc(out, is.na(x), NA) }
if_vars <- c("if_wlth_fbus_ass","if_wlth_fbus_deb","if_wlth_fbus_uni","if_wlth_home_net",
             "if_wlth_inve_ira","if_wlth_inve_stk","if_wlth_inve_uni","if_wlth_oass_net",
             "if_wlth_odeb_cre","if_wlth_odeb_fam","if_wlth_odeb_leg","if_wlth_odeb_med",
             "if_wlth_odeb_rem","if_wlth_odeb_stu","if_wlth_odeb_uni","if_wlth_real_ass",
             "if_wlth_real_deb","if_wlth_real_uni","if_wlth_savi_bnd","if_wlth_savi_bnk",
             "if_wlth_savi_uni","if_wlth_tot_net","if_wlth_tot_net_xh","if_wlth_vehi_net")
for (v in if_vars) psid_abridged <- collect_tv(psid_abridged, v, flag01)
