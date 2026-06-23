# =====================================================================
# R/collect/expenditures.R
# Builds: 16 expn_*_nd (nominal-dollar expenditure amounts, range passthrough)
#         + 10 if_expn_* flags (0/1).
# =====================================================================

# range passthrough: keep [lo,hi], everything else (incl. .) -> NA
# Most nominal-dollar expenditure inputs are already whole dollars, but the
# reference keeps expn_hlth_doc/pre_nd and expn_comp_tot_nd at their raw
# fractional-cents value for some years (e.g. 892.29) -- those three must NOT
# be truncated (confirmed empirically: trunc() strictly reduced their
# agreement). expn_hlth_ins_nd/expn_ccar_tot_nd looked fractional in the same
# spot-check but truncating them is in fact correct across the full series
# (exempting them regressed agreement hard), so they stay truncated. Every
# other expn_*_nd is truncated toward zero to match the reference; trunc()
# leaves the -1 sentinel and NA untouched. The family-size (_ndf) and
# inflation (_rd/_rdf) variants are derived downstream from these _nd values,
# so the choice here propagates to all four variants.
no_trunc <- c("expn_hlth_doc_nd", "expn_hlth_pre_nd", "expn_comp_tot_nd")
rng <- function(lo, hi, v) {
  keep_decimals <- v %in% no_trunc
  function(x, y) {
    out <- recode(x, lo %..% hi ~ keep, NA ~ NA)
    if (keep_decimals) out else trunc(out)
  }
}

# expn_tot_nd changed range between 1999 and 2001+
psid_abridged <- collect_tv(psid_abridged, "expn_tot_nd", function(x, y) {
  if (y == 1999) trunc(recode(x, -5000 %..% 500000 ~ keep, NA ~ NA))
  else           trunc(recode(x, 0 %..% 5000000 ~ keep, NA ~ NA))
})

dollar_ranges <- list(
  expn_tot_irv_nd  = c(500, 1000000),  expn_ccar_tot_nd = c(-500, 100000),
  expn_clot_tot_nd = c(-1000, 500000), expn_comp_tot_nd = c(0, 50000),
  expn_educ_tot_nd = c(-5000, 500000), expn_food_tot_nd = c(0, 150000),
  expn_hlth_tot_nd = c(-1500, 1500000),expn_hlth_doc_nd = c(-500, 500000),
  expn_hlth_hos_nd = c(-500, 500000),  expn_hlth_ins_nd = c(-1500, 500000),
  expn_hlth_pre_nd = c(-500, 500000),  expn_hous_tot_nd = c(-5000, 5000000),
  expn_orec_tot_nd = c(-1000, 500000), expn_tran_tot_nd = c(-5000, 500000),
  expn_trip_tot_nd = c(-100, 1000000))
for (v in names(dollar_ranges))
  psid_abridged <- collect_tv(psid_abridged, v, rng(dollar_ranges[[v]][1], dollar_ranges[[v]][2], v))

# if_expn_* flags: 0/1 passthrough
flag01 <- function(x, y) recode(x, c(0, 1) ~ keep, NA ~ NA)
for (v in c("if_expn_ccar_uni","if_expn_clot_uni","if_expn_comp_uni","if_expn_educ_uni",
            "if_expn_hlth_doc","if_expn_hlth_hos","if_expn_hlth_ins","if_expn_hlth_pre",
            "if_expn_orec_uni","if_expn_trip_uni"))
  psid_abridged <- collect_tv(psid_abridged, v, flag01)
