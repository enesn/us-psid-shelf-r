# =====================================================================
# R/collect/expenditures.R
# Builds: 16 expn_*_nd (nominal-dollar expenditure amounts, range passthrough)
#         + 10 if_expn_* flags (0/1).
# =====================================================================

# range passthrough: keep [lo,hi], everything else (incl. .) -> NA
rng <- function(lo, hi) function(x, y) {
  out <- rep(-1, length(x)); out <- rc(out, inrange(x, lo, hi), x); rc(out, is.na(x), NA)
}

# expn_tot_nd changed range between 1999 and 2001+
psid_abridged <- collect_tv(psid_abridged, "expn_tot_nd", function(x, y) {
  out <- rep(-1, length(x))
  if (y == 1999) out <- rc(out, inrange(x, -5000, 500000), x)
  else           out <- rc(out, inrange(x, 0, 5000000), x)
  rc(out, is.na(x), NA)
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
  psid_abridged <- collect_tv(psid_abridged, v, rng(dollar_ranges[[v]][1], dollar_ranges[[v]][2]))

# if_expn_* flags: 0/1 passthrough
flag01 <- function(x, y) { out <- rep(-1, length(x)); out <- rc(out, inlist(x, 0, 1), x); rc(out, is.na(x), NA) }
for (v in c("if_expn_ccar_uni","if_expn_clot_uni","if_expn_comp_uni","if_expn_educ_uni",
            "if_expn_hlth_doc","if_expn_hlth_hos","if_expn_hlth_ins","if_expn_hlth_pre",
            "if_expn_orec_uni","if_expn_trip_uni"))
  psid_abridged <- collect_tv(psid_abridged, v, flag01)
