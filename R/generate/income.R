# =====================================================================
# R/generate/income.R
# Derives:
#   * <labor|capital>_*_nd_rc_*  (reference-couple totals = RP + SP)
#   * income_topcoded_*  (data flag: 1 if ANY nominal income variable
#     is at a PSID top-code for the person-wave).
# The income collect scripts (earnings, family_income, labor_income,
# capital_income, income) standardize each era's growing dollar top-code to
# the 9999999 sentinel; the 2011+ family files carry no sentinel and instead
# truncate amounts at 9999997 (see dollar_topcodes() in R/programs.R).
# 0 = income reported, none at a top-code; NA = no nonmissing income
# variable that wave (incl. pre-1976 bracket-coded amounts, which cannot
# carry a dollar top-code). Computed from the *_nd sources before revise:
# the *_rd/*_rdf versions rescale the sentinel (mirroring the reference
# build), so this flag is the only reliable top-code marker for them.
# =====================================================================

# --- reference-couple (_rc) totals: RP + SP at the family-unit level ----
# For every labor_/capital_ *_nd stub with separate RP and SP amounts, build
# <stub>_rc_<y> = rp + sp for waves where BOTH sides exist and PSID provides
# no _rc variable (capital_business_income_nd_rc stops in 1992; the sum
# extends it 1993+). The family-file amounts are FU-level constants on every
# member row (merged by fuid at ingest), so the columnwise sum IS the couple
# total at the FU level. Either side at the era top-code sentinel -> the
# sentinel (like earn_tot_nd in R/generate/earnings.R); either side NA -> NA.
# RP-only stubs (garden, div_intst_trst) have no SP side to sum -> no _rc.
# Waves where a side is a 9-category bracket code, not dollars, are skipped
# (codes cannot be summed): only rent_div_intst_trst is hit -- its pre-1976
# amounts are bracket-coded while both sides exist (see R/collect headers);
# the other bracket-coded stubs have no SP side before 1993 anyway.
rc_bracket_until <- c(capital_rent_div_intst_trst_income_nd = 1975)
rc_stubs <- unique(sub("_rp_[0-9]{4}$", "",
  grep("^(labor|capital)_.*_nd_rp_[0-9]{4}$", names(psid_abridged), value = TRUE)))
n_rc <- 0L
for (stub in rc_stubs) {
  for (y in year) {
    if (!is.null(psid_abridged[[paste0(stub, "_rc_", y)]])) next
    if (stub %in% names(rc_bracket_until) && y <= rc_bracket_until[[stub]]) next
    rp <- psid_abridged[[paste0(stub, "_rp_", y)]]
    sp <- psid_abridged[[paste0(stub, "_sp_", y)]]
    if (is.null(rp) || is.null(sp)) next
    out <- as.vector(rp) + as.vector(sp)
    tc <- if (y >= 2011) 9999997 else 9999999
    out[rp %in% tc | sp %in% tc] <- tc
    psid_abridged[[paste0(stub, "_rc_", y)]] <-
      set_label(out, var_label(paste0(stub, "_rc"), y))
    n_rc <- n_rc + 1L
  }
}
message("  income: created ", n_rc, " *_nd_rc couple totals (RP + SP)")

# --- top-code flag -------------------------------------------------------
# income varcats as in dollar_topcodes(): earn_/finc_/labor_/capital_/farm_/
# business_/taxable_ stubs share the era top-code ladder
income_tc_varcats <- c("earn", "finc", "labo", "capi", "farm", "busi", "taxa")
gen_tv("income_topcoded", function(y) {
  cols <- grep(paste0("_nd(_rp|_sp|_rc)?_", y, "$"), names(psid_abridged), value = TRUE)
  cols <- cols[substr(cols, 1, 4) %in% income_tc_varcats]
  if (!length(cols)) return(NULL)
  tc <- c(9999999, if (y >= 2011) 9999997)
  any_tc  <- rep(FALSE, nrow(psid_abridged))
  any_val <- rep(FALSE, nrow(psid_abridged))
  for (col in cols) {
    x <- psid_abridged[[col]]
    any_tc  <- any_tc  | x %in% tc
    any_val <- any_val | !is.na(x)
  }
  ifelse(any_val, as.numeric(any_tc), NA_real_)
}, set = "topcodeflag_2cat")
