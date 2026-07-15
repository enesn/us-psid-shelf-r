# =====================================================================
# R/generate/income.R
# Derives: income_topcoded_*  (data flag: 1 if ANY nominal income variable
# is at a PSID top-code for the person-wave).
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
