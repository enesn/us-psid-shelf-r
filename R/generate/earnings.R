# =====================================================================
# R/generate/earnings.R
# Derives: earn_tot_nd_{rp,sp}_* (total labor earnings, propagated to FU members
# via the FU maximum) and earn_tot_nd_* (the per-person combined version).
# Total labor earnings = unified labor (<=1993) or wage + business (1994+).
# =====================================================================

for (role in c("rp", "sp")) {
  gen_tv(paste0("earn_tot_nd_", role), function(y) {
    tot <- paste0("earn_tot_nd_", role, "_", y)
    if (y <= 1993) {
      uni <- psid_abridged[[paste0("earn_uni_nd_", role, "_", y)]]
      if (is.null(uni)) return(NULL)
      temp <- uni                                                 # inherits uni's raw top-code
      register_topcode(tot, income_topcodes(paste0("earn_uni_nd_", role, "_", y), y))
    } else {
      wage <- psid_abridged[[paste0("earn_wage_nd_", role, "_", y)]]
      busi <- psid_abridged[[paste0("earn_busi_nd_", role, "_", y)]]
      if (is.null(wage) || is.null(busi)) return(NULL)
      # the components can top-code at different raw codes (1998-2003: wage at
      # 9999999, busi at 999999), and a sum has no codebook top-code of its own,
      # so a censored total takes the larger of the two -- the weaker bound.
      tc_w <- income_topcodes(paste0("earn_wage_nd_", role, "_", y), y)
      tc_b <- income_topcodes(paste0("earn_busi_nd_", role, "_", y), y)
      temp <- wage + busi
      hit  <- wage %in% tc_w | busi %in% tc_b
      if (length(c(tc_w, tc_b))) {
        temp[hit] <- max(c(tc_w, tc_b))                           # preserve top-code
        register_topcode(tot, max(c(tc_w, tc_b)))
      }
    }
    fu <- psid_abridged[[paste0("fuid_", y)]]
    out <- fu_max(temp, fu)
    out[is.na(fu)] <- NA                                          # no FU -> missing
    out
  })
}

# per-person total labor earnings (RP's for the RP, SP's for the SP)
combine_rpsp("earn_tot_nd")
