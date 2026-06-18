# =====================================================================
# R/generate/earnings.R
# Derives: earn_tot_nd_{rp,sp}_* (total labor earnings, propagated to FU members
# via the FU maximum) and earn_tot_nd_* (the per-person combined version).
# Total labor earnings = unified labor (<=1993) or wage + business (1994+).
# =====================================================================

for (role in c("rp", "sp")) {
  gen_tv(paste0("earn_tot_nd_", role), function(y) {
    if (y <= 1993) {
      uni <- psid_abridged[[paste0("earn_uni_nd_", role, "_", y)]]
      if (is.null(uni)) return(NULL)
      temp <- uni
    } else {
      wage <- psid_abridged[[paste0("earn_wage_nd_", role, "_", y)]]
      busi <- psid_abridged[[paste0("earn_busi_nd_", role, "_", y)]]
      if (is.null(wage) || is.null(busi)) return(NULL)
      temp <- wage + busi
      temp[wage %in% 9999999 | busi %in% 9999999] <- 9999999      # preserve top-code
    }
    fu <- psid_abridged[[paste0("fuid_", y)]]
    out <- fu_max(temp, fu)
    out[is.na(fu)] <- NA                                          # no FU -> missing
    out
  })
}

# per-person total labor earnings (RP's for the RP, SP's for the SP)
combine_rpsp("earn_tot_nd")
