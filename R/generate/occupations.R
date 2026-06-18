# =====================================================================
# R/generate/occupations.R
# Combined per-person occupation codes (RP's for the RP, SP's for the SP).
# =====================================================================
for (m in c("occ_1970c", paste0("occ_2000c_", c("1m","2m","3m","4m")),
            paste0("occ_2010c_", c("1m","2m","3m","4m"))))
  combine_rpsp(m)
