# =====================================================================
# R/generate/time_use.R
# Combined per-person weekly hours by activity (RP's for the RP, SP's for the SP).
# =====================================================================
for (m in c("time_acar","time_ccar","time_educ","time_hous","time_leis",
            "time_pers","time_shop","time_volu","time_work"))
  combine_rpsp(m)
