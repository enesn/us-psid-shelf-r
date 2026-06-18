# =====================================================================
# R/generate/geography.R
# Derives time-invariant cgeo_region / cgeo_state: the value the individual
# reported most often across waves (as RP/SP), ties broken by the most recent wave.
# =====================================================================

for (m in c("cgeo_region", "cgeo_state"))
  psid_abridged[[m]] <- .attach_vl(set_label(modal_recent(m), var_label(m)), set_for(m))
