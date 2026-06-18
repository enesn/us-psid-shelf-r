# =====================================================================
# R/generate/depression.R
# Derives: dep_score_cut_resp_* (cutoff flag) + non-resp versions of every
# depression measure (assigned to the actual RP/SP respondent via role_map).
# =====================================================================

# dep_score_cut_resp — K6 score >= 13 cutoff
gen_tv("dep_score_cut_resp", function(y) {
  s <- psid_abridged[[paste0("dep_score_tot_resp_", y)]]
  if (is.null(s)) return(NULL)
  case_when(inrange(s, 0, 12) ~ 0, inrange(s, 13, 99) ~ 1, .default = NA_real_)
}, "depcutoff_2cat")

# non-respondent (person-level) versions
for (m in c("dep_score_tot","dep_score_cut","dep_q1_freq","dep_q2_freq","dep_q3_freq",
            "dep_q4_freq","dep_q5_freq","dep_q6_freq","dep_degr"))
  role_map(m)
