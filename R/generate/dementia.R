# =====================================================================
# R/generate/dementia.R
# Derives: dmnt_score_tot_{ind,rp,sp}_*, dmnt_score_cut_{ind,rp,sp}_*, and the
# combined per-person versions of every dementia measure.  (Module: 2017+.)
# =====================================================================

.n <- nrow(psid_abridged)

# score totals per role = rowtotal of the 8 yes/no items (NA if all missing),
# assigned to the appropriate respondent.
for (role in c("ind", "rp", "sp")) {
  gen_tv(paste0("dmnt_score_tot_", role), function(y) {
    qs <- lapply(1:8, function(k) psid_abridged[[sprintf("dmnt_q%d_any_%s_%d", k, role, y)]])
    if (all(vapply(qs, is.null, logical(1)))) return(NULL)
    mat <- do.call(cbind, qs)
    tot <- ifelse(rowSums(!is.na(mat)) == 0, NA_real_, rowSums(mat, na.rm = TRUE))
    rel <- psid_abridged[[paste0("rel_ext_", y)]]; rex <- psid_abridged[[paste0("response_ext_", y)]]
    if (role == "rp") tot <- ifelse(inrange(rel, 100, 199) & rex %in% 0, tot, NA_real_)
    if (role == "sp") tot <- ifelse(inrange(rel, 200, 299) & rex %in% 0, tot, NA_real_)
    tot
  })
  gen_tv(paste0("dmnt_score_cut_", role), function(y) {
    s <- psid_abridged[[sprintf("dmnt_score_tot_%s_%d", role, y)]]
    if (is.null(s)) return(NULL)
    case_when(inrange(s, 0, 1) ~ 0, inrange(s, 2, 99) ~ 1, .default = NA_real_)
  }, "dmntcutoff_2cat")
}

# combined per-person versions
for (m in c("dmnt_score_tot", "dmnt_score_cut", "dmnt_elig",
            paste0("dmnt_q", 1:8, "_any")))
  combine_roles(m)
