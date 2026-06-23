# =====================================================================
# R/generate/disability.R
# Derives: {adl,iadl}_sum_tot_{ind,rp,sp}_*, {adl,iadl}_sum_any_{ind,rp,sp}_*,
# and the combined per-person versions of every ADL/IADL measure.
# (adl_q*: 7 items; iadl_q*: 6 items, reverse-coded 9->0 before summing.)
# =====================================================================

.n <- nrow(psid_abridged)
stub_q <- list(adl = 1:7, iadl = 1:6)

for (stub in names(stub_q)) {
  qn <- stub_q[[stub]]
  for (role in c("ind", "rp", "sp")) {
    gen_tv(paste0(stub, "_sum_tot_", role), function(y) {
      qs <- lapply(qn, function(k) psid_abridged[[sprintf("%s_q%d_any_%s_%d", stub, k, role, y)]])
      if (all(vapply(qs, is.null, logical(1)))) return(NULL)
      if (stub == "iadl") qs <- lapply(qs, function(z) { z[z %in% 9] <- 0; z })  # reverse-code 9 -> 0
      mat <- do.call(cbind, qs)
      tot <- ifelse(rowSums(!is.na(mat)) == 0, NA_real_, rowSums(mat, na.rm = TRUE))
      rel <- psid_abridged[[paste0("rel_ext_", y)]]; rex <- psid_abridged[[paste0("response_ext_", y)]]
      if (role == "rp") tot <- ifelse(inrange(rel, 100, 199) & rex %in% 0, tot, NA_real_)
      if (role == "sp") tot <- ifelse(inrange(rel, 200, 299) & rex %in% 0, tot, NA_real_)
      tot
    })
    gen_tv(paste0(stub, "_sum_any_", role), function(y) {
      s <- psid_abridged[[sprintf("%s_sum_tot_%s_%d", stub, role, y)]]
      if (is.null(s)) return(NULL)
      case_when(s %in% 0 ~ 0, inrange(s, 1, 99) ~ 1, .default = NA_real_)
    })
  }
}

# combined per-person versions of every measure: for 1992-1996 (ADL_*_ANY,
# IADL_*) the source is the unconditional ind value (Stata Step_06 file 09,
# lines 454-464/480-484); for 1999+/2003+ it's RP/SP gated by rel_ext/response_ext
# (lines 466-471/485-490) — two disjoint year ranges, not one overwriting the
# other. adl_q*_hlp has no _ind years at all, so combine_roles reduces to the
# same RP/SP-only behavior combine_rpsp gave it.
ind_var <- c("adl_sum_tot","adl_sum_any",
             as.vector(rbind(paste0("adl_q",1:7,"_any"), paste0("adl_q",1:7,"_hlp"))),
             "iadl_sum_tot","iadl_sum_any",
             as.vector(rbind(paste0("iadl_q",1:6,"_any"), paste0("iadl_q",1:6,"_hea"))))
for (m in ind_var) combine_roles(m)
