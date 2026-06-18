# =====================================================================
# R/generate/relationship_id.R
# Derives: rel_par_tot (number of identified parents), rel_chi_tot / rel_chi_rep
# (total / reported number of children, birth + adopted).
# =====================================================================

.n <- nrow(psid_abridged)
rf <- psid_abridged$rf_rel_chi

# rel_par_tot — count of the 6 parent slots with a valid unique ID
partypes <- c("bm", "bf", "am1", "am2", "af1", "af2")
par_tot <- rowSums(vapply(partypes,
  function(k) as.integer(!is.na(psid_abridged[[paste0("rel_par_", k, "_id")]])),
  numeric(.n)))
psid_abridged$rel_par_tot <- set_label(par_tot, var_label("rel_par_tot"))

# rel_chi_tot / rel_chi_rep — sum of birth + adopted children counts
tot_bio <- tot_ado <- ifelse(!(rf %in% 0), 0, NA_real_)
rep_bio <- rep_ado <- ifelse(!(rf %in% 0), 0, NA_real_)
for (i in 20:1) {
  id <- psid_abridged[[paste0("rel_chi", i, "_id")]]
  if (is.null(id)) next
  ty <- psid_abridged[[paste0("rel_chi", i, "_type")]]
  num <- psid_abridged[[paste0("rel_chi", i, "_num")]]; rp <- psid_abridged[[paste0("rel_chi", i, "_rep")]]
  ok <- !is.na(id) & !(rf %in% 0)
  tot_bio <- rc(tot_bio, ok & ty %in% 1 & inrange(num, 1, 20), num)
  tot_ado <- rc(tot_ado, ok & ty %in% 2 & inrange(num, 1, 20), num)
  rep_bio <- rc(rep_bio, ok & ty %in% 1 & inrange(rp, 1, 20), rp)
  rep_bio <- rc(rep_bio, !is.na(id) & ty %in% 1 & is.na(rp), NA)
  rep_ado <- rc(rep_ado, ok & ty %in% 2 & inrange(rp, 1, 20), rp)
  rep_ado <- rc(rep_ado, !is.na(id) & ty %in% 2 & is.na(rp), NA)
}
chi_tot <- rep(-1, .n)
chi_tot <- rc(chi_tot, !is.na(tot_bio) & !is.na(tot_ado) & !(rf %in% 0), tot_bio + tot_ado)
chi_tot <- rc(chi_tot, is.na(tot_bio) | is.na(tot_ado) | rf %in% 0, 0)
psid_abridged$rel_chi_tot <- set_label(chi_tot, var_label("rel_chi_tot"))

chi_rep <- rep(-1, .n)
chi_rep <- rc(chi_rep, !is.na(rep_bio) & !is.na(rep_ado) & !(rf %in% 0), rep_bio + rep_ado)
chi_rep <- rc(chi_rep, is.na(rep_bio) | is.na(rep_ado) | rf %in% 0, NA)
psid_abridged$rel_chi_rep <- set_label(chi_rep, var_label("rel_chi_rep"))
