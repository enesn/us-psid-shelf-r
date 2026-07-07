# =====================================================================
# R/generate/covid_19.R
# Derives: df_covid_univ_* (who answered the COVID module) + the combined
# per-person version of every COVID measure (gated by df_covid_univ).
# =====================================================================

.n <- nrow(psid_abridged)
# NB: must NOT be named `gc` — this file is sourced with local = FALSE, so a
# `gc` here would shadow base::gc in .GlobalEnv and turn every gc()/.safe_gc()
# call in 07-publish.R into a silent no-op (defeating the publish-stage OOM
# guards). Use a private name like the other generate files (.gcol etc.).
.gcy <- function(stub, y) psid_abridged[[paste0(stub, "_", y)]]

# df_covid_univ — respondent universe for the COVID module
gen_tv("df_covid_univ", function(y) {
  rel <- .gcy("rel_ext", y); sq <- .gcy("seqnum", y); rep_ind <- .gcy("df_covid_rep_ind", y)
  if (is.null(rel) || is.null(rep_ind)) return(NULL)   # COVID module not fielded
  notcur <- !inrange(rel, 100, 299) | !inrange(sq, 1, 20)
  case_when(
    inrange(rel, 100, 199) & inrange(sq, 1, 20) ~ 1,
    inrange(rel, 200, 299) & inrange(sq, 1, 20) ~ 2,
    notcur &  (rep_ind %in% 1)                  ~ 3,
    notcur & !(rep_ind %in% 1)                  ~ 0,
    .default = -1)
})

# combined per-person measure, picked by the universe flag
covid_combine <- function(measure) {
  set <- set_for(measure)
  for (y in year) {
    rp <- .gcy(paste0(measure, "_rp"), y); sp <- .gcy(paste0(measure, "_sp"), y); ind <- .gcy(paste0(measure, "_ind"), y)
    if (is.null(rp) && is.null(sp) && is.null(ind)) next
    univ <- .gcy("df_covid_univ", y)
    out <- rep(-1, .n)
    if (!is.null(rp))  out <- rc(out, out %in% -1 & univ %in% 1 & !is.na(rp), rp)
    if (!is.null(sp))  out <- rc(out, out %in% -1 & univ %in% 2 & !is.na(sp), sp)
    if (!is.null(ind)) out <- rc(out, out %in% -1 & univ %in% c(1, 2, 3), ind)
    out <- rc(out, univ %in% 0, NA)
    out <- rc(out, out %in% -1, NA)   # no source for this univ (e.g. univ==3 with no _ind variant) -> NA, never an unassigned -1
    .GlobalEnv$psid_abridged[[paste0(measure, "_", y)]] <- g_label(out, measure, y, set)
  }
}

for (m in c("covid_test","covid_medi_talk_any","covid_medi_talk_opi","covid_medi_diag_mo",
            "covid_medi_diag_yr","covid_medi_nodi_sym","covid_medi_nodi_mo","covid_medi_nodi_yr",
            "covid_check_test","covid_test_rece_mo","covid_test_rece_yr","covid_test_rece_typ",
            "covid_test_rece_res","covid_test_ling_any","covid_test_ling_typ","covid_test_ling_sev",
            "covid_check_diag","covid_diag_hosp_any","covid_diag_hosp_num","covid_diag_hosp_oxy",
            "covid_diag_hosp_icu","covid_diag_hosp_ven","covid_diag_hosp_oth","covid_diag_noho_sym",
            "covid_diag_noho_sev"))
  covid_combine(m)
