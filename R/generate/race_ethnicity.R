# =====================================================================
# R/generate/race_ethnicity.R
# Combines race (race_only_{1..4}m) with Spanish ethnicity (eth_only_span) into a
# 15-category race-ethnicity classification per role/year, collapses it to a
# 4-category version, combines RP/SP into per-person versions, and derives the
# time-invariant "majority across waves" classifications.
# =====================================================================

.n <- nrow(psid_abridged)
rc_col <- function(stub, y) psid_abridged[[paste0(stub, "_", y)]]

# race (1..7/9) x Spanish (1..7) -> 15-category extended classification
race_eth_ext_fn <- function(race, span, k) {
  sp <- inrange(span, 1, 7)
  out <- rep(-1, .n)
  out <- rc(out, race %in% 1 & !sp, 1); out <- rc(out, race %in% 2 & !sp, 2)
  out <- rc(out, race %in% 3 & !sp, 3); out <- rc(out, race %in% 4 & !sp, 4)
  out <- rc(out, race %in% 5 & !sp, 5); out <- rc(out, race %in% 6 & !sp, 6)
  out <- rc(out, race %in% c(7, 9) & !sp, 7)
  out <- rc(out, race %in% 1 & sp, 8);  out <- rc(out, race %in% 2 & sp, 9)
  out <- rc(out, race %in% 3 & sp, 10); out <- rc(out, race %in% 4 & sp, 11)
  out <- rc(out, race %in% 5 & sp, 12); out <- rc(out, race %in% 6 & sp, 13)
  out <- rc(out, race %in% c(7, 9) & sp, 14)
  if (k == 1) {
    out <- rc(out, (is.na(race) & sp) | race %in% 8, 15)
    out <- rc(out, is.na(race) & !sp, NA)
  } else {
    out <- rc(out, race %in% 8, 15)
    out <- rc(out, is.na(race), NA)
  }
  out
}

# per role/year: the 4 mention-specific 15-cat classifications + 4-cat + 15-cat
for (role in c("rp", "sp")) {
  for (k in 1:4)
    gen_tv(sprintf("race_eth_%dm_ext_%s", k, role), function(y) {
      race <- rc_col(sprintf("race_only_%dm_%s", k, role), y)
      if (is.null(race)) return(NULL)
      span <- rc_col(paste0("eth_only_span_", role), y)
      race_eth_ext_fn(race, span, k)
    })
  gen_tv(paste0("race_eth_", role), function(y) {
    e <- rc_col(paste0("race_eth_1m_ext_", role), y); if (is.null(e)) return(NULL)
    case_when(e %in% 1 ~ 1, e %in% 2 ~ 2, inrange(e, 3, 7) ~ 3, inrange(e, 8, 15) ~ 4, .default = NA_real_)
  }, "raceeth_4cat")
  gen_tv(paste0("race_eth_ext_", role), function(y) {
    e <- rc_col(paste0("race_eth_1m_ext_", role), y); if (is.null(e)) return(NULL); e
  })
}

# combined per-person versions (RP's for the RP, SP's for the SP)
for (k in 1:4) combine_rpsp(sprintf("race_eth_%dm_ext", k))
combine_rpsp("race_eth"); combine_rpsp("race_eth_ext")

# majority across waves (modal, recency tie-break) + 4-category collapse
collapse4 <- function(e) case_when(e %in% 1 ~ 1, e %in% 2 ~ 2, inrange(e, 3, 7) ~ 3,
                                   inrange(e, 8, 15) ~ 4, .default = NA_real_)
maj  <- modal_recent("race_eth_1m_ext")
psid_abridged$race_eth_maj      <- .attach_vl(set_label(maj, var_label("race_eth_maj")), set_for("race_eth_maj"))
psid_abridged$race_eth_maj_col  <- .attach_vl(set_label(collapse4(maj), var_label("race_eth_maj_col")), set_for("race_eth_maj_col"))
mmaj <- modal_recent("race_eth_2m_ext")
psid_abridged$race_eth_mm_maj     <- .attach_vl(set_label(mmaj, var_label("race_eth_mm_maj")), set_for("race_eth_mm_maj"))
psid_abridged$race_eth_mm_maj_col <- .attach_vl(set_label(collapse4(mmaj), var_label("race_eth_mm_maj_col")), set_for("race_eth_mm_maj_col"))
