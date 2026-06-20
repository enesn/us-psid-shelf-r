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
  sp <- span %in% 1:7    # "is Spanish"; NA span -> FALSE (Stata inrange(.)==0), so a
                         # known race with unknown ethnicity still classifies (not -1)
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
      if (is.null(span)) span <- rep(NA_real_, .n)   # ethnicity not asked this wave -> not Spanish
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

# multi-mention per-wave classifications, per role: pick the highest-priority
# race/ethnicity present across mentions 1-4 (Stata Step_06 file 09).
#   15-cat (race_eth_mm_ext) priority high->low: 9,10,11,12,13,14,15,8,2,3,4,5,6,7,1
#   4-cat  (race_eth_mm)      priority high->low: 4 (Hispanic) >2 (Black) >3 (other) >1 (White)
ext_prio <- c(1, 7, 6, 5, 4, 3, 2, 8, 15, 14, 13, 12, 11, 10, 9)   # low->high; assign in order, last wins
for (role in c("rp", "sp")) {
  ments <- function(y) lapply(1:4, function(k) {
    z <- rc_col(sprintf("race_eth_%dm_ext_%s", k, role), y)
    if (is.null(z)) rep(NA_real_, .n) else z
  })
  gen_tv(paste0("race_eth_mm_ext_", role), function(y) {
    if (is.null(rc_col(sprintf("race_eth_1m_ext_%s", role), y))) return(NULL)
    e <- ments(y); has <- function(v) Reduce(`|`, lapply(e, function(z) z %in% v))
    out <- rep(-1, .n)
    for (v in ext_prio) out[has(v)] <- v
    out[Reduce(`&`, lapply(e, is.na))] <- NA
    out
  }, "raceethnicity_15cat")
  gen_tv(paste0("race_eth_mm_", role), function(y) {
    if (is.null(rc_col(sprintf("race_eth_1m_ext_%s", role), y))) return(NULL)
    e <- ments(y); has <- function(v) Reduce(`|`, lapply(e, function(z) z %in% v))
    out <- rep(-1, .n)
    out[has(1)] <- 1; out[has(3:7)] <- 3; out[has(2)] <- 2; out[has(8:15)] <- 4
    out[Reduce(`&`, lapply(e, is.na))] <- NA
    out
  }, "raceeth_4cat")
}

# combined per-person versions (RP's for the RP, SP's for the SP)
for (k in 1:4) combine_rpsp(sprintf("race_eth_%dm_ext", k))
combine_rpsp("race_eth"); combine_rpsp("race_eth_ext")
combine_rpsp("race_eth_mm"); combine_rpsp("race_eth_mm_ext")

# majority across waves (modal, recency tie-break) — each modes over its own
# per-wave stub (Stata uses race_eth_ext/race_eth/race_eth_mm_ext/race_eth_mm).
attach_maj <- function(name, stub) psid_abridged[[name]] <<- .attach_vl(
  set_label(modal_recent(stub), var_label(name)), set_for(name))
attach_maj("race_eth_maj",        "race_eth_ext")
attach_maj("race_eth_maj_col",    "race_eth")
attach_maj("race_eth_mm_maj",     "race_eth_mm_ext")
attach_maj("race_eth_mm_maj_col", "race_eth_mm")
