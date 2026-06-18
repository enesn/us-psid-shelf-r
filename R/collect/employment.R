# =====================================================================
# R/collect/employment.R
# Builds: emp_stat_{1m,2m,3m}_{rp,sp}_*, emp_stat_1m_ind_*  (employment status,
# 1..8 passthrough; era-specific wild codes recoded to missing).
# =====================================================================

mk <- function(extra_miss, na_zero_dot = TRUE) function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 8), x)
  out <- rc(out, x %in% extra_miss, NA)
  if (na_zero_dot) out <- rc(out, inlist(x, 0) | is.na(x), NA)
  else             out <- rc(out, inlist(x, 0), NA)   # 1m_ind: only 0 -> NA
  out
}

psid_abridged <- collect_tv(psid_abridged, "emp_stat_1m_rp",  mk(c(9, 98, 99, 22)))
psid_abridged <- collect_tv(psid_abridged, "emp_stat_2m_rp",  mk(c(9)))
psid_abridged <- collect_tv(psid_abridged, "emp_stat_3m_rp",  mk(c(9)))
psid_abridged <- collect_tv(psid_abridged, "emp_stat_1m_sp",  mk(c(9, 98, 99, 35, 32)))
psid_abridged <- collect_tv(psid_abridged, "emp_stat_2m_sp",  mk(c(9, 99)))
psid_abridged <- collect_tv(psid_abridged, "emp_stat_3m_sp",  mk(c(9)))
psid_abridged <- collect_tv(psid_abridged, "emp_stat_1m_ind", mk(c(9), na_zero_dot = FALSE))
