# =====================================================================
# R/revise/recode_not_in_fu.R
# For each wave, flag whether the individual is a current FU member (from seqnum,
# response_ext, fuid + 4 hand-coded overrides) and set every time-varying value
# to missing for person-years in which they are NOT a current member -- except
# the variables that describe (non-)membership itself.
# =====================================================================

.n <- nrow(psid_abridged)
infu <- c(0, 101, 102, 103, 104, 105)

# stubs that are NEVER recoded (they describe the (non-)membership)
no_rc_stub <- c("seqnum", "response", "response_ext",
                "panel_current", "panel_inst", "panel_move")
# 4 hand-coded "active response despite not being a current member" fixes
overrides <- list(c(1969, 694001, 71, 105, 972), c(1970, 5254021, 71, 103, 4078),
                  c(1988, 5055006, 72, 105, 7093), c(1996, 2133175, 71, 102, 7531))

idv <- psid_abridged$id
flags <- list()
for (y in year) {
  fu  <- psid_abridged[[paste0("fuid_", y)]]
  rex <- psid_abridged[[paste0("response_ext_", y)]]
  sq  <- psid_abridged[[paste0("seqnum_", y)]]
  miss_fu <- is.na(fu)
  f <- rep(-1, .n)
  if (y == 1968) {
    f <- rc(f, !(rex %in% infu) | miss_fu, 0)
    f <- rc(f,  (rex %in% infu) & !miss_fu, 1)
  } else {
    in_seq <- inrange(sq, 1, 20) | inrange(sq, 51, 59)
    f <- rc(f, sq %in% 0 & (!(rex %in% infu) | miss_fu), 0)
    f <- rc(f, sq %in% 0 &  (rex %in% infu) & !miss_fu, 1)
    f <- rc(f, in_seq & (!(rex %in% infu) | miss_fu), 0)
    f <- rc(f, in_seq &  (rex %in% infu) & !miss_fu, 1)
    f <- rc(f, !(sq %in% 0) & !in_seq & (!(rex %in% infu) | miss_fu), 0)
  }
  for (ov in overrides) if (y == ov[1])
    f[idv %in% ov[2] & sq %in% ov[3] & rex %in% ov[4] & fu %in% ov[5]] <- 0
  flags[[as.character(y)]] <- f
}

# recode: set value to NA where the person is not a current member that wave
wave_rx <- paste0("_(", paste(year, collapse = "|"), ")$")
tv_cols <- grep(wave_rx, names(psid_abridged), value = TRUE)
n_recoded <- 0L
for (col in tv_cols) {
  y    <- as.integer(sub(paste0(".*", wave_rx), "\\1", col))
  stub <- sub(wave_rx, "", col)
  if (stub %in% no_rc_stub) next
  f <- flags[[as.character(y)]]
  if (is.null(f)) next
  psid_abridged[[col]][which(f %in% 0)] <- NA
  n_recoded <- n_recoded + 1L
}
message("  recode_not_in_fu: recoded ", n_recoded, " time-varying columns")
