# =====================================================================
# R/collect/disability.R
# 74 ADL/IADL items. Three recode shapes:
#   bin5      5->0, 1->1, 8/9->NA, 0/.->NA              (default, 54 vars)
#   bin5_w7   5->0, 1->1, 7->9, 8/9->NA, 0/.->NA        (iadl_q*_any_*, 18 vars)
#   bin5_n89  5->0, 1->1, 0/.->NA                       (adl_sum_any_ind,
#                                                         iadl_sum_any_ind)
# =====================================================================

bin5     <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, c(8, 9, 0, NA) ~ NA)
bin5_w7  <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, 7 ~ 9, c(8, 9, 0, NA) ~ NA)
bin5_n89 <- function(x, y) recode(x, 5 ~ 0, 1 ~ 1, c(0, NA) ~ NA)

all_vars <- c(
  paste0("adl_q", rep(1:7, each = 3), "_any_", c("ind","rp","sp")),
  paste0("adl_q", rep(1:7, each = 2), "_hlp_", c("rp","sp")),
  "adl_sum_any_ind", "adl_sum_hlp_ind",
  paste0("iadl_q", rep(1:6, each = 3), "_any_", c("ind","rp","sp")),
  paste0("iadl_q", rep(1:6, each = 3), "_hea_", c("ind","rp","sp")),
  "iadl_sum_any_ind")

w7_vars  <- paste0("iadl_q", rep(1:6, each = 3), "_any_", c("ind","rp","sp"))
n89_vars <- c("adl_sum_any_ind", "iadl_sum_any_ind")

for (v in all_vars) {
  fn <- if (v %in% w7_vars) bin5_w7 else if (v %in% n89_vars) bin5_n89 else bin5
  psid_abridged <- collect_tv(psid_abridged, v, fn)
}
