# =====================================================================
# R/collect/race_ethnicity.R
# Builds: eth_only_span_{rp,sp}_*, race_only_{1m,2m,3m,4m}_{rp,sp}_*
# All race_only_* slots share one year-gated scheme; the year ranges naturally
# select (slots that don't exist in early waves simply have no input there).
# =====================================================================

# Spanish/Hispanic ethnicity
eth_span <- function(x, y) recode(x,
  0 ~ 0, 1 ~ 1, 2 ~ 2, 3 ~ 3, 4 ~ 4, 5 ~ 5, 7 ~ 6, 6 ~ 7,
  c(9, NA) ~ NA)

# race scheme (PSID race coding changed across eras)
race_scheme <- function(x, y) {
  if (y >= 1968 && y <= 1984)
    recode(x, 1 ~ 1, 2 ~ 2, 7 ~ 7, 3 ~ 8, 8 ~ 9, c(0, 9, NA) ~ NA)
  else if (y >= 1985 && y <= 1993)
    recode(x, 1 ~ 1, 2 ~ 2, 3 ~ 3, 4 ~ 6, c(6, 7) ~ 7, 5 ~ 8, 8 ~ 9, c(0, 9, NA) ~ NA)
  else if (y >= 1994 && y <= 2003)
    recode(x, 1 ~ 1, 2 ~ 2, 3 ~ 3, 4 ~ 6, c(6, 7) ~ 7, 5 ~ 8, c(0, 8, 9, NA) ~ NA)
  else  # 2005+
    recode(x, 1 ~ 1, 2 ~ 2, 3 ~ 3, 4 ~ 4, 5 ~ 5, 7 ~ 7, c(0, 9, NA) ~ NA)
}

for (v in c("eth_only_span_rp", "eth_only_span_sp"))
  psid_abridged <- collect_tv(psid_abridged, v, eth_span)

for (slot in c("1m", "2m", "3m", "4m"))
  for (who in c("rp", "sp"))
    psid_abridged <- collect_tv(psid_abridged,
                                sprintf("race_only_%s_%s", slot, who), race_scheme)
