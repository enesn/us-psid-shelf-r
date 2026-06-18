# =====================================================================
# R/collect/race_ethnicity.R
# Builds: eth_only_span_{rp,sp}_*, race_only_{1m,2m,3m,4m}_{rp,sp}_*
# All race_only_* slots share one year-gated scheme; the year ranges naturally
# select (slots that don't exist in early waves simply have no input there).
# =====================================================================

# Spanish/Hispanic ethnicity
eth_span <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 0), 0); out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, inlist(x, 2), 2); out <- rc(out, inlist(x, 3), 3)
  out <- rc(out, inlist(x, 4), 4); out <- rc(out, inlist(x, 5), 5)
  out <- rc(out, inlist(x, 7), 6); out <- rc(out, inlist(x, 6), 7)
  rc(out, inlist(x, 9) | is.na(x), NA)
}

# race scheme (PSID race coding changed across eras)
race_scheme <- function(x, y) {
  out <- rep(-1, length(x))
  if (y >= 1968 && y <= 1984) {
    out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 2), 2)
    out <- rc(out, inlist(x, 7), 7); out <- rc(out, inlist(x, 3), 8)
    out <- rc(out, inlist(x, 8), 9)
    out <- rc(out, inlist(x, 0, 9) | is.na(x), NA)
  } else if (y >= 1985 && y <= 1993) {
    out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 2), 2)
    out <- rc(out, inlist(x, 3), 3); out <- rc(out, inlist(x, 4), 6)
    out <- rc(out, inlist(x, 6, 7), 7); out <- rc(out, inlist(x, 5), 8)
    out <- rc(out, inlist(x, 8), 9)
    out <- rc(out, inlist(x, 0, 9) | is.na(x), NA)
  } else if (y >= 1994 && y <= 2003) {
    out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 2), 2)
    out <- rc(out, inlist(x, 3), 3); out <- rc(out, inlist(x, 4), 6)
    out <- rc(out, inlist(x, 6, 7), 7); out <- rc(out, inlist(x, 5), 8)
    out <- rc(out, inlist(x, 0, 8, 9) | is.na(x), NA)
  } else {  # 2005+
    out <- rc(out, inlist(x, 1), 1); out <- rc(out, inlist(x, 2), 2)
    out <- rc(out, inlist(x, 3), 3); out <- rc(out, inlist(x, 4), 4)
    out <- rc(out, inlist(x, 5), 5); out <- rc(out, inlist(x, 7), 7)
    out <- rc(out, inlist(x, 0, 9) | is.na(x), NA)
  }
  out
}

for (v in c("eth_only_span_rp", "eth_only_span_sp"))
  psid_abridged <- collect_tv(psid_abridged, v, eth_span)

for (slot in c("1m", "2m", "3m", "4m"))
  for (who in c("rp", "sp"))
    psid_abridged <- collect_tv(psid_abridged,
                                sprintf("race_only_%s_%s", slot, who), race_scheme)
