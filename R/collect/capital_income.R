# =====================================================================
# R/collect/capital_income.R
# Builds: capital_*_nd_{rc,rp,sp}_*  (nominal-dollar asset/capital income:
# asset parts of business & farm income, rent, dividends, interest, trust
# funds & royalties, and the era-combined rent+div+interest(+trusts) totals).
# PSID dollar top-codes grow across eras; mapped to the standard 9999999.
# Business/farm/rent asset income can be a net loss: the negative range
# (incl. its loss top-code, e.g. -9999/-99999/-999999) passes through.
# In 1994-95 the business top-code means "Latino sample family" and in 1997
# "Immigrant sample family" (amounts not calculated for that sample) -> NA.
# Pre-1976 amounts are 9-category bracket codes, kept with 9 (NA) -> missing.
# =====================================================================

# --- asset part of business & farm income, head+spouse combined (1970-1992) ---
psid_abridged <- collect_tv(psid_abridged, "capital_business_income_nd_rc", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1980) recode(x, -9999 %..% 99998 ~ keep,   99999 ~ 9999999)
    else                recode(x, -99999 %..% 999998 ~ keep, 999999 ~ 9999999)  # 1981-92
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "capital_farm_income_nd_rc", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1980) recode(x, -9999 %..% 99998 ~ keep,   99999 ~ 9999999)
    else                recode(x, -99999 %..% 999998 ~ keep, 999999 ~ 9999999)  # 1981-92
  rc(out, is.na(x), NA)
})

# --- asset part of business income, by role (rp 1993+, sp 1985+) ---
psid_abridged <- collect_tv(psid_abridged, "capital_business_income_nd_rp", function(x, y) {
  out <-
    if (y == 1993)      recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ 9999999)
    else if (y == 1994) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ NA)
    else if (y == 1995) recode(x, -99999 %..% 9999998 ~ keep,  9999999 ~ NA)
    else if (y == 1996) recode(x, -99999 %..% 9999998 ~ keep)
    else if (y == 1997) recode(x, -99999 %..% 9999998 ~ keep,  9999999 ~ NA)
    else if (y <= 2009) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ 9999999)
    else                recode(x, -999997 %..% 9999997 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "capital_business_income_nd_sp", function(x, y) {
  out <-
    if (y <= 1992)      recode(x, -99999 %..% 999998 ~ keep,  999999 ~ 9999999)   # 1985-92
    else if (y == 1993) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) recode(x, -99999 %..% 9999998 ~ keep,  9999999 ~ NA)
    else if (y == 1996) recode(x, -999999 %..% 9999998 ~ keep)
    else if (y == 1997) recode(x, -99999 %..% 9999998 ~ keep,  9999999 ~ NA)
    else if (y <= 2009) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ 9999999)
    else                recode(x, -999997 %..% 9999997 ~ keep)
  rc(out, is.na(x), NA)
})

# --- rent + dividends + interest (+ trust funds) combined (the pre-1984
# --- one-question era; rp also fielded 1983 as a 6-digit frame) ---
psid_abridged <- collect_tv(psid_abridged, "capital_rent_div_intst_trst_income_nd_rp", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1982) recode(x, -9999 %..% 99998 ~ keep,   99999 ~ 9999999)
    else                recode(x, -99999 %..% 999998 ~ keep, 999999 ~ 9999999)  # 1983
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "capital_rent_div_intst_trst_income_nd_sp", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1983) recode(x, -9999 %..% 99998 ~ keep,   99999 ~ 9999999)
    else                recode(x, -99999 %..% 999998 ~ keep, 999999 ~ 9999999)  # 1984-92
  rc(out, is.na(x), NA)
})

# --- dividends + interest + trusts combined (rp only, 1984-1992) ---
psid_abridged <- collect_tv(psid_abridged, "capital_div_intst_trst_income_nd_rp", function(x, y) {
  rc(recode(x, 1 %..% 999998 ~ keep, 0 ~ keep, 999999 ~ 9999999), is.na(x), NA)
})

# --- rent (net; can be a loss), separate question from 1984 ---
psid_abridged <- collect_tv(psid_abridged, "capital_rental_income_nd_rp", function(x, y) {
  out <-
    if (y <= 2009) recode(x, -99999 %..% 999998 ~ keep, 999999 ~ 9999999)  # 1984-93, 2005-09
    else           recode(x, -99997 %..% 999997 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "capital_rental_income_nd_sp", function(x, y) {  # 2005+
  out <-
    if (y <= 2009) recode(x, -99999 %..% 999998 ~ keep, 999999 ~ 9999999)
    else           recode(x, -99997 %..% 999997 ~ keep)
  rc(out, is.na(x), NA)
})

# --- dividends / interest / trust funds & royalties, separate questions
# --- (1993, then 2005+; identical code frames, sp is 5-digit in 1993) ---
capital_dit_rp <- function(x, y) {
  out <-
    if (y <= 2009) recode(x, 1 %..% 999998 ~ keep, 0 ~ keep, 999999 ~ 9999999)  # 1993, 2005-09
    else           recode(x, 1 %..% 999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
}
capital_dit_sp <- function(x, y) {
  out <-
    if (y == 1993) recode(x, 1 %..% 99998 ~ keep,  0 ~ keep, 99999 ~ 9999999)
    else if (y <= 2009) recode(x, 1 %..% 999998 ~ keep, 0 ~ keep, 999999 ~ 9999999)
    else           recode(x, 1 %..% 999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
}
for (.m in c("capital_div_income", "capital_intst_income", "capital_trst_royalties")) {
  psid_abridged <- collect_tv(psid_abridged, paste0(.m, "_nd_rp"), capital_dit_rp)
  psid_abridged <- collect_tv(psid_abridged, paste0(.m, "_nd_sp"), capital_dit_sp)
}
rm(capital_dit_rp, capital_dit_sp, .m)
