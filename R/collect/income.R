# =====================================================================
# R/collect/income.R
# Builds: {farm,business,taxable}_*_nd_rc_*  (nominal-dollar FU-level income:
# farm gross revenue & expenses, unincorporated business net profit (raw) and
# income (assigned), farm income, head+spouse taxable income).
# PSID dollar top-codes grow across eras and are kept at their raw codebook
# value, marked with `topcode` (passthrough + registered, see R/programs.R).
# Net-income variables can be losses: the negative range incl. a magnitude
# loss top-code ("Loss of $X or more") passes through; the distinct
# "Loss, NA/DK how much" code (a loss of unknown size) -> NA.
# The raw questionnaire amounts (gross revenue, expenses, net profit) carry
# explicit DK / NA-refused codes -> NA, like labor_gig_income.
# In 1994-95 the assigned-variable top-code means "Latino sample family"
# (amounts not calculated for that sample) -> NA.
# Pre-1983 farm gross revenue is a 9-category bracket code, kept with
# 9 (NA) -> missing.
# NOTE: business_grossrevenue_nd_rc / business_expense_nd_rc are specced in
# publish_vars/var_labels but have no input_var_map entries yet; they are not
# collected here until their inputs are mapped.
# =====================================================================

# --- farm gross revenue & expenses (raw questionnaire amounts) ---
psid_abridged <- collect_tv(psid_abridged, "farm_grossrevenue_nd_rc", function(x, y) {
  out <-
    if (y <= 1982)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y == 1983) recode(x, 1 %..% 99997 ~ keep,   0 ~ keep, 99998 ~ topcode,   99999 ~ NA)
    else if (y <= 1992) recode(x, 1 %..% 999997 ~ keep,  0 ~ keep, 999998 ~ topcode,  999999 ~ NA)
    else if (y == 1993) recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep, 9999998 ~ topcode, 9999999 ~ NA)
    else                recode(x, 1 %..% 9999996 ~ keep, 0 ~ keep, 9999997 ~ topcode,
                               c(9999998, 9999999) ~ NA)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "farm_expense_nd_rc", function(x, y) {  # 1994+
  out <- recode(x, 1 %..% 9999996 ~ keep, 0 ~ keep, 9999997 ~ topcode,
                c(9999998, 9999999) ~ NA)
  rc(out, is.na(x), NA)
})

# --- unincorporated business net profit (raw, 2003+) ---
psid_abridged <- collect_tv(psid_abridged, "business_netprofit_nd_rc", function(x, y) {
  out <- recode(x, -999998 %..% 9999996 ~ keep, 9999997 ~ topcode,
                c(-999999, 9999998, 9999999) ~ NA)   # loss-NA/DK-how-much, DK, NA/refused
  rc(out, is.na(x), NA)
})

# --- business & farm income (assigned; 1993+) ---
psid_abridged <- collect_tv(psid_abridged, "business_income_nd_rc", function(x, y) {
  out <-
    if (y == 1993)      recode(x, -999998 %..% 9999998 ~ keep, -999999 ~ NA, 9999999 ~ topcode)
    else if (y <= 1995) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ NA)
    else if (y == 1996) recode(x, -99999 %..% 9999998 ~ keep)
    else if (y == 1997) recode(x, -99999 %..% 999998 ~ keep)
    else if (y <= 2009) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ topcode)
    else                recode(x, -999997 %..% 9999997 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "farm_income_nd_rc", function(x, y) {
  out <-
    if (y == 1993)      recode(x, -999998 %..% 9999998 ~ keep, -999999 ~ NA, 9999999 ~ topcode)
    else if (y == 1994) recode(x, -99999 %..% 999998 ~ keep, 999999 ~ NA)
    else if (y == 1995) recode(x, -9999 %..% 999998 ~ keep,  999999 ~ NA)
    else if (y == 1996) recode(x, -9999 %..% 999998 ~ keep)
    else if (y == 1997) recode(x, -99999 %..% 999998 ~ keep)
    else if (y == 1999) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ topcode)
    else if (y == 2001) recode(x, -99999 %..% 9999998 ~ keep,  9999999 ~ topcode)
    else if (y <= 2005) recode(x, -99999 %..% 999998 ~ keep,   999999 ~ topcode)
    else if (y <= 2009) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ topcode)
    else                recode(x, -999997 %..% 9999997 ~ keep)
  rc(out, is.na(x), NA)
})

# --- taxable income of head/RP + spouse ---
psid_abridged <- collect_tv(psid_abridged, "taxable_income_nd_rc", function(x, y) {
  out <-
    if (y <= 1978)      recode(x, -9999 %..% 99998 ~ keep,    99999 ~ topcode)
    else if (y <= 1980) recode(x, -99999 %..% 999998 ~ keep,  999999 ~ topcode)
    else if (y <= 1993) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ topcode)
    else if (y <= 1995) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ NA)
    else if (y == 1996) recode(x, -999999 %..% 9999998 ~ keep)
    else if (y == 1997) recode(x, -99998.99 %..% 999998.99 ~ keep)  # reported with cents
    else if (y <= 2009) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ topcode)
    else                recode(x, -999997 %..% 9999997 ~ keep)
  rc(out, is.na(x), NA)
})
