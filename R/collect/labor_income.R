# =====================================================================
# R/collect/labor_income.R
# Builds: labor_*_income_nd_{rp,sp}_*  (nominal-dollar labor income: total labor
# income and its components -- wages, labor parts of business/farm income,
# professional practice, market gardening, bonuses, overtime, tips,
# commissions, extra jobs, miscellaneous job-related income, gig work).
# PSID dollar top-codes grow across eras; mapped to the standard 9999999.
# In 1994-95 the era top-code means "Latino sample family" and in 1997
# "Immigrant sample family" (amounts not calculated for that sample) -> NA.
# Pre-1976 wage/business/farm/professional amounts are 9-category bracket
# codes, kept as codes with 9 (NA) -> missing.
# labor_{wage,business,farm}_income_* read the same input variables as the
# earn_{wage,busi,farm}_nd_* series; their recodes mirror R/collect/earnings.R.
# =====================================================================

# --- total labor income (1968-1993; the 1994+ family files only publish the
# --- excl.-business/farm total, collected separately below) ---
psid_abridged <- collect_tv(psid_abridged, "labor_tot_income_nd_rp", function(x, y) {
  out <-
    if (y <= 1969)      recode(x, 1 %..% 99998 ~ keep,   0 ~ keep)
    else if (y <= 1975) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep,       99999 ~ 9999999)
    else if (y <= 1978) recode(x, 2 %..% 99998 ~ keep,   c(0, 1) ~ keep, 99999 ~ 9999999)
    else if (y <= 1982) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep,       99999 ~ 9999999)
    else if (y <= 1992) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep,       999999 ~ 9999999)
    else                recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep,       9999999 ~ 9999999)  # 1993
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_tot_income_nd_sp", function(x, y) {
  out <-
    if (y <= 1969)      recode(x, 1 %..% 99998 ~ keep,   0 ~ keep)
    else if (y <= 1983) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else if (y <= 1992) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else                recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)  # 1993
  rc(out, is.na(x), NA)
})

# --- total labor income excl. business & farm labor (sp starts 1993, rp 1994) ---
psid_abridged <- collect_tv(psid_abridged, "labor_tot_income_excbussfarm_nd_rp", function(x, y) {
  out <-
    if (y <= 1995)      recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep)
    else if (y == 1997) recode(x, 0 %..% 999998.99 ~ keep, 999999 ~ 9999999)  # reported with cents
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_tot_income_excbussfarm_nd_sp", function(x, y) {
  out <-
    if (y == 1993)      recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep)
    else if (y == 1997) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep)
    else if (y == 1999) recode(x, 1 %..% 9999996 ~ keep, 0 ~ keep, 9999997 ~ 9999999)
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})

# --- wages & salaries (rp mirrors earn_wage_nd_rp: sample-conditional wild
# --- codes in 1994-95 / 1997; sp is only published 2015+) ---
psid_abridged <- collect_tv(psid_abridged, "labor_wage_income_nd_rp", function(x, y, df) {
  out <-
    if (y <= 1969)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1982) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else if (y <= 1992) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y == 1993) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) { o <- recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep)
                          rc(o, x %in% 9999999 & df$sample %in% 3, NA) }
    else if (y == 1996) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y == 1997) { o <- recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep)
                          rc(o, x %in% 9999999 & df$sample %in% 4, NA) }
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_wage_income_nd_sp", function(x, y) {  # 2015+
  rc(recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep), is.na(x), NA)
})

# --- labor part of business & farm income (mirror earn_{busi,farm}_nd_*) ---
psid_abridged <- collect_tv(psid_abridged, "labor_business_income_nd_rp", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1992) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else if (y == 1993) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y == 1997) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y <= 2003) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_business_income_nd_sp", function(x, y) {
  out <-
    if (y == 1993)      recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else if (y <= 1995) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y == 1997) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y <= 2003) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_farm_income_nd_rp", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1992) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else                recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)  # 1993
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_farm_income_nd_sp", function(x, y) {  # 1993 only
  rc(recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999), is.na(x), NA)
})

# --- professional practice or trade (1993 reverts to a 5-digit code frame) ---
psid_abridged <- collect_tv(psid_abridged, "labor_profes_income_nd_rp", function(x, y) {
  out <-
    if (y <= 1975)      recode(x, 0 %..% 8 ~ keep, 9 ~ NA)
    else if (y <= 1982) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else if (y <= 1992) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y == 1993) recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else if (y <= 1995) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep)
    else if (y == 1997) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ NA)
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_profes_income_nd_sp", function(x, y) {  # 2015+
  rc(recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep), is.na(x), NA)
})

# --- market gardening (rp only, 1993-2013; can be a net loss -- the negative
# --- range incl. its loss top-code passes through, like finc_tot_nd) ---
psid_abridged <- collect_tv(psid_abridged, "labor_garden_income_nd_rp", function(x, y) {
  out <-
    if (y == 1993)      recode(x, -9999 %..% 99998 ~ keep, 99999 ~ 9999999)
    else if (y == 1994) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ NA)
    else if (y == 1995) recode(x, -99999 %..% 9999998 ~ keep, 9999999 ~ NA)
    else if (y == 1996) recode(x, -999999 %..% 9999998 ~ keep)
    else if (y == 1997) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ NA)
    else if (y <= 2009) recode(x, -999999 %..% 9999998 ~ keep, 9999999 ~ 9999999)
    else                recode(x, -999997 %..% 9999997 ~ keep)
  rc(out, is.na(x), NA)
})

# --- bonuses / overtime / tips / commissions / miscellaneous job-related
# --- income: identical 6-digit code frames (rp 1993-2023, sp 2015-2023) ---
labor_comp6_rp <- function(x, y) {
  out <-
    if (y == 1993)      recode(x, 1 %..% 999998 ~ keep, 0 ~ keep, 999999 ~ 9999999)
    else if (y <= 1995) recode(x, 1 %..% 999998 ~ keep, 0 ~ keep, 999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 999998 ~ keep, 0 ~ keep)
    else if (y == 1997) recode(x, 1 %..% 999998 ~ keep, 0 ~ keep, 999999 ~ NA)
    else if (y <= 2009) recode(x, 1 %..% 999998 ~ keep, 0 ~ keep, 999999 ~ 9999999)
    else                recode(x, 1 %..% 999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
}
labor_comp6_sp <- function(x, y) {  # 2015+
  rc(recode(x, 1 %..% 999997 ~ keep, 0 ~ keep), is.na(x), NA)
}
for (.m in c("labor_bonus_income", "labor_overtime_income", "labor_tips_income",
             "labor_comissions_income", "labor_miscellaneous_income")) {
  psid_abridged <- collect_tv(psid_abridged, paste0(.m, "_nd_rp"), labor_comp6_rp)
  psid_abridged <- collect_tv(psid_abridged, paste0(.m, "_nd_sp"), labor_comp6_sp)
}
rm(labor_comp6_rp, labor_comp6_sp, .m)

# --- extra / second jobs (5-digit frame in 1993, 7-digit from 2003) ---
psid_abridged <- collect_tv(psid_abridged, "labor_extrawork_income_nd_rp", function(x, y) {
  out <-
    if (y == 1993)      recode(x, 1 %..% 99998 ~ keep,   0 ~ keep, 99999 ~ 9999999)
    else if (y <= 1995) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y == 1996) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep)
    else if (y == 1997) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ NA)
    else if (y <= 2001) recode(x, 1 %..% 999998 ~ keep,  0 ~ keep, 999999 ~ 9999999)
    else if (y <= 2009) recode(x, 1 %..% 9999998 ~ keep, 0 ~ keep, 9999999 ~ 9999999)
    else                recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_extrawork_income_nd_sp", function(x, y) {  # 2015+
  rc(recode(x, 1 %..% 9999997 ~ keep, 0 ~ keep), is.na(x), NA)
})

# --- gig work (2021+; raw amount with DK/NA codes, unlike the assigned vars) ---
psid_abridged <- collect_tv(psid_abridged, "labor_gig_income_nd_rp", function(x, y) {
  out <- recode(x, 1 %..% 9999996 ~ keep, 0 ~ keep, 9999997 ~ 9999999,
                c(9999998, 9999999) ~ NA)
  rc(out, is.na(x), NA)
})
psid_abridged <- collect_tv(psid_abridged, "labor_gig_income_nd_sp", function(x, y) {
  out <- recode(x, 1 %..% 9999996 ~ keep, 0 ~ keep, 9999997 ~ 9999999,
                c(9999998, 9999999) ~ NA)
  rc(out, is.na(x), NA)
})

# --- twin consistency check: these five series read the same input variables
# --- as the earnings domain and must be identical wave-for-wave (see header) ---
for (.tw in list(c("labor_wage_income_nd_rp",     "earn_wage_nd_rp"),
                 c("labor_business_income_nd_rp", "earn_busi_nd_rp"),
                 c("labor_business_income_nd_sp", "earn_busi_nd_sp"),
                 c("labor_farm_income_nd_rp",     "earn_farm_nd_rp"),
                 c("labor_farm_income_nd_sp",     "earn_farm_nd_sp"))) {
  .bad <- 0L
  for (.y in year) {
    a <- psid_abridged[[paste0(.tw[1], "_", .y)]]
    b <- psid_abridged[[paste0(.tw[2], "_", .y)]]
    if (is.null(a) && is.null(b)) next
    if (is.null(a) != is.null(b) || !identical(as.vector(a), as.vector(b))) .bad <- .bad + 1L
  }
  if (.bad) warning("labor_income twin check: ", .tw[1], " != ", .tw[2], " in ", .bad, " wave(s)")
  else message("  twin check: ", .tw[1], " == ", .tw[2], " in every shared wave")
}
rm(.tw, .bad, .y, a, b)
