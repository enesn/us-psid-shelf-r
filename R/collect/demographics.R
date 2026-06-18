# =====================================================================
# R/collect/demographics.R
# Builds: demo_sex, demo_age_rep_*, demo_age_gen_psid_*, demo_birth_month_sy_*,
#         demo_birth_year_sy_*, demo_death_year_psid
# =====================================================================

# (4.1) demo_sex  [ER32000]
psid_abridged <- collect_inv(psid_abridged, "demo_sex", function(x) recode(x,
  1 ~ 1,
  2 ~ 2,
  9 ~ NA))

# (4.2) demo_age_rep  — age reported at interview (1968–present)
psid_abridged <- collect_tv(psid_abridged, "demo_age_rep", function(x, y) recode(x,
  1          ~ 1,    # 1 or newborn
  2 %..% 110 ~ keep, # actual age 2..110
  999        ~ NA,   # NA, DK
  0          ~ NA))  # nonresponse this wave

# (4.3) demo_age_gen_psid — age generated from birth date, PSID version (1983–1992)
psid_abridged <- collect_tv(psid_abridged, "demo_age_gen_psid", function(x, y) recode(x,
  1         ~ 1,
  2 %..% 97 ~ keep,
  98        ~ 98,    # 98 or older
  c(99, 0)  ~ NA))

# (4.4) demo_birth_month_sy — birth month, time-varying (1983–present)
psid_abridged <- collect_tv(psid_abridged, "demo_birth_month_sy", function(x, y) recode(x,
  1 %..% 98 ~ keep,
  c(0, 99)  ~ NA))

# (4.5) demo_birth_year_sy — birth year, time-varying (1983–present)
psid_abridged <- collect_tv(psid_abridged, "demo_birth_year_sy", function(x, y) recode(x,
  1 %..% 9998 ~ keep,
  c(0, 9999)  ~ NA))

# (4.6) demo_death_year_psid — death year, PSID version  [single input]
psid_abridged <- collect_inv(psid_abridged, "demo_death_year_psid", function(x) recode(x,
  1967 %..% 2099 ~ keep,  # specific year
  1 %..% 1966    ~ keep,  # range of years
  2100 %..% 9998 ~ keep,  # range of years
  9999           ~ 9999,  # NA year of death
  0              ~ 0))     # not deceased
