# =====================================================================
# R/collect/demographics.R
# Builds: demo_sex, demo_age_rep_*, demo_age_gen_psid_*, demo_birth_month_sy_*,
#         demo_birth_year_sy_*, demo_death_year_psid
# =====================================================================

# (4.1) demo_sex  [ER32000]
psid_abridged <- collect_inv(psid_abridged, "demo_sex", function(x) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, inlist(x, 2), 2)
  out <- rc(out, inlist(x, 9), NA)
  out
})

# (4.2) demo_age_rep  — age reported at interview (1968–present)
psid_abridged <- collect_tv(psid_abridged, "demo_age_rep", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1),         1)     # 1 or newborn
  out <- rc(out, inrange(x, 2, 110),   x)     # actual age 2..110
  out <- rc(out, inlist(x, 999),       NA)    # NA, DK
  out <- rc(out, inlist(x, 0),         NA)    # nonresponse this wave
  out
})

# (4.3) demo_age_gen_psid — age generated from birth date, PSID version (1983–1992)
psid_abridged <- collect_tv(psid_abridged, "demo_age_gen_psid", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 1),        1)
  out <- rc(out, inrange(x, 2, 97),   x)
  out <- rc(out, inlist(x, 98),       98)     # 98 or older
  out <- rc(out, inlist(x, 99),       NA)
  out <- rc(out, inlist(x, 0),        NA)
  out
})

# (4.4) demo_birth_month_sy — birth month, time-varying (1983–present)
psid_abridged <- collect_tv(psid_abridged, "demo_birth_month_sy", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 98),   x)
  out <- rc(out, inlist(x, 0, 99),    NA)
  out
})

# (4.5) demo_birth_year_sy — birth year, time-varying (1983–present)
psid_abridged <- collect_tv(psid_abridged, "demo_birth_year_sy", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 9998), x)
  out <- rc(out, inlist(x, 0, 9999),  NA)
  out
})

# (4.6) demo_death_year_psid — death year, PSID version  [single input]
psid_abridged <- collect_inv(psid_abridged, "demo_death_year_psid", function(x) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1967, 2099), x)                          # specific year
  out <- rc(out, inrange(x, 1, 1966) | inrange(x, 2100, 9998), x)    # range of years
  out <- rc(out, inlist(x, 9999), 9999)                              # NA year of death
  out <- rc(out, inlist(x, 0),    0)                                 # not deceased
  out
})
