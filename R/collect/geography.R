# =====================================================================
# R/collect/geography.R
# Builds: geo_region_*, geo_state_*, geo_metro_*, cgeo_region_rp_*,
#         cgeo_region_sp_*, cgeo_state_rp_*, cgeo_state_sp_*
# =====================================================================

# FIPS state code (1997+) -> PSID alphabetical state code (1..51); 0 -> 800.
.fips2psid <- c(`1`=1,`4`=2,`5`=3,`6`=4,`8`=5,`9`=6,`10`=7,`11`=8,`12`=9,`13`=10,
                `16`=11,`17`=12,`18`=13,`19`=14,`20`=15,`21`=16,`22`=17,`23`=18,
                `24`=19,`25`=20,`26`=21,`27`=22,`28`=23,`29`=24,`30`=25,`31`=26,
                `32`=27,`33`=28,`34`=29,`35`=30,`36`=31,`37`=32,`38`=33,`39`=34,
                `40`=35,`41`=36,`42`=37,`44`=38,`45`=39,`46`=40,`47`=41,`48`=42,
                `49`=43,`50`=44,`51`=45,`53`=46,`54`=47,`55`=48,`56`=49,`2`=50,`15`=51)
fips2psid <- function(out, x) {
  for (f in names(.fips2psid)) out <- rc(out, inlist(x, as.numeric(f)), .fips2psid[[f]])
  out <- rc(out, inlist(x, 0), 800)
  out
}

# geo_region — Census region of FU
psid_abridged <- collect_tv(psid_abridged, "geo_region", function(x, y) {
  if (y >= 1968 && y <= 1969) recode(x, 1 %..% 4 ~ keep, NA ~ NA)
  else if (y == 1999)         recode(x, 1 %..% 6 ~ keep, c(9, 0, NA) ~ NA)
  else                        recode(x, 1 %..% 6 ~ keep, c(9, NA) ~ NA)  # 1970-1997, 2001+
})

# geo_state — state of FU
psid_abridged <- collect_tv(psid_abridged, "geo_state", function(x, y) recode(x,
  1 %..% 51 ~ keep, 0 ~ 800, c(99, NA) ~ NA))

# geo_metro — FU in a metropolitan area? (2015–present)
psid_abridged <- collect_tv(psid_abridged, "geo_metro", function(x, y) recode(x,
  2 ~ 0, 1 ~ 1, 0 ~ 9, c(9, NA) ~ NA))

# cgeo_region_rp — region of RP's birth/childhood
psid_abridged <- collect_tv(psid_abridged, "cgeo_region_rp", function(x, y) recode(x,
  1 %..% 6 ~ keep, c(9, NA) ~ NA))

# cgeo_region_sp — region of SP's birth/childhood (1976–present)
psid_abridged <- collect_tv(psid_abridged, "cgeo_region_sp", function(x, y) recode(x,
  1 %..% 6 ~ keep, c(9, 0, NA) ~ NA))

# cgeo_state_rp — state/country of RP's birth/childhood
psid_abridged <- collect_tv(psid_abridged, "cgeo_state_rp", function(x, y) {
  if (y == 1968)
    recode(x, 1 %..% 51 ~ keep,
           61 ~ 801, 62 ~ 802, 63 ~ 803, 64 ~ 804, 65 ~ 805, 66 ~ 806,
           c(99, NA) ~ NA)
  else if (y >= 1969 && y <= 1996)
    recode(x, 1 %..% 51 ~ keep, 0 ~ 800, c(99, NA) ~ NA)
  else  # 1997+: FIPS lookup, then 99/. -> NA
    rc(fips2psid(rep(-1, length(x)), x), inlist(x, 99) | is.na(x), NA)
})

# cgeo_state_sp — state/country of SP's birth/childhood (1976–present)
psid_abridged <- collect_tv(psid_abridged, "cgeo_state_sp", function(x, y) {
  if (y >= 1976 && y <= 1996)
    recode(x, 1 %..% 51 ~ keep, 0 ~ 800, c(99, NA) ~ NA)
  else  # 1997+
    rc(fips2psid(rep(-1, length(x)), x), inlist(x, 99) | is.na(x), NA)
})
