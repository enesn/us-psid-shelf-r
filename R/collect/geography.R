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
  out <- rep(-1, length(x))
  if (y >= 1968 && y <= 1969) {
    out <- rc(out, inrange(x, 1, 4), x)
    out <- rc(out, is.na(x), NA)
  } else if (y >= 1970 && y <= 1997) {
    out <- rc(out, inrange(x, 1, 6), x); out <- rc(out, inlist(x, 9) | is.na(x), NA)
  } else if (y == 1999) {
    out <- rc(out, inrange(x, 1, 6), x)
    out <- rc(out, inlist(x, 9, 0) | is.na(x), NA)
  } else {  # 2001+
    out <- rc(out, inrange(x, 1, 6), x); out <- rc(out, inlist(x, 9) | is.na(x), NA)
  }
  out
})

# geo_state — state of FU
psid_abridged <- collect_tv(psid_abridged, "geo_state", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 51), x)
  out <- rc(out, inlist(x, 0), 800)
  out <- rc(out, inlist(x, 99) | is.na(x), NA)
  out
})

# geo_metro — FU in a metropolitan area? (2015–present)
psid_abridged <- collect_tv(psid_abridged, "geo_metro", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 2), 0)
  out <- rc(out, inlist(x, 1), 1)
  out <- rc(out, inlist(x, 0), 9)
  out <- rc(out, inlist(x, 9) | is.na(x), NA)
  out
})

# cgeo_region_rp — region of RP's birth/childhood
psid_abridged <- collect_tv(psid_abridged, "cgeo_region_rp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 6), x)
  out <- rc(out, inlist(x, 9) | is.na(x), NA)
  out
})

# cgeo_region_sp — region of SP's birth/childhood (1976–present)
psid_abridged <- collect_tv(psid_abridged, "cgeo_region_sp", function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 6), x)
  out <- rc(out, inlist(x, 9, 0) | is.na(x), NA)
  out
})

# cgeo_state_rp — state/country of RP's birth/childhood
psid_abridged <- collect_tv(psid_abridged, "cgeo_state_rp", function(x, y) {
  out <- rep(-1, length(x))
  if (y == 1968) {
    out <- rc(out, inrange(x, 1, 51), x)
    out <- rc(out, inlist(x, 61), 801); out <- rc(out, inlist(x, 62), 802)
    out <- rc(out, inlist(x, 63), 803); out <- rc(out, inlist(x, 64), 804)
    out <- rc(out, inlist(x, 65), 805); out <- rc(out, inlist(x, 66), 806)
    out <- rc(out, inlist(x, 99) | is.na(x), NA)
  } else if (y >= 1969 && y <= 1996) {
    out <- rc(out, inrange(x, 1, 51), x); out <- rc(out, inlist(x, 0), 800)
    out <- rc(out, inlist(x, 99) | is.na(x), NA)
  } else {  # 1997+
    out <- fips2psid(out, x)
    out <- rc(out, inlist(x, 99) | is.na(x), NA)
  }
  out
})

# cgeo_state_sp — state/country of SP's birth/childhood (1976–present)
psid_abridged <- collect_tv(psid_abridged, "cgeo_state_sp", function(x, y) {
  out <- rep(-1, length(x))
  if (y >= 1976 && y <= 1996) {
    out <- rc(out, inrange(x, 1, 51), x); out <- rc(out, inlist(x, 0), 800)
    out <- rc(out, inlist(x, 99) | is.na(x), NA)
  } else {  # 1997+
    out <- fips2psid(out, x)
    out <- rc(out, inlist(x, 99) | is.na(x), NA)
  }
  out
})
