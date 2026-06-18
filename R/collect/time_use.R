# =====================================================================
# R/collect/time_use.R
# Builds: time_{acar,ccar,educ,hous,leis,pers,shop,volu,work}_{rp,sp}_*,
#         if_time_hous_{rp,sp}_*
# =====================================================================

# care hours: passthrough 1..168, 0 -> 0, 998/999/. -> NA
time_care <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 168), x)
  out <- rc(out, inlist(x, 0), 0)
  rc(out, inlist(x, 998, 999) | is.na(x), NA)
}
# generic hours with 112 top-code: passthrough 1..111, 0 -> 0, 112 -> 112
time_hrs <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inrange(x, 1, 111), x)
  out <- rc(out, inlist(x, 0), 0)
  out <- rc(out, inlist(x, 112), 112)
  rc(out, inlist(x, 998, 999) | is.na(x), NA)
}
# housework flag
if_hous <- function(x, y) {
  out <- rep(-1, length(x))
  out <- rc(out, inlist(x, 0), 0); out <- rc(out, inlist(x, 1), 1)
  rc(out, is.na(x), NA)
}
# housework hours (coding scheme changed across eras)
time_hous_fn <- function(x, y) {
  out <- rep(-1, length(x))
  if (y == 1976) {
    out <- rc(out, inlist(x, 0), 0); out <- rc(out, inlist(x, 1), 1)
    out <- rc(out, inrange(x, 2, 97), x); out <- rc(out, inlist(x, 98), 98)
    out <- rc(out, inlist(x, 99) | is.na(x), NA)
  } else if (y >= 1977 && y <= 1981) {
    out <- rc(out, inlist(x, 0), 0); out <- rc(out, inrange(x, 1, 97), x)
    out <- rc(out, inlist(x, 98), 98)
    out <- rc(out, inlist(x, 99) | is.na(x), NA)
  } else if (y >= 1983 && y <= 1993) {
    out <- rc(out, inlist(x, 0), 0); out <- rc(out, inlist(x, 1), 1)
    out <- rc(out, inrange(x, 2, 97), x); out <- rc(out, inlist(x, 98), 98)
    out <- rc(out, inlist(x, 99) | is.na(x), NA)
  } else if (y == 1994) {
    out <- rc(out, inlist(x, 0), 0); out <- rc(out, inlist(x, 1), 1)
    out <- rc(out, inrange(x, 2, 111), x); out <- rc(out, inlist(x, 112), 112)
    out <- rc(out, inlist(x, 998, 999) | is.na(x), NA)
  } else if (y >= 1995 && y <= 2009) {
    out <- rc(out, inlist(x, 0), 0); out <- rc(out, inrange(x, 0.1, 111), x)
    out <- rc(out, inlist(x, 112), 112)
    out <- rc(out, inlist(x, 998, 999) | is.na(x), NA)
  } else {  # 2011+
    out <- rc(out, inlist(x, 0), 0); out <- rc(out, inrange(x, 1, 111), x)
    out <- rc(out, inlist(x, 112), 112)
    out <- rc(out, inlist(x, 998, 999) | is.na(x), NA)
  }
  out
}

for (who in c("rp", "sp")) {
  for (v in c("time_acar", "time_ccar"))
    psid_abridged <- collect_tv(psid_abridged, paste0(v, "_", who), time_care)
  for (v in c("time_educ", "time_leis", "time_pers", "time_shop",
              "time_volu", "time_work"))
    psid_abridged <- collect_tv(psid_abridged, paste0(v, "_", who), time_hrs)
  psid_abridged <- collect_tv(psid_abridged, paste0("if_time_hous_", who), if_hous)
  psid_abridged <- collect_tv(psid_abridged, paste0("time_hous_", who), time_hous_fn)
}
