# =====================================================================
# R/generate/panel_status.R
# Derives: response_*, panel_current_*, panel_inst_*, panel_move_*,
#          panel_drop_stat, panel_rein_elig_*
# =====================================================================

.gn <- nrow(psid_abridged)
.gset <- function(x, nv, y = NULL, set = NULL) {
  x <- set_label(x, var_label(nv, y))
  if (!is.null(set)) { vl <- SPEC$value_labels[which(SPEC$value_labels$label_set %in% set), ]
    if (nrow(vl)) attr(x, "labels") <- setNames(as.numeric(vl$value), vl$label) }
  x
}
.rowmax <- function(lst) { if (!length(lst)) return(rep(NA_real_, .gn))
  m <- do.call(pmax, c(lst, list(na.rm = TRUE))); m[is.infinite(m)] <- NA; m }
# (gen_tv comes from R/programs.R — NULL-safe, uses g_label)

# response — collapse the extended reason-for-nonresponse into 10 categories
gen_tv("response", function(y) {
  s <- psid_abridged[[paste0("response_ext_", y)]]; out <- rep(-1, .gn)
  out <- rc(out, inrange(s, 0, 0), 0)
  for (k in 1:9) out <- rc(out, inrange(s, k*100, k*100+99), k)
  out
}, "response_10cat")

# panel_current — current FU member? (seqnum-driven, response_ext fallback)
gen_tv("panel_current", function(y) {
  s <- psid_abridged[[paste0("response_ext_", y)]]; q <- psid_abridged[[paste0("seqnum_", y)]]
  out <- rep(-1, .gn)
  out <- rc(out, !(s %in% 0), 0)
  out <- rc(out, s %in% 0, 1)
  out <- rc(out, q %in% 0 | inrange(q, 21, 99), 0)
  out <- rc(out, inrange(q, 1, 20), 1)
  out
}, "panelcurrent_2cat")

# panel_inst — in an institution (type)
gen_tv("panel_inst", function(y) {
  s <- psid_abridged[[paste0("response_ext_", y)]]; out <- rep(-1, .gn)
  out <- rc(out, !(s %in% c(101,102,103,104,105)), 0)
  out <- rc(out, s %in% 101, 1); out <- rc(out, s %in% 102, 2); out <- rc(out, s %in% 103, 3)
  out <- rc(out, s %in% 104, 4); out <- rc(out, s %in% 105, 5)
  out
}, "panelinst_6cat")

# panel_move — moved out since last wave? (seqnum 71-80)
gen_tv("panel_move", function(y) {
  q <- psid_abridged[[paste0("seqnum_", y)]]; out <- rep(0, .gn)
  out <- rc(out, inrange(q, 71, 80), 1)
  out
}, "panelmove_2cat")

# panel_drop_stat — SEO sample-drop / reinstatement status (1997+)
wlth_yrs <- year[30:n_year]
ds_l <- ri_l <- list(); samp <- psid_abridged$sample
for (y in wlth_yrs) {
  rex <- psid_abridged[[paste0("response_ext_", y)]]; pdr <- psid_abridged[[paste0("panel_drop_rein_", y)]]
  ds <- rep(NA_real_, .gn)
  ds <- rc(ds, (!(rex %in% 600) & samp %in% 2) | !(samp %in% 2), 0); ds <- rc(ds, rex %in% 600 & samp %in% 2, 1)
  ri <- rep(NA_real_, .gn)
  ri <- rc(ri, (!(pdr %in% 1) & samp %in% 2) | !(samp %in% 2), 0); ri <- rc(ri, pdr %in% 1 & samp %in% 2, 1)
  ds_l[[length(ds_l)+1]] <- ds; ri_l[[length(ri_l)+1]] <- ri
}
ds_max <- .rowmax(ds_l); ri_max <- .rowmax(ri_l)
pds <- rep(-1, .gn)
pds <- rc(pds, ds_max %in% 0 & ri_max %in% 0, 0)
pds <- rc(pds, ds_max %in% 1 & ri_max %in% 0, 1)
pds <- rc(pds, ds_max %in% c(0,1) & ri_max %in% 1, 2)
psid_abridged$panel_drop_stat <- .gset(pds, "panel_drop_stat", NULL, "paneldropstat_3cat")

# panel_rein_elig — FU has a 1997 CDS-eligible child (propagated across FU members)
grpmax <- function(val, grp) {            # within-group max (na.rm), assigned to all rows
  ave(val, ifelse(is.na(grp), -1, grp), FUN = function(z) { m <- max(z, na.rm = TRUE); if (is.infinite(m)) NA else m })
}
pre  <- psid_abridged$panel_rein_elig_ind
pc97 <- psid_abridged$panel_current_1997; fuid97 <- psid_abridged$fuid_1997
s97  <- ifelse(pc97 %in% 1, grpmax(ifelse(pc97 %in% 1, pre, NA), fuid97), 0)
for (y in seq(1997L, psid_lastwave, 2L)) {
  pcy <- psid_abridged[[paste0("panel_current_", y)]]; fuidy <- psid_abridged[[paste0("fuid_", y)]]
  reiny <- ifelse(pcy %in% 1, grpmax(ifelse(pcy %in% 1, s97, NA), fuidy), NA)
  out <- rep(-1, .gn); out <- rc(out, TRUE, reiny)
  psid_abridged[[paste0("panel_rein_elig_", y)]] <- .gset(out, "panel_rein_elig", y, "panelreinelig_2cat")
}
