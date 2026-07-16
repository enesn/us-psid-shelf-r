# =====================================================================
# R/generate/work_history.R
# Derives reference-couple (_rc) totals for the work-history variables:
#   total_years_worked_rc      = rp + sp  (# years worked full time, head+spouse)
#   last_year_working_hour_rc  = rp + sp  (total annual work hours, head+spouse)
# Both inputs are FU head/spouse attributes, so the columnwise RP+SP sum is the
# couple total at the family-unit level (never other FU members); NA if either
# side is missing.
#   total_years_worked carries non-additive codes -- the 95/96 specials ("less
#   than one year, age <=18" / "all years since 18") and the 97/98 "N or more"
#   top-codes (see R/collect/work_history.R). None can enter an arithmetic sum,
#   so _rc is NA whenever either side is >= 95 (no living person has a real 95+
#   count, so the threshold never clips a genuine value). The _rc therefore
#   carries NO value-label set: a couple sum like 70+26 = 96 must not inherit the
#   "all years since 18" special-code label.
#   last_year_working_hour is a clean hour count; its 9999+ top-code is never
#   observed, but if it ever appeared either side at 9999 -> 9999.
# =====================================================================

for (.wh_base in c("total_years_worked", "last_year_working_hour")) {
  .wh_years <- .wh_base == "total_years_worked"
  for (.wh_y in year) {
    .wh_rp <- psid_abridged[[paste0(.wh_base, "_rp_", .wh_y)]]
    .wh_sp <- psid_abridged[[paste0(.wh_base, "_sp_", .wh_y)]]
    if (is.null(.wh_rp) || is.null(.wh_sp)) next
    .wh_rpn <- as.vector(.wh_rp); .wh_spn <- as.vector(.wh_sp)
    .wh_out <- .wh_rpn + .wh_spn
    if (.wh_years) .wh_out[.wh_rpn >= 95 | .wh_spn >= 95] <- NA          # specials/top-codes: non-additive
    else           .wh_out[.wh_rpn %in% 9999 | .wh_spn %in% 9999] <- 9999 # unobserved hour top-code
    psid_abridged[[paste0(.wh_base, "_rc_", .wh_y)]] <-
      set_label(.wh_out, var_label(paste0(.wh_base, "_rc"), .wh_y))
  }
}
rm(.wh_base, .wh_years, .wh_y, .wh_rp, .wh_sp, .wh_rpn, .wh_spn, .wh_out)
