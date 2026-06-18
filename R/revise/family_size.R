# =====================================================================
# R/revise/family_size.R
# For every nominal-dollar variable (*_nd), create the family-size-adjusted
# version (*_ndf) = value / sqrt(fam_size_<year>), preserving PSID top-codes.
# =====================================================================

dc <- dollar_cols("_nd")
if (!is.null(dc)) {
  for (i in seq_len(nrow(dc))) {
    col <- dc$col[i]; y <- dc$year[i]; varcat <- dc$varcat[i]
    fs <- psid_abridged[[paste0("fam_size_", y)]]
    old <- psid_abridged[[col]]
    new <- ifelse(is.na(old), NA_real_, old / sqrt(fs))
    for (tc in dollar_topcodes(varcat, y)) new[old %in% tc] <- tc   # preserve sentinels
    newcol <- sub("_nd", "_ndf", dc$stub[i])                       # earn_wage_nd_rp -> earn_wage_ndf_rp
    lab <- attr(old, "label"); lab <- if (is.null(lab)) col else sub(" \\(nominal USD\\)", " (nominal USD, fam size adj)", lab)
    psid_abridged[[paste0(newcol, "_", y)]] <- set_label(new, lab)
  }
  message("  family_size: created ", nrow(dc), " *_ndf variables")
}
