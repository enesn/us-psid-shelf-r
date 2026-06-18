# =====================================================================
# R/revise/inflation.R
# For every nominal-dollar variable (*_nd and *_ndf), create the real-dollar
# version (*_rd / *_rdf) = value * pcepi[inflate_year] / pcepi[ref_year],
# preserving PSID top-codes. Income/earnings/expenditures use the prior tax year;
# wealth/home values use the survey year.
# =====================================================================

radix <- pcepi[as.character(inflate_year)]
dc <- dollar_cols("_nd")            # matches both _nd and _ndf stubs
if (!is.null(dc)) {
  for (i in seq_len(nrow(dc))) {
    col <- dc$col[i]; y <- dc$year[i]; varcat <- dc$varcat[i]
    ref_year <- if (varcat %in% c("earn", "expn", "finc")) y - 1L else y
    ratio <- radix / pcepi[as.character(ref_year)]
    old <- psid_abridged[[col]]
    new <- ifelse(is.na(old), NA_real_, old * ratio)
    for (tc in dollar_topcodes(varcat, y)) new[old %in% tc] <- tc
    newcol <- sub("_nd", "_rd", dc$stub[i])     # *_nd -> *_rd ; *_ndf -> *_rdf
    lab <- attr(old, "label")
    lab <- if (is.null(lab)) col else sub(" \\(nominal USD", sprintf(" (real USD %d", inflate_year), lab)
    psid_abridged[[paste0(newcol, "_", y)]] <- set_label(new, lab)
  }
  message("  inflation: created ", nrow(dc), " *_rd/*_rdf variables")
}
