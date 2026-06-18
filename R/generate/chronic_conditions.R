# =====================================================================
# R/generate/chronic_conditions.R
# Derives the combined per-person version of every chronic-condition measure
# (RP answer for the RP, SP answer for the SP, NA otherwise) via combine_rpsp.
# =====================================================================

conds <- c("arth","asth","canc","diab","emop","hatt","hdis","hibp",
           "ldis","lmem","lung","stro","ocon")
measures <- character(0)
for (c in conds) {
  measures <- c(measures, paste0("ccon_", c, "_diag_any"), paste0("ccon_", c, "_diag_age"),
                paste0("ccon_", c, "_degr"))
  # canc reports cancer status; every other condition reports a care measure
  measures <- c(measures, if (c == "canc") "ccon_canc_stat" else paste0("ccon_", c, "_care"))
  if (c == "canc") measures <- c(measures, "ccon_canc_type_1m", "ccon_canc_type_2m")
  if (c == "emop") measures <- c(measures, "ccon_emop_type_1m", "ccon_emop_type_2m", "ccon_emop_type_3m")
  if (c == "ocon") measures <- c(measures, "ccon_ocon_type")
  if (c %in% c("hatt", "stro")) measures <- c(measures, paste0("ccon_", c, "_diag_2nd"))
}
measures <- unique(measures)

for (m in measures) combine_rpsp(m)
