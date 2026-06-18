# =====================================================================
# 06-revise-variables.R  --  Revise variables
#
# Revise the collected/generated variables (PSID-SHELF construction):
#   * recode values to missing when not currently in the family unit
#   * family-size adjustments
#   * inflation adjustments  (PCEPI: value * pcepi[inflate] / pcepi[y])
#
# Each revision is a file under R/revise/ (recode_not_in_fu.R, family_size.R,
# inflation.R). Sourced if present; robust to partial coverage.
#
# Prerequisites: 01/03/04/05 already sourced.
# =====================================================================

stopifnot(exists("psid_abridged"), exists("SPEC"), exists("pcepi"))

banner <- function(m) message(sprintf("\n%s\n  %s\n%s", strrep("-", 60), m, strrep("-", 60)))

for (part in c("recode_not_in_fu", "family_size", "inflation")) {
  f <- file.path("R", "revise", paste0(part, ".R"))
  if (!file.exists(f)) {
    message("  [revise] SKIP (not present): ", part)
    next
  }
  banner(paste("revise:", part))
  source(f, local = FALSE)
}

message("\n[06-revise-variables] complete: ", ncol(psid_abridged), " columns")
