# =====================================================================
# 04-collect-inputs.R  --  Collect all PSID-SHELF input variables
#
# Collect & merge the PSID-SHELF input variables. Each domain file under
# R/collect/ builds one domain's input variables, adding its wide
# newvar_<year> columns to the in-memory psid_abridged (keyed by `id`, so
# adding columns is equivalent to a 1:1 merge on id).
#
# Prerequisites: 01-ingest.R, 03-shelf-parameters.R already sourced.
# =====================================================================

stopifnot(exists("psid_abridged"), exists("SPEC"), exists("collect_tv"))

# Canonical domain order (the order in which input variables are merged).
collect_domains <- c(
  "survey_identifiers", "panel_status", "sample_design",
  "demographics", "education", "family_type", "geography",
  "race_ethnicity", "time_use",
  "chronic_conditions", "covid_19", "dementia", "depression",
  "disability", "general_wellbeing",
  "earnings", "employment", "work_history", "expenditures", "family_income",
  "occupations", "primary_home", "wealth",
  "relationship_id", "labor_income", "capital_income", "income")

banner <- function(m) message(sprintf("\n%s\n  %s\n%s", strrep("-", 60), m, strrep("-", 60)))

for (dom in collect_domains) {
  f <- file.path("R", "collect", paste0(dom, ".R"))
  if (!file.exists(f)) {
    message("  [collect] SKIP (not present): ", dom)
    next
  }
  banner(paste("collect:", dom))
  t0 <- Sys.time()
  source(f, local = FALSE)
  message(sprintf("  done (%.1fs) — %d columns now",
                  as.numeric(difftime(Sys.time(), t0, units = "secs")),
                  ncol(psid_abridged)))
}

message("\n[04-collect-inputs] complete: ", ncol(psid_abridged), " columns")
