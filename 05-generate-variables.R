# =====================================================================
# 05-generate-variables.R  --  Create PSID-SHELF generated variables
#
# Create & merge the PSID-SHELF generated (derived) variables. Each domain file
# under R/generate/ derives one domain's summary / cross-year variables from the
# collected input variables, adding them to the in-memory psid_abridged.
#
# Prerequisites: 01/03/04 already sourced.
# =====================================================================

stopifnot(exists("psid_abridged"), exists("SPEC"))

generate_domains <- c(
  "survey_identifiers", "panel_status", "sample_design",
  "demographics", "education", "family_type", "geography",
  "race_ethnicity", "time_use",
  "chronic_conditions", "covid_19", "dementia", "depression",
  "disability", "general_wellbeing",
  "earnings", "employment", "expenditures", "family_income",
  "occupations", "primary_home", "wealth",
  "relationship_id")

banner <- function(m) message(sprintf("\n%s\n  %s\n%s", strrep("-", 60), m, strrep("-", 60)))

for (dom in generate_domains) {
  f <- file.path("R", "generate", paste0(dom, ".R"))
  if (!file.exists(f)) {
    message("  [generate] SKIP (not present): ", dom)
    next
  }
  banner(paste("generate:", dom))
  t0 <- Sys.time()
  source(f, local = FALSE)
  message(sprintf("  done (%.1fs) — %d columns now",
                  as.numeric(difftime(Sys.time(), t0, units = "secs")),
                  ncol(psid_abridged)))
}

message("\n[05-generate-variables] complete: ", ncol(psid_abridged), " columns")
