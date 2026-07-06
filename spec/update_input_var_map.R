# update_input_var_map.R
#
# Extends input_var_map.csv with any wave years present in the cross-year index
# but not yet in the map.  Run this script whenever psid-cross-year-index.xlsx
# is updated with a new survey wave.
#
# Usage (from the repo root):
#   Rscript spec/update_input_var_map.R
#
# Output:
#   spec/input_var_map.csv  — updated in place; new rows appended before the
#                             file is re-sorted by (newvar, year).
#
# Logic:
#   For each missing wave year Y, we find the most-recent prior wave year P
#   that exists in both the index and the map.  For every newvar that has a
#   mapping in wave P we look up its P variable number in column Y(P) of the
#   index and retrieve the matching value in column Y(Y).  Rows with no Y
#   counterpart (e.g. one-off supplements like COVID) are skipped silently.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
})

spec_dir   <- "spec"
index_path <- file.path(spec_dir, "psid-cross-year-index.xlsx")
map_path   <- file.path(spec_dir, "input_var_map.csv")

# ---- load ----------------------------------------------------------------
xl  <- read_excel(index_path)
ivm <- read_csv(map_path, show_col_types = FALSE)

# Year columns in the index: "Y1968", "Y1969", ..., "Y2023"
index_years <- sort(as.integer(sub("^Y", "", grep("^Y\\d{4}$", colnames(xl), value = TRUE))))
map_years   <- sort(unique(ivm$year))

missing_years <- setdiff(index_years, map_years)

if (length(missing_years) == 0) {
  message("input_var_map.csv is already up to date — nothing to add.")
  quit(save = "no")
}

message("Wave years to add: ", paste(missing_years, collapse = ", "))

new_rows <- vector("list", length(missing_years))

for (i in seq_along(missing_years)) {
  target_year <- missing_years[i]

  # Most-recent prior wave that is in both the index and the map
  prior_candidates <- intersect(map_years, index_years)
  prior_candidates <- prior_candidates[prior_candidates < target_year]
  if (length(prior_candidates) == 0) {
    message("  Skipping ", target_year, ": no prior reference wave available.")
    next
  }
  prior_year <- max(prior_candidates)

  col_prior  <- paste0("Y", prior_year)
  col_target <- paste0("Y", target_year)

  lookup <- xl %>%
    select(all_of(c(col_prior, col_target))) %>%
    filter(!is.na(.data[[col_prior]]), !is.na(.data[[col_target]])) %>%
    distinct()

  wave_prior <- ivm %>% filter(year == prior_year)

  added <- wave_prior %>%
    left_join(lookup, by = setNames(col_prior, "input_var")) %>%
    filter(!is.na(.data[[col_target]])) %>%
    transmute(newvar, year = target_year, input_var = .data[[col_target]])

  skipped <- nrow(wave_prior) - nrow(added)
  message(sprintf("  %d: %d rows added, %d skipped (no %d counterpart)",
                  target_year, nrow(added), skipped, target_year))

  new_rows[[i]] <- added
}

new_rows <- bind_rows(new_rows)

if (nrow(new_rows) == 0) {
  message("No new rows to write.")
  quit(save = "no")
}

updated <- bind_rows(ivm, new_rows) %>%
  arrange(newvar, year)

write_csv(updated, map_path)
message(sprintf("Done. Wrote %d rows to %s  (+%d new).",
                nrow(updated), map_path, nrow(new_rows)))
