# Species column auto-detection -------------------------------------------

#' Detect the species name column in a data frame
#'
#' Uses a two-stage heuristic: first checks for common column names, then
#' falls back to content-based detection (binomial name pattern).
#'
#' @param df A data frame.
#' @param arg_name Character. Name of the argument, used in error messages
#'   (e.g., `"x_species"` or `"y_species"`).
#'
#' @return A length-1 character vector: the detected column name.
#'
#' @details
#' **Stage 1 — Name matching.** Checks column names (case-insensitive) against
#' a priority list: `species`, `species_name`, `binomial`, `taxon`,
#' `scientificName`, `Scientific_name`, `canonical_name`, `tip.label`,
#' `PhyloName`, `Binomial`, `latin_name`, `sci_name`.
#'
#' **Stage 2 — Content heuristic.** If no name match, checks which character
#' columns have >50% of non-NA values matching the binomial pattern
#' `^[A-Z][a-z]+ [a-z]+`.
#'
#' If zero or multiple candidates are found, the function stops with an
#' informative error.
#'
#' @keywords internal
pr_detect_species_column <- function(df, arg_name = "x_species") {
  col_names <- names(df)


  # Stage 1: name-based matching
  priority <- c(
    "species", "species_name", "binomial", "taxon", "scientificName",
    "Scientific_name", "canonical_name", "tip.label", "PhyloName",
    "Binomial", "latin_name", "sci_name", "sciName", "phylo_name",
    "tip_label", "Species", "Species_name", "SPECIES"
  )

  for (candidate in priority) {
    hits <- col_names[tolower(col_names) == tolower(candidate)]
    if (length(hits) == 1) {
      cli_alert_info("Auto-detected species column: {.field {hits}}")
      return(hits)
    }
  }

  # Stage 2: content heuristic
  binomial_re <- "^[A-Z][a-z]+ [a-z]+"
  candidates <- character()

  for (col in col_names) {
    vals <- df[[col]]
    if (!is.character(vals) && !is.factor(vals)) next

    vals <- as.character(vals)
    non_na <- vals[!is.na(vals) & nchar(vals) > 0]
    if (length(non_na) < 3) next

    frac_binomial <- mean(grepl(binomial_re, non_na, perl = TRUE))
    if (frac_binomial > 0.5) {
      candidates <- c(candidates, col)
    }
  }

  if (length(candidates) == 1) {
    cli_alert_info("Auto-detected species column: {.field {candidates}}")
    return(candidates)
  }

  if (length(candidates) > 1) {
    abort(
      c(
        "Multiple columns look like species names.",
        "i" = paste0("Candidates: ", paste(candidates, collapse = ", ")),
        "i" = paste0("Please specify `", arg_name, "` explicitly.")
      ),
      call = caller_env()
    )
  }

  abort(
    c(
      "Could not detect a species name column.",
      "i" = paste0("Columns available: ", paste(col_names, collapse = ", ")),
      "i" = paste0("Please specify `", arg_name, "` explicitly.")
    ),
    call = caller_env()
  )
}
