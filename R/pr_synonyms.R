# Synonym resolution -------------------------------------------------------

#' Resolve synonyms between two name sets
#'
#' For names that remain unmatched after exact and normalised matching, queries
#' a taxonomic authority to find cases where both names resolve to the same
#' accepted name, or where one name is a synonym of the other.
#'
#' @param unmatched_x A character vector. Unmatched names from
#'   source x.
#' @param unmatched_y A character vector. Unmatched names from
#'   source y.
#' @param authority A length-1 character vector. Authority code, one
#'   of `pr_valid_authorities()`.
#' @param db_version A length-1 character vector, or `NULL`.
#' @param quiet Logical. Suppresses progress messages when `TRUE`.
#'
#' @return A tibble with columns: `name_x`, `name_y`, `name_resolved`,
#'   `match_source`, `notes`.
#' @keywords internal
pr_resolve_synonyms <- function(unmatched_x, unmatched_y,
                                authority = "col", db_version = NULL,
                                quiet = FALSE) {

  if (length(unmatched_x) == 0 || length(unmatched_y) == 0) {
    return(tibble(
      name_x        = character(),
      name_y        = character(),
      name_resolved = character(),
      match_source  = character(),
      notes         = character()
    ))
  }

  if (!quiet) {
    cli_alert_info(
      "Looking up {length(unmatched_x)} + {length(unmatched_y)} names in {.val {toupper(authority)}}..."
    )
  }

  # Look up both sets
  lookup_x <- pr_lookup_authority(unmatched_x, authority, db_version)
  lookup_y <- pr_lookup_authority(unmatched_y, authority, db_version)

  results <- list()

  # Strategy 1: x's accepted name matches a name in y directly
  for (i in seq_len(nrow(lookup_x))) {
    acc_x <- lookup_x$accepted_name[i]
    if (is.na(acc_x)) next

    # Does the accepted name of x appear in unmatched_y?
    match_idx <- which(unmatched_y == acc_x)
    if (length(match_idx) > 0) {
      results[[length(results) + 1]] <- tibble(
        name_x        = lookup_x$input[i],
        name_y        = unmatched_y[match_idx[1]],
        name_resolved = acc_x,
        match_source  = paste0(authority, "_synonym"),
        notes         = sprintf(
          "Synonym per %s: '%s' -> accepted '%s'",
          toupper(authority), lookup_x$input[i], acc_x
        )
      )
    }
  }

  # Strategy 2: y's accepted name matches a name in x directly
  for (j in seq_len(nrow(lookup_y))) {
    acc_y <- lookup_y$accepted_name[j]
    if (is.na(acc_y)) next

    # Already matched via strategy 1?
    already_matched_y <- vapply(results, function(r) r$name_y, character(1))
    if (lookup_y$input[j] %in% already_matched_y) next

    match_idx <- which(unmatched_x == acc_y)
    if (length(match_idx) > 0) {
      # Also check this x name wasn't already matched
      already_matched_x <- vapply(results, function(r) r$name_x, character(1))
      if (unmatched_x[match_idx[1]] %in% already_matched_x) next

      results[[length(results) + 1]] <- tibble(
        name_x        = unmatched_x[match_idx[1]],
        name_y        = lookup_y$input[j],
        name_resolved = acc_y,
        match_source  = paste0(authority, "_synonym"),
        notes         = sprintf(
          "Synonym per %s: '%s' -> accepted '%s'",
          toupper(authority), lookup_y$input[j], acc_y
        )
      )
    }
  }

  # Strategy 3: both resolve to the same accepted name
  for (i in seq_len(nrow(lookup_x))) {
    acc_x <- lookup_x$accepted_name[i]
    if (is.na(acc_x)) next

    # Already matched?
    already_matched_x <- if (length(results) > 0) {
      vapply(results, function(r) r$name_x, character(1))
    } else {
      character()
    }
    if (lookup_x$input[i] %in% already_matched_x) next

    for (j in seq_len(nrow(lookup_y))) {
      acc_y <- lookup_y$accepted_name[j]
      if (is.na(acc_y)) next

      already_matched_y <- if (length(results) > 0) {
        vapply(results, function(r) r$name_y, character(1))
      } else {
        character()
      }
      if (lookup_y$input[j] %in% already_matched_y) next

      if (acc_x == acc_y) {
        results[[length(results) + 1]] <- tibble(
          name_x        = lookup_x$input[i],
          name_y        = lookup_y$input[j],
          name_resolved = acc_x,
          match_source  = paste0(authority, "_synonym"),
          notes         = sprintf(
            "Both synonyms per %s: '%s' and '%s' -> accepted '%s'",
            toupper(authority), lookup_x$input[i], lookup_y$input[j], acc_x
          )
        )
        break
      }
    }
  }

  if (length(results) == 0) {
    return(tibble(
      name_x        = character(),
      name_y        = character(),
      name_resolved = character(),
      match_source  = character(),
      notes         = character()
    ))
  }

  do.call(rbind, results)
}
