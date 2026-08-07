# Crosswalk supplements -----------------------------------------------------

#' Apply a taxonomy crosswalk as a post-baseline supplement
#'
#' Use this after an initial [reconcile_data()] or [reconcile_tree()] run
#' when you want a taxonomy crosswalk to add matches without overwriting
#' exact, normalised, synonym, or fuzzy matches that the baseline cascade
#' already made.
#'
#' @details
#' Passing the output of [reconcile_crosswalk()] directly to the
#' `overrides` argument of a `reconcile_*()` call treats every row as a
#' locked manual decision. Those overrides are applied before the matching
#' cascade, so they can preempt exact or normalised matches. That is correct
#' for reviewed manual corrections, but risky when using a whole published
#' crosswalk automatically.
#'
#' `reconcile_crosswalk_supplement()` implements the safer pattern:
#'
#' \enumerate{
#'   \item run the baseline reconciliation first;
#'   \item convert the crosswalk to candidate overrides;
#'   \item keep only rows whose source name is still unresolved in `x` and
#'     whose target name is still unresolved in `y`;
#'   \item drop duplicate source or target candidates rather than choosing by
#'     row order;
#'   \item apply the remaining rows with [reconcile_override_batch()].
#' }
#'
#' By default, `one_to_one_only = TRUE`, so split/lump rows such as
#' `"1BL to many BT"` and `"Many BL to 1BT"` are not applied
#' automatically. If you set `one_to_one_only = FALSE`, duplicate source or
#' target candidates are still skipped and should be reviewed manually.
#'
#' @param reconciliation A [reconciliation] object returned by
#'   [reconcile_data()], [reconcile_tree()], or a related matcher.
#' @inheritParams reconcile_crosswalk
#' @param one_to_one_only Logical. If `TRUE` (default), keeps only
#'   one-to-one crosswalk rows before supplementing the baseline result.
#' @param quiet Logical. Suppresses informational messages when `TRUE`.
#'
#' @return An updated [reconciliation] object. If no unambiguous crosswalk
#'   rows can supplement the baseline result, the returned object is the
#'   input `reconciliation` with a `meta$crosswalk_supplement` audit entry.
#'
#' @family reconciliation functions
#' @seealso [reconcile_crosswalk()] for converting a crosswalk to an override
#'   table; [reconcile_override_batch()] for applying reviewed batches.
#'
#' @examples
#' x <- data.frame(species = c("Species old", "Species exact"))
#' y <- data.frame(species = c("Species new", "Species exact"))
#' crosswalk <- data.frame(
#'   from = "Species old",
#'   to = "Species new",
#'   type = "1BL to 1BT"
#' )
#'
#' baseline <- reconcile_data(
#'   x, y,
#'   x_species = "species",
#'   y_species = "species",
#'   authority = NULL,
#'   quiet = TRUE
#' )
#'
#' supplemented <- reconcile_crosswalk_supplement(
#'   baseline,
#'   crosswalk,
#'   from_col = "from",
#'   to_col = "to",
#'   match_type_col = "type",
#'   quiet = TRUE
#' )
#' reconcile_mapping(supplemented)
#'
#' @export
reconcile_crosswalk_supplement <- function(
  reconciliation,
  crosswalk,
  from_col,
  to_col,
  match_type_col = NULL,
  notes_col = NULL,
  one_to_one_only = TRUE,
  quiet = FALSE
) {
  validate_reconciliation(reconciliation)

  candidates <- if (quiet) {
    suppressMessages(
      reconcile_crosswalk(
        crosswalk = crosswalk,
        from_col = from_col,
        to_col = to_col,
        match_type_col = match_type_col,
        notes_col = notes_col,
        one_to_one_only = one_to_one_only
      )
    )
  } else {
    reconcile_crosswalk(
      crosswalk = crosswalk,
      from_col = from_col,
      to_col = to_col,
      match_type_col = match_type_col,
      notes_col = notes_col,
      one_to_one_only = one_to_one_only
    )
  }

  mapping <- reconcile_mapping(reconciliation)
  rank <- reconciliation$meta$rank
  if (is.null(rank) || is.na(rank)) {
    rank <- "species"
  }

  unresolved_x <- mapping$name_x[
    mapping$match_type == "unresolved" &
      mapping$in_x &
      !is.na(mapping$name_x)
  ]
  unresolved_y <- mapping$name_y[
    mapping$match_type == "unresolved" &
      mapping$in_y &
      !is.na(mapping$name_y)
  ]

  audit <- list(
    timestamp = Sys.time(),
    one_to_one_only = one_to_one_only,
    n_crosswalk_rows = nrow(candidates),
    n_unresolved_x = length(unresolved_x),
    n_unresolved_y = length(unresolved_y),
    n_available = 0L,
    n_ambiguous = 0L,
    n_applied = 0L
  )

  if (
    nrow(candidates) == 0 ||
      length(unresolved_x) == 0 ||
      length(unresolved_y) == 0
  ) {
    reconciliation$meta$crosswalk_supplement <- audit
    if (!quiet) {
      cli_alert_info("No crosswalk rows can supplement unresolved names.")
    }
    return(reconciliation)
  }

  lookup_x <- pr_normalized_lookup(unresolved_x, rank = rank)
  lookup_y <- pr_normalized_lookup(unresolved_y, rank = rank)

  candidates$name_x <- unname(
    lookup_x[
      as.character(pr_normalize_names(candidates$name_x, rank = rank))
    ]
  )
  candidates$name_y <- unname(
    lookup_y[
      as.character(pr_normalize_names(candidates$name_y, rank = rank))
    ]
  )

  candidates <- candidates[
    !is.na(candidates$name_x) &
      !is.na(candidates$name_y),
  ]
  audit$n_available <- nrow(candidates)

  if (nrow(candidates) == 0) {
    reconciliation$meta$crosswalk_supplement <- audit
    if (!quiet) {
      cli_alert_info("No crosswalk rows match still-unresolved names.")
    }
    return(reconciliation)
  }

  ambiguous <- duplicated(candidates$name_x) |
    duplicated(candidates$name_x, fromLast = TRUE) |
    duplicated(candidates$name_y) |
    duplicated(candidates$name_y, fromLast = TRUE)

  audit$n_ambiguous <- sum(ambiguous)
  if (any(ambiguous)) {
    candidates <- candidates[!ambiguous, ]
    if (!quiet) {
      cli_alert_warning(
        "Skipped {audit$n_ambiguous} ambiguous crosswalk row{?s}; review duplicate source/target candidates manually."
      )
    }
  }

  if (nrow(candidates) == 0) {
    reconciliation$meta$crosswalk_supplement <- audit
    if (!quiet) {
      cli_alert_info("No unambiguous crosswalk rows left to apply.")
    }
    return(reconciliation)
  }

  candidates$action <- "accept"
  candidates$note <- candidates$user_note

  result <- suppressMessages(
    reconcile_override_batch(
      reconciliation,
      candidates,
      quiet = TRUE
    )
  )

  audit$n_applied <- nrow(candidates)
  result$meta$crosswalk_supplement <- audit

  if (!quiet) {
    cli_alert_success(
      "Applied {audit$n_applied} post-baseline crosswalk supplement{?s}."
    )
  }

  result
}

pr_normalized_lookup <- function(x, rank = "species") {
  norm <- as.character(pr_normalize_names(x, rank = rank))
  keep <- !is.na(norm) & !duplicated(norm)
  stats::setNames(x[keep], norm[keep])
}
