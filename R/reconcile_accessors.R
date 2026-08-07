# Accessor and action functions for reconciliation objects -----------------

#' Extract the per-name mapping table from a reconciliation
#'
#' Returns the mapping tibble inside a [reconciliation] object. Use this
#' when you want to filter matches programmatically (e.g. pull all
#' unresolved species, all fuzzy matches above a given score, or join
#' the mapping back to the original data frame).
#'
#' @param reconciliation A [reconciliation] object returned by
#'   [reconcile_tree()], [reconcile_data()], [reconcile_trees()],
#'   [reconcile_to_trees()], or [reconcile_multi()].
#' @param include_unused_overrides Logical. Appends the rejected
#'   override rows to the returned tibble when `TRUE`, with
#'   `match_type = "override_unused"`, `match_score = NA`, and the
#'   `notes` column carrying the rejection reason
#'   (`name_x_not_in_data`, `name_y_not_in_target`, or
#'   `already_matched`). Default `FALSE` for backward compatibility ---
#'   the bare mapping tibble has the same shape as before.
#'
#' @return A tibble with one row per unique name seen in either source
#'   and the following columns:
#'   \describe{
#'     \item{`name_x`}{Statement: this column holds the original name
#'       as it appeared in source `x` (your data). `NA` for rows that
#'       exist only in source `y` (e.g. tree tips not in your data).}
#'     \item{`name_y`}{Statement: this column holds the original name
#'       as it appeared in source `y` (the reference dataset or tree).
#'       `NA` for rows that exist only in source `x`.}
#'     \item{`name_resolved`}{The accepted/canonical name returned by
#'       the taxonomic authority, when synonym resolution was used.
#'       `NA` when `authority = NULL` or no synonym was found.}
#'     \item{`match_type`}{One of `"exact"`, `"normalized"`,
#'       `"synonym"`, `"fuzzy"`, `"manual"` (set via
#'       [reconcile_override()]), `"flagged"` (low-confidence, needs
#'       review), `"unresolved"`, or --- when
#'       `include_unused_overrides = TRUE` --- `"override_unused"`
#'       (override row not applied because of missing names or prior
#'       matches).}
#'     \item{`match_score`}{Numeric in \[0, 1\]. `1` for
#'       exact/normalized/synonym/manual matches; a genus-weighted
#'       Levenshtein score for fuzzy matches; `NA` for unresolved and
#'       for unused-override rows.}
#'     \item{`match_source`}{Where the match came from: `"exact"`,
#'       `"normalisation"`, the taxadb authority code (e.g. `"col"`),
#'       `"fuzzy"`, or `"user_override"`.}
#'     \item{`in_x`}{Logical. This column records whether the name was
#'       present in source `x`.}
#'     \item{`in_y`}{Logical. This column records whether the name was
#'       present in source `y`.}
#'     \item{`notes`}{Free-text notes, populated e.g. when a name is
#'       flagged for review or when an override carries a user comment.
#'       For `match_type = "override_unused"` rows this column carries
#'       the rejection reason.}
#'   }
#'
#' @family reconciliation functions
#' @seealso [reconcile_summary()] for a printed breakdown;
#'   [reconcile_suggest()] for near-miss candidates for unresolved
#'   names; [reconcile_apply()] to turn the mapping into an aligned
#'   data-tree pair. The unused-override rows surfaced by
#'   `include_unused_overrides = TRUE` mirror the `unused_overrides`
#'   slot on the [reconciliation] object.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' rec <- reconcile_tree(avonet_subset, tree_jetz,
#'                       x_species = "Species1", authority = NULL)
#' mapping <- reconcile_mapping(rec)
#'
#' # How many species matched?
#' sum(mapping$in_x & mapping$in_y)
#'
#' # Which species are in the data but missing from the tree?
#' head(mapping[mapping$in_x & !mapping$in_y, c("name_x", "match_type")])
#'
#' # Append rejected overrides for audit
#' mapping_full <- reconcile_mapping(rec, include_unused_overrides = TRUE)
#'
#' @export
reconcile_mapping <- function(reconciliation, include_unused_overrides = FALSE) {
  validate_reconciliation(reconciliation)
  if (!isTRUE(include_unused_overrides)) {
    return(reconciliation$mapping)
  }

  unused <- reconciliation$unused_overrides
  if (is.null(unused) || nrow(unused) == 0) {
    return(reconciliation$mapping)
  }

  unused_rows <- tibble(
    name_x        = unused$name_x,
    name_y        = unused$name_y,
    name_resolved = NA_character_,
    match_type    = "override_unused",
    match_score   = NA_real_,
    match_source  = "user_override",
    in_x          = FALSE,
    in_y          = FALSE,
    notes         = unused$reason
  )
  rbind(reconciliation$mapping, unused_rows)
}


#' Manually override a single name in a reconciliation
#'
#' Apply a single hand-curated decision to a [reconciliation] object.
#' Use this to accept a match the matching cascade rejected (typically
#' a flagged fuzzy hit), remove a spurious match, or force a new
#' mapping that the cascade missed. ("Cascade" here means the
#' four-stage matching pipeline run by [reconcile_tree()] and
#' [reconcile_data()] --- exact, normalised, synonym, fuzzy --- as
#' described in `?prepR4pcm`.) The override is recorded in the
#' provenance log so that you and your reviewers can audit every
#' manual decision.
#'
#' For applying many overrides at once (e.g. from a curated CSV), see
#' [reconcile_override_batch()]; for interactive decisions in the
#' console, see [reconcile_review()]; for published taxonomy crosswalks,
#' see [reconcile_crosswalk()].
#'
#' @param reconciliation A [reconciliation] object.
#' @param name_x A length-1 character vector. The name as it appears in source `x`
#'   (your data). Must match a value already present in `mapping$name_x`.
#' @param name_y A length-1 character vector or `NULL`. The name in source `y` (the
#'   tree or reference dataset) that `name_x` should be mapped to.
#'   `NULL` is only valid when `action = "reject"`.
#' @param action A length-1 character vector. What the override does:
#'   \describe{
#'     \item{`"accept"` (default)}{Confirm a proposed match. Use after
#'       reviewing a flagged fuzzy or synonym hit.}
#'     \item{`"reject"`}{Remove an existing match and return both names
#'       to the unresolved pool. Use when the cascade over-matched
#'       (e.g. an aggressive fuzzy score linked the wrong species).}
#'     \item{`"replace"`}{Set a new match, overwriting whatever the
#'       cascade produced for `name_x`.}
#'   }
#' @param note A length-1 character vector. A short justification for the override,
#'   stored in the provenance log and in `mapping$notes`. Strongly
#'   recommended --- future you will want to know why this decision was
#'   made.
#'
#' @return An updated [reconciliation] object. The existing row for
#'   `name_x` is replaced with one whose `match_type` is `"manual"` and
#'   `match_source` is `"user_override"`.
#'
#' @family reconciliation functions
#' @seealso [reconcile_override_batch()] for bulk overrides;
#'   [reconcile_suggest()] for near-miss candidates;
#'   [reconcile_crosswalk()] for published taxonomy crosswalks.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' rec <- reconcile_tree(avonet_subset, tree_jetz,
#'                       x_species = "Species1", authority = NULL)
#'
#' # Pick an unresolved species and hand-assign it for illustration
#' unresolved <- reconcile_mapping(rec)
#' unresolved <- unresolved[unresolved$match_type == "unresolved" &
#'                            unresolved$in_x, ]
#' if (nrow(unresolved) > 0) {
#'   rec <- reconcile_override(
#'     rec,
#'     name_x = unresolved$name_x[1],
#'     name_y = tree_jetz$tip.label[1],
#'     note   = "Demo: manual assignment"
#'   )
#' }
#'
#' @export
reconcile_override <- function(reconciliation, name_x, name_y = NULL,
                               action = c("accept", "reject", "replace"),
                               note = "") {

  validate_reconciliation(reconciliation)
  action <- match.arg(action)

  # Record the override
  new_override <- tibble(
    name_x    = name_x,
    name_y    = name_y %||% NA_character_,
    action    = action,
    user_note = note,
    timestamp = Sys.time()
  )
  reconciliation$overrides <- rbind(reconciliation$overrides, new_override)

  # Apply the override to the mapping
  mapping <- reconciliation$mapping

  if (action == "accept" || action == "replace") {
    if (is.null(name_y)) {
      abort("Must provide `name_y` for 'accept' or 'replace' actions.",
            call = caller_env())
    }

    # Remove any existing row for name_x
    mapping <- mapping[!(mapping$name_x %in% name_x & !is.na(mapping$name_x)), ]

    # Also remove name_y from unresolved y-only rows
    mapping <- mapping[!(mapping$name_y %in% name_y &
                           mapping$match_type == "unresolved" &
                           !mapping$in_x), ]

    # Add the manual match
    mapping <- rbind(mapping, tibble(
      name_x        = name_x,
      name_y        = name_y,
      name_resolved = NA_character_,
      match_type    = "manual",
      match_score   = 1.0,
      match_source  = "user_override",
      in_x          = TRUE,
      in_y          = TRUE,
      notes         = note
    ))

  } else if (action == "reject") {
    # Mark the match as unresolved
    idx <- which(mapping$name_x == name_x & !is.na(mapping$name_x))
    if (length(idx) > 0) {
      old_y <- mapping$name_y[idx[1]]
      mapping$match_type[idx[1]] <- "unresolved"
      mapping$match_score[idx[1]] <- NA_real_
      mapping$match_source[idx[1]] <- NA_character_
      mapping$in_y[idx[1]] <- FALSE
      mapping$name_y[idx[1]] <- NA_character_
      mapping$notes[idx[1]] <- paste("Rejected:", note)

      # Re-add the y name as unresolved if it was matched
      if (!is.na(old_y) && !old_y %in% mapping$name_y) {
        mapping <- rbind(mapping, tibble(
          name_x        = NA_character_,
          name_y        = old_y,
          name_resolved = NA_character_,
          match_type    = "unresolved",
          match_score   = NA_real_,
          match_source  = NA_character_,
          in_x          = FALSE,
          in_y          = TRUE,
          notes         = "Re-added after rejection"
        ))
      }
    }
  }

  reconciliation$mapping <- mapping
  reconciliation$counts <- pr_compute_counts(mapping)

  cli_alert_success("Override applied: '{name_x}' -> '{name_y %||% 'NA'}' ({action})")

  reconciliation
}


#' Apply a reconciliation to produce an aligned data-tree pair
#'
#' Turn a [reconciliation] object into an analysis-ready data frame and
#' pruned phylogenetic tree whose species labels agree. This is the step
#' that feeds directly into [caper::pgls()], [MCMCglmm::MCMCglmm()],
#' [phytools::fastAnc()], or any other PCM that expects matching names
#' in data and tree.
#'
#' Rows in `data` whose species have no match in the tree (and tips in
#' `tree` whose species have no match in the data) are handled according
#' to `drop_unresolved`. Matched data rows are kept as-is. Matched tree
#' tips are renamed to the source-`x` (data-side) name when the tree-side
#' label differs, so downstream PCM software can look up tips by the
#' species names in your data frame.
#'
#' @param reconciliation A [reconciliation] object returned by
#'   [reconcile_tree()], [reconcile_data()], or a related matcher.
#' @param data A data frame to align. If `NULL`, only the tree is
#'   processed and the returned `data` slot is `NULL`.
#' @param tree An `ape::phylo` object (or path to a Newick / Nexus
#'   file) to align. If `NULL`, only the data frame is processed and
#'   the returned `tree` slot is `NULL`. (Passing both `data = NULL`
#'   and `tree = NULL` is allowed but produces an empty result; the
#'   normal use is to pass at least one of them.)
#' @param species_col A length-1 character vector. Column in `data`
#'   containing species names. Auto-detected from a small set of
#'   common heuristics (e.g. `species`, `Species1`, `scientific_name`)
#'   when `NULL`; the heuristics list is not exhaustive --- pass the
#'   column name explicitly if your data uses a non-standard label.
#' @param drop_unresolved Logical. Drops unmatched rows and tips when `TRUE`.
#'   Defaults to `FALSE` (keep everything and just warn). Set to `TRUE`
#'   when preparing data for an analysis that cannot tolerate mismatches.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`data`}{The aligned data frame (or `NULL` if `data` was
#'       not supplied).}
#'     \item{`tree`}{The aligned `phylo` object (or `NULL` if `tree`
#'       was not supplied).}
#'   }
#'
#' @family reconciliation functions
#' @seealso [reconcile_tree()] to build the reconciliation;
#'   [reconcile_merge()] when you want a single merged data frame
#'   instead of aligned data + tree; [reconcile_export()] to write
#'   everything to disk.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' rec <- reconcile_tree(avonet_subset, tree_jetz,
#'                       x_species = "Species1", authority = NULL)
#'
#' aligned <- reconcile_apply(rec,
#'                            data = avonet_subset,
#'                            tree = tree_jetz,
#'                            species_col = "Species1",
#'                            drop_unresolved = TRUE)
#' nrow(aligned$data)
#' ape::Ntip(aligned$tree)
#'
#' # aligned$data and aligned$tree are ready for downstream PCM tools
#'
#' @export
reconcile_apply <- function(reconciliation, data = NULL, tree = NULL,
                            species_col = NULL,
                            drop_unresolved = FALSE) {

  validate_reconciliation(reconciliation)
  mapping <- reconciliation$mapping

  # Get matched names (in both x and y)
  matched <- mapping[mapping$in_x & mapping$in_y, ]

  result <- list(data = NULL, tree = NULL)

  # Align data frame

  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      abort("`data` must be a data frame.", call = caller_env())
    }

    if (is.null(species_col)) {
      species_col <- pr_detect_species_column(data, "species_col")
    }
    if (!species_col %in% names(data)) {
      cli::cli_abort(c(
        "Column {.val {species_col}} not found in {.arg data}.",
        "i" = "Available columns: {.val {names(data)}}."
      ))
    }

    data_names <- as.character(data[[species_col]])

    if (drop_unresolved) {
      keep <- data_names %in% matched$name_x
      data <- data[keep, , drop = FALSE]
      n_dropped <- sum(!keep)
      if (n_dropped > 0) {
        cli_alert_warning("Dropped {n_dropped} rows with unresolved species from data")
      }
    }

    result$data <- data
  }

  # Align tree
  if (!is.null(tree)) {
    tree <- pr_load_tree(tree)
    tree <- pr_align_tree(tree, mapping, drop_unresolved = drop_unresolved)

    if (drop_unresolved) {
      n_tips <- length(tree$tip.label)
      cli_alert_info("Tree has {n_tips} tips after alignment")
    }

    result$tree <- tree
  }

  result
}


#' Load overrides from a data frame or file path
#'
#' @param overrides A data frame, file path to CSV, or NULL.
#' @return A data frame with columns `name_x`, `name_y`, and optionally
#'   `user_note`, or NULL.
#' @keywords internal
pr_load_overrides <- function(overrides) {
  if (is.null(overrides)) return(NULL)

  if (is.character(overrides) && length(overrides) == 1) {
    if (!file.exists(overrides)) {
      abort(
        c("Overrides file not found.", "x" = paste0("Path: ", overrides)),
        call = caller_env()
      )
    }
    overrides <- utils::read.csv(overrides, stringsAsFactors = FALSE)
  }

  if (!is.data.frame(overrides)) {
    abort("`overrides` must be a data frame or a file path to a CSV.",
          call = caller_env())
  }

  if (!"name_x" %in% names(overrides) || !"name_y" %in% names(overrides)) {
    abort(
      c(
        "`overrides` must have columns `name_x` and `name_y`.",
        "i" = paste0("Columns found: ", paste(names(overrides), collapse = ", "))
      ),
      call = caller_env()
    )
  }

  if (!"user_note" %in% names(overrides)) {
    overrides$user_note <- "Pre-built override"
  }

  overrides
}
