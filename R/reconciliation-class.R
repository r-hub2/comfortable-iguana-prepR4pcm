# reconciliation S3 class --------------------------------------------------

#' The `reconciliation` S3 class
#'
#' A `reconciliation` object is the shared data structure that every
#' matching function in \pkg{prepR4pcm} returns, and that every
#' downstream function consumes. You will never build one by hand;
#' call [reconcile_tree()], [reconcile_data()], [reconcile_trees()],
#' [reconcile_to_trees()], or [reconcile_multi()] instead. This page
#' documents the structure so you can poke at the internals when
#' debugging or writing custom helpers.
#'
#' @section Structure:
#' A `reconciliation` is an S3 list with five components:
#' \describe{
#'   \item{`mapping`}{A tibble with one row per unique name seen in
#'     either source. Columns are documented in [reconcile_mapping()]:
#'     `name_x`, `name_y`, `name_resolved`, `match_type` (one of
#'     `"exact"`, `"normalized"`, `"synonym"`, `"fuzzy"`, `"manual"`,
#'     `"flagged"`, `"unresolved"`, or --- when surfaced via
#'     `reconcile_mapping(include_unused_overrides = TRUE)` ---
#'     `"override_unused"`), `match_score`, `match_source`, `in_x`,
#'     `in_y`, `notes`.}
#'   \item{`meta`}{A named list of provenance metadata --- call
#'     signature, timestamp, source labels, taxonomic authority,
#'     fuzzy settings, resolve mode, rank, \pkg{prepR4pcm} version.}
#'   \item{`counts`}{A named list of match-type counts, used by the
#'     print method and by [reconcile_summary()].}
#'   \item{`overrides`}{A tibble logging manual corrections applied
#'     via [reconcile_override()] or [reconcile_override_batch()].}
#'   \item{`unused_overrides`}{A tibble of overrides that the cascade
#'     could NOT apply, with columns `name_x`, `name_y`, and `reason`
#'     (one of `name_x_not_in_data`, `name_y_not_in_target`, or
#'     `already_matched`). Empty when no overrides were supplied or
#'     when every override applied successfully. Surfaced in
#'     `reconcile_summary()`, `reconcile_report()` (HTML),
#'     `reconcile_export()` (as `<prefix>_unused_overrides.csv`), and
#'     `reconcile_mapping(include_unused_overrides = TRUE)`.}
#' }
#'
#' @section Methods:
#' Standard S3 methods are defined for `print()`, `summary()` (which
#' dispatches to [reconcile_summary()]), and `format()`.
#'
#' @section Accessing the object:
#' \itemize{
#'   \item [reconcile_mapping()] --- extract the per-name tibble.
#'   \item [reconcile_summary()] --- human-readable breakdown.
#'   \item [reconcile_apply()] --- align data and tree.
#'   \item [reconcile_merge()] --- join two datasets.
#'   \item [reconcile_override()] / [reconcile_override_batch()] ---
#'     manual corrections.
#' }
#'
#' @param mapping A tibble with the mapping table (see above).
#' @param meta A named list of provenance metadata.
#' @param counts A named list of summary counts. Computed from
#'   `mapping` if `NULL`.
#' @param overrides A tibble of manual overrides (empty by default).
#' @param unused_overrides A tibble of overrides that could not be
#'   applied, with columns `name_x`, `name_y`, `reason`. If `NULL`,
#'   pulled from `attr(mapping, "unused_overrides")` when present,
#'   else initialised empty.
#'
#' @return An object of class `reconciliation`.
#'
#' @aliases reconciliation
#' @name reconciliation
#' @keywords internal
new_reconciliation <- function(mapping, meta, counts = NULL, overrides = NULL,
                               unused_overrides = NULL) {

  if (is.null(overrides)) {
    overrides <- tibble(
      name_x    = character(),
      name_y    = character(),
      action    = character(),
      user_note = character(),
      timestamp = as.POSIXct(character())
    )
  }

  if (is.null(unused_overrides)) {
    unused_overrides <- attr(mapping, "unused_overrides")
    if (is.null(unused_overrides)) {
      unused_overrides <- tibble(
        name_x = character(),
        name_y = character(),
        reason = character()
      )
    }
  }
  # Strip the attribute from `mapping` so it's not duplicated.
  attr(mapping, "unused_overrides") <- NULL

  if (is.null(counts)) {
    counts <- pr_compute_counts(mapping)
  }

  structure(
    list(
      mapping          = mapping,
      meta             = meta,
      counts           = counts,
      overrides        = overrides,
      unused_overrides = unused_overrides
    ),
    class = "reconciliation"
  )
}

#' Validate a reconciliation object
#'
#' Checks that all required components are present and correctly typed.
#'
#' @param reconciliation A `reconciliation` object.
#' @return `reconciliation`, invisibly, if valid. Throws an error otherwise.
#' @keywords internal
validate_reconciliation <- function(reconciliation) {
  if (!inherits(reconciliation, "reconciliation")) {
    abort("Input must be a <reconciliation> object.", call = caller_env())
  }

  required_fields <- c("mapping", "meta", "counts", "overrides")
  missing <- setdiff(required_fields, names(reconciliation))
  if (length(missing) > 0) {
    abort(
      paste0("Reconciliation object is missing fields: ",
             paste(missing, collapse = ", ")),
      call = caller_env()
    )
  }

  required_cols <- c("name_x", "name_y", "name_resolved", "match_type",
                     "match_score", "match_source", "in_x", "in_y", "notes")
  missing_cols <- setdiff(required_cols, names(reconciliation$mapping))
  if (length(missing_cols) > 0) {
    abort(
      paste0("Mapping table is missing columns: ",
             paste(missing_cols, collapse = ", ")),
      call = caller_env()
    )
  }

  invisible(reconciliation)
}

#' Compute summary counts from a mapping table
#'
#' @param mapping A mapping tibble.
#' @return A named list of counts.
#' @keywords internal
pr_compute_counts <- function(mapping) {
  types <- mapping$match_type

  list(
    n_x            = sum(mapping$in_x, na.rm = TRUE),
    n_y            = sum(mapping$in_y, na.rm = TRUE),
    n_exact        = sum(types == "exact", na.rm = TRUE),
    n_normalized   = sum(types == "normalized", na.rm = TRUE),
    n_synonym      = sum(types == "synonym", na.rm = TRUE),
    n_fuzzy        = sum(types == "fuzzy", na.rm = TRUE),
    n_unresolved_x = sum(types == "unresolved" & mapping$in_x &
                           !mapping$in_y, na.rm = TRUE),
    n_unresolved_y = sum(types == "unresolved" & !mapping$in_x &
                           mapping$in_y, na.rm = TRUE),
    n_flagged      = sum(types == "flagged", na.rm = TRUE),
    n_manual       = sum(types == "manual", na.rm = TRUE),
    n_augmented    = sum(types == "augmented", na.rm = TRUE)
  )
}


# S3 methods ---------------------------------------------------------------

#' @export
print.reconciliation <- function(x, ...) {
  meta <- x$meta
  counts <- x$counts

  type_label <- switch(
    meta$type,
    data_data = "data vs data",
    data_tree = "data vs tree",
    tree_tree = "tree vs tree",
    multi     = "multiple datasets vs tree",
    meta$type
  )

  cli_h1("Reconciliation: {type_label}")
  cli_bullets(c(
    " " = "Source x: {meta$x_source}",
    " " = "Source y: {meta$y_source}",
    " " = "Authority: {meta$authority %||% 'none'}",
    " " = "Timestamp: {format(meta$timestamp, '%Y-%m-%d %H:%M:%S')}"
  ))

  n_total_x <- counts$n_x
  pct <- function(n) {
    if (n_total_x == 0) return("")
    sprintf(" (%4.1f%%)", 100 * n / n_total_x)
  }

  # Coverage bar
  n_matched <- counts$n_exact + counts$n_normalized + counts$n_synonym +
               counts$n_fuzzy + counts$n_manual
  if (n_total_x > 0) {
    pct_matched <- n_matched / n_total_x
    bar_width <- 30
    filled <- round(pct_matched * bar_width)
    bar <- paste0(
      strrep("\u2588", filled),
      strrep("\u2591", bar_width - filled)
    )
    cli_alert_info(
      "Match coverage: [{bar}] {sprintf('%.0f%%', pct_matched * 100)} ({n_matched}/{n_total_x})"
    )
  }

  cli_h2("Match summary")
  bullets <- c(
    "*" = "Exact:              {.val {counts$n_exact}}{pct(counts$n_exact)}",
    "*" = "Normalized:         {.val {counts$n_normalized}}{pct(counts$n_normalized)}",
    "*" = "Synonym:            {.val {counts$n_synonym}}{pct(counts$n_synonym)}",
    "*" = "Fuzzy:              {.val {counts$n_fuzzy}}{pct(counts$n_fuzzy)}",
    "*" = "Manual:             {.val {counts$n_manual}}{pct(counts$n_manual)}",
    "!" = "Unresolved (x only):{.val {counts$n_unresolved_x}}{pct(counts$n_unresolved_x)}",
    "!" = "Unresolved (y only):{.val {counts$n_unresolved_y}}",
    "!" = "Flagged for review: {.val {counts$n_flagged}}"
  )
  n_unused_ov <- if (!is.null(x$unused_overrides))
                   nrow(x$unused_overrides) else 0L
  if (n_unused_ov > 0) {
    bullets <- c(bullets,
                 "!" = "Overrides unused:   {.val {n_unused_ov}}")
  }
  cli_bullets(bullets)

  cli_alert_info(
    "Use {.fn reconcile_summary} for details, {.fn reconcile_mapping} for the full table."
  )

  invisible(x)
}

#' @export
summary.reconciliation <- function(object, ...) {
  reconcile_summary(object, detail = "brief", ...)
}

#' @export
format.reconciliation <- function(x, ...) {
  meta <- x$meta
  counts <- x$counts
  n <- counts$n_x

  type_label <- switch(
    meta$type,
    data_data = "data vs data",
    data_tree = "data vs tree",
    tree_tree = "tree vs tree",
    multi     = "multi vs tree",
    meta$type
  )

  c(
    sprintf("Reconciliation: %s (%s)", type_label,
            format(meta$timestamp, "%Y-%m-%d")),
    sprintf("  x: %s (%d names) | y: %s",
            meta$x_source, n, meta$y_source),
    sprintf("  Matched: %d exact, %d normalized, %d synonym, %d fuzzy, %d manual",
            counts$n_exact, counts$n_normalized, counts$n_synonym,
            counts$n_fuzzy, counts$n_manual),
    sprintf("  Unresolved: %d (x only), %d (y only), %d flagged",
            counts$n_unresolved_x, counts$n_unresolved_y, counts$n_flagged)
  )
}
