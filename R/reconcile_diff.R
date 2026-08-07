# Compare two reconciliation objects ----------------------------------------

#' Diff two reconciliations to see what changed
#'
#' Compare a "before" and "after" [reconciliation] and list every
#' species whose outcome differs: newly matched, newly unresolved,
#' promoted to a higher-confidence match type, or linked to a different
#' target. Useful for:
#' \itemize{
#'   \item checking the effect of adding a taxonomy crosswalk or a
#'     batch of manual overrides,
#'   \item comparing two taxonomic authorities (e.g. Catalogue of Life
#'     vs GBIF),
#'   \item auditing changes between runs before and after tightening
#'     the fuzzy threshold.
#' }
#'
#' @param x A [reconciliation] object --- the "before" state.
#' @param y A [reconciliation] object --- the "after" state. Must be
#'   reconciled against the same `x` data so that `name_x` values are
#'   comparable.
#' @param quiet Logical. Suppresses the console summary when `TRUE`.
#'   Default `FALSE`.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{`gained`}{Tibble of species matched in `y` but unresolved
#'       in `x`.}
#'     \item{`lost`}{Tibble of species matched in `x` but unresolved in
#'       `y`.}
#'     \item{`type_changed`}{Tibble of species whose `match_type`
#'       differs between the two runs.}
#'     \item{`target_changed`}{Tibble of species whose `name_y`
#'       differs.}
#'     \item{`unused_overrides_diff`}{Tibble of overrides that are in
#'       the `unused_overrides` slot of one reconciliation but not
#'       the other; columns `name_x`, `name_y`, `reason`, `side`
#'       (`"x"` or `"y"`).}
#'     \item{`summary`}{A one-row tibble with counts: `n_gained`,
#'       `n_lost`, `n_type_changed`, `n_target_changed`, `n_shared`,
#'       `n_unused_override_diff`.}
#'   }
#'
#' @family reconciliation functions
#' @seealso [reconcile_crosswalk()] for building an override table from
#'   a published taxonomy crosswalk; [reconcile_override_batch()] for
#'   applying many hand edits.
#'
#' @examples
#' x <- data.frame(species = c("A a", "A old", "B c"))
#' tree <- ape::read.tree(text = "((A_a:1,A_new:1):1,B_c:2);")
#'
#' # Without manual overrides
#' r1 <- reconcile_tree(x, tree, x_species = "species",
#'                      authority = NULL, quiet = TRUE)
#'
#' # With one manual override
#' overrides <- data.frame(name_x = "A old", name_y = "A new",
#'                         match_type = "manual")
#' r2 <- reconcile_tree(x, tree, x_species = "species",
#'                      authority = NULL, overrides = overrides,
#'                      quiet = TRUE)
#'
#' d <- reconcile_diff(r1, r2, quiet = TRUE)
#' cat("Gained:", nrow(d$gained), "| Lost:", nrow(d$lost), "\n")
#'
#' @export
reconcile_diff <- function(x, y, quiet = FALSE) {

  validate_reconciliation(x)

  validate_reconciliation(y)

  map_x <- x$mapping
  map_y <- y$mapping


  # Keep only species from source x (in_x == TRUE)
  mx <- map_x[map_x$in_x & !is.na(map_x$name_x), ]
  my <- map_y[map_y$in_x & !is.na(map_y$name_x), ]

  # Deduplicate by name_x (keep first occurrence)
  mx <- mx[!duplicated(mx$name_x), ]
  my <- my[!duplicated(my$name_x), ]

  # Identify shared species

  shared_names <- intersect(mx$name_x, my$name_x)

  mx_shared <- mx[match(shared_names, mx$name_x), ]
  my_shared <- my[match(shared_names, my$name_x), ]

  resolved_types <- c("exact", "normalized", "synonym", "fuzzy",
                       "manual", "augmented")

  matched_x <- mx_shared$match_type %in% resolved_types
  matched_y <- my_shared$match_type %in% resolved_types

  # Gained: unresolved/flagged in x, resolved in y
  gained_idx <- !matched_x & matched_y
  gained <- tibble(
    name_x         = my_shared$name_x[gained_idx],
    name_y_new     = my_shared$name_y[gained_idx],
    match_type_old = mx_shared$match_type[gained_idx],
    match_type_new = my_shared$match_type[gained_idx],
    match_score    = my_shared$match_score[gained_idx]
  )

  # Lost: resolved in x, unresolved/flagged in y
  lost_idx <- matched_x & !matched_y
  lost <- tibble(
    name_x         = mx_shared$name_x[lost_idx],
    name_y_old     = mx_shared$name_y[lost_idx],
    match_type_old = mx_shared$match_type[lost_idx],
    match_type_new = my_shared$match_type[lost_idx]
  )

  # Type changed: both resolved but different match_type
  both_matched <- matched_x & matched_y
  type_diff <- mx_shared$match_type != my_shared$match_type & both_matched
  type_changed <- tibble(
    name_x         = mx_shared$name_x[type_diff],
    match_type_old = mx_shared$match_type[type_diff],
    match_type_new = my_shared$match_type[type_diff],
    name_y         = my_shared$name_y[type_diff]
  )

  # Target changed: both resolved but different name_y
  name_y_x <- ifelse(is.na(mx_shared$name_y), "", mx_shared$name_y)
  name_y_y <- ifelse(is.na(my_shared$name_y), "", my_shared$name_y)
  target_diff <- name_y_x != name_y_y & both_matched
  target_changed <- tibble(
    name_x     = mx_shared$name_x[target_diff],
    name_y_old = mx_shared$name_y[target_diff],
    name_y_new = my_shared$name_y[target_diff]
  )

  # Unused-overrides diff: overrides in one reconciliation's
  # unused_overrides slot but not the other. Useful when comparing a
  # before-and-after run after editing the override table.
  ux <- x$unused_overrides
  uy <- y$unused_overrides
  if (is.null(ux)) ux <- tibble(name_x = character(), name_y = character(),
                                 reason = character())
  if (is.null(uy)) uy <- tibble(name_x = character(), name_y = character(),
                                 reason = character())
  ux_keys <- paste(ux$name_x, ux$name_y, ux$reason, sep = "\003")
  uy_keys <- paste(uy$name_x, uy$name_y, uy$reason, sep = "\003")
  only_in_x <- ux[!(ux_keys %in% uy_keys), , drop = FALSE]
  only_in_y <- uy[!(uy_keys %in% ux_keys), , drop = FALSE]
  unused_overrides_diff <- rbind(
    cbind(only_in_x, side = rep_len("x", nrow(only_in_x))),
    cbind(only_in_y, side = rep_len("y", nrow(only_in_y)))
  )

  # Summary
  summary_tbl <- tibble(
    n_gained                = nrow(gained),
    n_lost                  = nrow(lost),
    n_type_changed          = nrow(type_changed),
    n_target_changed        = nrow(target_changed),
    n_shared                = length(shared_names),
    n_unused_override_diff  = nrow(unused_overrides_diff)
  )

  # Console output
  if (!quiet) {
    cli_h2("Reconciliation diff")
    cli_bullets(c(
      " " = "Shared species (in both x and y): {.val {length(shared_names)}}",
      "*" = "Gained matches:    {.val {nrow(gained)}}",
      "!" = "Lost matches:      {.val {nrow(lost)}}",
      "*" = "Type changed:      {.val {nrow(type_changed)}}",
      "*" = "Target changed:    {.val {nrow(target_changed)}}"
    ))

    if (nrow(gained) > 0) {
      n_show <- min(nrow(gained), 5)
      cli_h2("Gained (first {n_show})")
      for (i in seq_len(n_show)) {
        cli_alert_success(
          '"{gained$name_x[i]}" -> "{gained$name_y_new[i]}" ({gained$match_type_new[i]})'
        )
      }
      if (nrow(gained) > 5) {
        cli_alert_info("... and {nrow(gained) - 5} more")
      }
    }

    if (nrow(lost) > 0) {
      n_show <- min(nrow(lost), 5)
      cli_h2("Lost (first {n_show})")
      for (i in seq_len(n_show)) {
        cli_alert_warning(
          '"{lost$name_x[i]}" was "{lost$name_y_old[i]}" ({lost$match_type_old[i]})'
        )
      }
      if (nrow(lost) > 5) {
        cli_alert_info("... and {nrow(lost) - 5} more")
      }
    }
  }

  list(
    gained                = gained,
    lost                  = lost,
    type_changed          = type_changed,
    target_changed        = target_changed,
    unused_overrides_diff = unused_overrides_diff,
    summary               = summary_tbl
  )
}
