# Base R plots for reconciliation results -----------------------------------

#' Plot the match composition of a reconciliation
#'
#' Draw a one-glance bar or pie chart of how species names were
#' resolved (exact, normalised, synonym, fuzzy, flagged, manual,
#' unresolved). Uses base R graphics only, so no additional packages
#' are required.
#'
#' @param reconciliation A [reconciliation] object returned by
#'   [reconcile_tree()], [reconcile_data()], or a related matcher.
#' @param type A length-1 character vector. Plot style:
#'   \describe{
#'     \item{`"bar"` (default)}{Horizontal stacked bar chart. Best for
#'       slides, reports, and scripting.}
#'     \item{`"pie"`}{Pie chart. Useful when the match types are
#'       roughly balanced.}
#'   }
#' @param ... Additional arguments passed on to [graphics::barplot()]
#'   or [graphics::pie()] (e.g. `main`, `col`, `border`).
#'
#' @return The input `reconciliation`, invisibly, so you can use the
#'   function in a pipe.
#'
#' @family reconciliation functions
#' @seealso [reconcile_summary()] for a textual breakdown;
#'   [reconcile_report()] for a full HTML audit trail.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' rec <- reconcile_tree(avonet_subset, tree_jetz,
#'                       x_species = "Species1", authority = NULL)
#' reconcile_plot(rec)
#' reconcile_plot(rec, type = "pie")
#'
#' @export
reconcile_plot <- function(reconciliation, type = c("bar", "pie"), ...) {

  validate_reconciliation(reconciliation)

  type <- match.arg(type)

  counts <- reconciliation$counts

  # Categories, colours, and counts (in display order)
  categories <- c(
    Exact      = counts$n_exact,
    Normalized = counts$n_normalized,
    Synonym    = counts$n_synonym,
    Fuzzy      = counts$n_fuzzy,
    Manual     = counts$n_manual,
    Flagged    = counts$n_flagged,
    Unresolved = counts$n_unresolved_x + counts$n_unresolved_y

)

  palette <- c(
    Exact      = "#4CAF50",
    Normalized = "#2196F3",
    Synonym    = "#9C27B0",
    Fuzzy      = "#FF9800",
    Manual     = "#009688",
    Flagged    = "#FFC107",
    Unresolved = "#F44336"
  )

  # Keep only categories with count > 0
  keep <- categories > 0
  categories <- categories[keep]
  palette    <- palette[names(categories)]

  n_matched <- sum(categories) - (counts$n_unresolved_x + counts$n_unresolved_y)
  n_total_x <- counts$n_x

  if (type == "bar") {
    pr_plot_bar(categories, palette, n_matched, n_total_x, ...)
  } else {
    pr_plot_pie(categories, palette, ...)
  }

  invisible(reconciliation)
}


# Internal plotting helpers ------------------------------------------------

#' Draw horizontal stacked bar chart
#' @keywords internal
#' @noRd
pr_plot_bar <- function(categories, palette, n_matched, n_total_x, ...) {

  vals <- matrix(categories, ncol = 1)
  old_par <- graphics::par(mar = c(4, 1, 3, 1))
  on.exit(graphics::par(old_par), add = TRUE)

  dots <- list(...)
  bar_args <- list(
    height    = vals,
    horiz     = TRUE,
    col       = palette,
    border    = NA,
    axes      = FALSE,
    names.arg = "",
    main      = dots$main %||% "Match composition",
    sub       = sprintf("Matched: %d / %d", n_matched, n_total_x)
  )
  # Merge extra args (excluding main to avoid duplicate)
  dots$main <- NULL
  bar_args <- c(bar_args, dots)

  do.call(graphics::barplot, bar_args)

  graphics::legend(
    "bottomright",
    legend = sprintf("%s (%d)", names(categories), categories),
    fill   = palette,
    border = NA,
    bty    = "n",
    cex    = 0.85
  )
}

#' Draw pie chart
#' @keywords internal
#' @noRd
pr_plot_pie <- function(categories, palette, ...) {
  labels <- sprintf("%s (%d)", names(categories), categories)

  dots <- list(...)
  pie_args <- list(
    x      = categories,
    labels = labels,
    col    = palette,
    border = NA,
    main   = dots$main %||% "Match composition"
  )
  dots$main <- NULL
  pie_args <- c(pie_args, dots)

  do.call(graphics::pie, pie_args)
}
