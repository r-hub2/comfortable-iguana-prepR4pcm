#' Write an aligned dataset, tree, and mapping table to disk
#'
#' Apply a reconciliation and save three files: the aligned CSV, the
#' pruned tree, and the full mapping tibble. Intended for producing
#' analysis-ready, archivable outputs --- drop the three files into a
#' Zenodo deposit or a project's `data-output/` folder alongside the
#' reconciliation report and you have a fully documented provenance
#' trail.
#'
#' @param reconciliation A [reconciliation] object returned by
#'   [reconcile_tree()], [reconcile_data()], or a related matcher.
#' @param data A data frame to align. If `NULL`, only the tree and mapping
#'   are written.
#' @param tree An `ape::phylo` object or file path. If `NULL`, only the
#'   data and mapping are written.
#' @param species_col A length-1 character vector. Column name in `data`
#'   containing species names. Auto-detected when `NULL`.
#' @param dir A length-1 character vector. Path to the output directory
#'   that will receive the exported files (e.g. a project's
#'   `data-output/` folder, or a staging directory before a Zenodo
#'   deposit). Created if it does not exist. By default, a unique
#'   temporary directory is used so the function does not write to the
#'   current working directory unless you explicitly request it.
#' @param prefix A length-1 character vector. File name prefix. Default
#'   `"reconciled"`.
#' @param tree_format A length-1 character vector. Tree output format:
#'   `"nexus"` (default) or `"newick"`.
#' @param drop_unresolved Logical. Drops unresolved species when `TRUE`.
#'   Default `TRUE`.
#'
#' @return A named list of file paths (invisibly):
#'   `$data` (CSV), `$tree` (Nexus or Newick), `$mapping` (CSV), and
#'   `$unused_overrides` (CSV; `NULL` when there are no rejected
#'   overrides on the reconciliation).
#'
#' @family reconciliation functions
#' @seealso [reconcile_apply()] for in-memory alignment without writing
#'   to disk; [reconcile_report()] for a self-contained HTML audit
#'   trail.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' result <- reconcile_tree(avonet_subset, tree_jetz,
#'                          x_species = "Species1", authority = NULL)
#' out_dir <- tempfile("export_")
#' files <- reconcile_export(result,
#'                           data = avonet_subset, tree = tree_jetz,
#'                           species_col = "Species1",
#'                           dir = out_dir, prefix = "avonet_jetz")
#' files$data     # path to CSV
#' files$tree     # path to Nexus tree
#' files$mapping  # path to mapping CSV
#' unlink(out_dir, recursive = TRUE)  # clean up
#'
#' @export
reconcile_export <- function(reconciliation, data = NULL, tree = NULL,
                              species_col = NULL,
                              dir = tempfile("prepR4pcm-export-"),
                              prefix = "reconciled",
                              tree_format = c("nexus", "newick"),
                              drop_unresolved = TRUE) {

  validate_reconciliation(reconciliation)
  tree_format <- match.arg(tree_format)

  # Create output directory
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  # Apply the reconciliation
  aligned <- reconcile_apply(
    reconciliation,
    data = data,
    tree = tree,
    species_col = species_col,
    drop_unresolved = drop_unresolved
  )

  paths <- list(data = NULL, tree = NULL, mapping = NULL)

  # Write aligned data
  if (!is.null(aligned$data)) {
    path_data <- file.path(dir, paste0(prefix, "_data.csv"))
    utils::write.csv(aligned$data, path_data, row.names = FALSE)
    paths$data <- path_data
    cli_alert_success("Data written to {.path {path_data}}")
  }

  # Write aligned tree
  if (!is.null(aligned$tree)) {
    ext <- if (tree_format == "nexus") ".nex" else ".nwk"
    path_tree <- file.path(dir, paste0(prefix, "_tree", ext))

    if (tree_format == "nexus") {
      ape::write.nexus(aligned$tree, file = path_tree)
    } else {
      ape::write.tree(aligned$tree, file = path_tree)
    }
    paths$tree <- path_tree
    cli_alert_success("Tree written to {.path {path_tree}}")
  }

  # Write mapping table
  path_mapping <- file.path(dir, paste0(prefix, "_mapping.csv"))
  utils::write.csv(
    as.data.frame(reconciliation$mapping),
    path_mapping,
    row.names = FALSE
  )
  paths$mapping <- path_mapping
  cli_alert_success("Mapping written to {.path {path_mapping}}")

  # Write unused-override audit trail when non-empty. Always reflected
  # in the returned `paths` list so callers can rely on its shape, but
  # the CSV is only created when there is something to write.
  paths$unused_overrides <- NULL
  unused <- reconciliation$unused_overrides
  if (!is.null(unused) && nrow(unused) > 0) {
    path_unused <- file.path(dir, paste0(prefix, "_unused_overrides.csv"))
    utils::write.csv(
      as.data.frame(unused),
      path_unused,
      row.names = FALSE
    )
    paths$unused_overrides <- path_unused
    cli_alert_success(
      "Unused overrides written to {.path {path_unused}}"
    )
  }

  invisible(paths)
}
