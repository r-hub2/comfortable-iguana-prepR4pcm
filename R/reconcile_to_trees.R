#' Reconcile one dataset against multiple phylogenetic trees
#'
#' Takes a single data frame and matches it against each tree in a named
#' list, returning one `reconciliation` object per tree. This is the
#' standard workflow for generating separate tree-compatible datasets
#' aligned to different phylogenies (e.g., Clements 2023, 2024, 2025,
#' Jetz 2012).
#'
#' Species names in `x` are normalised once and reused across all trees,
#' so synonym lookups are not repeated.
#'
#' @param x A data frame.
#' @param trees A named list of `ape::phylo` objects or file paths.
#' @param x_species A length-1 character vector. Column name in `x` containing species
#'   names. Auto-detected if `NULL`.
#' @inheritParams reconcile_data
#'
#' @return A named list of [reconciliation] objects, one per tree, with
#'   the same names as `trees`.
#'
#' @family reconciliation functions
#' @seealso [reconcile_tree()] for the single-tree case;
#'   [reconcile_diff()] to compare two reconciliations (e.g. to quantify
#'   how many species are gained or lost by switching taxonomies).
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' data(tree_clements25)
#' results <- reconcile_to_trees(
#'   avonet_subset,
#'   trees = list(jetz = tree_jetz, clements = tree_clements25),
#'   x_species = "Species1",
#'   authority = NULL
#' )
#' # Compare overlap across trees
#' sapply(results, function(r) r$counts$n_exact)
#'
#' @export
reconcile_to_trees <- function(x, trees,
                                x_species = NULL,
                                authority = "col",
                                rank = c("species", "subspecies"),
                                overrides = NULL,
                                db_version = NULL,
                                fuzzy = FALSE,
                                fuzzy_threshold = 0.9,
                                resolve = c("flag", "first"),
                                quiet = FALSE,
                                x_label = NULL) {

  # Capture source label before any modifications to x
  x_source <- x_label %||% deparse(substitute(x))

  rank <- match.arg(rank)
  resolve <- match.arg(resolve)

  # Validate inputs
  if (!is.data.frame(x)) abort("`x` must be a data frame.", call = caller_env())

  if (!is.list(trees) || length(trees) == 0) {
    abort("`trees` must be a non-empty named list of phylo objects or file paths.",
          call = caller_env())
  }

  if (is.null(names(trees))) {
    names(trees) <- paste0("tree_", seq_along(trees))
  }

  authority <- pr_validate_authority(authority)

  # Detect species column once
  if (is.null(x_species)) x_species <- pr_detect_species_column(x, "x_species")
  if (!x_species %in% names(x)) {
    abort(paste0("Column '", x_species, "' not found in `x`."),
          call = caller_env())
  }

  # Input guards: empty data, all-NA species, factor columns
  if (nrow(x) == 0) abort("`x` has 0 rows.", call = caller_env())

  if (is.factor(x[[x_species]])) {
    cli_alert_warning("Converting factor column '{x_species}' in `x` to character.")
    x[[x_species]] <- as.character(x[[x_species]])
  }

  names_x <- as.character(x[[x_species]])

  if (all(is.na(names_x))) {
    abort("All species names in `x` are NA.", call = caller_env())
  }

  # Load overrides once
  overrides_df <- pr_load_overrides(overrides)

  if (!quiet) {
    cli_alert_info(
      "Reconciling {length(unique(names_x))} data names against {length(trees)} trees"
    )
  }

  # Run cascade for each tree
  results <- stats::setNames(
    vector("list", length(trees)),
    names(trees)
  )

  for (nm in names(trees)) {
    tree_obj <- pr_load_tree(trees[[nm]])
    tips <- tree_obj$tip.label

    tree_source <- if (is.character(trees[[nm]])) {
      basename(trees[[nm]])
    } else {
      sprintf("phylo (%d tips)", length(tips))
    }

    if (!quiet) {
      cli_alert_info("  [{nm}] {length(tips)} tips")
    }

    mapping <- pr_run_cascade(
      names_x         = names_x,
      names_y         = tips,
      authority       = authority,
      db_version      = db_version,
      rank            = rank,
      overrides       = overrides_df,
      fuzzy           = fuzzy,
      fuzzy_threshold = fuzzy_threshold,
      resolve         = resolve,
      quiet           = quiet
    )

    meta <- list(
      call             = match.call(),
      type             = "data_tree",
      timestamp        = Sys.time(),
      authority        = authority %||% "none",
      db_version       = db_version %||% "latest",
      fuzzy            = fuzzy,
      fuzzy_threshold  = if (fuzzy) fuzzy_threshold else NA_real_,
      fuzzy_method     = if (fuzzy) "component_levenshtein" else NA_character_,
      resolve          = resolve,
      prepR4pcm_version = as.character(utils::packageVersion("prepR4pcm")),
      x_source         = x_source,
      y_source         = tree_source,
      rank             = rank,
      tree_name        = nm
    )

    result <- new_reconciliation(mapping = mapping, meta = meta)

    if (!quiet) {
      n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
      n_total <- result$counts$n_x
      cli_alert_success("  [{nm}] Matched {n_matched}/{n_total} names")
    }

    results[[nm]] <- result
  }

  results
}
