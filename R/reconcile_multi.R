#' Reconcile several datasets against one phylogenetic tree
#'
#' Match several trait or occurrence datasets against a single
#' phylogenetic tree in one call. Species that appear in more than one
#' dataset are reconciled once; the combined mapping records which
#' dataset(s) each species belongs to, making it easy to identify the
#' set of species with complete trait coverage.
#'
#' @param datasets A named list of data frames. The names are used as
#'   dataset labels (e.g. `morpho`, `nests`, `plumage`) in the output.
#' @param tree An `ape::phylo` object, or a path to a Newick/Nexus
#'   file.
#' @param species_cols Character vector. Species column name in each
#'   dataset. If length 1, the same column name is used for every
#'   dataset. Auto-detected from each data frame if `NULL`.
#' @inheritParams reconcile_data
#'
#' @return A [reconciliation] object. The mapping tibble gains one
#'   logical column per input dataset (e.g. `in_morpho`, `in_nests`)
#'   indicating which datasets contained each species.
#'
#' @family reconciliation functions
#' @seealso [reconcile_tree()] for the single-dataset case;
#'   [reconcile_merge()] to join two datasets after reconciliation.
#'
#' @examples
#' data(avonet_subset)
#' data(nesttrait_subset)
#' data(tree_jetz)
#' datasets <- list(
#'   morpho = avonet_subset,
#'   nests  = nesttrait_subset
#' )
#' result <- reconcile_multi(datasets, tree_jetz,
#'                           species_cols = c("Species1", "Scientific_name"),
#'                           authority = NULL)
#' print(result)
#'
#' @export
reconcile_multi <- function(datasets, tree,
                            species_cols = NULL,
                            authority = "col",
                            rank = c("species", "subspecies"),
                            overrides = NULL,
                            db_version = NULL,
                            fuzzy = FALSE,
                            fuzzy_threshold = 0.9,
                            resolve = c("flag", "first"),
                            quiet = FALSE) {

  rank <- match.arg(rank)
  resolve <- match.arg(resolve)

  # Validate inputs
  if (!is.list(datasets) || length(datasets) == 0) {
    abort("`datasets` must be a non-empty named list of data frames.",
          call = caller_env())
  }

  if (is.null(names(datasets))) {
    names(datasets) <- paste0("dataset_", seq_along(datasets))
  }

  for (nm in names(datasets)) {
    if (!is.data.frame(datasets[[nm]])) {
      abort(paste0("Element '", nm, "' of `datasets` is not a data frame."),
            call = caller_env())
    }
    if (nrow(datasets[[nm]]) == 0) {
      abort(paste0("Element '", nm, "' of `datasets` has 0 rows."),
            call = caller_env())
    }
  }

  # Handle species_cols
  if (!is.null(species_cols)) {
    if (length(species_cols) == 1) {
      species_cols <- rep(species_cols, length(datasets))
    }
    if (length(species_cols) != length(datasets)) {
      abort(
        paste0("`species_cols` must have length 1 or ", length(datasets), "."),
        call = caller_env()
      )
    }
  }

  # Load tree once
  tree_obj <- pr_load_tree(tree)
  tips <- tree_obj$tip.label

  tree_source <- if (is.character(tree)) {
    basename(tree)
  } else {
    sprintf("phylo (%d tips)", length(tips))
  }

  # Collect all unique species names per-dataset and across datasets.
  # Per-dataset names are kept so we can add `in_<dataset>` logical
  # columns to the mapping. Issue #10 (Ayumi).
  per_dataset_names <- vector("list", length(datasets))
  names(per_dataset_names) <- names(datasets)
  for (i in seq_along(datasets)) {
    df <- datasets[[i]]
    col <- if (!is.null(species_cols)) {
      species_cols[i]
    } else {
      pr_detect_species_column(df, paste0("species in '", names(datasets)[i], "'"))
    }
    per_dataset_names[[i]] <- unique(stats::na.omit(as.character(df[[col]])))
  }
  all_names <- unique(unlist(per_dataset_names, use.names = FALSE))

  if (!quiet) {
    cli_alert_info(
      "Reconciling {length(all_names)} unique names from {length(datasets)} datasets vs {length(tips)} tree tips"
    )
  }

  # Load overrides
  overrides_df <- pr_load_overrides(overrides)

  # Run cascade on combined names. multi_x = TRUE so the same tree
  # tip can resolve multiple x names that differ only in formatting
  # (e.g. `Homo_sapiens` from one dataset and `Homo sapiens` from
  # another both resolve to the tree tip via normalisation). Issue
  # #10 (Ayumi).
  mapping <- pr_run_cascade(
    names_x         = all_names,
    names_y         = tips,
    authority       = authority,
    db_version      = db_version,
    rank            = rank,
    overrides       = overrides_df,
    fuzzy           = fuzzy,
    fuzzy_threshold = fuzzy_threshold,
    resolve         = resolve,
    multi_x         = TRUE,
    quiet           = quiet
  )

  # Add one logical `in_<dataset>` column per input dataset, indicating
  # whether each `name_x` row appeared in that dataset. Documented in
  # the function's @return; was missing from the implementation. The
  # column is TRUE when the literal name_x is in the dataset's
  # species list (matched verbatim, including formatting).
  for (i in seq_along(datasets)) {
    col_name <- paste0("in_", names(datasets)[i])
    mapping[[col_name]] <- mapping$name_x %in% per_dataset_names[[i]]
  }

  # Build metadata
  meta <- list(
    call             = match.call(),
    type             = "multi",
    timestamp        = Sys.time(),
    authority        = authority %||% "none",
    db_version       = db_version %||% "latest",
    fuzzy            = fuzzy,
    fuzzy_threshold  = if (fuzzy) fuzzy_threshold else NA_real_,
    fuzzy_method     = if (fuzzy) "component_levenshtein" else NA_character_,
    resolve          = resolve,
    prepR4pcm_version = as.character(utils::packageVersion("prepR4pcm")),
    x_source         = paste(names(datasets), collapse = ", "),
    y_source         = tree_source,
    rank             = rank,
    dataset_names    = names(datasets)
  )

  result <- new_reconciliation(mapping = mapping, meta = meta)

  if (!quiet) {
    n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
    cli_alert_success("Matched {n_matched}/{length(all_names)} unique names to tree tips")
  }

  result
}
