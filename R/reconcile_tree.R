#' Reconcile species names between a dataset and a phylogenetic tree
#'
#' Match the species in a trait data frame (`x`) to the tip labels of a
#' phylogenetic tree (`tree`), producing a [reconciliation] object ready
#' to feed into [reconcile_apply()], PGLS, phylogenetic GLMMs, ancestral
#' state reconstruction, or any other phylogenetic comparative method
#' (PCM). This is typically the first function you call in a `prepR4pcm`
#' workflow.
#'
#' @details
#' Internally, `reconcile_tree()` treats the tree's tip labels as the
#' `y` argument of [reconcile_data()] and runs the same four-stage
#' matching cascade (exact -> normalized -> synonym -> fuzzy). Tip labels
#' typically differ from data names only in formatting (underscores,
#' capitalisation, authority strings), so even with `authority = NULL`
#' you usually recover most matches at the *normalized* stage. Turn on
#' `fuzzy = TRUE` to also catch spelling mistakes.
#'
#' After reconciliation, the typical workflow is:
#' \enumerate{
#'   \item Inspect with [reconcile_summary()] or [reconcile_plot()].
#'   \item Investigate unresolved names with [reconcile_suggest()] and
#'     fix them with [reconcile_override()] or
#'     [reconcile_override_batch()].
#'   \item Produce an aligned data frame and pruned tree via
#'     [reconcile_apply()].
#'   \item Optionally, graft orphan species onto the tree with
#'     [reconcile_augment()] (exploratory only; always run sensitivity
#'     analyses).
#' }
#'
#' @param x A data frame containing the trait data. Must have one
#'   column of scientific names.
#' @param tree An `ape::phylo` object, or a length-1 character vector
#'   giving the path to a Newick (`.nwk`, `.tre`, `.tree`) or Nexus
#'   (`.nex`, `.nexus`) file. File format is auto-detected.
#' @param x_species A length-1 character vector. Name of the column
#'   in `x` containing scientific names (the same column referenced
#'   by `x` above; the term \dQuote{species names} elsewhere in this
#'   help page is a synonym for the same scientific names). When
#'   `NULL`, the column is auto-detected from a small list of common
#'   labels (e.g. `species`, `Species1`, `scientific_name`); the list
#'   is not exhaustive --- pass the column name explicitly if your
#'   data uses a non-standard label.
#' @inheritParams reconcile_data
#'
#' @return A [reconciliation] object with `meta$type == "data_tree"`.
#'   The `mapping` tibble has one row per unique name: matched species
#'   (`in_x & in_y`), data-only orphans (`in_x & !in_y`, candidates for
#'   [reconcile_augment()]), and tree-only orphans (`!in_x & in_y`,
#'   candidates for [reconcile_apply()] to prune).
#'
#' @family reconciliation functions
#' @seealso [reconcile_apply()] to produce an aligned data-tree pair;
#'   [reconcile_augment()] to add orphan species back to the tree;
#'   [reconcile_to_trees()] to reconcile against several trees at once;
#'   [reconcile_data()] for the data-only counterpart.
#'
#' @references
#' Paradis, E. & Schliep, K. (2019) ape 5.0: an environment for modern
#' phylogenetics and evolutionary analyses in R. \emph{Bioinformatics}
#' 35:526--528. \doi{10.1093/bioinformatics/bty633}
#'
#' @examples
#' # Reconcile the bundled AVONET subset against the Jetz et al. (2012)
#' # bird tree. `authority = NULL` keeps the example offline; in a real
#' # analysis you would usually set `authority = "col"` (Catalogue of
#' # Life) to pick up taxonomic synonyms.
#' data(avonet_subset)
#' data(tree_jetz)
#'
#' rec <- reconcile_tree(
#'   avonet_subset, tree_jetz,
#'   x_species = "Species1",
#'   authority = NULL,
#'   fuzzy     = TRUE          # also catch typos
#' )
#' rec                         # one-line status
#' reconcile_summary(rec)      # full breakdown by match type
#'
#' # Produce aligned data + pruned tree ready for PGLS / PGLMM
#' aligned <- reconcile_apply(rec,
#'                            data = avonet_subset,
#'                            tree = tree_jetz,
#'                            species_col = "Species1",
#'                            drop_unresolved = TRUE)
#' nrow(aligned$data)
#' ape::Ntip(aligned$tree)
#'
#' @export
reconcile_tree <- function(x, tree,
                           x_species = NULL,
                           authority = "col",
                           rank = c("species", "subspecies"),
                           overrides = NULL,
                           db_version = NULL,
                           fuzzy = FALSE,
                           fuzzy_threshold = 0.9,
                           flag_threshold = 0.95,
                           resolve = c("flag", "first"),
                           quiet = FALSE,
                           x_label = NULL) {

  # Capture source label before any modifications to x
  x_source <- x_label %||% deparse(substitute(x))

  rank <- match.arg(rank)
  resolve <- match.arg(resolve)

  # Validate data frame
  if (!is.data.frame(x)) abort("`x` must be a data frame.", call = caller_env())

  authority <- pr_validate_authority(authority)

  # Detect species column
  if (is.null(x_species)) x_species <- pr_detect_species_column(x, "x_species")
  if (!x_species %in% names(x)) {
    abort(paste0("Column '", x_species, "' not found in `x`."),
          call = caller_env())
  }

  # Load tree
  tree_obj <- pr_load_tree(tree)
  tips <- tree_obj$tip.label

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

  # Describe tree source for metadata
  tree_source <- if (is.character(tree)) {
    basename(tree)
  } else {
    sprintf("phylo (%d tips)", length(tips))
  }

  # Load overrides
  overrides_df <- pr_load_overrides(overrides)

  if (!quiet) {
    cli_alert_info(
      "Reconciling {length(unique(names_x))} data names vs {length(tips)} tree tips"
    )
  }

  # Run cascade
  mapping <- pr_run_cascade(
    names_x         = names_x,
    names_y         = tips,
    authority       = authority,
    db_version      = db_version,
    rank            = rank,
    overrides       = overrides_df,
    fuzzy           = fuzzy,
    fuzzy_threshold = fuzzy_threshold,
    flag_threshold  = flag_threshold,
    resolve         = resolve,
    quiet           = quiet
  )

  # Build metadata
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
    rank             = rank
  )

  result <- new_reconciliation(mapping = mapping, meta = meta)

  # Surface unused overrides (issue #8a). Silent drops are surprising;
  # warn the user and direct them to `result$unused_overrides`.
  if (!quiet && nrow(result$unused_overrides) > 0) {
    pr_warn_unused_overrides(result$unused_overrides)
  }

  if (!quiet) {
    n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
    n_total <- result$counts$n_x
    cli_alert_success("Matched {n_matched}/{n_total} data names to tree tips")
  }

  result
}
