#' Reconcile tip labels between two phylogenetic trees
#'
#' Compare the tip labels of two phylogenetic trees and report which
#' species are shared, which differ only in formatting or synonymy, and
#' which appear in only one of the two trees. Use this when assessing
#' the impact of switching phylogenies (e.g., Jetz et al. 2012 vs
#' Clements 2025) before deciding which tree to use in a downstream PCM.
#'
#' @param tree1 An `ape::phylo` object, or a character(1) path to a
#'   Newick/Nexus tree file.
#' @param tree2 An `ape::phylo` object, or a character(1) path to a
#'   Newick/Nexus tree file.
#' @inheritParams reconcile_data
#'
#' @return A [reconciliation] object with `meta$type == "tree_tree"`.
#'
#' @family reconciliation functions
#' @seealso [reconcile_diff()] to quantify gains/losses between two
#'   reconciliations; [reconcile_to_trees()] when you want to match a
#'   single dataset against many trees at once.
#'
#' @examples
#' data(tree_jetz)
#' data(tree_clements25)
#' rec <- reconcile_trees(tree_jetz, tree_clements25, authority = NULL)
#' rec
#' # How many tips are shared across both trees?
#' sum(reconcile_mapping(rec)$in_x & reconcile_mapping(rec)$in_y)
#'
#' @export
reconcile_trees <- function(tree1, tree2,
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

  authority <- pr_validate_authority(authority)

  # Load trees
  tree1_obj <- pr_load_tree(tree1)
  tree2_obj <- pr_load_tree(tree2)

  tips1 <- tree1_obj$tip.label
  tips2 <- tree2_obj$tip.label

  # Source labels for metadata
  tree1_source <- if (is.character(tree1)) {
    basename(tree1)
  } else {
    sprintf("tree1 (%d tips)", length(tips1))
  }
  tree2_source <- if (is.character(tree2)) {
    basename(tree2)
  } else {
    sprintf("tree2 (%d tips)", length(tips2))
  }

  # Load overrides
  overrides_df <- pr_load_overrides(overrides)

  if (!quiet) {
    cli_alert_info(
      "Reconciling {length(tips1)} tips (tree1) vs {length(tips2)} tips (tree2)"
    )
  }

  # Run cascade
  mapping <- pr_run_cascade(
    names_x         = tips1,
    names_y         = tips2,
    authority       = authority,
    db_version      = db_version,
    rank            = rank,
    overrides       = overrides_df,
    fuzzy           = fuzzy,
    fuzzy_threshold = fuzzy_threshold,
    resolve         = resolve,
    quiet           = quiet
  )

  # Build metadata
  meta <- list(
    call             = match.call(),
    type             = "tree_tree",
    timestamp        = Sys.time(),
    authority        = authority %||% "none",
    db_version       = db_version %||% "latest",
    fuzzy            = fuzzy,
    fuzzy_threshold  = if (fuzzy) fuzzy_threshold else NA_real_,
    fuzzy_method     = if (fuzzy) "component_levenshtein" else NA_character_,
    resolve          = resolve,
    prepR4pcm_version = as.character(utils::packageVersion("prepR4pcm")),
    x_source         = tree1_source,
    y_source         = tree2_source,
    rank             = rank
  )

  result <- new_reconciliation(mapping = mapping, meta = meta)

  if (!quiet) {
    n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
    cli_alert_success(
      "Matched {n_matched}/{length(tips1)} tips between trees"
    )
  }

  result
}
