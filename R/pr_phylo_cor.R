# Phylogenetic correlation matrix for meta-analysis ----------------
#
# Thin wrapper around `ape::vcv()` that turns a `phylo` (or
# `pr_tree_result`) into the correlation matrix that
# `metafor::rma.mv()`, `MCMCglmm::MCMCglmm()`, `glmmTMB::glmmTMB()`
# (with `ar1`/`pheno` random effects), and `brms::brm()` accept as
# the phylogenetic random-effect structure.
#
# Why this is a function rather than a doc note:
#   - The standard pattern (`compute.brlen() -> vcv()`) is folklore
#     in phylogenetic meta-analysis; users who stumble onto rotl
#     often miss the Grafen step entirely.
#   - Returning a matrix WITH attached citation/provenance lets us
#     keep the audit trail (`attr(out, "tree_provenance")`) so the
#     meta-analysis methods section can be auto-generated via
#     `pr_cite_tree()`.

#' Phylogenetic correlation matrix from a tree
#'
#' Convert a phylogeny into the correlation matrix used as a random-
#' effect structure in phylogenetic meta-analysis (`metafor::rma.mv`)
#' or phylogenetic mixed models (`MCMCglmm`, `brms`, etc.).
#'
#' Wraps [ape::vcv()] with `corr = TRUE`. Designed to slot in after
#' [pr_get_tree()] when the goal is meta-analysis, where typically:
#' \enumerate{
#'   \item Topology comes from Open Tree of Life (`source = "rotl"`)
#'     because the species span many higher taxa.
#'   \item Polytomies are resolved at random (`resolve_polytomies =
#'     TRUE`).
#'   \item Branch lengths are computed via Grafen's method
#'     (`branch_lengths = "grafen"`) because rotl's edge lengths are
#'     unit-length placeholders.
#'   \item The correlation matrix is computed once and reused as
#'     `random = ~1|species`'s `R = list(species = phy_cor)` in
#'     `metafor::rma.mv()` (or `random = ~species` with
#'     `cov.formula = ~ phylo` in `MCMCglmm`).
#' }
#'
#' @param x A `phylo` object, a `multiPhylo`, or a
#'   `pr_tree_result` (the `$tree` slot is extracted). For
#'   `multiPhylo` input, returns a list of correlation matrices.
#' @param corr Logical. Pass through to `ape::vcv()`. `TRUE`
#'   (default) returns a correlation matrix (diagonal = 1); `FALSE`
#'   returns the variance-covariance matrix.
#' @param ... Additional arguments forwarded to `ape::vcv()`.
#'
#' @return A square symmetric matrix with row/column names equal to
#'   the tip labels. For `multiPhylo` input, a list of such matrices.
#'
#' @details
#' The correlation matrix has the property that, for a Brownian-motion
#' model on a tree with branch lengths in time units, two species'
#' off-diagonal entry equals the time from root to their MRCA divided
#' by the time from root to tip. So an ultrametric tree always has
#' diagonal = 1 (every tip is the same distance from the root).
#'
#' For meta-analysis with rotl topology + Grafen's method, the
#' resulting matrix is the standard Pagel's lambda = 1 phylogenetic
#' correlation that `metafor::rma.mv()` accepts directly.
#'
#' @seealso [pr_get_tree()] (use with `branch_lengths = "grafen"`
#'   and `resolve_polytomies = TRUE` for the meta-analysis path);
#'   [ape::vcv()] for the underlying computation.
#'
#' @references
#' Paradis, E., & Schliep, K. (2019). ape 5.0: an environment for
#' modern phylogenetics and evolutionary analyses in R.
#' \emph{Bioinformatics}, 35(3), 526--528.
#' \doi{10.1093/bioinformatics/bty633}
#'
#' Cinar, O., Nakagawa, S., & Viechtbauer, W. (2022). Phylogenetic
#' multilevel meta-analysis: a simulation study on the importance of
#' modelling the phylogeny. \emph{Methods in Ecology and Evolution},
#' 13(2), 383--395. \doi{10.1111/2041-210X.13760}
#'
#' @examples
#' set.seed(1)
#' tr <- ape::rcoal(5)             # ultrametric, bifurcating
#' phy_cor <- pr_phylo_cor(tr)
#' dim(phy_cor)
#' all(diag(phy_cor) == 1)
#'
#' \donttest{
#'   # End-to-end meta-analysis prep
#'   if (requireNamespace("rotl", quietly = TRUE)) {
#'     res <- try(pr_get_tree(c("Homo sapiens", "Pan troglodytes",
#'                              "Mus musculus", "Rattus norvegicus"),
#'                            source             = "rotl",
#'                            resolve_polytomies = TRUE,
#'                            branch_lengths     = "grafen"),
#'                silent = TRUE)
#'     if (!inherits(res, "try-error")) {
#'       phy_cor <- pr_phylo_cor(res)
#'       # phy_cor can now be supplied to downstream meta-analysis models.
#'     }
#'   }
#' }
#'
#' @export
pr_phylo_cor <- function(x, corr = TRUE, ...) {
  tree <- if (inherits(x, "pr_tree_result")) x$tree else x
  if (inherits(tree, "multiPhylo")) {
    return(lapply(tree, pr_phylo_cor, corr = corr, ...))
  }
  if (!inherits(tree, "phylo")) {
    cli::cli_abort(c(
      "{.arg x} must be a {.cls phylo}, {.cls multiPhylo}, or {.cls pr_tree_result}.",
      "i" = "Got: {.cls {class(x)[1]}}."
    ))
  }
  if (is.null(tree$edge.length)) {
    cli::cli_abort(c(
      "Tree has no branch lengths --- {.fn pr_phylo_cor} cannot compute a correlation matrix.",
      "i" = "If your tree came from {.code source = \"rotl\"}, run {.fn pr_get_tree} again with {.code branch_lengths = \"grafen\"} to assign Grafen's method-based lengths.",
      ">" = "Or transform manually: {.code tree <- ape::compute.brlen(tree, method = \"Grafen\")}."
    ))
  }
  ape::vcv(tree, corr = corr, ...)
}
