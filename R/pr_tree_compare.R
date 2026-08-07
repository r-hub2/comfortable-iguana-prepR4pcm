# Compare two or more phylogenetic trees ------------------------------
#
# When users fetch from multiple backends (e.g. rotl + fishtree) they
# often want to know: do these two trees agree? Where do they
# disagree? `pr_tree_compare()` answers that with a small bundle of
# standard metrics: tip-set overlap, Robinson-Foulds distance on the
# common subtree, branch-length agreement when both trees are dated.

#' Compare two or more phylogenetic trees
#'
#' Computes a small set of standard metrics for comparing trees that
#' come from different backends (or different runs of the same
#' backend). Designed for the common case of "I retrieved a tree from
#' rotl and another from fishtree --- do they agree?"
#'
#' @param ... Two or more `phylo` objects, or two or more
#'   `pr_tree_result` objects (the `tree` slot is extracted), or
#'   `multiPhylo` objects (the first tree is used). Trees can be
#'   passed as positional arguments or as a named list.
#' @param prune_to_common Logical. Restrict each tree to the shared
#'   tip set before computing topology metrics? Default `TRUE` ---
#'   without this, RF distance is undefined when tip sets differ.
#'
#' @return A list with class `pr_tree_compare` and components:
#' \describe{
#'   \item{`n_trees`}{Number of input trees.}
#'   \item{`tip_sets`}{Named list of character vectors, one per tree.}
#'   \item{`shared_tips`}{Tips present in every input tree.}
#'   \item{`unique_to`}{Named list, one per tree, of tips present in
#'     that tree but not in every other tree.}
#'   \item{`n_shared`}{Length-1 integer.}
#'   \item{`pairwise_jaccard`}{Square matrix; `(i, j)` is the Jaccard
#'     index of `tip_sets[[i]] vs tip_sets[[j]]`.}
#'   \item{`pairwise_rf`}{Square matrix of Robinson-Foulds distances
#'     between pairs of trees pruned to `shared_tips`. `NA` when the
#'     pair has < 4 shared tips.}
#'   \item{`pairwise_branch_cor`}{Square matrix of Pearson
#'     correlations between matching edge lengths in each pair, or
#'     `NA` when one or both trees have no branch lengths.}
#' }
#'
#' @details
#' RF distance is computed via [ape::dist.topo()] with the default
#' method. Branch-length correlation matches edges by their tip-set
#' bipartition: for each edge in tree A, the corresponding edge in
#' tree B (if any) is the one that splits the same set of tips. The
#' Pearson correlation is taken over the matched edge-length pairs;
#' edges whose bipartition is absent in the other tree are dropped.
#' This is a proper bipartition-matched correlation as introduced in
#' Kuhner & Felsenstein (1994) for tree comparison.
#'
#' @seealso [pr_get_tree()] for retrieval; [reconcile_apply()] for
#'   combining a chosen tree with a dataset.
#'
#' @references
#' Kuhner, M. K., & Felsenstein, J. (1994). A simulation comparison
#' of phylogeny algorithms under equal and unequal evolutionary rates.
#' *Molecular Biology and Evolution* 11(3): 459--468.
#' \doi{10.1093/oxfordjournals.molbev.a040126}
#'
#' Robinson, D. F., & Foulds, L. R. (1981). Comparison of phylogenetic
#' trees. *Mathematical Biosciences* 53(1--2): 131--147.
#' \doi{10.1016/0025-5564(81)90043-2}
#'
#' @examples
#' # Two trees with identical tip sets
#' set.seed(1)
#' t1 <- ape::rtree(10)
#' t2 <- ape::rtree(10, tip.label = t1$tip.label)
#' cmp <- pr_tree_compare(t1, t2)
#' cmp$n_shared
#' cmp$pairwise_rf
#'
#' # Two trees with overlapping but not identical tips
#' t3 <- ape::rtree(8, tip.label = t1$tip.label[1:8])
#' cmp <- pr_tree_compare(t1, t3)
#' cmp$pairwise_jaccard
#'
#' @export
pr_tree_compare <- function(..., prune_to_common = TRUE) {
  trees <- .pr_compare_collect_trees(...)
  if (length(trees) < 2L) {
    cli::cli_abort(c(
      "{.fn pr_tree_compare} requires at least two trees.",
      "i" = "Got: {length(trees)}."
    ))
  }
  n <- length(trees)
  nm <- names(trees)
  if (is.null(nm)) nm <- paste0("tree", seq_len(n))

  tip_sets <- lapply(trees, function(t) t$tip.label)
  names(tip_sets) <- nm

  shared <- Reduce(intersect, tip_sets)
  unique_to <- lapply(seq_along(tip_sets), function(i) {
    others <- Reduce(union, tip_sets[-i])
    setdiff(tip_sets[[i]], others)
  })
  names(unique_to) <- nm

  # Pairwise Jaccard
  jaccard <- matrix(NA_real_, nrow = n, ncol = n,
                     dimnames = list(nm, nm))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    a <- tip_sets[[i]]; b <- tip_sets[[j]]
    jaccard[i, j] <- length(intersect(a, b)) / max(1L, length(union(a, b)))
  }

  # Pairwise RF distance (on the common subtree per pair)
  rf <- matrix(NA_real_, nrow = n, ncol = n,
                dimnames = list(nm, nm))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j) {
      rf[i, j] <- 0
      next
    }
    common <- intersect(tip_sets[[i]], tip_sets[[j]])
    if (length(common) < 4L) next
    a <- if (prune_to_common) {
      ape::keep.tip(trees[[i]], common)
    } else trees[[i]]
    b <- if (prune_to_common) {
      ape::keep.tip(trees[[j]], common)
    } else trees[[j]]
    rf[i, j] <- tryCatch(
      suppressWarnings(ape::dist.topo(c(a, b))[1]),
      error = function(e) NA_real_
    )
  }

  # Pairwise branch-length correlation (on common subtree, when both
  # trees have edge lengths). Uses bipartition matching: for each
  # internal edge in tree A, find the edge in tree B that splits the
  # same set of tips and correlate the lengths. Edges with no
  # matching bipartition in the other tree are dropped.
  blcor <- matrix(NA_real_, nrow = n, ncol = n,
                   dimnames = list(nm, nm))
  for (i in seq_len(n)) for (j in seq_len(n)) {
    if (i == j) {
      blcor[i, j] <- 1
      next
    }
    a <- trees[[i]]; b <- trees[[j]]
    if (is.null(a$edge.length) || is.null(b$edge.length)) next
    common <- intersect(tip_sets[[i]], tip_sets[[j]])
    if (length(common) < 4L) next
    a <- ape::keep.tip(a, common)
    b <- ape::keep.tip(b, common)
    blcor[i, j] <- .pr_bipartition_branch_cor(a, b)
  }

  out <- list(
    n_trees             = n,
    tip_sets            = tip_sets,
    shared_tips         = shared,
    unique_to           = unique_to,
    n_shared            = length(shared),
    pairwise_jaccard    = jaccard,
    pairwise_rf         = rf,
    pairwise_branch_cor = blcor
  )
  class(out) <- "pr_tree_compare"
  out
}


#' @export
print.pr_tree_compare <- function(x, ...) {
  cli::cli_h1("Tree comparison ({x$n_trees} trees)")
  cli::cli_bullets(c(
    "*" = "Shared tips: {.val {x$n_shared}}",
    "*" = "Tip-set Jaccard (pairwise):"
  ))
  print(round(x$pairwise_jaccard, 3))
  cli::cli_alert_info("Robinson-Foulds distance (pairwise, on shared subtree):")
  print(round(x$pairwise_rf, 3))
  if (any(!is.na(x$pairwise_branch_cor) & x$pairwise_branch_cor != 1)) {
    cli::cli_alert_info("Branch-length correlation (pairwise):")
    print(round(x$pairwise_branch_cor, 3))
  }
  invisible(x)
}


# Internal: bipartition-matched branch-length correlation -----------
#
# For two trees on the same tip set, match every edge in tree A to
# the edge in tree B that induces the same bipartition (same set of
# tips on the descendant side). When no matching bipartition exists,
# drop the edge from the correlation. Also handle terminal (tip)
# edges by matching on the tip label.

.pr_bipartition_branch_cor <- function(a, b) {
  # Build bipartition descriptors:
  #   - For terminal edges: the tip label.
  #   - For internal edges: a sorted, comma-joined list of descendant tips.
  desc_a <- .pr_edge_descriptors(a)
  desc_b <- .pr_edge_descriptors(b)
  if (is.null(desc_a) || is.null(desc_b)) return(NA_real_)
  shared <- intersect(names(desc_a), names(desc_b))
  if (length(shared) < 2L) return(NA_real_)
  bl_a <- desc_a[shared]
  bl_b <- desc_b[shared]
  tryCatch(
    suppressWarnings(stats::cor(bl_a, bl_b)),
    error = function(e) NA_real_
  )
}


# Build a named numeric vector: names are bipartition keys (sorted
# tip lists for internal edges, tip labels for terminal edges); values
# are the matching edge lengths.

.pr_edge_descriptors <- function(tree) {
  if (is.null(tree$edge.length)) return(NULL)
  n_tips <- ape::Ntip(tree)
  edges  <- tree$edge
  bls    <- tree$edge.length

  # Pre-compute the descendant-tip set for every node via post-order
  # traversal. tip nodes (1..n_tips) descend only from themselves.
  parents  <- edges[, 1]
  children <- edges[, 2]
  desc <- vector("list", max(c(parents, children)))
  for (i in seq_len(n_tips)) desc[[i]] <- tree$tip.label[i]

  # Children appear before their parent in a post-order sweep; ape's
  # default edge ordering isn't always strict post-order, so iterate
  # until no node's descendant set changes (small N: cheap).
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (k in seq_len(nrow(edges))) {
      p <- parents[k]; c <- children[k]
      if (is.null(desc[[c]])) next
      old <- desc[[p]]
      old_or_empty <- if (is.null(old)) character() else old
      merged <- union(old_or_empty, desc[[c]])
      if (!identical(sort(merged), sort(old_or_empty))) {
        desc[[p]] <- merged
        changed <- TRUE
      }
    }
  }

  # Build keys for every edge
  keys <- vapply(seq_len(nrow(edges)), function(k) {
    c <- children[k]
    if (c <= n_tips) {
      tree$tip.label[c]
    } else {
      paste(sort(desc[[c]]), collapse = ",")
    }
  }, character(1))

  out <- stats::setNames(bls, keys)
  out
}


# Internal: turn ... into a named list of phylo --------------------------

.pr_compare_collect_trees <- function(...) {
  args <- list(...)
  out <- list()
  for (i in seq_along(args)) {
    a <- args[[i]]
    nm <- names(args)[i]
    if (is.null(nm) || !nzchar(nm)) nm <- paste0("tree", i)
    if (inherits(a, "phylo")) {
      out[[nm]] <- a
    } else if (inherits(a, "multiPhylo")) {
      out[[nm]] <- a[[1]]
    } else if (inherits(a, "pr_tree_result")) {
      tree <- a$tree
      if (inherits(tree, "multiPhylo")) tree <- tree[[1]]
      out[[nm]] <- tree
    } else if (is.list(a) && !inherits(a, "phylo")) {
      # User passed a list of trees as a single argument; recurse.
      sub <- do.call(.pr_compare_collect_trees, a)
      out <- c(out, sub)
    } else {
      cli::cli_abort(c(
        "Each argument must be a {.cls phylo}, {.cls multiPhylo}, or {.cls pr_tree_result}.",
        "i" = "Got: {.cls {class(a)[1]}} (argument {i})."
      ))
    }
  }
  out
}
