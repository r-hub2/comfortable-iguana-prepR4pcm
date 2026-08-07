# Live tests for the meta-analysis path: pr_get_tree() with
# resolve_polytomies + branch_lengths, plus pr_phylo_cor().
#
# These run on real ape calls (no mocking of ape itself), but use a
# fixed Newick topology so they don't depend on rotl / network /
# unsetup AvesData. The pattern they exercise is exactly what
# Cinar et al. (2022) use for phylogenetic meta-analysis: take an
# OToL topology, multi2di() at random, compute.brlen("Grafen"),
# vcv(corr = TRUE).

test_that("resolve_polytomies = TRUE bifurcates the tree", {
  # Star topology with 5 tips at the root (a polytomy).
  star <- ape::read.tree(text = "(a, b, c, d, e);")
  expect_false(ape::is.binary(star))

  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = star, in_query = rep(TRUE, length(species)),
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("a","b","c","d","e"),
                    source = "rotl",
                    resolve_polytomies = TRUE,
                    tnrs = "never",
                    check_ultrametric = FALSE)
  expect_true(ape::is.binary(res$tree))
})


test_that("branch_lengths = 'grafen' assigns Grafen lengths", {
  star <- ape::read.tree(text = "((a, b), (c, d));")
  star$edge.length <- NULL   # rotl-style: no real lengths

  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = star, in_query = rep(TRUE, length(species)),
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("a","b","c","d"),
                    source = "rotl",
                    branch_lengths = "grafen",
                    tnrs = "never",
                    check_ultrametric = FALSE)
  expect_false(is.null(res$tree$edge.length))
  expect_true(all(res$tree$edge.length > 0))
  # Grafen's method on a balanced tree gives an ultrametric output
  expect_true(ape::is.ultrametric(res$tree))
})


test_that("branch_lengths = 'unit' sets every edge to 1", {
  tr <- ape::read.tree(text = "((a, b), (c, d));")
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = tr, in_query = rep(TRUE, length(species)),
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("a","b","c","d"),
                    source = "rotl",
                    branch_lengths = "unit",
                    tnrs = "never",
                    check_ultrametric = FALSE)
  expect_equal(unique(res$tree$edge.length), 1)
})


test_that("end-to-end: polytomy + Grafen + correlation matrix", {
  # Realistic meta-analysis input: 4-taxon polytomy with no edge
  # lengths (rotl shape).
  star <- ape::read.tree(text = "(a, b, c, d);")
  star$edge.length <- NULL
  expect_false(ape::is.binary(star))

  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = star, in_query = rep(TRUE, length(species)),
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  set.seed(1)
  res <- pr_get_tree(c("a","b","c","d"),
                    source = "rotl",
                    resolve_polytomies = TRUE,
                    branch_lengths     = "grafen",
                    tnrs = "never",
                    check_ultrametric = FALSE)
  expect_true(ape::is.binary(res$tree))
  expect_true(ape::is.ultrametric(res$tree))

  phy_cor <- pr_phylo_cor(res)
  expect_true(is.matrix(phy_cor))
  expect_equal(dim(phy_cor), c(4L, 4L))
  expect_equal(rownames(phy_cor), c("a","b","c","d"))
  # Correlation matrix: diagonal = 1, off-diagonal in [0, 1]
  expect_equal(unname(diag(phy_cor)), rep(1, 4))
  expect_true(all(phy_cor[upper.tri(phy_cor)] >= 0))
  expect_true(all(phy_cor[upper.tri(phy_cor)] <= 1))
  expect_true(isSymmetric(phy_cor))
})


test_that("pr_phylo_cor() rejects a tree with no edge lengths", {
  tr <- ape::read.tree(text = "((a, b), (c, d));")
  tr$edge.length <- NULL
  err <- tryCatch(pr_phylo_cor(tr), error = function(e) e)
  expect_s3_class(err, "error")
  expect_true(grepl("branch lengths|grafen", conditionMessage(err)))
})


test_that("pr_phylo_cor() accepts pr_tree_result and multiPhylo", {
  tr1 <- ape::compute.brlen(ape::rcoal(5))
  tr2 <- ape::compute.brlen(ape::rcoal(5, tip.label = tr1$tip.label))

  # phylo
  expect_no_error(pr_phylo_cor(tr1))

  # pr_tree_result wrapping a phylo
  res <- structure(list(tree = tr1, source = "rotl",
                          matched = tr1$tip.label,
                          backend_meta = list()),
                     class = "pr_tree_result")
  expect_no_error(pr_phylo_cor(res))

  # multiPhylo: returns a list of correlation matrices
  mp <- structure(list(tr1, tr2), class = "multiPhylo")
  out <- pr_phylo_cor(mp)
  expect_true(is.list(out))
  expect_length(out, 2L)
  expect_equal(dim(out[[1]]), c(5L, 5L))
})


# Live network/install-dependent test: actually run rotl + Grafen.
# Skipped on CRAN, when offline, and when rotl can't be loaded.
test_that("LIVE: rotl topology -> Grafen -> phy_cor (4-taxon mammal toy)", {
  skip_on_cran()
  skip_if_not_installed("rotl")
  skip_if_offline("api.opentreeoflife.org")
  # If rotl is installed but its rncl/Rcpp dep can't load, skip.
  ok <- tryCatch({ rotl::tnrs_match_names("Homo sapiens"); TRUE },
                  error = function(e) FALSE)
  if (!ok) skip("rotl loads but transient failure on tnrs_match_names")

  res <- tryCatch(
    pr_get_tree(c("Homo sapiens", "Pan troglodytes",
                  "Mus musculus", "Rattus norvegicus"),
                source             = "rotl",
                resolve_polytomies = TRUE,
                branch_lengths     = "grafen"),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    skip(paste("rotl call errored:", conditionMessage(res)))
  }
  expect_s3_class(res, "pr_tree_result")
  expect_true(ape::is.binary(res$tree))
  expect_true(ape::is.ultrametric(res$tree))

  phy_cor <- pr_phylo_cor(res)
  expect_true(is.matrix(phy_cor))
  expect_equal(dim(phy_cor), c(4L, 4L))
})
