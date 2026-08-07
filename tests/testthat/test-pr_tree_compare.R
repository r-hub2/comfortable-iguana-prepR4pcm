# pr_tree_compare() tests
#
# Verify:
#   - Reject 0/1 trees
#   - Identical trees -> Jaccard = 1, RF = 0
#   - Disjoint tip sets -> Jaccard 0, RF NA (< 4 shared tips)
#   - Partial overlap -> 0 < Jaccard < 1
#   - Accepts phylo / multiPhylo / pr_tree_result
#   - print method runs

mini_phylo <- function(tip_labels) {
  ape::read.tree(text = paste0("(",
                                paste(tip_labels, collapse = ","),
                                ");"))
}


test_that("pr_tree_compare() requires >= 2 trees", {
  t1 <- ape::rtree(5)
  expect_error(pr_tree_compare(t1), "at least two")
  expect_error(pr_tree_compare(), "at least two")
})


test_that("pr_tree_compare() on identical trees has Jaccard 1, RF 0", {
  set.seed(1)
  t1 <- ape::rtree(8)
  cmp <- pr_tree_compare(t1, t1)
  expect_equal(cmp$n_trees, 2L)
  expect_equal(cmp$n_shared, 8L)
  expect_equal(cmp$pairwise_jaccard[1, 2], 1)
  expect_equal(cmp$pairwise_rf[1, 2], 0)
})


test_that("pr_tree_compare() on disjoint tips has Jaccard 0, RF NA", {
  t1 <- ape::rtree(5, tip.label = c("a", "b", "c", "d", "e"))
  t2 <- ape::rtree(5, tip.label = c("f", "g", "h", "i", "j"))
  cmp <- pr_tree_compare(t1, t2)
  expect_equal(cmp$n_shared, 0L)
  expect_equal(cmp$pairwise_jaccard[1, 2], 0)
  expect_true(is.na(cmp$pairwise_rf[1, 2]))
})


test_that("pr_tree_compare() partial overlap has 0 < Jaccard < 1", {
  t1 <- ape::rtree(6, tip.label = c("a", "b", "c", "d", "e", "f"))
  t2 <- ape::rtree(6, tip.label = c("d", "e", "f", "g", "h", "i"))
  cmp <- pr_tree_compare(t1, t2)
  expect_equal(cmp$n_shared, 3L)
  expect_lt(cmp$pairwise_jaccard[1, 2], 1)
  expect_gt(cmp$pairwise_jaccard[1, 2], 0)
})


test_that("pr_tree_compare() accepts pr_tree_result inputs", {
  t1 <- ape::rtree(5)
  res1 <- list(tree = t1, source = "rotl", matched = t1$tip.label,
                unmatched = character(), backend_meta = list())
  class(res1) <- "pr_tree_result"
  res2 <- res1
  expect_no_error(pr_tree_compare(res1, res2))
})


test_that("pr_tree_compare() accepts multiPhylo inputs (uses first tree)", {
  set.seed(2)
  t1 <- ape::rtree(5)
  mp <- structure(list(t1, t1, t1), class = "multiPhylo")
  expect_no_error(pr_tree_compare(mp, t1))
})


test_that("print.pr_tree_compare() runs without error", {
  t1 <- ape::rtree(6)
  t2 <- ape::rtree(6, tip.label = t1$tip.label)
  cmp <- pr_tree_compare(t1, t2)
  expect_no_error(capture.output(print(cmp)))
})


test_that("pr_tree_compare() rejects non-tree input", {
  t1 <- ape::rtree(3)
  expect_error(pr_tree_compare(t1, "foo"),
               "phylo|multiPhylo|pr_tree_result")
})
