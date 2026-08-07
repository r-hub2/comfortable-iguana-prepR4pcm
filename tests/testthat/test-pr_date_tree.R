# pr_date_tree() — wraps datelife::datelife_use(). The actual datelife
# call is mocked so the suite doesn't depend on the GitHub-only datelife
# package being installed nor on its remote chronogram database being
# reachable on CI. The tests verify:
#
#   1. Input validation (phylo / multiPhylo only; n_dated positive int).
#   2. Single-tree default returns a phylo with the pr_tree_result class.
#   3. n_dated > 1 returns a multiPhylo (capped at n_dated).
#   4. backend_meta$tree_provenance has one entry per returned tree.
#   5. mapping records per-tip audit rows.
#   6. source is always "datelife".
#   7. Helpful error when datelife isn't installed.

mini_phylo <- function(tip_labels) {
  ape::read.tree(
    text = paste0("(", paste(tip_labels, collapse = ","), ");")
  )
}


test_that("pr_date_tree validates input type", {
  expect_error(pr_date_tree("not a tree"), "phylo")
  expect_error(pr_date_tree(list(a = 1)), "phylo")
})


test_that("pr_date_tree validates n_dated", {
  tr <- mini_phylo(c("a", "b", "c"))
  expect_error(pr_date_tree(tr, n_dated = 0), "positive integer")
  expect_error(pr_date_tree(tr, n_dated = c(1, 2)), "length-1")
  expect_error(pr_date_tree(tr, n_dated = -3), "positive integer")
  expect_error(pr_date_tree(tr, n_dated = NA_real_), "positive integer")
  expect_error(pr_date_tree(tr, n_dated = NaN), "positive integer")
  expect_error(pr_date_tree(tr, n_dated = 1.5), "positive integer")
})


test_that("pr_date_tree returns pr_tree_result with single phylo by default", {
  tr <- mini_phylo(c("Rhea_americana", "Struthio_camelus"))
  testthat::local_mocked_bindings(
    .pr_date_tree_datelife = function(
      tree,
      n_dated = 1L,
      dating_method = "bladj",
      ...
    ) {
      list(
        tree = tree,
        matched = tree$tip.label,
        unmatched = character(),
        backend_meta = list(
          backend = "datelife",
          dating_method = dating_method,
          n_returned = 1L
        )
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_date_tree(tr)
  expect_s3_class(res, "pr_tree_result")
  expect_s3_class(res$tree, "phylo")
  expect_equal(res$source, "datelife")
  expect_equal(res$backend_meta$dating_method, "bladj")
})


test_that("pr_date_tree with n_dated > 1 returns multiPhylo", {
  tr <- mini_phylo(c("Rhea_americana", "Struthio_camelus"))
  testthat::local_mocked_bindings(
    .pr_date_tree_datelife = function(
      tree,
      n_dated = 1L,
      dating_method = "bladj",
      ...
    ) {
      mp <- structure(list(tree, tree, tree), class = "multiPhylo")
      list(
        tree = mp,
        matched = tree$tip.label,
        unmatched = character(),
        backend_meta = list(
          backend = "datelife",
          dating_method = dating_method,
          n_returned = 3L
        )
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_date_tree(tr, n_dated = 5)
  expect_s3_class(res$tree, "multiPhylo")
  expect_length(res$tree, 3L) # mock returned 3
})


test_that("pr_date_tree populates per-tree provenance", {
  tr <- mini_phylo(c("Rhea_americana", "Struthio_camelus"))
  testthat::local_mocked_bindings(
    .pr_date_tree_datelife = function(
      tree,
      n_dated = 1L,
      dating_method = "bladj",
      ...
    ) {
      mp <- structure(list(tree, tree), class = "multiPhylo")
      list(
        tree = mp,
        matched = tree$tip.label,
        unmatched = character(),
        backend_meta = list(
          backend = "datelife",
          dating_method = dating_method,
          n_returned = 2L
        )
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_date_tree(tr, n_dated = 2)
  expect_length(res$backend_meta$tree_provenance, 2L)
  expect_equal(res$backend_meta$tree_provenance[[1]]$source_index, 1L)
  expect_equal(res$backend_meta$tree_provenance[[2]]$source_index, 2L)
})


test_that("pr_date_tree returns a per-tip mapping table", {
  tr <- mini_phylo(c("a", "b"))
  testthat::local_mocked_bindings(
    .pr_date_tree_datelife = function(
      tree,
      n_dated = 1L,
      dating_method = "bladj",
      ...
    ) {
      list(
        tree = tree,
        matched = tree$tip.label,
        unmatched = character(),
        backend_meta = list(
          backend = "datelife",
          dating_method = dating_method,
          n_returned = 1L
        )
      )
    },
    .package = "prepR4pcm"
  )

  res <- pr_date_tree(tr)

  expect_named(
    res$mapping,
    c(
      "input_name",
      "normalized_name",
      "query_name",
      "tree_name",
      "in_tree",
      "match_type",
      "placement_status",
      "tnrs_number_matches",
      "tnrs_is_synonym",
      "tnrs_approximate_match",
      "tnrs_flags"
    )
  )
  expect_equal(res$mapping$input_name, c("a", "b"))
  expect_equal(res$mapping$tree_name, c("a", "b"))
  expect_equal(res$mapping$in_tree, c(TRUE, TRUE))
})


test_that("pr_date_tree without datelife errors helpfully", {
  tr <- mini_phylo(c("Rhea_americana", "Struthio_camelus"))
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "datelife")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(pr_date_tree(tr), error = function(e) e)
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("datelife", msg, fixed = TRUE))
  expect_true(grepl("phylotastic", msg, fixed = TRUE))
})
