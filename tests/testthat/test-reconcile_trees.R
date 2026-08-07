test_that("reconcile_trees finds overlapping tips", {
  tree1 <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
  tree2 <- ape::read.tree(text = "((B:1,C:1):1,D:2);")

  result <- reconcile_trees(tree1, tree2, authority = NULL, quiet = TRUE)

  expect_s3_class(result, "reconciliation")
  expect_equal(result$meta$type, "tree_tree")

  matched <- result$mapping[result$mapping$in_x & result$mapping$in_y, ]
  expect_equal(nrow(matched), 2L)  # B and C
  expect_true(all(c("B", "C") %in% matched$name_x))
})

test_that("reconcile_trees handles no overlap", {
  tree1 <- ape::read.tree(text = "(A:1,B:1);")
  tree2 <- ape::read.tree(text = "(C:1,D:1);")

  result <- reconcile_trees(tree1, tree2, authority = NULL, quiet = TRUE)

  matched <- result$mapping[result$mapping$in_x & result$mapping$in_y, ]
  expect_equal(nrow(matched), 0L)
  expect_equal(result$counts$n_unresolved_x, 2L)
  expect_equal(result$counts$n_unresolved_y, 2L)
})

test_that("reconcile_trees matches normalised tips", {
  tree1 <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  tree2 <- ape::read.tree(text = "(Homo_sapiens:1,Gorilla_gorilla:1);")

  result <- reconcile_trees(tree1, tree2, authority = NULL, quiet = TRUE)

  matched <- result$mapping[result$mapping$in_x & result$mapping$in_y, ]
  expect_equal(nrow(matched), 1L)
  expect_equal(matched$name_x, "Homo_sapiens")
})


# --- M8. reconcile_trees tree-vs-tree grid ----------------------------------

test_that("M8: reconcile_trees handles various overlap × tree shape combos", {
  make_pair <- function(overlap) {
    if (overlap == "full") {
      t1 <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
      t2 <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
    } else if (overlap == "partial") {
      t1 <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
      t2 <- ape::read.tree(text = "((B:1,C:1):1,D:2);")
    } else {
      t1 <- ape::read.tree(text = "(A:1,B:1);")
      t2 <- ape::read.tree(text = "(C:1,D:1);")
    }
    list(t1 = t1, t2 = t2)
  }

  expected_overlap <- list(
    full = 3, partial = 2, none = 0
  )

  for (overlap in names(expected_overlap)) {
    pair <- make_pair(overlap)
    res <- suppressMessages(
      reconcile_trees(pair$t1, pair$t2, authority = NULL, quiet = TRUE)
    )
    expect_s3_class(res, "reconciliation")
    expect_equal(res$meta$type, "tree_tree",
                 info = sprintf("overlap=%s", overlap))

    matched <- res$mapping[res$mapping$in_x & res$mapping$in_y, ]
    expect_equal(nrow(matched), expected_overlap[[overlap]],
                 info = sprintf("overlap=%s", overlap))
  }
})


test_that("M8: reconcile_trees symmetric in matched species count", {
  t1 <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
  t2 <- ape::read.tree(text = "((B:1,C:1):1,D:2);")

  r_12 <- suppressMessages(
    reconcile_trees(t1, t2, authority = NULL, quiet = TRUE)
  )
  r_21 <- suppressMessages(
    reconcile_trees(t2, t1, authority = NULL, quiet = TRUE)
  )

  n_matched_12 <- sum(r_12$mapping$in_x & r_12$mapping$in_y, na.rm = TRUE)
  n_matched_21 <- sum(r_21$mapping$in_x & r_21$mapping$in_y, na.rm = TRUE)
  expect_equal(n_matched_12, n_matched_21)
})
