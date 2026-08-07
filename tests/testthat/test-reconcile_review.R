test_that("reconcile_review returns unchanged when no matches to review", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  r <- reconcile_tree(df, tree, x_species = "species",
                       authority = NULL, quiet = TRUE)

  # No flagged matches → warns about non-interactive, returns unchanged
  result <- suppressWarnings(reconcile_review(r, type = "flagged"))
  expect_identical(result$mapping, r$mapping)
})

test_that("reconcile_review rejects non-reconciliation input", {
  expect_error(
    reconcile_review(list(a = 1)),
    "reconciliation"
  )
})

test_that("reconcile_review accepts valid type arguments", {
  df <- data.frame(species = c("Homo sapiens"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  r <- reconcile_tree(df, tree, x_species = "species",
                       authority = NULL, quiet = TRUE)

  # All valid types should complete without error (warnings expected for non-interactive)
  expect_no_error(suppressWarnings(reconcile_review(r, type = "flagged")))
  expect_no_error(suppressWarnings(reconcile_review(r, type = "fuzzy")))
  expect_no_error(suppressWarnings(reconcile_review(r, type = "all_unresolved")))
})
