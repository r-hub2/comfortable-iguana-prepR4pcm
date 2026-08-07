test_that("resolve='flag' flags low-confidence fuzzy matches", {
  result <- pr_run_cascade(
    names_x = c("Parus mejor"),
    names_y = c("Parus major"),
    authority = NULL,
    fuzzy = TRUE,
    fuzzy_threshold = 0.8,
    resolve = "flag"
  )
  # Score should be above 0.8 but below 0.95 → flagged
  flagged <- result[result$match_type == "flagged", ]
  expect_true(nrow(flagged) >= 1)
  expect_equal(flagged$name_x[1], "Parus mejor")
  expect_true(flagged$match_score[1] < 0.95)
})

test_that("resolve='first' accepts all fuzzy matches", {
  result <- pr_run_cascade(
    names_x = c("Parus mejor"),
    names_y = c("Parus major"),
    authority = NULL,
    fuzzy = TRUE,
    fuzzy_threshold = 0.8,
    resolve = "first"
  )
  fuzzy_rows <- result[result$match_type == "fuzzy", ]
  expect_true(nrow(fuzzy_rows) >= 1)
  expect_equal(fuzzy_rows$name_x[1], "Parus mejor")
  # Should NOT be flagged
  flagged <- result[result$match_type == "flagged", ]
  expect_equal(nrow(flagged), 0)
})

test_that("resolve='flag' does not flag high-confidence fuzzy matches", {
  # Near-identical names should score >= 0.95 and not be flagged
  result <- pr_run_cascade(
    names_x = c("Turdus merula"),
    names_y = c("Turdus merulb"),
    authority = NULL,
    fuzzy = TRUE,
    fuzzy_threshold = 0.8,
    resolve = "flag"
  )
  matched <- result[result$in_x & result$in_y, ]
  if (nrow(matched) > 0 && matched$match_score[1] >= 0.95) {
    expect_equal(matched$match_type[1], "fuzzy")
  }
})

test_that("resolve parameter is passed through reconcile_data", {
  df1 <- data.frame(species = c("Parus mejor"), stringsAsFactors = FALSE)
  df2 <- data.frame(species = c("Parus major"), stringsAsFactors = FALSE)

  rec <- reconcile_data(
    df1, df2,
    x_species = "species",
    y_species = "species",
    authority = NULL,
    fuzzy = TRUE,
    fuzzy_threshold = 0.8,
    resolve = "first",
    quiet = TRUE
  )
  expect_equal(rec$meta$resolve, "first")
})

test_that("resolve parameter is passed through reconcile_tree", {
  tree <- ape::rtree(3, tip.label = c("Parus_major", "Homo_sapiens", "Pan_troglodytes"))
  df <- data.frame(species = c("Parus mejor"), stringsAsFactors = FALSE)

  rec <- reconcile_tree(
    df, tree,
    x_species = "species",
    authority = NULL,
    fuzzy = TRUE,
    fuzzy_threshold = 0.8,
    resolve = "first",
    quiet = TRUE
  )
  expect_equal(rec$meta$resolve, "first")
})
