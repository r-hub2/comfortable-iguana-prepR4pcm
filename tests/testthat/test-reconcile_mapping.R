test_that("reconcile_mapping returns a tibble with expected columns", {
  df_x <- data.frame(species = c("A b", "C d"), val = 1:2,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "E f"), score = 1:2,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  mapping <- reconcile_mapping(rec)
  expect_s3_class(mapping, "tbl_df")
  expected_cols <- c("name_x", "name_y", "name_resolved",
                     "match_type", "match_score", "match_source",
                     "in_x", "in_y", "notes")
  expect_true(all(expected_cols %in% names(mapping)))
})

test_that("reconcile_mapping errors on non-reconciliation input", {
  expect_error(reconcile_mapping(list()), "reconciliation")
  expect_error(reconcile_mapping("not_a_rec"), "reconciliation")
})

test_that("reconcile_mapping returns correct match types", {
  df_x <- data.frame(species = c("A b", "C d", "E f"),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d", "G h"),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  mapping <- reconcile_mapping(rec)
  # Should have exact matches for shared species and unresolved for unique ones
  expect_true("exact" %in% mapping$match_type)
  expect_true("unresolved" %in% mapping$match_type)
})
