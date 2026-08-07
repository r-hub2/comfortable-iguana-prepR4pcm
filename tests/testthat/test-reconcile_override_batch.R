test_that("reconcile_override_batch applies overrides correctly", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp", "Mystery sp"))
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  batch <- data.frame(
    name_x = c("Unknown sp", "Mystery sp"),
    name_y = c("Pan_troglodytes", "Gorilla_gorilla"),
    action = c("accept", "accept"),
    note   = c("Batch test 1", "Batch test 2"),
    stringsAsFactors = FALSE
  )

  result2 <- reconcile_override_batch(result, batch, quiet = TRUE)

  mapping <- reconcile_mapping(result2)
  manual_rows <- mapping[mapping$match_type == "manual", ]
  expect_equal(nrow(manual_rows), 2L)
  expect_true("Unknown sp" %in% manual_rows$name_x)
  expect_true("Mystery sp" %in% manual_rows$name_x)
})

test_that("reconcile_override_batch accepts CSV file path", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  batch <- data.frame(
    name_x = "Unknown sp",
    name_y = "Pan_troglodytes",
    action = "accept",
    note   = "From CSV",
    stringsAsFactors = FALSE
  )
  utils::write.csv(batch, tmp, row.names = FALSE)

  result2 <- reconcile_override_batch(result, tmp, quiet = TRUE)
  mapping <- reconcile_mapping(result2)
  manual_rows <- mapping[mapping$match_type == "manual", ]
  expect_equal(nrow(manual_rows), 1L)
  expect_equal(manual_rows$name_x, "Unknown sp")
})

test_that("reconcile_override_batch defaults action to accept when column missing", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # No action column
  batch <- data.frame(
    name_x = "Unknown sp",
    name_y = "Pan_troglodytes",
    stringsAsFactors = FALSE
  )

  result2 <- reconcile_override_batch(result, batch, quiet = TRUE)
  mapping <- reconcile_mapping(result2)
  manual_rows <- mapping[mapping$match_type == "manual", ]
  expect_equal(nrow(manual_rows), 1L)
})

test_that("reconcile_override_batch rejects invalid input", {
  df <- data.frame(species = "Homo sapiens")
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # Not a data frame or path
  expect_error(
    reconcile_override_batch(result, 42),
    "data frame"
  )

  # Missing name_x column
  expect_error(
    reconcile_override_batch(result, data.frame(name_y = "Foo bar")),
    "name_x"
  )

  # Non-existent file
  expect_error(
    reconcile_override_batch(result, "/nonexistent/file.csv"),
    "not found"
  )

  # Invalid action values
  expect_error(
    reconcile_override_batch(
      result,
      data.frame(name_x = "Homo sapiens", action = "invalid",
                 stringsAsFactors = FALSE)
    ),
    "Invalid action"
  )
})

test_that("reconcile_override_batch records overrides in log", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  batch <- data.frame(
    name_x = "Unknown sp",
    name_y = "Pan_troglodytes",
    action = "accept",
    note   = "Test note",
    stringsAsFactors = FALSE
  )

  result2 <- reconcile_override_batch(result, batch, quiet = TRUE)
  expect_equal(nrow(result2$overrides), 1L)
  expect_equal(result2$overrides$action, "accept")
  expect_equal(result2$overrides$user_note, "Test note")
})

test_that("reconcile_override_batch handles reject action", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  batch <- data.frame(
    name_x = "Pan troglodytes",
    action = "reject",
    note   = "Reject test",
    stringsAsFactors = FALSE
  )

  result2 <- reconcile_override_batch(result, batch, quiet = TRUE)
  mapping <- reconcile_mapping(result2)
  pan_row <- mapping[mapping$name_x == "Pan troglodytes" &
                       !is.na(mapping$name_x), ]
  expect_equal(pan_row$match_type, "unresolved")
})


# --- M6. reconcile_override_batch input shape grid --------------------------

test_that("M6: batch overrides applied via data frame and via CSV produce identical results", {
  df <- data.frame(
    species = c("Homo sapiens", "Unknown a", "Unknown b"),
    stringsAsFactors = FALSE
  )
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  batch <- data.frame(
    name_x = c("Unknown a", "Unknown b"),
    name_y = c("Pan_troglodytes", "Gorilla_gorilla"),
    action = c("accept", "accept"),
    note   = c("a", "b"),
    stringsAsFactors = FALSE
  )

  r_df  <- reconcile_override_batch(rec, batch, quiet = TRUE)

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(batch, tmp, row.names = FALSE)
  r_csv <- reconcile_override_batch(rec, tmp, quiet = TRUE)

  m_df  <- reconcile_mapping(r_df)
  m_csv <- reconcile_mapping(r_csv)

  # Manual rows should be identical in both paths
  manual_df  <- m_df[m_df$match_type == "manual", ]
  manual_csv <- m_csv[m_csv$match_type == "manual", ]
  expect_equal(nrow(manual_df), nrow(manual_csv))
  expect_equal(sort(manual_df$name_x), sort(manual_csv$name_x))
})


test_that("M6: batch grid of mixed actions applied correctly in one call", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes", "Unknown a", "Unknown b"),
    stringsAsFactors = FALSE
  )
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  batch <- data.frame(
    name_x = c("Unknown a", "Pan troglodytes", "Unknown b"),
    name_y = c("Gorilla_gorilla", NA_character_, "Pan_troglodytes"),
    action = c("accept", "reject", "replace"),
    note   = c("acc", "rej", "rep"),
    stringsAsFactors = FALSE
  )

  res <- reconcile_override_batch(rec, batch, quiet = TRUE)
  expect_equal(nrow(res$overrides), 3L)
  m <- reconcile_mapping(res)
  # Unknown a should now be manual/override and matched to Gorilla
  row_a <- m[m$name_x == "Unknown a" & !is.na(m$name_x), ]
  expect_true(row_a$match_type %in% c("manual", "override"))
  # Pan troglodytes should be unresolved
  row_p <- m[m$name_x == "Pan troglodytes" & !is.na(m$name_x), ]
  expect_equal(row_p$match_type, "unresolved")
})
