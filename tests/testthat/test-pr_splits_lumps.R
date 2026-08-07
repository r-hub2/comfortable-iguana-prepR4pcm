test_that("pr_detect_splits_lumps returns empty for no synonym matches", {
  mapping <- tibble::tibble(
    name_x        = c("Homo sapiens", "Pan troglodytes"),
    name_y        = c("Homo sapiens", "Pan troglodytes"),
    name_resolved = NA_character_,
    match_type    = c("exact", "exact"),
    match_score   = c(1, 1),
    match_source  = c("exact_string", "exact_string"),
    in_x          = c(TRUE, TRUE),
    in_y          = c(TRUE, TRUE),
    notes         = c("", "")
  )
  sl <- pr_detect_splits_lumps(mapping)
  expect_equal(nrow(sl$splits), 0)
  expect_equal(nrow(sl$lumps), 0)
})

test_that("pr_detect_splits_lumps detects lumps", {
  # Two x names share the same resolved name → lump

  mapping <- tibble::tibble(
    name_x        = c("Species A", "Species B"),
    name_y        = c("Species C", "Species C"),
    name_resolved = c("Accepted name", "Accepted name"),
    match_type    = c("synonym", "synonym"),
    match_score   = c(0.95, 0.95),
    match_source  = c("col_synonym", "col_synonym"),
    in_x          = c(TRUE, TRUE),
    in_y          = c(TRUE, TRUE),
    notes         = c("test", "test")
  )
  sl <- pr_detect_splits_lumps(mapping)
  expect_true(nrow(sl$lumps) >= 1)
  expect_equal(sl$lumps$name_resolved[1], "Accepted name")
  expect_equal(sl$lumps$n_x[1], 2)
})

test_that("pr_detect_splits_lumps detects splits", {
  # One x name maps to two y names via same resolved name → split
  mapping <- tibble::tibble(
    name_x        = c("Species A", "Species A"),
    name_y        = c("Species B", "Species C"),
    name_resolved = c("Accepted name", "Accepted name"),
    match_type    = c("synonym", "synonym"),
    match_score   = c(0.95, 0.95),
    match_source  = c("col_synonym", "col_synonym"),
    in_x          = c(TRUE, TRUE),
    in_y          = c(TRUE, TRUE),
    notes         = c("test", "test")
  )
  sl <- pr_detect_splits_lumps(mapping)
  expect_true(nrow(sl$splits) >= 1)
  expect_equal(sl$splits$n_y[1], 2)
})

test_that("reconcile_splits_lumps validates input", {
  expect_error(
    reconcile_splits_lumps("not a reconciliation"),
    "reconciliation"
  )
})

test_that("reconcile_splits_lumps works on reconciliation objects", {
  mapping <- tibble::tibble(
    name_x        = c("Homo sapiens"),
    name_y        = c("Homo sapiens"),
    name_resolved = NA_character_,
    match_type    = c("exact"),
    match_score   = c(1),
    match_source  = c("exact_string"),
    in_x          = c(TRUE),
    in_y          = c(TRUE),
    notes         = c("")
  )
  meta <- list(
    call = NULL, type = "data_tree", timestamp = Sys.time(),
    authority = "none", db_version = "latest", fuzzy = FALSE,
    fuzzy_threshold = NA_real_, fuzzy_method = NA_character_,
    resolve = "flag", prepR4pcm_version = "0.2.0",
    x_source = "test", y_source = "test", rank = "species"
  )
  rec <- new_reconciliation(mapping = mapping, meta = meta)
  sl <- reconcile_splits_lumps(rec, quiet = TRUE)
  expect_equal(nrow(sl$splits), 0)
  expect_equal(nrow(sl$lumps), 0)
})
