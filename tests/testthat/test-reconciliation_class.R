test_that("new_reconciliation creates valid object", {
  mapping <- tibble::tibble(
    name_x        = "Homo sapiens",
    name_y        = "Homo sapiens",
    name_resolved = NA_character_,
    match_type    = "exact",
    match_score   = 1.0,
    match_source  = "exact_string",
    in_x          = TRUE,
    in_y          = TRUE,
    notes         = ""
  )
  meta <- list(
    call = NULL, type = "data_data", timestamp = Sys.time(),
    authority = "col", db_version = "latest",
    fuzzy = FALSE, fuzzy_threshold = NA, fuzzy_method = NA,
    resolve = "flag", prepR4pcm_version = "0.1.0",
    x_source = "df1", y_source = "df2", rank = "species"
  )

  obj <- new_reconciliation(mapping, meta)
  expect_s3_class(obj, "reconciliation")
  expect_equal(obj$counts$n_exact, 1L)
  expect_equal(obj$counts$n_x, 1L)
})

test_that("validate_reconciliation catches bad objects", {
  expect_error(validate_reconciliation(list()), "must be a <reconciliation>")
})

test_that("print.reconciliation works", {
  mapping <- tibble::tibble(
    name_x        = c("Homo sapiens", "Pan troglodytes"),
    name_y        = c("Homo sapiens", NA),
    name_resolved = NA_character_,
    match_type    = c("exact", "unresolved"),
    match_score   = c(1.0, NA),
    match_source  = c("exact_string", NA),
    in_x          = c(TRUE, TRUE),
    in_y          = c(TRUE, FALSE),
    notes         = c("", "No match found in source y")
  )
  meta <- list(
    call = NULL, type = "data_tree", timestamp = Sys.time(),
    authority = "col", db_version = "latest",
    fuzzy = FALSE, fuzzy_threshold = NA, fuzzy_method = NA,
    resolve = "flag", prepR4pcm_version = "0.1.0",
    x_source = "test_data", y_source = "test_tree", rank = "species"
  )

  obj <- new_reconciliation(mapping, meta)
  # cli output goes to stderr; test that print returns invisibly
  expect_invisible(print(obj))
})

test_that("format.reconciliation returns character vector", {
  mapping <- tibble::tibble(
    name_x        = "A",
    name_y        = "A",
    name_resolved = NA_character_,
    match_type    = "exact",
    match_score   = 1.0,
    match_source  = "exact_string",
    in_x          = TRUE,
    in_y          = TRUE,
    notes         = ""
  )
  meta <- list(
    call = NULL, type = "data_data", timestamp = Sys.time(),
    authority = "none", db_version = "latest",
    fuzzy = FALSE, fuzzy_threshold = NA, fuzzy_method = NA,
    resolve = "flag", prepR4pcm_version = "0.1.0",
    x_source = "a", y_source = "b", rank = "species"
  )

  obj <- new_reconciliation(mapping, meta)
  fmt <- format(obj)
  expect_type(fmt, "character")
  expect_true(length(fmt) > 0)
})
