# Regression tests for #83: with the default db_version = NULL,
# pr_lookup_authority used to forward version = NULL to taxadb::filter_name.
# taxadb 0.2.1's parse_schema() then ran `if (version == "latest")` and
# errored with logical(0), producing
#   "no match found for schema: dwc provider: col version:"
# Fix: only include the `version` argument when the caller supplied one.

test_that("filter_name receives no version arg when db_version = NULL (#83)", {
  skip_if_not_installed("taxadb")

  captured <- NULL
  fake_filter_name <- function(...) {
    captured <<- list(...)
    tibble::tibble(
      scientificName       = "Salmo salar",
      taxonomicStatus      = "accepted",
      taxonID              = "COL:1",
      acceptedNameUsageID  = "COL:1"
    )
  }

  testthat::local_mocked_bindings(
    filter_name = fake_filter_name,
    .package    = "taxadb"
  )
  testthat::local_mocked_bindings(
    pr_ensure_db = function(authority, db_version = NULL) invisible(authority),
    .package     = "prepR4pcm"
  )

  res <- pr_lookup_authority(
    c("Salmo salar"),
    authority  = "col",
    db_version = NULL
  )

  expect_s3_class(res, "data.frame")
  expect_false(
    "version" %in% names(captured),
    info = paste0(
      "When db_version is NULL, pr_lookup_authority must NOT forward ",
      "version = NULL to taxadb::filter_name (taxadb 0.2.1 crashes ",
      "inside parse_schema)."
    )
  )
})

test_that("filter_name receives the explicit version when db_version is set", {
  skip_if_not_installed("taxadb")

  captured <- NULL
  fake_filter_name <- function(...) {
    captured <<- list(...)
    tibble::tibble(
      scientificName       = "Salmo salar",
      taxonomicStatus      = "accepted",
      taxonID              = "COL:1",
      acceptedNameUsageID  = "COL:1"
    )
  }

  testthat::local_mocked_bindings(
    filter_name = fake_filter_name,
    .package    = "taxadb"
  )
  testthat::local_mocked_bindings(
    pr_ensure_db = function(authority, db_version = NULL) invisible(authority),
    .package     = "prepR4pcm"
  )

  pr_lookup_authority(
    c("Salmo salar"),
    authority  = "col",
    db_version = "22.12"
  )

  expect_equal(captured$version, "22.12")
})

test_that("scientificName-only taxadb output does not crash subscripting (#83)", {
  skip_if_not_installed("taxadb")

  # Real taxadb 0.2.1 returns filter_name output WITHOUT an `input`
  # column. The previous code did
  #   all_hits$scientificName == name | all_hits$input == name
  # which evaluates the RHS to `NULL == name` -> logical(0). Tibble
  # then complained: "Unknown or uninitialised column: `input`".
  fake_filter_name <- function(...) {
    tibble::tibble(
      scientificName       = c("Salmo salar", "Esox lucius"),
      taxonomicStatus      = c("accepted", "accepted"),
      taxonID              = c("COL:1", "COL:2"),
      acceptedNameUsageID  = c("COL:1", "COL:2")
    )
  }

  testthat::local_mocked_bindings(
    filter_name = fake_filter_name,
    .package    = "taxadb"
  )
  testthat::local_mocked_bindings(
    pr_ensure_db = function(authority, db_version = NULL) invisible(authority),
    .package     = "prepR4pcm"
  )

  expect_warning(
    res <- pr_lookup_authority(
      c("Salmo salar", "Esox lucius"),
      authority = "col"
    ),
    regexp = NA  # no warning expected
  )
  expect_equal(nrow(res), 2)
  expect_equal(res$status, c("accepted", "accepted"))
})
