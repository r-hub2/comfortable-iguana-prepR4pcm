# Tests: TNRS match-metadata capture.
#
# .pr_resolve_query() pulls rotl::tnrs_match_names()'s structured
# columns (number_matches, is_synonym, approximate_match, flags) into a
# `tnrs_audit` table and warns when a name is a homonym
# (number_matches > 1). .pr_build_tree_mapping() surfaces those columns
# in `result$mapping`.

# A tnrs_match_names() mock that returns the full audit-column set.
# Each of `number_matches`, `is_synonym`, `approximate_match`, `flags`
# is recycled to one row per unique input name; pass a vector aligned
# to the de-duplicated input order to vary a column per name.
fake_tnrs_audit <- function(unique_name = identity,
                            number_matches = 1L,
                            is_synonym = FALSE,
                            approximate_match = FALSE,
                            flags = "") {
  function(names, ...) {
    u <- unique(names)
    recycle <- function(x) if (length(x) == 1L) rep(x, length(u)) else x
    data.frame(
      search_string     = tolower(u),
      unique_name       = if (is.function(unique_name)) {
        unique_name(u)
      } else {
        recycle(unique_name)
      },
      number_matches    = recycle(number_matches),
      is_synonym        = recycle(is_synonym),
      approximate_match = recycle(approximate_match),
      flags             = recycle(flags),
      stringsAsFactors  = FALSE
    )
  }
}


test_that(".pr_resolve_query captures TNRS audit columns", {
  skip_if_not_installed("rotl")

  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_audit(
      number_matches    = c(1L, 1L),
      is_synonym        = c(FALSE, TRUE),
      approximate_match = c(FALSE, TRUE),
      flags             = c("", "sibling_higher")
    ),
    .package = "rotl"
  )

  res <- .pr_resolve_query(
    c("Salmo salar", "Esox lucius"),
    source = "fishtree", tnrs = "always"
  )

  expect_s3_class(res$tnrs_audit, "data.frame")
  expect_equal(nrow(res$tnrs_audit), 2L)
  expect_named(
    res$tnrs_audit,
    c("tnrs_number_matches", "tnrs_is_synonym",
      "tnrs_approximate_match", "tnrs_flags")
  )
  expect_equal(res$tnrs_audit$tnrs_number_matches, c(1L, 1L))
  expect_equal(res$tnrs_audit$tnrs_is_synonym, c(FALSE, TRUE))
  expect_equal(res$tnrs_audit$tnrs_approximate_match, c(FALSE, TRUE))
  expect_equal(res$tnrs_audit$tnrs_flags, c("", "sibling_higher"))
})


test_that(".pr_resolve_query warns when TNRS reports a homonym", {
  skip_if_not_installed("rotl")

  # "Prunella" is a genuine homonym (a bird genus and a plant genus);
  # TNRS reports number_matches = 2 for it, 1 for the unambiguous name.
  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_audit(number_matches = c(2L, 1L)),
    .package = "rotl"
  )

  expect_warning(
    res <- .pr_resolve_query(
      c("Prunella", "Salmo salar"),
      source = "fishtree", tnrs = "always"
    ),
    "homonym"
  )
  expect_equal(res$tnrs_audit$tnrs_number_matches, c(2L, 1L))
})


test_that(".pr_resolve_query does not warn when every name has one match", {
  skip_if_not_installed("rotl")

  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_audit(number_matches = 1L),
    .package = "rotl"
  )

  expect_no_warning(
    .pr_resolve_query(
      c("Salmo salar", "Esox lucius"),
      source = "fishtree", tnrs = "always"
    )
  )
})


test_that(".pr_resolve_query returns NULL tnrs_audit when TNRS does not run", {
  res <- .pr_resolve_query(
    c("Salmo salar", "Esox lucius"),
    source = "fishtree", tnrs = "never"
  )
  expect_null(res$tnrs_audit)
})


test_that(".pr_build_tree_mapping surfaces TNRS audit columns", {
  audit <- tibble::tibble(
    tnrs_number_matches    = c(1L, 2L),
    tnrs_is_synonym        = c(FALSE, TRUE),
    tnrs_approximate_match = c(FALSE, FALSE),
    tnrs_flags             = c("", "sibling_higher")
  )
  tree <- ape::read.tree(text = "(Salmo_salar:1,Esox_lucius:1);")

  m <- .pr_build_tree_mapping(
    input_name      = c("Salmo salar", "Esox lucius"),
    normalized_name = c("Salmo salar", "Esox lucius"),
    query_name      = c("Salmo salar", "Esox lucius"),
    in_tree         = c(TRUE, TRUE),
    tree            = tree,
    tnrs_audit      = audit
  )

  expect_true(all(
    c("tnrs_number_matches", "tnrs_is_synonym",
      "tnrs_approximate_match", "tnrs_flags") %in% names(m)
  ))
  expect_equal(m$tnrs_number_matches, c(1L, 2L))
  expect_equal(m$tnrs_is_synonym, c(FALSE, TRUE))
  expect_equal(m$tnrs_flags, c("", "sibling_higher"))
})


test_that(".pr_build_tree_mapping fills NA TNRS columns when audit is NULL", {
  tree <- ape::read.tree(text = "(Salmo_salar:1,Esox_lucius:1);")

  m <- .pr_build_tree_mapping(
    input_name      = c("Salmo salar", "Esox lucius"),
    normalized_name = c("Salmo salar", "Esox lucius"),
    query_name      = c("Salmo salar", "Esox lucius"),
    in_tree         = c(TRUE, TRUE),
    tree            = tree,
    tnrs_audit      = NULL
  )

  expect_true(all(is.na(m$tnrs_number_matches)))
  expect_true(all(is.na(m$tnrs_flags)))
  expect_type(m$tnrs_number_matches, "integer")
  expect_type(m$tnrs_flags, "character")
})
