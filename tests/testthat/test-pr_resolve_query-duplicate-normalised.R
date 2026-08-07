# Regression tests: .pr_resolve_query() must realign tnrs_match_names()
# output to the (possibly duplicated) `normalised` vector.
#
# Bug: `rotl::tnrs_match_names()` de-duplicates its input, so when two
# distinct input names normalise to the same string (e.g. "Esox lucius"
# and "Esox_lucius" both normalise to "Esox lucius"), the TNRS result
# had fewer rows than `normalised`. Comparing `tnrs_res$unique_name`
# against `normalised` then recycled a short vector against a long one
# ("longer object length is not a multiple of shorter object length")
# and silently mis-matched species. The fix queries the unique
# normalised names and realigns by the lowercased `search_string`.

# tnrs_match_names() de-duplicates its input and returns one row per
# unique name, with a lowercased `search_string`. This mock mirrors
# that contract; `resolver` lets a test inject name substitutions.
fake_tnrs_factory <- function(resolver = identity) {
  function(names, ...) {
    u <- unique(names)
    data.frame(
      search_string = tolower(u),
      unique_name   = resolver(u),
      stringsAsFactors = FALSE
    )
  }
}

test_that(".pr_resolve_query realigns TNRS results when normalised names duplicate", {
  skip_if_not_installed("rotl")

  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_factory(),  # no substitutions
    .package = "rotl"
  )

  # "Esox lucius" and "Esox_lucius" both normalise to "Esox lucius".
  input <- c("Esox lucius", "Esox_lucius", "Oncorhynchus mykiss")

  expect_no_warning(
    res <- .pr_resolve_query(input, source = "fishtree", tnrs = "always")
  )
  expect_equal(res$original, input)
  expect_length(res$query, length(input))
  # each element resolves to its own species, not a shifted neighbour
  # (ignore_attr: pr_normalize_names() attaches a normalisation_log attr)
  expect_equal(res$query,
               c("Esox lucius", "Esox lucius", "Oncorhynchus mykiss"),
               ignore_attr = TRUE)
})

test_that(".pr_resolve_query maps a TNRS substitution to the correct species", {
  skip_if_not_installed("rotl")

  # TNRS rewrites "Esox lucius" -> "Esox reichertii" (stand-in synonym).
  # Both forms of Esox lucius must pick up the substitute; the unrelated
  # species must be left alone.
  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_factory(
      function(u) ifelse(u == "Esox lucius", "Esox reichertii", u)
    ),
    .package = "rotl"
  )

  input <- c("Esox lucius", "Esox_lucius", "Oncorhynchus mykiss")
  # the TNRS-substitution cli warning is expected here; not under test
  res <- suppressWarnings(
    .pr_resolve_query(input, source = "fishtree", tnrs = "always")
  )

  expect_equal(res$query,
               c("Esox reichertii", "Esox reichertii", "Oncorhynchus mykiss"),
               ignore_attr = TRUE)
  # replacement map is keyed by the ORIGINAL input strings
  expect_setequal(names(res$tnrs_replacements),
                  c("Esox lucius", "Esox_lucius"))
})

test_that(".pr_resolve_query is unaffected when no normalised names collide", {
  skip_if_not_installed("rotl")

  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_factory(),
    .package = "rotl"
  )

  input <- c("Salmo salar", "Esox lucius", "Gadus morhua")
  res <- .pr_resolve_query(input, source = "fishtree", tnrs = "always")
  expect_equal(res$query, input, ignore_attr = TRUE)
})


# Regression tests: tnrs_match_names()'s `unique_name` carries Open Tree
# homonym / rank qualifiers (e.g. "Oncorhynchus mykiss (species in
# domain Eukaryota)"). .pr_resolve_query() must strip these before the
# resolved name reaches the backend query, and must not mistake a
# qualifier-only difference for a genuine TNRS substitution.

test_that(".pr_resolve_query strips OTL qualifiers from the resolved query", {
  skip_if_not_installed("rotl")

  # TNRS echoes each name back with a trailing "(species in domain ...)"
  # qualifier but makes no genuine substitution.
  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_factory(
      function(u) paste0(u, " (species in domain Eukaryota)")
    ),
    .package = "rotl"
  )

  input <- c("Salmo salar", "Esox lucius")
  # A qualifier-only difference is not a substitution: no warning fires
  # and `tnrs_replacements` stays NULL.
  expect_no_warning(
    res <- .pr_resolve_query(input, source = "fishtree", tnrs = "always")
  )
  expect_equal(res$query, c("Salmo salar", "Esox lucius"),
               ignore_attr = TRUE)
  expect_null(res$tnrs_replacements)
})

test_that(".pr_resolve_query records a genuine substitution with the qualifier stripped", {
  skip_if_not_installed("rotl")

  # TNRS rewrites "Esox lucius" -> "Esox reichertii" AND tacks on an OTL
  # rank qualifier. The substitution must be recorded; the qualifier
  # must not leak into either the query or the replacement map.
  testthat::local_mocked_bindings(
    tnrs_match_names = fake_tnrs_factory(
      function(u) ifelse(u == "Esox lucius",
                         "Esox reichertii (species in domain Eukaryota)",
                         u)
    ),
    .package = "rotl"
  )

  input <- c("Salmo salar", "Esox lucius")
  res <- suppressWarnings(
    .pr_resolve_query(input, source = "fishtree", tnrs = "always")
  )

  expect_equal(res$query, c("Salmo salar", "Esox reichertii"),
               ignore_attr = TRUE)
  # the substitution is recorded, keyed by the original input ...
  expect_named(res$tnrs_replacements, "Esox lucius")
  # ... and its value is the clean binomial, not "Esox reichertii (...)"
  expect_equal(unname(res$tnrs_replacements), "Esox reichertii",
               ignore_attr = TRUE)
})
