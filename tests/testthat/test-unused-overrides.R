# Issue #8a: overrides whose name_y is not in the tree (or whose
# name_x is not in the data) used to be silently dropped. The
# reconciliation object now exposes them as `$unused_overrides` and
# `reconcile_tree()` warns when any are present.

# Small reusable fixtures
mk_data <- function() {
  data.frame(
    species = c("Homo sapiens", "Pan troglodytes", "Gorilla gorilla"),
    stringsAsFactors = FALSE
  )
}
mk_tree <- function() {
  ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")
}


test_that("reconciliation object always carries an unused_overrides slot", {
  res <- reconcile_tree(mk_data(), mk_tree(),
                        x_species = "species",
                        authority = NULL, quiet = TRUE)
  expect_true("unused_overrides" %in% names(res))
  expect_s3_class(res$unused_overrides, "data.frame")
  expect_equal(nrow(res$unused_overrides), 0L)
  expect_equal(names(res$unused_overrides), c("name_x", "name_y", "reason"))
})


test_that("override with name_y not in tree is recorded with reason name_y_not_in_target", {
  bad_overrides <- data.frame(
    name_x = "Fake_species",
    name_y = "Pampa_curvipennis",  # not in mk_tree()
    user_note = "test override with target not in tree",
    stringsAsFactors = FALSE
  )
  res <- reconcile_tree(mk_data(), mk_tree(),
                        x_species = "species",
                        authority = NULL,
                        overrides = bad_overrides,
                        quiet = TRUE)
  expect_equal(nrow(res$unused_overrides), 1L)
  expect_equal(res$unused_overrides$reason, "name_x_not_in_data")
  # Note: in this fixture BOTH names are missing; the priority order
  # in pr_cascade returns name_x_not_in_data first when both are missing.
})


test_that("override with name_x not in data is recorded", {
  ov <- data.frame(
    name_x = "Bogus species",
    name_y = "Homo_sapiens",
    user_note = "x missing",
    stringsAsFactors = FALSE
  )
  res <- reconcile_tree(mk_data(), mk_tree(),
                        x_species = "species",
                        authority = NULL,
                        overrides = ov,
                        quiet = TRUE)
  expect_equal(nrow(res$unused_overrides), 1L)
  expect_equal(res$unused_overrides$reason, "name_x_not_in_data")
})


test_that("override with name_y not in tree (but name_x present) is recorded", {
  ov <- data.frame(
    name_x = "Homo sapiens",         # in data
    name_y = "Pampa_curvipennis",    # not in tree
    user_note = "y missing",
    stringsAsFactors = FALSE
  )
  res <- reconcile_tree(mk_data(), mk_tree(),
                        x_species = "species",
                        authority = NULL,
                        overrides = ov,
                        quiet = TRUE)
  expect_equal(nrow(res$unused_overrides), 1L)
  expect_equal(res$unused_overrides$reason, "name_y_not_in_target")
})


test_that("valid override is applied and unused_overrides stays empty", {
  ov <- data.frame(
    name_x = "Homo sapiens",
    name_y = "Homo_sapiens",
    user_note = "exact mapping",
    stringsAsFactors = FALSE
  )
  res <- reconcile_tree(mk_data(), mk_tree(),
                        x_species = "species",
                        authority = NULL,
                        overrides = ov,
                        quiet = TRUE)
  expect_equal(nrow(res$unused_overrides), 0L)
  manual <- res$mapping[res$mapping$match_type == "manual", ]
  expect_equal(nrow(manual), 1L)
})


test_that("reconcile_tree warns about unused overrides when not quiet", {
  bad <- data.frame(
    name_x = "Homo sapiens",
    name_y = "Pampa_curvipennis",
    user_note = "y missing",
    stringsAsFactors = FALSE
  )
  expect_message(
    reconcile_tree(mk_data(), mk_tree(),
                   x_species = "species",
                   authority = NULL,
                   overrides = bad,
                   quiet = FALSE),
    "could not be applied"
  )
})


test_that("reconcile_summary reports unused-override count when non-zero", {
  bad <- data.frame(
    name_x = "Homo sapiens",
    name_y = "Pampa_curvipennis",
    user_note = "y missing",
    stringsAsFactors = FALSE
  )
  res <- reconcile_tree(mk_data(), mk_tree(),
                        x_species = "species",
                        authority = NULL,
                        overrides = bad,
                        quiet = TRUE)
  out <- capture.output(reconcile_summary(res, format = "console", detail = "brief"))
  expect_true(any(grepl("Overrides unused: 1", out, fixed = TRUE)))
})
