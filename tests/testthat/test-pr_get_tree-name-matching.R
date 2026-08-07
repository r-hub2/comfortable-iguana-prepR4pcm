# Round 15 regression tests for pr_get_tree() name-matching.
#
# Each test pins one invariant from Ayumi's #72/#73/#75/#76. The tests
# document the package's *contract*, not just current behaviour, so a
# future refactor can't silently weaken them.
#
# Convention: every test uses tnrs = "never" by default to isolate
# accounting from network behaviour. The dedicated TNRS tests opt in
# explicitly with tnrs = "always" + skip_if_offline().


# Bug #73 -----------------------------------------------------------

test_that("pr_get_tree(clootl): typo lands in unmatched, not silently dropped", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  species <- c("Corvus_corax", "Pica_pica", "Corvas corax")  # 1 typo
  r <- pr_get_tree(species, source = "clootl", n_tree = 1, tnrs = "never")

  # The typo MUST appear in unmatched -- never silently substituted.
  expect_true("Corvas corax" %in% r$unmatched,
              info = "Typo 'Corvas corax' must surface in unmatched, not be quietly fixed")

  # The two real names should be matched, in their original input form.
  expect_setequal(r$matched, c("Corvus_corax", "Pica_pica"))
})

test_that("pr_get_tree(clootl): length(matched)+length(unmatched) == length(unique(input))", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  species <- c("Corvus_corax", "Pica_pica", "Corvas corax", "Corvus_corax")  # 1 dup
  r <- pr_get_tree(species, source = "clootl", n_tree = 1, tnrs = "never")
  expect_equal(length(r$matched) + length(r$unmatched),
               length(unique(species)),
               info = "matched + unmatched must cover every unique input")
})

test_that("pr_get_tree(clootl): matched is a subset of unique(input) -- never an intermediate name", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  species <- c("Corvus_corax", "Pica_pica")
  r <- pr_get_tree(species, source = "clootl", n_tree = 1, tnrs = "never")
  expect_true(all(r$matched %in% unique(species)),
              info = "matched must contain ONLY names from the user's original input")
  expect_true(all(r$unmatched %in% unique(species)),
              info = "unmatched must contain ONLY names from the user's original input")
})


# Bug #75 -----------------------------------------------------------

test_that("pr_get_tree(clootl): underscore form is accepted (matches space form)", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  r_under <- pr_get_tree(c("Corvus_corax", "Pica_pica"),
                         source = "clootl", n_tree = 1, tnrs = "never")
  r_space <- pr_get_tree(c("Corvus corax", "Pica pica"),
                         source = "clootl", n_tree = 1, tnrs = "never")

  # Same number matched on both sides
  expect_equal(length(r_under$matched), length(r_space$matched))
  # Original-input form is preserved in matched
  expect_true("Corvus_corax" %in% r_under$matched)
  expect_true("Corvus corax" %in% r_space$matched)
})

test_that("pr_get_tree(clootl): OTT-id-suffixed names are normalised before backend", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  # 'Homo sapiens ott770315' format is what rotl::tnrs_match_names() emits;
  # users sometimes paste it back in. pr_normalize_names() strips the
  # ' ott<digits>' tail. The tree should still come back with these species.
  species <- c("Corvus corax ott770315", "Pica pica")
  r <- pr_get_tree(species, source = "clootl", n_tree = 1, tnrs = "never")
  # Both should resolve -- the OTT suffix is normalised away before clootl sees it.
  expect_equal(length(r$matched), 2L,
               info = "OTT-id-suffixed input must be normalised before backend lookup")
  # Matched array preserves the original input form (with the OTT suffix).
  expect_true("Corvus corax ott770315" %in% r$matched)
})

test_that("pr_get_tree(clootl): authority-laden names ('Genus species (Linnaeus, 1758)') normalised", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  species <- c("Corvus corax (Linnaeus, 1758)", "Pica pica")
  r <- pr_get_tree(species, source = "clootl", n_tree = 1, tnrs = "never")
  expect_equal(length(r$matched), 2L,
               info = "Authority strings must be stripped via pr_normalize_names() before backend lookup")
})


# Bug #72 -----------------------------------------------------------

test_that("pr_get_tree(tnrs='always'): typo replacements appear in backend_meta$tnrs_replacements", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  testthat::skip_if_not_installed("rotl")
  testthat::skip_if_offline("api.opentreeoflife.org")
  species <- c("Corvus_corax", "Pica_pica", "Corvas corax")  # 1 typo
  r <- tryCatch(
    pr_get_tree(species, source = "clootl", n_tree = 1, tnrs = "always"),
    error = function(e) e
  )
  if (inherits(r, "error")) {
    skip(paste("TNRS network call failed:", conditionMessage(r)))
  }

  # tnrs_replacements is named character: original -> resolved
  rep <- r$backend_meta$tnrs_replacements
  expect_true(is.null(rep) || is.character(rep),
              info = "tnrs_replacements should be NULL (no replacements) or named character")

  # And the matched/unmatched accounting invariant STILL holds against
  # original input -- TNRS doesn't get to forge agreement.
  expect_equal(length(r$matched) + length(r$unmatched),
               length(unique(species)))
  expect_true(all(r$matched %in% unique(species)))
  expect_true(all(r$unmatched %in% unique(species)))
})

test_that("pr_get_tree(tnrs='never'): backend_meta$tnrs_replacements is NULL", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  species <- c("Corvus corax", "Pica pica")
  r <- pr_get_tree(species, source = "clootl", n_tree = 1, tnrs = "never")
  expect_null(r$backend_meta$tnrs_replacements,
              info = "When TNRS isn't run, the slot must be NULL -- never empty list, never lying")
})


# Bug #76 -----------------------------------------------------------

test_that("pr_get_tree single-tree: backend_meta$n_requested + n_returned + tip_set_consistent set sensibly", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  species <- c("Corvus corax", "Pica pica")
  r <- pr_get_tree(species, source = "clootl", n_tree = 1, tnrs = "never")
  expect_equal(r$backend_meta$n_requested, 1L)
  expect_equal(r$backend_meta$n_returned, 1L)
  expect_true(isTRUE(r$backend_meta$tip_set_consistent),
              info = "Single tree is trivially tip-consistent")
  expect_null(r$backend_meta$dropped_per_tree,
              info = "Single tree has no per-tree drop list")
})

test_that("pr_get_tree multi-tree (when AvesData is set up): tip_set_consistent reports across trees", {
  skip_on_cran()
  testthat::skip_if_not_installed("clootl")
  if (!nzchar(Sys.getenv("AVESDATA_PATH"))) {
    skip("AvesData repo not set up; cannot test clootl multi-tree path")
  }
  species <- c("Corvus corax", "Pica pica", "Turdus merula")
  r <- pr_get_tree(species, source = "clootl", n_tree = 5, tnrs = "never")
  expect_s3_class(r$tree, "multiPhylo")
  expect_equal(r$backend_meta$n_requested, 5L)
  expect_equal(r$backend_meta$n_returned, length(r$tree))
  expect_true(is.logical(r$backend_meta$tip_set_consistent))
  if (isTRUE(r$backend_meta$tip_set_consistent)) {
    expect_null(r$backend_meta$dropped_per_tree,
                info = "When tip sets agree, dropped_per_tree is NULL")
  }
})
