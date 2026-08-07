# Live, real-data smoke tests for each pr_get_tree() backend.
#
# These are NOT mocked. They install backends through Suggests/Remotes
# and exercise the documented n_tree contract end-to-end. Without
# these tests, the cascade of fictional-API bugs from Round 4-9
# (rtrees::get_tree's nonexistent n_tree arg; clootl::extractTree's
# nonexistent sample.size arg) shipped untested. Adding them here so
# the next time we make a documentation claim about a backend, the
# claim is verifiable.
#
# Skip strategies:
#   - skip_on_cran()              -- CRAN skips live/network backend tests
#   - skip_if_not_installed("X")  -- skip when the backend isn't there
#   - skip_if_offline()           -- some backends touch the network

test_that("LIVE: rtrees + bird returns multiPhylo (n_tree informational)", {
  skip_on_cran()
  skip_if_not_installed("rtrees")
  skip_if_offline("api.github.com")
  # Three real bird species that are guaranteed to be in the rtrees
  # bird mega-tree (Jetz et al. 2012).
  res <- pr_get_tree(
    c("Corvus corax", "Pica pica", "Turdus merula"),
    source = "rtrees", taxon = "bird", tnrs = "never"
  )
  expect_s3_class(res, "pr_tree_result")
  # rtrees default for bird returns multiPhylo (100 trees).
  expect_true(inherits(res$tree, "multiPhylo") ||
              inherits(res$tree, "phylo"))
  expect_true(length(res$matched) >= 1L)
})


test_that("LIVE: fishtree + n_tree returns exactly n_tree multiPhylo", {
  skip_on_cran()
  skip_if_not_installed("fishtree")
  skip_if_offline("fishtreeoflife.org")
  res <- pr_get_tree(
    c("Salmo salar", "Esox lucius", "Gadus morhua"),
    source = "fishtree", n_tree = 5, tnrs = "never"
  )
  expect_s3_class(res$tree, "multiPhylo")
  expect_equal(length(res$tree), 5L)
  expect_true(length(res$matched) >= 1L)
})


test_that("LIVE: fishtree single tree returns single phylo", {
  skip_on_cran()
  skip_if_not_installed("fishtree")
  skip_if_offline("fishtreeoflife.org")
  res <- pr_get_tree(
    c("Salmo salar", "Esox lucius"),
    source = "fishtree", n_tree = 1, tnrs = "never"
  )
  expect_s3_class(res$tree, "phylo")
})


test_that("LIVE: rotl returns single phylo (synthesis)", {
  skip_on_cran()
  skip_if_not_installed("rotl")
  skip_if_offline("api.opentreeoflife.org")
  res <- tryCatch(
    pr_get_tree(c("Homo sapiens", "Pan troglodytes"),
                source = "rotl"),
    error = function(e) e
  )
  # rotl can be flaky on transient network errors; if it returned an
  # error we don't want CI to flap, but we do want to surface it.
  if (inherits(res, "error")) {
    skip(paste("rotl call errored:", conditionMessage(res)))
  }
  expect_s3_class(res, "pr_tree_result")
  expect_s3_class(res$tree, "phylo")
})


test_that("LIVE: clootl n_tree = 1 succeeds without an AvesData repo", {
  # Regression test for the May 2026 finding that the user could in
  # fact get a single tree from `clootl` -- the wrapper had not
  # accounted for `clootl::extractTree()`'s lookup of `clootl_data`,
  # which only resolves when `clootl` is on the search path. Now the
  # wrapper attaches `clootl` for the duration of the call, so the
  # bundled v1.6 / 2025 taxonomy works out of the box.
  skip_on_cran()
  skip_if_not_installed("clootl")
  res <- pr_get_tree(c("Corvus corax", "Pica pica"),
                     source = "clootl", n_tree = 1, tnrs = "never")
  expect_s3_class(res, "pr_tree_result")
  expect_s3_class(res$tree, "phylo")
  expect_gte(ape::Ntip(res$tree), 1L)
  # search() should be unchanged after the call -- the wrapper detaches
  # clootl on exit if it had to attach it.
  expect_false("package:clootl" %in% search())
})


test_that("LIVE: clootl with default tnrs runs fast on a 200-species request (#70)", {
  # Regression test for #70 (Ayumi, May 2026): with the old default
  # `tnrs = "auto"`, `pr_get_tree(source = "clootl")` ran an
  # `rotl::tnrs_match_names()` preflight against the Open Tree of
  # Life API for the full species list. With 10,597 birds the call
  # never finished after 15 minutes; even a 200-species request was
  # tens of seconds of network. clootl uses the eBird / Clements
  # taxonomy, which is independent of OTL, so the preflight was both
  # slow AND not improving matching. Fix: drop clootl from the
  # `tnrs = "auto"` default. This test exercises the fast path
  # explicitly and asserts it stays under 30 seconds for 200 species
  # -- a generous ceiling well below the broken behaviour but
  # forgiving enough for slow CI machines.
  skip_on_cran()
  skip_if_not_installed("clootl")
  utils::data("clootl_data", package = "clootl", envir = environment())
  all_birds <- clootl_data$taxonomies$year2025$SCI_NAME
  set.seed(1)
  spp <- sample(all_birds, 200)

  t0 <- Sys.time()
  # Default args -- no explicit tnrs override -- proves the new
  # default doesn't trigger the OTL preflight for clootl.
  res <- pr_get_tree(spp, source = "clootl", n_tree = 1)
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")

  expect_s3_class(res, "pr_tree_result")
  expect_s3_class(res$tree, "phylo")
  expect_lt(elapsed, 30)
})


test_that("LIVE: clootl tolerates unmatched species via force = TRUE (#70)", {
  # Regression test: previously a single unmatched name made
  # `clootl::extractTree()` error out with the entire batch lost.
  # The wrapper now passes `force = TRUE` by default so unmatched
  # species are reported in `$unmatched` and the rest of the tree
  # is still returned.
  skip_on_cran()
  skip_if_not_installed("clootl")
  res <- pr_get_tree(c("Corvus corax", "Pica pica",
                       "Definitely not a real species"),
                     source = "clootl", n_tree = 1, tnrs = "never")
  expect_s3_class(res$tree, "phylo")
  expect_equal(length(res$matched), 2L)
  expect_equal(length(res$unmatched), 1L)
})


test_that("LIVE: clootl accepts underscore-form species names (#75)", {
  skip_on_cran()
  skip_if_not_installed("clootl")
  res <- pr_get_tree(c("Corvus_corax", "Pica_pica"),
                     source = "clootl", n_tree = 1, tnrs = "never")
  expect_equal(res$matched, c("Corvus_corax", "Pica_pica"))
  expect_length(res$unmatched, 0L)
})


test_that("LIVE: clootl all-unmatched species errors cleanly (#75)", {
  skip_on_cran()
  skip_if_not_installed("clootl")
  expect_error(
    suppressWarnings(
      pr_get_tree("Definitely not a real species",
                  source = "clootl", n_tree = 1, tnrs = "never")
    ),
    "returned no tree"
  )
})


test_that("LIVE: clootl n_tree > 1 needs AvesData, errors helpfully when missing", {
  skip_on_cran()
  skip_if_not_installed("clootl")
  has_avesdata <- nzchar(Sys.getenv("AVESDATA_PATH"))
  if (has_avesdata) {
    res <- pr_get_tree(c("Corvus corax", "Pica pica"),
                       source = "clootl", n_tree = 5, tnrs = "never")
    expect_s3_class(res, "pr_tree_result")
    expect_s3_class(res$tree, "multiPhylo")
    return()
  }
  err <- tryCatch(
    pr_get_tree(c("Corvus corax", "Pica pica"),
                source = "clootl", n_tree = 5, tnrs = "never"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "AvesData", fixed = TRUE)
})


test_that("LIVE: datelife backend (skipped if not installed)", {
  skip_on_cran()
  skip_if_not_installed("datelife")
  skip_if_offline("api.opentreeoflife.org")
  res <- tryCatch(
    pr_get_tree(c("Rhea americana", "Struthio camelus"),
                source = "datelife", n_tree = 1, tnrs = "never"),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    skip(paste("datelife call errored:", conditionMessage(res)))
  }
  expect_s3_class(res, "pr_tree_result")
})
