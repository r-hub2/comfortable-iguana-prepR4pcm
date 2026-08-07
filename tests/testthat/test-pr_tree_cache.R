# Cache tests use a tempdir cache so the user's actual cache is
# untouched. We exercise:
#   - pr_tree_cache_dir() get/set
#   - pr_tree_cache_status() on empty + populated cache
#   - pr_tree_cache_clear() with confirm = FALSE
#   - .pr_tree_cache_key() determinism + result-sensitive request fields
#   - .pr_tree_cache_get/put round-trip
#   - integration with pr_get_tree(cache = TRUE): hit, miss

mini_phylo <- function(tip_labels) {
  ape::read.tree(text = paste0("(", paste(tip_labels, collapse = ","), ");"))
}


test_that("pr_tree_cache_dir() returns a path; setting it works", {
  td <- tempfile("prtree-test-cache-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)

  pr_tree_cache_dir(td)
  expect_equal(normalizePath(pr_tree_cache_dir()), normalizePath(td))
  expect_true(dir.exists(td))
})


test_that("pr_tree_cache_status() empty cache returns 0-row data.frame", {
  td <- tempfile("prtree-test-cache-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  status <- pr_tree_cache_status()
  expect_s3_class(status, "data.frame")
  expect_equal(nrow(status), 0L)
  expect_setequal(names(status), c("source", "hash", "size_kb", "modified"))
})


test_that(".pr_tree_cache_key is deterministic and result-sensitive", {
  k1 <- .pr_tree_cache_key(
    c("Homo sapiens", "Pan troglodytes"),
    source = "rotl",
    n_tree = 1L
  )
  k2 <- .pr_tree_cache_key(
    c("Pan troglodytes", "Homo sapiens"),
    source = "rotl",
    n_tree = 1L
  )
  expect_false(identical(k1, k2))

  # Different n_tree -> different key
  k3 <- .pr_tree_cache_key(c("Homo sapiens"), source = "rotl", n_tree = 5L)
  k4 <- .pr_tree_cache_key(c("Homo sapiens"), source = "rotl", n_tree = 1L)
  expect_false(identical(k3, k4))

  # Different source -> different key
  k5 <- .pr_tree_cache_key(c("Homo sapiens"), source = "fishtree", n_tree = 1L)
  k6 <- .pr_tree_cache_key(c("Homo sapiens"), source = "rotl", n_tree = 1L)
  expect_false(identical(k5, k6))

  # Different backend argument values -> different key
  k7 <- .pr_tree_cache_key(
    c("Salmo salar"),
    source = "fishtree",
    n_tree = 1L,
    type = "chronogram"
  )
  k8 <- .pr_tree_cache_key(
    c("Salmo salar"),
    source = "fishtree",
    n_tree = 1L,
    type = "phylogram"
  )
  expect_false(identical(k7, k8))

  # Different TNRS choices -> different key when original species match
  k9 <- .pr_tree_cache_key(
    c("Salmo salar"),
    source = "fishtree",
    n_tree = 1L,
    tnrs = "auto"
  )
  k10 <- .pr_tree_cache_key(
    c("Salmo salar"),
    source = "fishtree",
    n_tree = 1L,
    tnrs = "never"
  )
  expect_false(identical(k9, k10))
})


test_that(".pr_tree_cache_put / get round-trip works", {
  td <- tempfile("prtree-test-cache-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  fake <- list(
    tree = mini_phylo(c("a", "b")),
    source = "rotl",
    matched = c("a", "b"),
    backend_meta = list()
  )
  class(fake) <- "pr_tree_result"

  key <- "abc123"
  .pr_tree_cache_put(key, "rotl", fake)
  got <- .pr_tree_cache_get(key, "rotl")
  expect_s3_class(got, "pr_tree_result")
  expect_equal(got$matched, c("a", "b"))
})


test_that("pr_tree_cache_clear() removes files (non-interactive)", {
  td <- tempfile("prtree-test-cache-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  fake <- list(
    tree = mini_phylo(c("a", "b")),
    source = "rotl",
    matched = "a",
    unmatched = "b",
    backend_meta = list()
  )
  class(fake) <- "pr_tree_result"
  .pr_tree_cache_put("k1", "rotl", fake)
  .pr_tree_cache_put("k2", "fishtree", fake)
  expect_equal(nrow(pr_tree_cache_status()), 2L)

  removed <- pr_tree_cache_clear(confirm = FALSE)
  expect_equal(removed, 2L)
  expect_equal(nrow(pr_tree_cache_status()), 0L)
})


test_that("pr_tree_cache_clear(source = X) only removes matching", {
  td <- tempfile("prtree-test-cache-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  fake <- list(
    tree = mini_phylo(c("a")),
    source = "rotl",
    matched = "a",
    unmatched = character(),
    backend_meta = list()
  )
  class(fake) <- "pr_tree_result"
  .pr_tree_cache_put("k1", "rotl", fake)
  .pr_tree_cache_put("k2", "fishtree", fake)

  pr_tree_cache_clear(confirm = FALSE, source = "rotl")
  rest <- pr_tree_cache_status()
  expect_equal(nrow(rest), 1L)
  expect_equal(rest$source, "fishtree")
})


test_that("pr_get_tree(cache = TRUE) writes and reads the cache", {
  td <- tempfile("prtree-test-cache-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  call_count <- 0L
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      call_count <<- call_count + 1L
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        backend_meta = list(call_count = call_count)
      )
    },
    .package = "prepR4pcm"
  )

  # First call: miss, backend invoked
  res1 <- pr_get_tree("foo", source = "rotl", cache = TRUE)
  expect_equal(call_count, 1L)

  # Second identical call: hit, backend NOT invoked
  res2 <- pr_get_tree("foo", source = "rotl", cache = TRUE)
  expect_equal(call_count, 1L)
  expect_equal(res1$matched, res2$matched)
})


test_that("pr_get_tree cache preserves original input spelling", {
  td <- tempfile("prtree-test-cache-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  call_count <- 0L
  testthat::local_mocked_bindings(
    .pr_get_tree_clootl = function(species, n_tree = 1L, ...) {
      call_count <<- call_count + 1L
      list(
        tree = mini_phylo(c("Corvus_corax", "Pica_pica")),
        in_query = species == "Corvus corax",
        backend_meta = list(call_count = call_count)
      )
    },
    .package = "prepR4pcm"
  )

  res1 <- pr_get_tree(
    "Corvus_corax",
    source = "clootl",
    cache = TRUE,
    tnrs = "never"
  )
  res2 <- pr_get_tree(
    "Corvus corax",
    source = "clootl",
    cache = TRUE,
    tnrs = "never"
  )

  expect_equal(call_count, 2L)
  expect_equal(res1$matched, "Corvus_corax")
  expect_equal(res2$matched, "Corvus corax")
})
