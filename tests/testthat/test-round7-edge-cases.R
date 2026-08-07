# Round 7 edge cases for the Round 6 surface
# ===========================================
#
# These tests deliberately exercise paths that the happy-path tests
# don't reach: corrupt cache files, missing arg validation, exotic
# tree shapes for pr_tree_compare, multiple n_tree paths, etc.

mini_phylo <- function(tip_labels) {
  ape::read.tree(text = paste0("(",
                                paste(tip_labels, collapse = ","),
                                ");"))
}


# Cache: corruption / permission edge cases ---------------------------

test_that("cache_get returns NULL on corrupt cache file", {
  td <- tempfile("prtree-edge-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  # Drop a deliberately corrupt .rds file at the expected path
  sub <- file.path(td, "rotl")
  dir.create(sub, recursive = TRUE)
  writeBin(charToRaw("not actually an rds file"),
           file.path(sub, "abc.rds"))

  expect_null(.pr_tree_cache_get("abc", "rotl"))
})


test_that("cache_get returns NULL on missing key", {
  td <- tempfile("prtree-edge-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  expect_null(.pr_tree_cache_get("nonexistent_key", "rotl"))
})


test_that("cache_clear on empty / nonexistent cache returns 0L silently", {
  td <- tempfile("prtree-edge-")
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  options(prepR4pcm.cache_dir = td)   # don't create the dir

  removed <- pr_tree_cache_clear(confirm = FALSE)
  expect_equal(removed, 0L)
})


test_that("cache key is sensitive to taxon", {
  k1 <- .pr_tree_cache_key(c("foo"), source = "rtrees", n_tree = 1L,
                            taxon = "bird")
  k2 <- .pr_tree_cache_key(c("foo"), source = "rtrees", n_tree = 1L,
                            taxon = "fish")
  expect_false(identical(k1, k2))
})


test_that("pr_tree_cache_dir accepts a path that doesn't exist yet", {
  td <- tempfile("prtree-fresh-")
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  expect_false(dir.exists(td))
  pr_tree_cache_dir(td)
  expect_true(dir.exists(td))
})


test_that("pr_tree_cache_dir rejects non-character / multi-element input", {
  expect_error(pr_tree_cache_dir(123), "character vector")
  expect_error(pr_tree_cache_dir(c("a", "b")), "length-1")
})


# Status probe edge cases --------------------------------------------

test_that("pr_get_tree_status with check_network = TRUE returns logical or NA", {
  testthat::local_mocked_bindings(
    .pr_check_backend_reachable = function(source) FALSE,
    .package = "prepR4pcm"
  )
  status <- pr_get_tree_status(check_network = TRUE)
  # reachable should be logical or NA
  expect_true(all(is.logical(status$reachable) | is.na(status$reachable)))
})


# pr_tree_compare exotic tree shapes ---------------------------------

test_that("pr_tree_compare handles trees with no edge lengths", {
  t1 <- mini_phylo(c("a", "b", "c", "d"))
  t1$edge.length <- NULL
  t2 <- mini_phylo(c("a", "b", "c", "d"))
  t2$edge.length <- NULL
  cmp <- pr_tree_compare(t1, t2)
  # branch-length correlation should be NA for at least one off-diagonal
  expect_true(is.na(cmp$pairwise_branch_cor[1, 2]))
})


test_that("pr_tree_compare handles 3+ trees", {
  set.seed(3)
  t1 <- ape::rtree(5)
  t2 <- ape::rtree(5, tip.label = t1$tip.label)
  t3 <- ape::rtree(5, tip.label = t1$tip.label)
  cmp <- pr_tree_compare(t1, t2, t3)
  expect_equal(cmp$n_trees, 3L)
  expect_equal(dim(cmp$pairwise_jaccard), c(3L, 3L))
  expect_equal(dim(cmp$pairwise_rf), c(3L, 3L))
})


test_that("pr_tree_compare handles named arguments", {
  set.seed(4)
  t1 <- ape::rtree(5)
  t2 <- ape::rtree(5, tip.label = t1$tip.label)
  cmp <- pr_tree_compare(rotl = t1, fishtree = t2)
  expect_equal(rownames(cmp$pairwise_jaccard),
               c("rotl", "fishtree"))
  expect_equal(rownames(cmp$pairwise_rf),
               c("rotl", "fishtree"))
})


test_that("pr_tree_compare diagonal Jaccard = 1, RF = 0", {
  t1 <- ape::rtree(6)
  t2 <- ape::rtree(6, tip.label = t1$tip.label)
  cmp <- pr_tree_compare(t1, t2)
  expect_equal(diag(cmp$pairwise_jaccard), c(tree1 = 1, tree2 = 1))
  expect_equal(diag(cmp$pairwise_rf), c(tree1 = 0, tree2 = 0))
})


# n_tree edge cases on the public API --------------------------------

test_that("n_tree is converted to integer (numeric input accepted)", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(tree = mini_phylo(species), in_query = rep(TRUE, length(species)),
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("foo", source = "rotl", n_tree = 5)   # numeric 5
  expect_identical(seen, 5L)                          # plumbed as integer
})


test_that("min_match boundary values 0 and 1 are accepted", {
  testthat::local_mocked_bindings(
    pr_get_tree_status = function(check_network = FALSE) {
      data.frame(
        source = c("rotl"), installed = TRUE, version = "1.0",
        needs_network = FALSE, reachable = NA,
        install_hint = "...", source_repo = "...",
        stringsAsFactors = FALSE
      )
    },
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species), in_query = rep(TRUE, length(species)),
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  expect_no_error(pr_get_tree("foo", source = "auto", min_match = 0))
  expect_no_error(pr_get_tree("foo", source = "auto", min_match = 1))
})


# Provenance shape on dated / multi-tree paths -----------------------

test_that("backend_meta$tree_provenance n_tips is correct for single phylo", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species),
           in_query = rep(TRUE, length(species)),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("a", "b", "c"), source = "rotl")
  prov <- res$backend_meta$tree_provenance
  expect_length(prov, 1L)
  expect_equal(prov[[1]]$n_tips, ape::Ntip(res$tree))
})


# Cache + n_tree integration ------------------------------------------

test_that("cache distinguishes n_tree = 1 from n_tree > 1", {
  td <- tempfile("prtree-edge-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  call_count <- 0L
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      call_count <<- call_count + 1L
      tree <- if (n_tree > 1L) {
        structure(replicate(n_tree, mini_phylo(species),
                              simplify = FALSE),
                   class = "multiPhylo")
      } else {
        mini_phylo(species)
      }
      list(tree = tree, in_query = rep(TRUE, length(species)),
           backend_meta = list(n_returned = n_tree))
    },
    .package = "prepR4pcm"
  )

  pr_get_tree("foo", source = "fishtree", n_tree = 1L,
               cache = TRUE, tnrs = "never")
  expect_equal(call_count, 1L)
  pr_get_tree("foo", source = "fishtree", n_tree = 5L,
               cache = TRUE, tnrs = "never")
  expect_equal(call_count, 2L)
  pr_get_tree("foo", source = "fishtree", n_tree = 1L,
               cache = TRUE, tnrs = "never")
  expect_equal(call_count, 2L)   # n_tree = 1 hit cache from first call
})
