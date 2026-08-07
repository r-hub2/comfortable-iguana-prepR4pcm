# Round 7 integration tests
# ==========================
#
# Exercise the full prepR4pcm tree-handling pipeline end-to-end with
# mocked backends. The point is to catch breakage that would only
# surface when functions chain together (a unit test for one piece
# wouldn't catch a contract violation when piece A feeds piece B).

mini_phylo <- function(tip_labels) {
  ape::read.tree(text = paste0("(",
                                paste(tip_labels, collapse = ","),
                                ");"))
}


test_that("reconcile_data -> pr_get_tree -> pr_cite_tree pipeline works", {
  # 1. reconcile_data on a small dataset
  rec <- reconcile_data(
    data.frame(species = c("Homo sapiens", "Pan troglodytes")),
    data.frame(species = c("Homo sapiens", "Pan troglodytes")),
    x_species = "species", y_species = "species",
    authority = NULL, quiet = TRUE
  )
  # 2. pr_get_tree on the reconciliation
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species),
           in_query = rep(TRUE, length(species)),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(rec, source = "rotl")
  expect_s3_class(res, "pr_tree_result")
  expect_true(!is.null(res$backend_meta$tree_provenance))

  # 3. pr_cite_tree on the result
  out <- capture.output(invisible(pr_cite_tree(res, format = "text")))
  out <- paste(out, collapse = "\n")
  expect_true(grepl("rotl", out))
  expect_true(grepl("Open Tree", out))
})


test_that("multi-tree pipeline preserves per-tree provenance end to end", {
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      mp <- structure(
        replicate(3, mini_phylo(species), simplify = FALSE),
        class = "multiPhylo"
      )
      list(tree = mp,
           in_query = rep(TRUE, length(species)),
           backend_meta = list(reference = "Rabosky 2018"))
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("Salmo salar", "Esox lucius"),
                     source = "fishtree", n_tree = 3, tnrs = "never")
  expect_s3_class(res$tree, "multiPhylo")
  expect_length(res$backend_meta$tree_provenance, 3L)

  # pr_cite_tree should expose all 3 per-tree citations
  out <- capture.output(invisible(pr_cite_tree(res, format = "text")))
  out <- paste(out, collapse = "\n")
  expect_true(grepl("Per-tree", out))
})


test_that("pr_tree_compare on two pr_tree_result inputs works end-to-end", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species), in_query = rep(TRUE, length(species)),
           unmatched = character(), backend_meta = list())
    },
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species), in_query = rep(TRUE, length(species)),
           unmatched = character(), backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  # tnrs = "never": prevents the preflight from rewriting the toy
  # species list against the live OToL server when rotl is installed
  # (CI Linux but not the bare-bones macOS dev box). Without this,
  # r1's tree has tips "a","b","c","d" but r2's tree has tips like
  # "Anolis...","Beta..." (rotl's TNRS partial-matches) and the
  # intersection is empty.
  r1 <- pr_get_tree(c("a", "b", "c", "d"), source = "rotl",
                    tnrs = "never")
  r2 <- pr_get_tree(c("a", "b", "c", "d"), source = "fishtree",
                    tnrs = "never")
  cmp <- pr_tree_compare(rotl = r1, fishtree = r2)
  expect_s3_class(cmp, "pr_tree_compare")
  expect_equal(cmp$n_shared, 4L)
})


test_that("cache survives a save/load cycle", {
  td <- tempfile("prtree-int-")
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  prev <- getOption("prepR4pcm.cache_dir", default = NULL)
  on.exit(options(prepR4pcm.cache_dir = prev), add = TRUE)
  pr_tree_cache_dir(td)

  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species), in_query = rep(TRUE, length(species)),
           backend_meta = list(stamp = Sys.time()))
    },
    .package = "prepR4pcm"
  )
  res1 <- pr_get_tree("foo", source = "rotl", cache = TRUE)

  # Open a new "session" by re-reading the cache directly
  status <- pr_tree_cache_status()
  expect_equal(nrow(status), 1L)
  expect_equal(status$source, "rotl")
})


test_that("auto dispatcher records its attempts in backend_meta", {
  testthat::local_mocked_bindings(
    pr_get_tree_status = function(check_network = FALSE) {
      data.frame(
        source = c("rotl", "fishtree"),
        installed = TRUE, version = "1.0",
        needs_network = c(TRUE, TRUE), reachable = NA,
        install_hint = "...", source_repo = "...",
        stringsAsFactors = FALSE
      )
    },
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species[1]),
           in_query = seq_along(species) == 1L, backend_meta = list())
    },
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      list(tree = mini_phylo(species),
           in_query = rep(TRUE, length(species)),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  # tnrs = "never" same reason as above (Linux-CI rotl preflight)
  res <- pr_get_tree(c("a", "b"), source = "auto", min_match = 1.0,
                     tnrs = "never")
  expect_equal(res$source, "fishtree")
  expect_true(!is.null(res$backend_meta$auto_attempts))
  expect_true("rotl" %in% names(res$backend_meta$auto_attempts))
  expect_equal(res$backend_meta$auto_chose, "fishtree")
})
