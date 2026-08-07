# Round 8 tests: V.PhyloMaker / U.PhyloMaker backends, bipartition
# correlation, ultrametric check.
#
# All external-package interactions are mocked so the suite doesn't
# depend on the GitHub-only V.PhyloMaker / U.PhyloMaker / datelife
# packages being installed.

mini_phylo <- function(tip_labels) {
  ape::read.tree(text = paste0("(",
                                paste(tip_labels, collapse = ","),
                                ");"))
}


# 1. reconcile_augment(source = "vphylomaker") --------------------------

test_that("source = 'vphylomaker' dispatches to the helper", {
  tree <- ape::rtree(5, tip.label = c(
    "Quercus_robur", "Quercus_alba",
    "Pinus_sylvestris", "Pinus_taeda", "Acer_rubrum"
  ))
  df <- data.frame(
    species = c(gsub("_", " ", tree$tip.label), "Quercus rubra"),
    trait = rnorm(6),
    stringsAsFactors = FALSE
  )
  rec <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )

  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_augment_vphylomaker = function(species_to_add, tree,
                                         scenarios = "S3",
                                         quiet = FALSE, ...) {
      called <<- TRUE
      list(
        tree = tree,
        augmented = tibble::tibble(
          species = species_to_add,
          genus = "Quercus",
          placed_near = "V.PhyloMaker (S3)",
          branch_length = NA_real_,
          method = "vphylomaker/S3",
          n_congeners = NA_integer_
        ),
        skipped = tibble::tibble(species = character(),
                                  genus = character(),
                                  reason = character()),
        backend_meta = list(backend = "vphylomaker",
                             package = "V.PhyloMaker3",
                             scenarios = "S3")
      )
    },
    .package = "prepR4pcm"
  )
  res <- reconcile_augment(rec, tree, source = "vphylomaker",
                            quiet = TRUE,
                            check_ultrametric = FALSE)
  expect_true(called)
  expect_equal(res$meta$source, "vphylomaker")
  expect_equal(res$meta$backend_meta$scenarios, "S3")
})


test_that("source = 'vphylomaker' errors helpfully when neither V2 nor V1 installed", {
  tree <- ape::rtree(2, tip.label = c("Quercus_robur", "Pinus_sylvestris"))
  df <- data.frame(species = c("Quercus robur", "Pinus sylvestris",
                                "Quercus alba"),
                    stringsAsFactors = FALSE)
  rec <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (package %in% c("V.PhyloMaker2", "V.PhyloMaker")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(
    reconcile_augment(rec, tree, source = "vphylomaker",
                      quiet = TRUE, check_ultrametric = FALSE),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("V.PhyloMaker2", msg, fixed = TRUE))
  expect_true(grepl("V.PhyloMaker", msg, fixed = TRUE))
})


# 2. reconcile_augment(source = "uphylomaker") --------------------------

test_that("source = 'uphylomaker' dispatches to the helper", {
  tree <- ape::rtree(3, tip.label = c("Homo_sapiens", "Pan_troglodytes",
                                       "Gorilla_gorilla"))
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes",
                                "Gorilla gorilla", "Homo erectus"),
                    stringsAsFactors = FALSE)
  rec <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_augment_uphylomaker = function(species_to_add, tree,
                                         gen.list = NULL,
                                         scenario = "S3",
                                         quiet = FALSE, ...) {
      called <<- TRUE
      list(
        tree = tree,
        augmented = tibble::tibble(
          species = species_to_add,
          genus = "Homo",
          placed_near = "U.PhyloMaker (S3)",
          branch_length = NA_real_,
          method = "uphylomaker/S3",
          n_congeners = NA_integer_
        ),
        skipped = tibble::tibble(species = character(),
                                  genus = character(),
                                  reason = character()),
        backend_meta = list(backend = "uphylomaker",
                             scenario = "S3")
      )
    },
    .package = "prepR4pcm"
  )
  res <- reconcile_augment(rec, tree, source = "uphylomaker",
                            quiet = TRUE, check_ultrametric = FALSE)
  expect_true(called)
  expect_equal(res$meta$source, "uphylomaker")
})


test_that("source = 'uphylomaker' errors when U.PhyloMaker is missing", {
  tree <- ape::rtree(2, tip.label = c("a_b", "c_d"))
  df <- data.frame(species = c("a b", "c d", "e f"),
                    stringsAsFactors = FALSE)
  rec <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "U.PhyloMaker")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(
    reconcile_augment(rec, tree, source = "uphylomaker",
                      quiet = TRUE, check_ultrametric = FALSE),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_true(grepl("U.PhyloMaker", conditionMessage(err), fixed = TRUE))
})


# 3. Bipartition-matched branch-length correlation ---------------------

test_that("bipartition correlation: identical trees give cor = 1", {
  set.seed(7)
  t <- ape::rtree(8)
  cmp <- pr_tree_compare(t, t)
  # Diagonal = 1 always. Off-diagonal: identical tree pair = 1.
  expect_equal(cmp$pairwise_branch_cor[1, 2], 1, tolerance = 1e-6)
})


test_that("bipartition correlation: scaled identical trees give cor = 1", {
  set.seed(8)
  t1 <- ape::rtree(8)
  t2 <- t1
  t2$edge.length <- t1$edge.length * 5   # uniform scale
  cmp <- pr_tree_compare(t1, t2)
  expect_equal(cmp$pairwise_branch_cor[1, 2], 1, tolerance = 1e-6)
})


test_that("bipartition correlation: NA when trees lack edge lengths", {
  t1 <- ape::rtree(5)
  t1$edge.length <- NULL
  t2 <- ape::rtree(5, tip.label = t1$tip.label)
  t2$edge.length <- NULL
  cmp <- pr_tree_compare(t1, t2)
  expect_true(is.na(cmp$pairwise_branch_cor[1, 2]))
})


test_that("bipartition correlation: handles disjoint topologies", {
  # Two trees on completely different tips: NA correlation
  t1 <- ape::rtree(5, tip.label = letters[1:5])
  t2 <- ape::rtree(5, tip.label = letters[10:14])
  cmp <- pr_tree_compare(t1, t2)
  expect_true(is.na(cmp$pairwise_branch_cor[1, 2]))
})


# 4. check_ultrametric ------------------------------------------------

test_that("check_ultrametric warns on non-ultrametric backend output", {
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      # Return a non-ultrametric tree
      tr <- mini_phylo(species)
      tr$edge.length <- c(1, 2)  # asymmetric
      list(tree = tr, in_query = rep(TRUE, length(species)),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  expect_warning(
    pr_get_tree(c("a", "b"), source = "fishtree",
                check_ultrametric = TRUE, tnrs = "never"),
    "ultrametric"
  )
})


test_that("check_ultrametric = FALSE suppresses the warning", {
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      tr <- mini_phylo(species)
      tr$edge.length <- c(1, 2)
      list(tree = tr, in_query = rep(TRUE, length(species)),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  expect_no_warning(
    pr_get_tree(c("a", "b"), source = "fishtree",
                check_ultrametric = FALSE, tnrs = "never")
  )
})


test_that("check_ultrametric does NOT warn on rotl (no real branch lengths)", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      tr <- mini_phylo(species)
      tr$edge.length <- NULL
      list(tree = tr, in_query = rep(TRUE, length(species)),
           backend_meta = list())
    },
    .package = "prepR4pcm"
  )
  expect_no_warning(
    pr_get_tree(c("a", "b"), source = "rotl",
                check_ultrametric = TRUE)
  )
})


test_that("check_ultrametric in reconcile_augment skips branch_length = 'zero'", {
  tree <- ape::rtree(3, tip.label = c("Parus_major", "Parus_minor",
                                       "Corvus_corax"))
  df <- data.frame(species = c("Parus major", "Parus minor",
                                "Corvus corax", "Parus ater"),
                    stringsAsFactors = FALSE)
  rec <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
  # branch_length = "zero" produces non-ultrametric by design; should NOT warn
  expect_no_warning(
    suppressMessages(reconcile_augment(rec, tree,
                                        branch_length = "zero",
                                        seed = 42, quiet = TRUE,
                                        check_ultrametric = TRUE))
  )
})


# 5. .pr_is_tree_ultrametric -------------------------------------------

test_that(".pr_is_tree_ultrametric returns NA when no edge lengths", {
  t <- ape::rtree(4)
  t$edge.length <- NULL
  expect_true(is.na(.pr_is_tree_ultrametric(t)))
})


test_that(".pr_is_tree_ultrametric handles multiPhylo (all ultrametric)", {
  t1 <- ape::rcoal(5)
  t2 <- ape::rcoal(5, tip.label = t1$tip.label)
  mp <- structure(list(t1, t2), class = "multiPhylo")
  expect_true(.pr_is_tree_ultrametric(mp))
})


test_that(".pr_is_tree_ultrametric handles multiPhylo (one non-ultrametric)", {
  t1 <- ape::rcoal(5)              # ultrametric
  t2 <- ape::rtree(5, tip.label = t1$tip.label)  # NOT ultrametric
  mp <- structure(list(t1, t2), class = "multiPhylo")
  expect_false(.pr_is_tree_ultrametric(mp))
})
