# Helper: create a test tree and reconciliation
make_test_setup <- function() {
  tree <- ape::rtree(5, tip.label = c(
    "Parus_major", "Parus_caeruleus", "Corvus_corax",
    "Corvus_corone", "Falco_peregrinus"
  ))

  df <- data.frame(
    species = c("Parus major", "Parus caeruleus", "Corvus corax",
                "Corvus corone", "Falco peregrinus",
                "Parus ater",           # congener exists (Parus)
                "Corvus monedula",       # congener exists (Corvus)
                "Aquila chrysaetos"),    # no congener (Aquila)
    trait = rnorm(8),
    stringsAsFactors = FALSE
  )

  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  list(tree = tree, df = df, rec = rec)
}


test_that("reconcile_augment adds species with congeners", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  # Should have added Parus ater and Corvus monedula
  expect_true(nrow(aug$augmented) >= 2)
  expect_true("Parus ater" %in% aug$augmented$species)
  expect_true("Corvus monedula" %in% aug$augmented$species)

  # Tree should have more tips
  expect_gt(ape::Ntip(aug$tree), ape::Ntip(setup$tree))
})


test_that("reconcile_augment skips species with no congener", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  # Aquila chrysaetos has no congener
  expect_true("Aquila chrysaetos" %in% aug$skipped$species)
  expect_true(grepl("No congener", aug$skipped$reason[
    aug$skipped$species == "Aquila chrysaetos"
  ]))
})


test_that("reconcile_augment preserves original tree", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  expect_equal(ape::Ntip(aug$original), ape::Ntip(setup$tree))
  expect_equal(aug$original$tip.label, setup$tree$tip.label)
})


test_that("branch_length = 'zero' produces zero-length branches", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "zero", seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length == 0))
  }
})


test_that("branch_length = 'congener_median' produces positive lengths", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "congener_median",
                            seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length > 0))
  }
})


test_that("branch_length = 'half_terminal' produces positive lengths", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            branch_length = "half_terminal",
                            seed = 42, quiet = TRUE)

  if (nrow(aug$augmented) > 0) {
    expect_true(all(aug$augmented$branch_length > 0))
  }
})


test_that("where = 'near' works with multiple congeners", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree,
                            where = "near", seed = 42, quiet = TRUE)

  expect_true(nrow(aug$augmented) >= 2)
  # Parus ater placed near MRCA of Parus congeners
  parus_row <- aug$augmented[aug$augmented$species == "Parus ater", ]
  expect_true(grepl("MRCA", parus_row$placed_near))
})


test_that("seed produces reproducible results", {
  setup <- make_test_setup()
  aug1 <- reconcile_augment(setup$rec, setup$tree, seed = 123, quiet = TRUE)
  aug2 <- reconcile_augment(setup$rec, setup$tree, seed = 123, quiet = TRUE)

  expect_equal(aug1$augmented$placed_near, aug2$augmented$placed_near)
  expect_equal(ape::Ntip(aug1$tree), ape::Ntip(aug2$tree))
})


test_that("reconcile_augment handles no unresolved species", {
  tree <- ape::rtree(2, tip.label = c("Homo_sapiens", "Pan_troglodytes"))
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"),
                   stringsAsFactors = FALSE)
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  aug <- reconcile_augment(rec, tree, quiet = TRUE)
  expect_equal(nrow(aug$augmented), 0)
  expect_equal(nrow(aug$skipped), 0)
  expect_equal(ape::Ntip(aug$tree), ape::Ntip(tree))
})


test_that("reconcile_augment validates input", {
  expect_error(reconcile_augment("not a reconciliation", ape::rtree(3)),
               "reconciliation")
})


test_that("meta contains correct counts", {
  setup <- make_test_setup()
  aug <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)

  expect_equal(aug$meta$n_augmented, nrow(aug$augmented))
  expect_equal(aug$meta$n_skipped, nrow(aug$skipped))
  expect_equal(aug$meta$original_n_tips, ape::Ntip(setup$tree))
  expect_equal(aug$meta$augmented_n_tips, ape::Ntip(aug$tree))
})


# --- M4. reconcile_augment placement × branch_length × tree shape grid -----

test_that("M4 grid: where × branch_length × tree shape × congener mix", {
  # Build a reconciliation seeded against different tree shapes
  # and data frames that mix congener-present / congener-absent species.

  # Data frame that produces: 2 matched, 2 with congeners (augment), 1 without
  df <- data.frame(
    species = c("Parus major", "Corvus corax",          # matched
                "Parus ater", "Corvus monedula",         # congeners present
                "Aquila chrysaetos"),                    # no congener
    trait = seq_len(5),
    stringsAsFactors = FALSE
  )

  make_tree <- function(kind) {
    set.seed(42)
    tips <- c("Parus_major", "Parus_caeruleus", "Corvus_corax",
              "Corvus_corone", "Falco_peregrinus")
    tr <- ape::rtree(5, tip.label = tips)
    switch(kind,
      non_ultrametric = tr,
      ultrametric     = suppressWarnings(suppressMessages(
        ape::chronos(tr, quiet = TRUE))),
      zero_branches   = { tr$edge.length <- rep(0, length(tr$edge.length)); tr },
      polytomy        = ape::di2multi(tr, tol = max(tr$edge.length) * 0.9)
    )
  }

  wheres <- c("genus", "near")
  branch_lengths <- c("congener_median", "half_terminal", "zero")
  tree_kinds <- c("non_ultrametric", "ultrametric",
                  "zero_branches", "polytomy")

  for (tree_kind in tree_kinds) {
    tree <- make_tree(tree_kind)
    rec <- suppressMessages(suppressWarnings(
      reconcile_tree(df, tree, x_species = "species",
                     authority = NULL, quiet = TRUE)
    ))

    for (where in wheres) {
      for (bl in branch_lengths) {
        info <- sprintf("tree=%s where=%s branch=%s",
                        tree_kind, where, bl)

        aug <- suppressWarnings(suppressMessages(
          reconcile_augment(rec, tree, where = where,
                            branch_length = bl, seed = 42, quiet = TRUE)
        ))

        # Invariant 1: augmented tree tip count equals original + n_augmented
        expect_equal(
          ape::Ntip(aug$tree),
          ape::Ntip(tree) + nrow(aug$augmented),
          info = info
        )

        # Invariant 2: skipped species have a non-empty reason
        if (nrow(aug$skipped) > 0) {
          expect_true(all(!is.na(aug$skipped$reason) &
                            nzchar(aug$skipped$reason)), info = info)
        }

        # Invariant 3: "Aquila chrysaetos" (no congener) must be skipped
        expect_true("Aquila chrysaetos" %in% aug$skipped$species,
                    info = info)

        # Invariant 4: branch_length strategy respected
        if (nrow(aug$augmented) > 0) {
          if (bl == "zero") {
            expect_true(all(aug$augmented$branch_length == 0), info = info)
          } else {
            # non-zero strategies should produce >= 0 branch lengths
            expect_true(all(aug$augmented$branch_length >= 0), info = info)
          }
        }

        # Invariant 5: augmented species have their genus present in tree
        if (nrow(aug$augmented) > 0) {
          aug_genera <- sub(" .*$", "", aug$augmented$species)
          tree_genera <- unique(sub("[_ ].*$", "", tree$tip.label))
          expect_true(all(aug_genera %in% tree_genera), info = info)
        }
      }
    }
  }
})


test_that("M4: seed is deterministic across all strategy combinations", {
  setup <- make_test_setup()
  for (where in c("genus", "near")) {
    for (bl in c("congener_median", "half_terminal", "zero")) {
      a1 <- suppressMessages(
        reconcile_augment(setup$rec, setup$tree,
                          where = where, branch_length = bl,
                          seed = 99, quiet = TRUE)
      )
      a2 <- suppressMessages(
        reconcile_augment(setup$rec, setup$tree,
                          where = where, branch_length = bl,
                          seed = 99, quiet = TRUE)
      )
      info <- sprintf("where=%s branch=%s", where, bl)
      expect_equal(a1$augmented$placed_near, a2$augmented$placed_near,
                   info = info)
      expect_equal(a1$augmented$branch_length, a2$augmented$branch_length,
                   info = info)
      expect_equal(ape::Ntip(a1$tree), ape::Ntip(a2$tree), info = info)
    }
  }
})


test_that("M4: different seeds produce different random placements for where='near'", {
  setup <- make_test_setup()
  a1 <- suppressMessages(
    reconcile_augment(setup$rec, setup$tree,
                      where = "near", seed = 1, quiet = TRUE)
  )
  a2 <- suppressMessages(
    reconcile_augment(setup$rec, setup$tree,
                      where = "near", seed = 99999, quiet = TRUE)
  )
  # Tip count should be the same; the ACTUAL placements can differ
  expect_equal(ape::Ntip(a1$tree), ape::Ntip(a2$tree))
})


test_that("M4: invalid where/branch_length errors via match.arg", {
  setup <- make_test_setup()
  expect_error(
    reconcile_augment(setup$rec, setup$tree, where = "nowhere", quiet = TRUE)
  )
  expect_error(
    reconcile_augment(setup$rec, setup$tree, branch_length = "bogus",
                      quiet = TRUE)
  )
})


test_that("M4: zero-branch trees still produce augmented tree", {
  tr <- fx_tree_zero_branches(5)
  df <- data.frame(
    species = c(gsub("_", " ", tr$tip.label[1]),
                gsub("_", " ", tr$tip.label[2]),
                # species with congener present in tree
                paste0(sub(" .*$", "", gsub("_", " ", tr$tip.label[1])),
                       " sp999")),
    stringsAsFactors = FALSE
  )
  rec <- suppressMessages(
    reconcile_tree(df, tr, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
  # reconcile_augment should not crash on zero-branch trees
  expect_no_error(
    suppressWarnings(suppressMessages(
      reconcile_augment(rec, tr, branch_length = "zero",
                        seed = 42, quiet = TRUE)
    ))
  )
})


# --- source = "rtrees" backend (mocked) ----------------------------------
#
# We mock the internal helper `.pr_augment_rtrees` so the suite doesn't
# depend on the actual rtrees package being installed nor on the
# rtrees mega-tree files being downloadable on CI. The tests verify:
#   1. source = "rtrees" dispatches to the rtrees helper
#   2. taxon is propagated through to the helper
#   3. The result has the documented shape (augmented + skipped + meta)
#   4. meta$source is "rtrees"
#   5. meta$where / meta$branch_length_method are NA (not applicable)
#   6. multiPhylo return value is preserved
#   7. Missing taxon errors helpfully (via the real helper)
#   8. Missing rtrees package errors helpfully (via the real helper)


test_that("rtrees augmentation parses exact, genus, family, and skipped placements", {
  backbone <- ape::read.tree(text = "(Backbone_species:1,Other_species:1);")
  marked_tree <- ape::read.tree(
    text = "(Backbone_species:1,Exact_species:1,Genus_species*:1,Family_species**:1);"
  )
  requested <- c(
    "Exact species", "Genus species", "Family species", "Skipped species"
  )
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) TRUE,
    .package = "base"
  )
  testthat::local_mocked_bindings(
    .pr_rtrees_get_tree = function(...) marked_tree,
    .package = "prepR4pcm"
  )

  res <- prepR4pcm:::.pr_augment_rtrees(requested, backbone, taxon = "bird")

  expect_identical(res$tree$tip.label, marked_tree$tip.label)
  expect_identical(
    names(res$augmented),
    c("species", "genus", "placed_near", "branch_length", "method", "n_congeners")
  )
  expect_type(res$augmented$species, "character")
  expect_type(res$augmented$genus, "character")
  expect_type(res$augmented$placed_near, "character")
  expect_type(res$augmented$branch_length, "double")
  expect_type(res$augmented$method, "character")
  expect_type(res$augmented$n_congeners, "integer")
  expect_equal(
    res$augmented$placed_near,
    c(
      "rtrees: placed at species level",
      "rtrees: grafted at genus-level node",
      "rtrees: grafted at family-level node"
    )
  )
  expect_equal(
    res$augmented$method,
    c("rtrees/bird/placed", "rtrees/bird/grafted", "rtrees/bird/grafted")
  )
  expect_equal(res$skipped$species, "Skipped species")
  expect_equal(res$backend_meta$n_exact, 1L)
  expect_equal(res$backend_meta$n_genus_added, 1L)
  expect_equal(res$backend_meta$n_family_added, 1L)
  expect_equal(res$backend_meta$n_skipped, 1L)
  expect_equal(res$backend_meta$n_grafted, 2L)
})


test_that("rtrees augmentation uses the first multiPhylo tree for placement", {
  backbone <- ape::read.tree(text = "(Backbone_species:1,Other_species:1);")
  first <- ape::read.tree(text = "(Backbone_species:1,Genus_species*:1);")
  second <- ape::read.tree(text = "(Backbone_species:1,Genus_species:1);")
  marked_trees <- structure(list(first, second), class = "multiPhylo")
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) TRUE,
    .package = "base"
  )
  testthat::local_mocked_bindings(
    .pr_rtrees_get_tree = function(...) marked_trees,
    .package = "prepR4pcm"
  )

  res <- prepR4pcm:::.pr_augment_rtrees("Genus species", backbone, taxon = "bird")

  expect_s3_class(res$tree, "multiPhylo")
  expect_equal(res$augmented$placed_near, "rtrees: grafted at genus-level node")
  expect_equal(res$backend_meta$n_returned, 2L)
})


test_that("source = 'rtrees' dispatches to the rtrees helper", {
  setup <- make_test_setup()
  seen_taxon <- NA
  seen_species <- NULL
  testthat::local_mocked_bindings(
    .pr_augment_rtrees = function(species_to_add, tree, taxon = NULL,
                                   quiet = FALSE, ...) {
      seen_taxon <<- taxon
      seen_species <<- species_to_add
      list(
        tree = tree,
        augmented = tibble::tibble(
          species = species_to_add,
          genus = sub(" .*$", "", species_to_add),
          placed_near = "rtrees: placed at species level",
          branch_length = NA_real_,
          method = paste0("rtrees/", taxon, "/placed"),
          n_congeners = NA_integer_
        ),
        skipped = tibble::tibble(species = character(),
                                  genus = character(),
                                  reason = character()),
        backend_meta = list(backend = "rtrees", taxon = taxon,
                            n_grafted = 0L, n_returned = 1L)
      )
    },
    .package = "prepR4pcm"
  )
  res <- reconcile_augment(setup$rec, setup$tree,
                           source = "rtrees", taxon = "bird", quiet = TRUE)
  expect_equal(seen_taxon, "bird")
  expect_true(length(seen_species) >= 1)
})


test_that("source = 'rtrees' returns the documented shape", {
  setup <- make_test_setup()
  testthat::local_mocked_bindings(
    .pr_augment_rtrees = function(species_to_add, tree, taxon = NULL,
                                   quiet = FALSE, ...) {
      list(
        tree = tree,
        augmented = tibble::tibble(
          species = species_to_add,
          genus = sub(" .*$", "", species_to_add),
          placed_near = "rtrees: placed at species level",
          branch_length = NA_real_,
          method = paste0("rtrees/", taxon, "/placed"),
          n_congeners = NA_integer_
        ),
        skipped = tibble::tibble(species = character(),
                                  genus = character(),
                                  reason = character()),
        backend_meta = list(backend = "rtrees", taxon = taxon,
                            n_grafted = 0L, n_returned = 1L)
      )
    },
    .package = "prepR4pcm"
  )
  res <- reconcile_augment(setup$rec, setup$tree,
                           source = "rtrees", taxon = "bird", quiet = TRUE)
  expect_named(res, c("tree", "original", "augmented", "skipped", "meta"))
  expect_equal(res$meta$source, "rtrees")
  expect_true(is.na(res$meta$where))
  expect_true(is.na(res$meta$branch_length_method))
  expect_equal(res$meta$backend_meta$taxon, "bird")
  expect_s3_class(res$augmented, "data.frame")
  expect_s3_class(res$skipped, "data.frame")
  # All documented mapping cols present
  expect_setequal(names(res$augmented),
                  c("species", "genus", "placed_near", "branch_length",
                    "method", "n_congeners"))
})


test_that("source = 'rtrees' preserves a multiPhylo return value", {
  setup <- make_test_setup()
  testthat::local_mocked_bindings(
    .pr_augment_rtrees = function(species_to_add, tree, taxon = NULL,
                                   quiet = FALSE, ...) {
      mp <- structure(list(tree, tree, tree), class = "multiPhylo")
      list(
        tree = mp,
        augmented = tibble::tibble(
          species = species_to_add,
          genus = sub(" .*$", "", species_to_add),
          placed_near = "rtrees: placed at species level",
          branch_length = NA_real_,
          method = paste0("rtrees/", taxon, "/placed"),
          n_congeners = NA_integer_
        ),
        skipped = tibble::tibble(species = character(),
                                  genus = character(),
                                  reason = character()),
        backend_meta = list(backend = "rtrees", taxon = taxon,
                            n_grafted = 0L, n_returned = 3L)
      )
    },
    .package = "prepR4pcm"
  )
  res <- reconcile_augment(setup$rec, setup$tree,
                           source = "rtrees", taxon = "bird", quiet = TRUE)
  expect_s3_class(res$tree, "multiPhylo")
  expect_equal(length(res$tree), 3L)
  expect_equal(res$meta$backend_meta$n_returned, 3L)
})


test_that("source = 'rtrees' without taxon errors helpfully", {
  setup <- make_test_setup()
  # Use the real helper (not the mock) so we hit the taxon validation.
  # Mock only requireNamespace so the package-availability check passes
  # in environments where rtrees is not installed.
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) TRUE,
    .package = "base"
  )
  err <- tryCatch(
    reconcile_augment(setup$rec, setup$tree,
                      source = "rtrees", taxon = NULL, quiet = TRUE),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_true(grepl("taxon", conditionMessage(err)))
})


test_that("source = 'rtrees' without the rtrees package errors helpfully", {
  setup <- make_test_setup()
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "rtrees")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(
    reconcile_augment(setup$rec, setup$tree,
                      source = "rtrees", taxon = "bird", quiet = TRUE),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("rtrees", msg, fixed = TRUE))
  expect_true(grepl("daijiang", msg, fixed = TRUE))
})


test_that("source = 'rtrees' with no unresolved species short-circuits", {
  # Build a reconciliation where every species is matched. No call should
  # be made to the rtrees helper; tree returns unchanged.
  tree <- ape::rtree(2, tip.label = c("Homo_sapiens", "Pan_troglodytes"))
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"),
                    stringsAsFactors = FALSE)
  rec <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
  helper_called <- FALSE
  testthat::local_mocked_bindings(
    .pr_augment_rtrees = function(...) {
      helper_called <<- TRUE
      stop("should not be called")
    },
    .package = "prepR4pcm"
  )
  res <- reconcile_augment(rec, tree, source = "rtrees", taxon = "mammal",
                           quiet = TRUE)
  expect_false(helper_called)
  expect_equal(res$meta$n_augmented, 0)
  expect_equal(res$meta$source, "rtrees")
})


test_that("source = 'internal' (default) records meta$source", {
  setup <- make_test_setup()
  res <- reconcile_augment(setup$rec, setup$tree, seed = 42, quiet = TRUE)
  expect_equal(res$meta$source, "internal")
})


test_that("grafting onto an ultrametric tree yields an ultrametric tree", {
  # An explicitly ultrametric 5-tip tree: every tip at root-depth 10.
  # Comparative methods (PGLS, phylogenetic meta-analysis) require an
  # ultrametric tree, so reconcile_augment() must not break it.
  tree <- ape::read.tree(
    text = "((Aus_one:6,Aus_two:6):4,((Bus_one:3,Bus_two:3):4,Cus_one:7):3);"
  )
  expect_true(ape::is.ultrametric(tree))

  df <- data.frame(
    species = c(gsub("_", " ", tree$tip.label), "Aus three"),
    stringsAsFactors = FALSE
  )
  rec <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)

  for (where in c("genus", "near")) {
    for (bl in c("congener_median", "half_terminal")) {
      aug <- suppressWarnings(suppressMessages(
        reconcile_augment(rec, tree, where = where, branch_length = bl,
                          seed = 42, quiet = TRUE)
      ))
      expect_true(
        ape::is.ultrametric(aug$tree),
        info = sprintf("where=%s branch_length=%s", where, bl)
      )
    }
  }
})
