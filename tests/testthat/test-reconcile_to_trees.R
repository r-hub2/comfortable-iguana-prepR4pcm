test_that("reconcile_to_trees returns named list of reconciliations", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes", "Gorilla gorilla"))
  tree1 <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")
  tree2 <- ape::read.tree(text = "(Homo_sapiens:1,Pongo_pygmaeus:1);")

  results <- reconcile_to_trees(
    df,
    trees = list(tree_a = tree1, tree_b = tree2),
    x_species = "species",
    authority = NULL,
    quiet = TRUE
  )

  expect_type(results, "list")
  expect_equal(names(results), c("tree_a", "tree_b"))
  expect_s3_class(results$tree_a, "reconciliation")
  expect_s3_class(results$tree_b, "reconciliation")

  # tree_a should match all 3 species
  expect_equal(
    sum(results$tree_a$mapping$in_x & results$tree_a$mapping$in_y, na.rm = TRUE),
    3L
  )

  # tree_b should match only Homo sapiens
  expect_equal(
    sum(results$tree_b$mapping$in_x & results$tree_b$mapping$in_y, na.rm = TRUE),
    1L
  )
})

test_that("reconcile_to_trees assigns default names", {
  df <- data.frame(species = "Homo sapiens")
  tree1 <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  results <- reconcile_to_trees(
    df,
    trees = list(tree1),
    x_species = "species",
    authority = NULL,
    quiet = TRUE
  )

  expect_equal(names(results), "tree_1")
})

test_that("reconcile_to_trees errors on invalid input", {
  expect_error(
    reconcile_to_trees("not a df", list(ape::rtree(3)),
                        authority = NULL),
    "must be a data frame"
  )

  df <- data.frame(species = "A sp")
  expect_error(
    reconcile_to_trees(df, list(), authority = NULL),
    "non-empty"
  )
})


# --- M8. reconcile_to_trees multi-tree grid ---------------------------------

test_that("M8 grid: n_trees × overlap × tree_sizes", {
  # Shared data frame
  df <- data.frame(
    species = c("Parus major", "Corvus corax", "Turdus merula",
                "Falco peregrinus"),
    stringsAsFactors = FALSE
  )

  # Helpers to build trees with known overlap patterns
  make_tree_full <- function() {
    # Contains all 4 df species
    ape::read.tree(text = "(((Parus_major:1,Corvus_corax:1):1,Turdus_merula:1):1,Falco_peregrinus:1);")
  }
  make_tree_partial <- function(extra_tip = "Extra_sp") {
    # Contains 2 of 4 df species
    ape::read.tree(text = sprintf(
      "((Parus_major:1,Corvus_corax:1):1,%s:1);", extra_tip
    ))
  }
  make_tree_none <- function() {
    ape::read.tree(text = "((Alpha_beta:1,Gamma_delta:1):1,Epsilon_zeta:1);")
  }

  for (n_trees in c(1, 3, 10)) {
    for (overlap in c("full", "partial", "none")) {
      trees <- lapply(seq_len(n_trees), function(i) {
        switch(overlap,
          full    = make_tree_full(),
          partial = make_tree_partial(extra_tip = paste0("Extra_", i)),
          none    = make_tree_none()
        )
      })
      names(trees) <- paste0("t", seq_len(n_trees))

      info <- sprintf("n_trees=%d overlap=%s", n_trees, overlap)

      results <- suppressMessages(
        reconcile_to_trees(df, trees = trees,
                           x_species = "species",
                           authority = NULL, quiet = TRUE)
      )

      # Invariant 1: exactly one reconciliation per tree, named correctly
      expect_equal(length(results), n_trees, info = info)
      expect_equal(names(results), names(trees), info = info)

      # Invariant 2: every result is a reconciliation
      for (r in results) {
        expect_true(inherits(r, "reconciliation"), info = info)
      }

      # Invariant 3: name_x consistent across reconciliations
      name_xs <- lapply(results, function(r) {
        sort(stats::na.omit(unique(r$mapping$name_x)))
      })
      if (length(name_xs) > 1) {
        ref <- name_xs[[1]]
        for (i in seq_along(name_xs)[-1]) {
          expect_equal(name_xs[[i]], ref, info = info)
        }
      }

      # Invariant 4: matched counts respect overlap pattern
      matched_counts <- sapply(results, function(r) {
        sum(r$mapping$in_x & r$mapping$in_y, na.rm = TRUE)
      })
      switch(overlap,
        full    = expect_true(all(matched_counts == 4), info = info),
        partial = expect_true(all(matched_counts == 2), info = info),
        none    = expect_true(all(matched_counts == 0), info = info)
      )
    }
  }
})


test_that("M8: reconcile_to_trees accepts multiPhylo directly", {
  df <- data.frame(
    species = c("Parus major", "Corvus corax"),
    stringsAsFactors = FALSE
  )
  trees <- fx_tree_multiphylo(k = 3, n = 5)
  results <- suppressMessages(
    reconcile_to_trees(df, trees, x_species = "species",
                       authority = NULL, quiet = TRUE)
  )
  expect_equal(length(results), 3)
  for (r in results) expect_s3_class(r, "reconciliation")
})


test_that("M8: aggregated coverage across trees >= any single tree", {
  df <- data.frame(
    species = c("Parus major", "Corvus corax", "Turdus merula",
                "Falco peregrinus"),
    stringsAsFactors = FALSE
  )
  # Two disjoint partial trees — each covers 2 species, together cover 4
  tree_a <- ape::read.tree(text = "(Parus_major:1,Corvus_corax:1);")
  tree_b <- ape::read.tree(text = "(Turdus_merula:1,Falco_peregrinus:1);")
  results <- suppressMessages(
    reconcile_to_trees(df, list(a = tree_a, b = tree_b),
                       x_species = "species",
                       authority = NULL, quiet = TRUE)
  )
  # Compute union of matched species names across trees
  matched_union <- unique(unlist(lapply(results, function(r) {
    m <- r$mapping[r$mapping$in_x & r$mapping$in_y, ]
    m$name_x
  })))
  # Should be all 4
  expect_equal(length(matched_union), 4)
  # Individual tree max coverage is 2
  ind <- sapply(results, function(r) {
    sum(r$mapping$in_x & r$mapping$in_y, na.rm = TRUE)
  })
  expect_true(length(matched_union) >= max(ind))
})
