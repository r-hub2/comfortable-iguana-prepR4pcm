test_that("reconcile_apply drops unresolved species", {
  df <- data.frame(
    species = c("Homo sapiens", "Missing species"),
    mass = c(70, 100)
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  aligned <- reconcile_apply(result, data = df, tree = tree,
                              species_col = "species",
                              drop_unresolved = TRUE)

  expect_equal(nrow(aligned$data), 1L)
  expect_equal(aligned$data$species, "Homo sapiens")
  expect_equal(ape::Ntip(aligned$tree), 1L)
})

test_that("reconcile_apply keeps all when drop_unresolved = FALSE", {
  df <- data.frame(
    species = c("Homo sapiens", "Missing species"),
    mass = c(70, 100)
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  aligned <- reconcile_apply(result, data = df, tree = tree,
                              species_col = "species",
                              drop_unresolved = FALSE)

  expect_equal(nrow(aligned$data), 2L)
  expect_equal(ape::Ntip(aligned$tree), 2L)
})

test_that("reconcile_apply works with data only", {
  df <- data.frame(species = c("Homo sapiens"), mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  aligned <- reconcile_apply(result, data = df, species_col = "species",
                              drop_unresolved = TRUE)

  expect_equal(nrow(aligned$data), 1L)
  expect_null(aligned$tree)
})

test_that("reconcile_apply rejects non-reconciliation input", {
  expect_error(
    reconcile_apply(list(), data = data.frame(species = "A b")),
    "reconciliation"
  )
})

test_that("reconcile_apply rejects non-data.frame data", {
  df <- data.frame(species = "Homo sapiens", mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)
  expect_error(
    reconcile_apply(result, data = "not_a_df", species_col = "species"),
    "data.*must be a data frame"
  )
})

test_that("reconcile_apply rejects unknown species_col", {
  df <- data.frame(species = "Homo sapiens", mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1);")
  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  expect_error(
    reconcile_apply(result, data = df, species_col = "bad",
                    drop_unresolved = TRUE),
    "Column .bad. not found"
  )
})

test_that("reconcile_apply works with tree only", {
  df <- data.frame(species = c("Homo sapiens"), mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  aligned <- reconcile_apply(result, tree = tree, drop_unresolved = TRUE)

  expect_null(aligned$data)
  expect_s3_class(aligned$tree, "phylo")
})


# --- M10. reconcile_apply input × drop_unresolved grid ---------------------

test_that("M10 grid: reconcile_apply input shape × drop_unresolved", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes", "Unknown species"),
    mass = c(70, 50, 100),
    stringsAsFactors = FALSE
  )
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  # Apply with data only × drop
  for (drop in c(TRUE, FALSE)) {
    info_d <- sprintf("data_only drop=%s", drop)
    a <- reconcile_apply(rec, data = df, species_col = "species",
                         drop_unresolved = drop)
    expect_null(a$tree, info = info_d)
    expect_s3_class(a$data, "data.frame")
    expected_rows <- if (drop) 2 else 3
    expect_equal(nrow(a$data), expected_rows, info = info_d)
  }

  # Apply with tree only × drop
  for (drop in c(TRUE, FALSE)) {
    info_t <- sprintf("tree_only drop=%s", drop)
    a <- reconcile_apply(rec, tree = tree, drop_unresolved = drop)
    expect_null(a$data, info = info_t)
    expect_s3_class(a$tree, "phylo")
    expected_tips <- if (drop) 2 else 3
    expect_equal(ape::Ntip(a$tree), expected_tips, info = info_t)
  }

  # Apply with both × drop
  for (drop in c(TRUE, FALSE)) {
    info_b <- sprintf("both drop=%s", drop)
    a <- reconcile_apply(rec, data = df, tree = tree,
                         species_col = "species", drop_unresolved = drop)
    expect_s3_class(a$data, "data.frame")
    expect_s3_class(a$tree, "phylo")
    if (drop) {
      expect_equal(nrow(a$data), 2, info = info_b)
      expect_equal(ape::Ntip(a$tree), 2, info = info_b)
    } else {
      expect_equal(nrow(a$data), 3, info = info_b)
      expect_equal(ape::Ntip(a$tree), 3, info = info_b)
    }
  }
})


test_that("M10: reconcile_apply with neither data nor tree returns both NULL", {
  df <- data.frame(species = "Homo sapiens", stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)
  # Documented behaviour: both inputs optional; returning both as NULL is the
  # no-op result of supplying neither.
  out <- reconcile_apply(rec, drop_unresolved = TRUE)
  expect_null(out$data)
  expect_null(out$tree)
})


test_that("M10: reconcile_apply preserves non-species columns in data", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50),
    habitat = c("terrestrial", "arboreal"),
    stringsAsFactors = FALSE
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  a <- reconcile_apply(rec, data = df, species_col = "species",
                       drop_unresolved = TRUE)
  expect_true(all(c("species", "mass", "habitat") %in% names(a$data)))
  expect_equal(a$data$mass, df$mass)
  expect_equal(a$data$habitat, df$habitat)
})
