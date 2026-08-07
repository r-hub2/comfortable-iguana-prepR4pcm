test_that("reconcile_export writes data, tree, and mapping files", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  out_dir <- tempfile("export_test")
  on.exit(unlink(out_dir, recursive = TRUE))

  paths <- reconcile_export(result, data = df, tree = tree,
                             species_col = "species",
                             dir = out_dir, prefix = "test")

  expect_true(file.exists(paths$data))
  expect_true(file.exists(paths$tree))
  expect_true(file.exists(paths$mapping))

  # Check data is readable
  exported_data <- read.csv(paths$data)
  expect_true("species" %in% names(exported_data))

  # Check tree is readable
  exported_tree <- ape::read.nexus(paths$tree)
  expect_s3_class(exported_tree, "phylo")

  # Check mapping is readable
  exported_mapping <- read.csv(paths$mapping)
  expect_true("match_type" %in% names(exported_mapping))
})

test_that("reconcile_export handles newick format", {
  df <- data.frame(species = "Homo sapiens", mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  out_dir <- tempfile("newick_test")
  on.exit(unlink(out_dir, recursive = TRUE))

  paths <- reconcile_export(result, data = df, tree = tree,
                             species_col = "species",
                             dir = out_dir, tree_format = "newick")

  expect_true(grepl("\\.nwk$", paths$tree))
  expect_true(file.exists(paths$tree))
})

test_that("reconcile_export works with data only", {
  df <- data.frame(species = "Homo sapiens", mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  out_dir <- tempfile("data_only_test")
  on.exit(unlink(out_dir, recursive = TRUE))

  paths <- reconcile_export(result, data = df, species_col = "species",
                             dir = out_dir)

  expect_true(file.exists(paths$data))
  expect_null(paths$tree)
  expect_true(file.exists(paths$mapping))
})

test_that("reconcile_export default output directory is temporary", {
  df <- data.frame(species = "Homo sapiens", mass = 70)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  paths <- reconcile_export(result, data = df, species_col = "species")
  on.exit(unlink(dirname(paths$mapping), recursive = TRUE), add = TRUE)

  temp_root <- normalizePath(tempdir(), mustWork = TRUE)
  export_dir <- normalizePath(dirname(paths$mapping), mustWork = TRUE)

  expect_true(startsWith(export_dir, temp_root))
  expect_true(file.exists(paths$data))
  expect_true(file.exists(paths$mapping))
  expect_null(paths$tree)
})


# --- M10. reconcile_export format × drop_unresolved grid --------------------

test_that("M10 grid: reconcile_export format × drop_unresolved", {
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

  for (fmt in c("nexus", "newick")) {
    for (drop in c(TRUE, FALSE)) {
      info <- sprintf("format=%s drop=%s", fmt, drop)
      out_dir <- tempfile("m10_export")
      on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

      paths <- reconcile_export(rec, data = df, tree = tree,
                                species_col = "species",
                                dir = out_dir, tree_format = fmt,
                                drop_unresolved = drop)

      expect_true(file.exists(paths$data), info = info)
      expect_true(file.exists(paths$tree), info = info)
      expect_true(file.exists(paths$mapping), info = info)

      if (fmt == "newick") {
        expect_true(grepl("\\.nwk$", paths$tree), info = info)
      } else {
        expect_true(grepl("\\.(nex|nexus)$", paths$tree), info = info)
      }

      # Round-trip the exported data
      exported_data <- read.csv(paths$data)
      expected_n <- if (drop) 2 else 3
      expect_equal(nrow(exported_data), expected_n, info = info)

      # Round-trip the exported tree
      exported_tree <- if (fmt == "newick") {
        ape::read.tree(paths$tree)
      } else {
        ape::read.nexus(paths$tree)
      }
      expect_s3_class(exported_tree, "phylo")
      expected_ntip <- if (drop) 2 else 3
      expect_equal(ape::Ntip(exported_tree), expected_ntip, info = info)
    }
  }
})


test_that("M10: reconcile_export creates non-existent directory", {
  df <- data.frame(species = "Homo sapiens", mass = 70,
                   stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  out_dir <- file.path(tempfile("m10_nested"), "sub", "dir")
  on.exit(unlink(dirname(dirname(out_dir)), recursive = TRUE))

  paths <- reconcile_export(rec, data = df, tree = tree,
                            species_col = "species", dir = out_dir)
  expect_true(file.exists(paths$data))
})


test_that("M10: reconcile_export prefix applied to all output files", {
  df <- data.frame(species = "Homo sapiens", mass = 70,
                   stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  out_dir <- tempfile("m10_prefix")
  on.exit(unlink(out_dir, recursive = TRUE))

  paths <- reconcile_export(rec, data = df, tree = tree,
                            species_col = "species",
                            dir = out_dir, prefix = "myprefix")
  expect_true(grepl("myprefix", basename(paths$data)))
  expect_true(grepl("myprefix", basename(paths$tree)))
  expect_true(grepl("myprefix", basename(paths$mapping)))
})
