test_that("reconcile_multi works with two datasets", {
  df1 <- data.frame(species = c("A b", "C d"), val = 1:2,
                    stringsAsFactors = FALSE)
  df2 <- data.frame(species = c("A b", "E f"), score = 1:2,
                    stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,(C_d:1,E_f:1):1);")

  result <- reconcile_multi(
    list(d1 = df1, d2 = df2), tree,
    species_cols = "species",
    authority = NULL, quiet = TRUE
  )

  expect_s3_class(result, "reconciliation")
  mapping <- reconcile_mapping(result)
  # All 3 unique species should appear in the mapping
  matched <- mapping[mapping$in_x & mapping$in_y, ]
  expect_equal(nrow(matched), 3)
})

test_that("reconcile_multi works with a single dataset", {
  df <- data.frame(species = c("A b"), val = 1,
                   stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")

  result <- reconcile_multi(
    list(only = df), tree,
    species_cols = "species",
    authority = NULL, quiet = TRUE
  )

  expect_s3_class(result, "reconciliation")
})

test_that("reconcile_multi errors on empty datasets list", {
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_multi(list(), tree, authority = NULL, quiet = TRUE),
    "non-empty"
  )
})

test_that("reconcile_multi errors on 0-row dataset", {
  df <- data.frame(species = character(0), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_multi(list(empty = df), tree,
                    species_cols = "species",
                    authority = NULL, quiet = TRUE),
    "0 rows"
  )
})

test_that("reconcile_multi errors on mismatched species_cols length", {
  df1 <- data.frame(species = c("A b"), val = 1,
                    stringsAsFactors = FALSE)
  df2 <- data.frame(species = c("C d"), val = 2,
                    stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_multi(list(d1 = df1, d2 = df2), tree,
                    species_cols = c("species", "species", "extra"),
                    authority = NULL, quiet = TRUE),
    "species_cols"
  )
})


# --- M9. reconcile_multi combinatorial grid --------------------------------

test_that("M9 grid: n_datasets × species_cols × overlap", {
  # Base tree containing A, B, C, D tips
  tree <- ape::read.tree(
    text = "(((A_b:1,C_d:1):1,E_f:1):1,G_h:1);"
  )

  # Build datasets with a configurable overlap with the tree tips
  make_df <- function(kind) {
    switch(kind,
      full = data.frame(species = c("A b", "C d", "E f", "G h"),
                        val = 1:4, stringsAsFactors = FALSE),
      partial = data.frame(species = c("A b", "Z z"),
                           val = 1:2, stringsAsFactors = FALSE),
      none = data.frame(species = c("X x", "Y y"),
                        val = 1:2, stringsAsFactors = FALSE)
    )
  }

  for (n_datasets in c(1, 2, 5)) {
    for (species_col_pattern in c("all_same", "all_different")) {
      for (overlap in c("full", "partial", "none")) {

        datasets <- lapply(seq_len(n_datasets), function(i) make_df(overlap))
        names(datasets) <- paste0("d", seq_len(n_datasets))

        if (species_col_pattern == "all_different") {
          # Rename the species column uniquely per dataset
          sp_cols <- paste0("sp", seq_len(n_datasets))
          for (i in seq_along(datasets)) {
            names(datasets[[i]])[1] <- sp_cols[i]
          }
        } else {
          sp_cols <- "species"
        }

        info <- sprintf("n=%d cols=%s overlap=%s",
                        n_datasets, species_col_pattern, overlap)

        res <- suppressMessages(
          reconcile_multi(datasets, tree,
                          species_cols = sp_cols,
                          authority = NULL, quiet = TRUE)
        )

        expect_true(inherits(res, "reconciliation"), info = info)

        # Invariant: matched counts consistent with overlap
        mapping <- res$mapping
        n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
        switch(overlap,
          full    = expect_true(n_matched >= 4, info = info),
          partial = expect_true(n_matched >= 1 && n_matched < 4, info = info),
          none    = expect_equal(n_matched, 0, info = info)
        )
      }
    }
  }
})


test_that("M9: reconcile_multi unions species names across datasets", {
  # Two datasets with overlapping but distinct species lists
  df1 <- data.frame(species = c("A b", "C d"), val1 = 1:2,
                    stringsAsFactors = FALSE)
  df2 <- data.frame(species = c("C d", "E f"), val2 = 1:2,
                    stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "((A_b:1,C_d:1):1,E_f:1);")

  res <- suppressMessages(
    reconcile_multi(list(d1 = df1, d2 = df2), tree,
                    species_cols = "species",
                    authority = NULL, quiet = TRUE)
  )

  # The mapping should cover all 3 unique names
  all_x_names <- stats::na.omit(unique(res$mapping$name_x))
  expect_equal(length(all_x_names), 3)
  expect_true(all(c("A b", "C d", "E f") %in% all_x_names))
})


test_that("M9: reconcile_multi errors informatively on NULL tree", {
  df <- data.frame(species = "A b", stringsAsFactors = FALSE)
  expect_error(
    reconcile_multi(list(d1 = df), NULL, species_cols = "species",
                    authority = NULL, quiet = TRUE)
  )
})


# Issue #10 (Ayumi): same species across datasets in different
# formats should both resolve to the tree tip via normalisation.
test_that("reconcile_multi: same species in different formats both resolve (#10)", {
  range_df <- data.frame(species = c("Homo_sapiens", "Pan_troglodytes"),
                          stringsAsFactors = FALSE)
  nest_df  <- data.frame(species = c("Homo sapiens", "Pan troglodytes"),
                          stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(Homo_sapiens:1, Pan_troglodytes:1);")

  res <- reconcile_multi(
    list(range = range_df, nest = nest_df),
    tree,
    species_cols = "species",
    authority    = NULL,
    quiet        = TRUE
  )

  # Both formats appear, both resolve to a tip.
  underscores_resolved <- res$mapping$in_y[
    res$mapping$name_x %in% c("Homo_sapiens", "Pan_troglodytes")
  ]
  spaces_resolved <- res$mapping$in_y[
    res$mapping$name_x %in% c("Homo sapiens", "Pan troglodytes")
  ]
  expect_true(all(underscores_resolved))
  expect_true(all(spaces_resolved))

  # No unresolved x names
  unres <- res$mapping$match_type == "unresolved" & res$mapping$in_x
  expect_equal(sum(unres), 0L)

  # Per-dataset coverage matches the per-dataset name count
  in_range_resolved <- sum(res$mapping$in_range & res$mapping$in_y)
  in_nest_resolved  <- sum(res$mapping$in_nest  & res$mapping$in_y)
  expect_equal(in_range_resolved, 2L)
  expect_equal(in_nest_resolved,  2L)
})


test_that("reconcile_multi: in_<dataset> columns are present and correct (#10)", {
  df1 <- data.frame(species = c("Homo sapiens", "Pan troglodytes"),
                     stringsAsFactors = FALSE)
  df2 <- data.frame(species = c("Pan troglodytes", "Gorilla gorilla"),
                     stringsAsFactors = FALSE)
  tree <- ape::read.tree(
    text = "(Homo_sapiens:1, (Pan_troglodytes:1, Gorilla_gorilla:1));"
  )

  res <- reconcile_multi(
    list(d1 = df1, d2 = df2), tree,
    species_cols = "species",
    authority    = NULL, quiet = TRUE
  )

  expect_true("in_d1" %in% names(res$mapping))
  expect_true("in_d2" %in% names(res$mapping))

  # Pan troglodytes is in both datasets; Homo only in d1; Gorilla only in d2
  is_pan     <- res$mapping$name_x == "Pan troglodytes"
  is_homo    <- res$mapping$name_x == "Homo sapiens"
  is_gorilla <- res$mapping$name_x == "Gorilla gorilla"
  expect_true(res$mapping$in_d1[is_pan] && res$mapping$in_d2[is_pan])
  expect_true(res$mapping$in_d1[is_homo] && !res$mapping$in_d2[is_homo])
  expect_true(!res$mapping$in_d1[is_gorilla] && res$mapping$in_d2[is_gorilla])
})


test_that("reconcile_multi mapping joins safely back to each dataset (#10)", {
  # The whole point of issue #10: each dataset's species names
  # appear verbatim in the mapping, so a left join from the
  # original dataset works.
  d1 <- data.frame(species = c("Homo_sapiens", "Pan_troglodytes"),
                    trait1 = c(1.5, 2.5))
  d2 <- data.frame(species = c("Homo sapiens", "Pan troglodytes"),
                    trait2 = c(10, 20))
  tree <- ape::read.tree(text = "(Homo_sapiens:1, Pan_troglodytes:1);")

  res <- reconcile_multi(list(d1 = d1, d2 = d2), tree,
                          species_cols = "species",
                          authority    = NULL, quiet = TRUE)

  # Join each dataset on its raw species column to the mapping;
  # every row should find a match (no NAs in the joined name_y).
  d1$tip <- res$mapping$name_y[match(d1$species, res$mapping$name_x)]
  d2$tip <- res$mapping$name_y[match(d2$species, res$mapping$name_x)]
  expect_false(any(is.na(d1$tip)))
  expect_false(any(is.na(d2$tip)))
  # Both datasets resolve to the same tree tips
  expect_equal(sort(d1$tip), sort(d2$tip))
})
