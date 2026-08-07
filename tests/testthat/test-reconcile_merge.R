test_that("reconcile_merge() produces an inner join by default", {
  data(avonet_subset, package = "prepR4pcm")
  data(nesttrait_subset, package = "prepR4pcm")

  rec <- reconcile_data(avonet_subset, nesttrait_subset,
                        x_species = "Species1",
                        y_species = "Scientific_name",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, avonet_subset, nesttrait_subset,
                            species_col_x = "Species1",
                            species_col_y = "Scientific_name")

  expect_true(is.data.frame(merged))
  expect_true("species_resolved" %in% names(merged))

  # Inner join: only matched species
  n_matched <- sum(rec$mapping$in_x & rec$mapping$in_y &
                     rec$mapping$match_type != "unresolved", na.rm = TRUE)
  expect_equal(nrow(merged), n_matched)

  # No NA in species_resolved
  expect_false(any(is.na(merged$species_resolved)))
})

test_that("reconcile_merge() handles left join", {
  df_x <- data.frame(species = c("A b", "C d", "E f"), val = 1:3,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d"), score = c(10, 20),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "left")

  # Left join keeps all x rows (including unresolved if drop_unresolved = FALSE)
  # With drop_unresolved = TRUE (default), "E f" has NA join key and is excluded
  # in the inner-join-like behaviour, but left join keeps it
  expect_true(is.data.frame(merged))
  expect_true("species_resolved" %in% names(merged))
})

test_that("reconcile_merge() disambiguates common columns", {
  df_x <- data.frame(species = c("A b", "C d"), value = 1:2,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d"), value = 3:4,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species")

  expect_true("value_x" %in% names(merged))
  expect_true("value_y" %in% names(merged))
})

test_that("reconcile_merge() uses custom suffixes", {
  df_x <- data.frame(species = c("A b"), value = 1,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b"), value = 2,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            suffix = c(".data1", ".data2"))

  expect_true("value.data1" %in% names(merged))
  expect_true("value.data2" %in% names(merged))
})

test_that("reconcile_merge() rejects non-reconciliation input", {
  expect_error(
    reconcile_merge(list(), data.frame(), data.frame()),
    "reconciliation"
  )
})

test_that("reconcile_merge() rejects non-data.frame inputs", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")

  rec <- reconcile_tree(avonet_subset, tree_jetz,
                        x_species = "Species1", authority = NULL,
                        quiet = TRUE)

  expect_error(reconcile_merge(rec, "not_a_df", data.frame()),
               "data_x.*must be a data frame")
})

test_that("reconcile_merge() species_resolved is first column", {
  df_x <- data.frame(species = c("A b", "C d"), x_val = 1:2,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d"), y_val = 3:4,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species")

  expect_equal(names(merged)[1], "species_resolved")
})


# --- Regression tests for NA cartesian explosion (issue #495) ---

test_that("reconcile_merge() left join does not explode with asymmetric data", {

  # Simulate Ayumi's scenario: small data_x, large data_y, few shared species
  shared  <- paste("Genus", letters[1:5])
  only_x  <- paste("Xonly", letters[1:5])
  only_y  <- paste("Yonly", letters[1:95])

  df_x <- data.frame(
    species = c(shared, only_x),
    val_x   = seq_len(10),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c(shared, only_y),
    val_y   = seq_len(100),
    stringsAsFactors = FALSE
  )

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Left join: must return exactly nrow(df_x) rows, NOT a cartesian product
  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "left")

  expect_equal(nrow(merged), nrow(df_x))

  # 5 matched rows have non-NA species_resolved
  expect_equal(sum(!is.na(merged$species_resolved)), 5)

  # 5 unmatched x rows have NA species_resolved
  expect_equal(sum(is.na(merged$species_resolved)), 5)
})

test_that("reconcile_merge() full join does not explode with asymmetric data", {
  shared <- paste("Genus", letters[1:2])
  only_x <- paste("Xonly", letters[1:3])
  only_y <- paste("Yonly", letters[1:4])

  df_x <- data.frame(species = c(shared, only_x), val = seq_len(5),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c(shared, only_y), score = seq_len(6),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "full")

  # Full join: 2 matched + 3 x-only + 4 y-only = 9 rows
  expect_equal(nrow(merged), 9)

  # 2 matched rows have non-NA species_resolved
  expect_equal(sum(!is.na(merged$species_resolved)), 2)
})

test_that("reconcile_merge() inner join drops all unmatched", {
  shared <- paste("Genus", letters[1:3])
  only_x <- paste("Xonly", letters[1:7])
  only_y <- paste("Yonly", letters[1:10])

  df_x <- data.frame(species = c(shared, only_x), val = seq_len(10),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c(shared, only_y), score = seq_len(13),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "inner")

  expect_equal(nrow(merged), 3)
  expect_false(any(is.na(merged$species_resolved)))
})

test_that("reconcile_merge() warns about duplicate species", {
  # data_x has 2 rows per species (e.g., male + female)
  df_x <- data.frame(
    species = c("A b", "A b", "C d", "C d"),
    sex     = c("M", "F", "M", "F"),
    mass    = c(10, 8, 20, 18),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("A b", "C d"),
    score   = c(5, 9),
    stringsAsFactors = FALSE
  )

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Should warn about duplicates and produce 4 rows (2 per species × 1 y row)
  expect_message(
    merged <- reconcile_merge(rec, df_x, df_y,
                              species_col_x = "species",
                              species_col_y = "species",
                              how = "inner"),
    "Duplicate species"
  )
  expect_equal(nrow(merged), 4)
})


# --- Tests for drop_unresolved parameter ---

test_that("drop_unresolved = TRUE removes NA species_resolved rows from left join", {
  shared <- paste("Genus", letters[1:3])
  only_x <- paste("Xonly", letters[1:4])

  df_x <- data.frame(species = c(shared, only_x), val = seq_len(7),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c(shared, "Yonly a"), score = seq_len(4),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Default (drop_unresolved = FALSE): keeps all x rows
  merged_keep <- reconcile_merge(rec, df_x, df_y,
                                 species_col_x = "species",
                                 species_col_y = "species",
                                 how = "left",
                                 drop_unresolved = FALSE)
  expect_equal(nrow(merged_keep), 7)
  expect_equal(sum(is.na(merged_keep$species_resolved)), 4)

  # drop_unresolved = TRUE: removes unmatched x rows
  merged_drop <- reconcile_merge(rec, df_x, df_y,
                                 species_col_x = "species",
                                 species_col_y = "species",
                                 how = "left",
                                 drop_unresolved = TRUE)
  expect_equal(nrow(merged_drop), 3)
  expect_false(any(is.na(merged_drop$species_resolved)))
})

test_that("drop_unresolved = TRUE removes NA species_resolved rows from full join", {
  shared <- paste("Genus", letters[1:2])
  only_x <- paste("Xonly", letters[1:3])
  only_y <- paste("Yonly", letters[1:4])

  df_x <- data.frame(species = c(shared, only_x), val = seq_len(5),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c(shared, only_y), score = seq_len(6),
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Default: full join keeps everything
  merged_keep <- reconcile_merge(rec, df_x, df_y,
                                 species_col_x = "species",
                                 species_col_y = "species",
                                 how = "full",
                                 drop_unresolved = FALSE)
  expect_equal(nrow(merged_keep), 9)  # 2 matched + 3 x-only + 4 y-only

  # drop_unresolved = TRUE: only matched rows survive
  merged_drop <- reconcile_merge(rec, df_x, df_y,
                                 species_col_x = "species",
                                 species_col_y = "species",
                                 how = "full",
                                 drop_unresolved = TRUE)
  expect_equal(nrow(merged_drop), 2)
  expect_false(any(is.na(merged_drop$species_resolved)))
})

test_that("drop_unresolved has no effect on inner join", {
  df_x <- data.frame(species = c("A b", "C d", "E f"), val = 1:3,
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("A b", "C d"), score = 1:2,
                     stringsAsFactors = FALSE)

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  merged_t <- reconcile_merge(rec, df_x, df_y,
                              species_col_x = "species",
                              species_col_y = "species",
                              how = "inner",
                              drop_unresolved = TRUE)
  merged_f <- reconcile_merge(rec, df_x, df_y,
                              species_col_x = "species",
                              species_col_y = "species",
                              how = "inner",
                              drop_unresolved = FALSE)
  expect_equal(nrow(merged_t), nrow(merged_f))
  expect_equal(nrow(merged_t), 2)
})


# --- M1. reconcile_merge: how × drop_unresolved × suffix × data shape grid --

test_that("M1 grid: merge row counts are correct for every cell", {
  # Configurable data shapes. Each returns a list(x, y, expected_*) so the
  # test knows what row counts to expect.
  shapes <- list(
    symmetric = function() {
      x <- fx_df_asymmetric(shared = 5, only_x = 0, only_y = 0)
      list(x = x$x, y = x$y, shared = 5, only_x = 0, only_y = 0)
    },
    x_larger = function() {
      x <- fx_df_asymmetric(shared = 3, only_x = 7, only_y = 2)
      list(x = x$x, y = x$y, shared = 3, only_x = 7, only_y = 2)
    },
    y_larger = function() {
      x <- fx_df_asymmetric(shared = 3, only_x = 2, only_y = 15)
      list(x = x$x, y = x$y, shared = 3, only_x = 2, only_y = 15)
    },
    full_overlap = function() {
      x <- fx_df_asymmetric(shared = 6, only_x = 0, only_y = 0)
      list(x = x$x, y = x$y, shared = 6, only_x = 0, only_y = 0)
    }
    # NB: a "no_overlap" shape would cause reconcile_data to build a mapping
    # where every row is unresolved; downstream merge behaviour is exercised
    # by the existing asymmetric regression tests.
  )

  suffix_options <- list(
    default = c("_x", "_y"),
    dotted  = c(".a", ".b")
  )

  for (shape_name in names(shapes)) {
    shape <- shapes[[shape_name]]()
    rec <- reconcile_data(shape$x, shape$y,
                          x_species = "species", y_species = "species",
                          authority = NULL, quiet = TRUE)

    for (how in c("inner", "left", "full")) {
      for (drop in c(TRUE, FALSE)) {
        for (sfx_name in names(suffix_options)) {
          sfx <- suffix_options[[sfx_name]]

          merged <- suppressMessages(reconcile_merge(
            rec, shape$x, shape$y,
            species_col_x = "species", species_col_y = "species",
            how = how, suffix = sfx, drop_unresolved = drop
          ))

          # Predicted row count
          expected <- switch(
            how,
            inner = shape$shared,
            left  = if (drop) shape$shared else shape$shared + shape$only_x,
            full  = if (drop) shape$shared else shape$shared + shape$only_x + shape$only_y
          )

          info <- sprintf(
            "shape=%s how=%s drop=%s suffix=%s",
            shape_name, how, drop, sfx_name
          )

          expect_equal(nrow(merged), expected, info = info)

          # species_resolved is always the first column
          expect_equal(names(merged)[1], "species_resolved", info = info)

          # NA counts are consistent with drop_unresolved
          na_count <- sum(is.na(merged$species_resolved))
          if (drop) {
            expect_equal(na_count, 0, info = info)
          }

          # No unexplained cartesian blow-up: row count <= upper bound
          upper <- shape$shared + shape$only_x + shape$only_y
          expect_true(nrow(merged) <= upper, info = info)
        }
      }
    }
  }
})


test_that("M1 suffix option actually renames common columns", {
  asym <- fx_df_asymmetric(shared = 3, only_x = 0, only_y = 0)
  # Add a column with the same name in both data frames
  asym$x$common_col <- letters[seq_len(3)]
  asym$y$common_col <- LETTERS[seq_len(3)]

  rec <- reconcile_data(asym$x, asym$y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Default suffix
  merged_default <- suppressMessages(reconcile_merge(
    rec, asym$x, asym$y,
    species_col_x = "species", species_col_y = "species"
  ))
  expect_true("common_col_x" %in% names(merged_default))
  expect_true("common_col_y" %in% names(merged_default))

  # Custom suffix
  merged_dot <- suppressMessages(reconcile_merge(
    rec, asym$x, asym$y,
    species_col_x = "species", species_col_y = "species",
    suffix = c(".a", ".b")
  ))
  expect_true("common_col.a" %in% names(merged_dot))
  expect_true("common_col.b" %in% names(merged_dot))
})


test_that("M1 scalability: asymmetric #495-shape with 500+ y-only rows does not explode", {
  skip_on_cran()
  asym <- fx_df_asymmetric(shared = 50, only_x = 20, only_y = 500)
  rec <- reconcile_data(asym$x, asym$y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  t0 <- Sys.time()
  merged_left <- suppressMessages(reconcile_merge(
    rec, asym$x, asym$y,
    species_col_x = "species", species_col_y = "species",
    how = "left"
  ))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_equal(nrow(merged_left), nrow(asym$x))
  expect_lt(elapsed, 10)  # regression guard for #495
})


test_that("M1 suffix validation rejects wrong length", {
  asym <- fx_df_asymmetric(shared = 3, only_x = 0, only_y = 0)
  rec <- reconcile_data(asym$x, asym$y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  expect_error(
    reconcile_merge(rec, asym$x, asym$y,
                    species_col_x = "species", species_col_y = "species",
                    suffix = "_only_one"),
    "suffix"
  )
  expect_error(
    reconcile_merge(rec, asym$x, asym$y,
                    species_col_x = "species", species_col_y = "species",
                    suffix = c("a", "b", "c")),
    "suffix"
  )
})
