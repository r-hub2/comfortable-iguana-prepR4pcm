# R1-R8: adversarial / robustness tests.
# These tests pass deliberately bad or edge-case inputs to every entry point
# and assert that each function fails cleanly with an informative error or
# successfully handles the edge case. The point is to catch silent failures
# or cryptic errors when users feed in real-world messy data.

# --- R1. Empty inputs -------------------------------------------------------
test_that("R1: every reconcile_*() entry point errors clearly on empty data frame", {
  df_empty <- fx_df_empty()
  df_ok    <- data.frame(species = "A b", stringsAsFactors = FALSE)
  tree_ok  <- ape::read.tree(text = "(A_b:1,C_d:1);")

  # reconcile_data — both directions
  expect_error(
    reconcile_data(df_empty, df_ok,
                   x_species = "species", y_species = "species",
                   authority = NULL, quiet = TRUE),
    "0 rows"
  )
  expect_error(
    reconcile_data(df_ok, df_empty,
                   x_species = "species", y_species = "species",
                   authority = NULL, quiet = TRUE),
    "0 rows"
  )

  # reconcile_tree
  expect_error(
    reconcile_tree(df_empty, tree_ok, x_species = "species",
                   authority = NULL, quiet = TRUE),
    "0 rows"
  )

  # reconcile_multi
  expect_error(
    reconcile_multi(list(d1 = df_empty), tree_ok,
                    species_cols = "species",
                    authority = NULL, quiet = TRUE),
    "0 rows"
  )
})


# --- R2. All-NA species columns --------------------------------------------
test_that("R2: all-NA species column errors clearly", {
  df_na <- fx_df_all_na()
  df_ok <- data.frame(species = "A b", stringsAsFactors = FALSE)
  tree  <- ape::read.tree(text = "(A_b:1,C_d:1);")

  expect_error(
    reconcile_data(df_na, df_ok,
                   x_species = "species", y_species = "species",
                   authority = NULL, quiet = TRUE),
    "All species names.*NA"
  )
  expect_error(
    reconcile_tree(df_na, tree,
                   x_species = "species",
                   authority = NULL, quiet = TRUE),
    "All species names.*NA"
  )
})


# --- R3. Factor columns ----------------------------------------------------
test_that("R3: factor species columns are coerced with a message", {
  df_factor <- fx_df_factor()
  df_other  <- df_factor  # both factor

  expect_message(
    res <- reconcile_data(df_factor, df_other,
                          x_species = "species", y_species = "species",
                          authority = NULL, quiet = FALSE),
    "factor"
  )
  expect_s3_class(res, "reconciliation")
})


# --- R4. Unicode torture ---------------------------------------------------
test_that("R4: diacritics and non-Latin scripts do not crash reconcile_data", {
  # ASCII / Latin-1 / Greek / Japanese mixture
  df_x <- data.frame(
    species = c(
      "Passer domesticus M\u00fcller, 1776",          # German
      "Turdus merula Linn\u00e9, 1758",                # French
      "\u0391\u03ba\u03c1\u03b9\u03b4\u03b1 sp.",      # Greek "Akrida sp."
      "\u30b9\u30ba\u30e1 sp."                          # Japanese "Suzume sp."
    ),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("Passer domesticus", "Turdus merula", "Other species"),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    res <- reconcile_data(df_x, df_y,
                          x_species = "species", y_species = "species",
                          authority = NULL, quiet = TRUE)
  )
  # The two ASCII-equivalent species should match via normalisation
  expect_true(res$counts$n_normalized >= 2)
})


# --- R5. Single-element / minimal valid inputs -----------------------------
test_that("R5: minimal valid inputs (1 row, 1 tip) work", {
  df_single <- fx_df_single()
  tree_single <- fx_tree_single()

  # Single-row vs single-row
  res_dd <- reconcile_data(df_single, df_single,
                           x_species = "species", y_species = "species",
                           authority = NULL, quiet = TRUE)
  expect_s3_class(res_dd, "reconciliation")
  expect_equal(res_dd$counts$n_exact, 1L)

  # Single-row vs single-tip tree
  res_dt <- reconcile_tree(df_single, tree_single,
                           x_species = "species",
                           authority = NULL, quiet = TRUE)
  expect_s3_class(res_dt, "reconciliation")
  expect_equal(sum(res_dt$mapping$in_x & res_dt$mapping$in_y, na.rm = TRUE), 1L)
})


# --- R6. Large inputs (performance smoke test) ------------------------------
test_that("R6: performance smoke test on 2000-row data and tree", {
  skip_on_cran()

  df    <- fx_df_large(n = 1000)
  tree  <- fx_tree_large(n = 1000)

  t0 <- Sys.time()
  rec_dd <- reconcile_data(df, df,
                           x_species = "species", y_species = "species",
                           authority = NULL, quiet = TRUE)
  rec_dt <- reconcile_tree(df, tree, x_species = "species",
                           authority = NULL, quiet = TRUE)
  merged <- reconcile_merge(rec_dd, df, df,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "inner")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_s3_class(rec_dd, "reconciliation")
  expect_s3_class(rec_dt, "reconciliation")
  expect_s3_class(merged, "data.frame")
  expect_lt(elapsed, 60)
})


# --- R7. Invalid types -----------------------------------------------------
test_that("R7: invalid types produce informative errors", {
  df <- data.frame(species = "A b", stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")

  # reconcile_data: non-df x
  expect_error(
    reconcile_data("string", df, authority = NULL),
    "data frame"
  )
  # reconcile_data: non-df y
  expect_error(
    reconcile_data(df, "string", authority = NULL),
    "data frame"
  )

  # reconcile_tree: non-df x
  expect_error(
    reconcile_tree(42, tree, authority = NULL),
    "data frame"
  )

  # reconcile_apply: non-reconciliation x
  expect_error(
    reconcile_apply(list(), data = df, species_col = "species"),
    "reconciliation"
  )

  # reconcile_diff: non-reconciliation
  rec <- reconcile_data(df, df, x_species = "species",
                        y_species = "species",
                        authority = NULL, quiet = TRUE)
  expect_error(reconcile_diff(rec, "not_a_rec"), "reconciliation")
  expect_error(reconcile_diff("not_a_rec", rec), "reconciliation")

  # reconcile_suggest: non-reconciliation
  expect_error(reconcile_suggest("nope"), "reconciliation")
})


# --- R8. Missing or malformed columns --------------------------------------
test_that("R8: missing species column or wrong type produces clear error", {
  df_with_other <- data.frame(taxon = c("A b", "C d"), val = 1:2,
                              stringsAsFactors = FALSE)
  df_ok <- data.frame(species = c("A b", "C d"), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")

  # Pointing to a column that does not exist
  expect_error(
    reconcile_data(df_with_other, df_ok,
                   x_species = "species", y_species = "species",
                   authority = NULL, quiet = TRUE),
    "species|column"
  )

  # Pointing to a numeric column rather than character — silently coerced;
  # the result should be a reconciliation with no matches.
  df_numeric <- data.frame(species = c(1, 2), stringsAsFactors = FALSE)
  res <- reconcile_data(df_numeric, df_ok,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)
  expect_s3_class(res, "reconciliation")
  expect_equal(res$counts$n_exact + res$counts$n_normalized, 0L)
})
