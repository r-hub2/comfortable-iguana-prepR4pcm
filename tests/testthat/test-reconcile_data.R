test_that("reconcile_data works with exact matches", {
  df1 <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  df2 <- data.frame(species = c("Homo sapiens", "Gorilla gorilla"))

  result <- reconcile_data(df1, df2, authority = NULL, quiet = TRUE)

  expect_s3_class(result, "reconciliation")
  expect_equal(result$meta$type, "data_data")
  expect_equal(result$counts$n_exact, 1L)
})

test_that("reconcile_data detects species column automatically", {
  df1 <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  df2 <- data.frame(
    species = c("Homo sapiens", "Gorilla gorilla"),
    mass = c(70, 150)
  )

  result <- reconcile_data(df1, df2, authority = NULL, quiet = TRUE)
  expect_s3_class(result, "reconciliation")
})

test_that("reconcile_data handles normalised matches", {
  df1 <- data.frame(species = c("Homo_sapiens", "Pan_troglodytes"))
  df2 <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))

  result <- reconcile_data(df1, df2, authority = NULL, quiet = TRUE)

  expect_equal(result$counts$n_normalized, 2L)
  expect_equal(result$counts$n_exact, 0L)
})

test_that("reconcile_data errors on invalid input", {
  expect_error(
    reconcile_data("not a df", data.frame(species = "A"), authority = NULL),
    "must be a data frame"
  )
})

test_that("reconcile_data errors on 0-row data frame", {
  df_empty <- data.frame(species = character(0), stringsAsFactors = FALSE)
  df_ok <- data.frame(species = "A b", stringsAsFactors = FALSE)
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
})

test_that("reconcile_data errors on all-NA species column", {
  df_na <- data.frame(species = c(NA, NA), stringsAsFactors = FALSE)
  df_ok <- data.frame(species = "A b", stringsAsFactors = FALSE)
  expect_error(
    reconcile_data(df_na, df_ok,
                   x_species = "species", y_species = "species",
                   authority = NULL, quiet = TRUE),
    "All species names.*NA"
  )
})

test_that("reconcile_data handles factor species columns", {
  df1 <- data.frame(species = factor(c("A b", "C d")))
  df2 <- data.frame(species = factor(c("A b", "E f")))
  expect_message(
    result <- reconcile_data(df1, df2,
                             x_species = "species", y_species = "species",
                             authority = NULL, quiet = FALSE),
    "factor"
  )
  expect_s3_class(result, "reconciliation")
})


# --- M2. reconcile_data matching cascade grid -------------------------------

test_that("M2 grid: fuzzy × threshold × resolve × rank × data shape", {
  # Data shapes we know the exact answer for
  shapes <- list(
    clean = function() {
      x <- data.frame(species = c("Parus major", "Corvus corax", "Turdus merula"),
                      stringsAsFactors = FALSE)
      y <- data.frame(species = c("Parus major", "Corvus corax", "Turdus merula"),
                      stringsAsFactors = FALSE)
      list(x = x, y = y, expect_exact = 3)
    },
    underscore = function() {
      x <- data.frame(species = c("Parus_major", "Corvus_corax"),
                      stringsAsFactors = FALSE)
      y <- data.frame(species = c("Parus major", "Corvus corax"),
                      stringsAsFactors = FALSE)
      list(x = x, y = y, expect_normalized = 2)
    },
    mixed_case = function() {
      x <- data.frame(species = c("parus MAJOR", "CORVUS corax"),
                      stringsAsFactors = FALSE)
      y <- data.frame(species = c("Parus major", "Corvus corax"),
                      stringsAsFactors = FALSE)
      list(x = x, y = y, expect_normalized = 2)
    },
    diacritics = function() {
      x <- data.frame(
        species = c("Passer domesticus M\u00fcller, 1776",
                    "Turdus merula Linn\u00e9, 1758"),
        stringsAsFactors = FALSE
      )
      y <- data.frame(
        species = c("Passer domesticus", "Turdus merula"),
        stringsAsFactors = FALSE
      )
      list(x = x, y = y, expect_normalized = 2)
    },
    typos = function() {
      x <- data.frame(
        species = c("Parus mejor", "Corvus cxrax"),
        stringsAsFactors = FALSE
      )
      y <- data.frame(
        species = c("Parus major", "Corvus corax"),
        stringsAsFactors = FALSE
      )
      list(x = x, y = y, expect_fuzzy_when_on = 2)
    }
  )

  for (shape_name in names(shapes)) {
    shape <- shapes[[shape_name]]()

    for (fuzzy in c(TRUE, FALSE)) {
      thresholds <- if (fuzzy) c(0.7, 0.85, 0.95) else NA
      for (thr in thresholds) {
        for (resolve in c("flag", "first")) {
          for (rank in c("species", "subspecies")) {

            info <- sprintf(
              "shape=%s fuzzy=%s thr=%s resolve=%s rank=%s",
              shape_name, fuzzy, thr, resolve, rank
            )

            args <- list(
              x = shape$x, y = shape$y,
              x_species = "species", y_species = "species",
              authority = NULL,
              fuzzy = fuzzy,
              resolve = resolve,
              rank = rank,
              quiet = TRUE
            )
            if (fuzzy) args$fuzzy_threshold <- thr

            res <- suppressMessages(do.call(reconcile_data, args))
            expect_true(inherits(res, "reconciliation"), info = info)

            counts <- res$counts

            # Invariant 1: exact matches are stable regardless of fuzzy settings
            if (!is.null(shape$expect_exact)) {
              expect_equal(counts$n_exact, shape$expect_exact, info = info)
            }

            # Invariant 2: normalized matches are detected even with fuzzy=FALSE
            if (!is.null(shape$expect_normalized)) {
              expect_equal(counts$n_normalized, shape$expect_normalized,
                           info = info)
            }

            # Invariant 3: fuzzy matches found only when fuzzy is on, and
            # only when threshold permits
            if (!is.null(shape$expect_fuzzy_when_on)) {
              if (fuzzy && thr <= 0.85) {
                expect_equal(counts$n_fuzzy + counts$n_flagged,
                             shape$expect_fuzzy_when_on,
                             info = info)
              } else if (!fuzzy) {
                expect_equal(counts$n_fuzzy, 0, info = info)
              }
            }

            # Invariant 4: matched + unresolved_x = n_x
            total_x <- nrow(shape$x)
            n_matched_x <- counts$n_exact + counts$n_normalized +
                           counts$n_synonym + counts$n_fuzzy +
                           counts$n_manual + counts$n_flagged
            expect_equal(n_matched_x + counts$n_unresolved_x, total_x,
                         info = info)
          }
        }
      }
    }
  }
})


test_that("M2: matched count is monotonically non-decreasing as fuzzy threshold decreases", {
  df_x <- data.frame(
    species = c("Parus mejor",    # 1 edit
                "Corvus cxrax",   # 1 edit
                "Turdus merla",   # 1 edit
                "Aquila chrisaetos"),  # 1 edit
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("Parus major", "Corvus corax", "Turdus merula", "Aquila chrysaetos"),
    stringsAsFactors = FALSE
  )

  counts <- sapply(c(0.95, 0.85, 0.7, 0.5), function(thr) {
    r <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, fuzzy = TRUE,
                        fuzzy_threshold = thr, quiet = TRUE)
    r$counts$n_exact + r$counts$n_normalized + r$counts$n_fuzzy +
      r$counts$n_flagged
  })

  # Non-decreasing as threshold drops
  expect_true(all(diff(counts) >= 0),
              info = sprintf("counts at thresholds 0.95/0.85/0.7/0.5: %s",
                             paste(counts, collapse = ", ")))
})


test_that("M2: invalid authority string errors with helpful message", {
  df <- data.frame(species = "Parus major", stringsAsFactors = FALSE)
  expect_error(
    reconcile_data(df, df,
                   x_species = "species", y_species = "species",
                   authority = "not_an_authority", quiet = TRUE),
    "Unknown authority"
  )
})


test_that("M2: invalid rank errors via match.arg", {
  df <- data.frame(species = "Parus major", stringsAsFactors = FALSE)
  expect_error(
    reconcile_data(df, df,
                   x_species = "species", y_species = "species",
                   authority = NULL, rank = "family", quiet = TRUE)
  )
})


test_that("M2: invalid resolve errors via match.arg", {
  df <- data.frame(species = "Parus major", stringsAsFactors = FALSE)
  expect_error(
    reconcile_data(df, df,
                   x_species = "species", y_species = "species",
                   authority = NULL, resolve = "nope", quiet = TRUE)
  )
})


test_that("M2: species column auto-detection works on custom column names", {
  df <- fx_df_custom_col("Scientific_Name")
  # reconcile_data with NULL species columns should detect it
  res <- reconcile_data(df, df,
                        authority = NULL, quiet = TRUE)
  expect_s3_class(res, "reconciliation")
  expect_equal(res$counts$n_exact, nrow(df))
})


test_that("M2: diacritic-authority names match their ASCII twins", {
  # The #A4 regression: names differing only in author diacritic should
  # normalize to the same binomial and match each other
  df_x <- data.frame(
    species = c("Passer domesticus M\u00fcller, 1776",
                "Turdus merula Linn\u00e9, 1758"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("Passer domesticus Mueller, 1776",
                "Turdus merula Linne, 1758"),
    stringsAsFactors = FALSE
  )
  res <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)
  # Both should match via normalization (not exact)
  expect_equal(res$counts$n_normalized, 2)
  expect_equal(res$counts$n_unresolved_x, 0)
})
