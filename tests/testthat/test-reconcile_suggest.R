test_that("reconcile_suggest returns tibble with correct columns", {
  df <- data.frame(species = c("Homo sapiens", "Parus mejor", "Unknown species"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Parus_major:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 3, quiet = TRUE)
  expect_s3_class(suggestions, "tbl_df")
  expect_true(all(c("unresolved", "suggestion", "score") %in% names(suggestions)))
})

test_that("reconcile_suggest returns at most n suggestions per unresolved", {
  df <- data.frame(species = c("Homo sapiens", "Parus mejor", "Zappa confluentus"))
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,(Parus_major:0.5,Parus_minor:0.5):0.5):1,Gorilla_gorilla:2);"
  )

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 2, threshold = 0.3, quiet = TRUE)

  # Check no unresolved name has more than n suggestions
  counts <- table(suggestions$unresolved)
  expect_true(all(counts <= 2))
})

test_that("reconcile_suggest scores are between 0 and 1", {
  df <- data.frame(species = c("Homo sapiens", "Parus mejor"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Parus_major:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 3, threshold = 0.3, quiet = TRUE)

  if (nrow(suggestions) > 0) {
    expect_true(all(suggestions$score >= 0 & suggestions$score <= 1))
  }
})

test_that("reconcile_suggest works with bundled data", {
  data(avonet_subset)
  data(tree_jetz)
  result <- reconcile_tree(avonet_subset, tree_jetz,
                            x_species = "Species1", authority = NULL,
                            quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 2, quiet = TRUE)
  expect_s3_class(suggestions, "tbl_df")
  expect_true(all(c("unresolved", "suggestion", "score") %in% names(suggestions)))
})

test_that("reconcile_suggest returns empty tibble when no unresolved species", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  suggestions <- reconcile_suggest(result, n = 3, quiet = TRUE)
  expect_s3_class(suggestions, "tbl_df")
  expect_equal(nrow(suggestions), 0)
  expect_true(all(c("unresolved", "suggestion", "score") %in% names(suggestions)))
})

test_that("reconcile_suggest respects threshold", {
  df <- data.frame(species = c("Abc xyz"))
  tree <- ape::read.tree(text = "(Mno_pqr:1,Stu_vwx:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # Very high threshold should exclude all

  suggestions <- reconcile_suggest(result, n = 3, threshold = 0.99, quiet = TRUE)
  expect_equal(nrow(suggestions), 0)
})

test_that("reconcile_suggest errors on non-reconciliation input", {
  expect_error(
    reconcile_suggest(list(a = 1)),
    "reconciliation"
  )
})


# --- M5. reconcile_suggest n × threshold grid -------------------------------

test_that("suggestion counts respect n across many n values", {
  # Build a reconciliation with several unresolved typo names and many
  # plausible y candidates
  df_x <- data.frame(
    species = c("Parus mejor", "Corvus cxrax", "Turdus merulla",
                "Aquila chrisaetos", "Falco peregrnus"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("Parus major", "Parus minor", "Parus ater",
                "Corvus corax", "Corvus corone", "Corvus monedula",
                "Turdus merula", "Turdus philomelos", "Turdus pilaris",
                "Aquila chrysaetos", "Aquila heliaca",
                "Falco peregrinus", "Falco tinnunculus"),
    stringsAsFactors = FALSE
  )
  rec <- reconcile_data(df_x, df_y, x_species = "species",
                        y_species = "species",
                        authority = NULL, quiet = TRUE)

  for (n in c(1, 2, 3, 5, 10)) {
    out <- reconcile_suggest(rec, n = n, threshold = 0.5, quiet = TRUE)
    # Every unresolved name has at most n suggestions
    counts <- table(out$unresolved)
    expect_true(all(counts <= n),
                info = sprintf("n=%d: some unresolved exceeded n", n))
  }
})


test_that("threshold monotonically reduces suggestion count", {
  df_x <- data.frame(
    species = c("Parus mejor", "Corvus cxrax", "Turdus merulla"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("Parus major", "Corvus corax", "Turdus merula",
                "Aquila chrysaetos"),
    stringsAsFactors = FALSE
  )
  rec <- reconcile_data(df_x, df_y, x_species = "species",
                        y_species = "species",
                        authority = NULL, quiet = TRUE)

  counts <- sapply(c(0.5, 0.7, 0.85, 0.95), function(thr) {
    nrow(reconcile_suggest(rec, n = 10, threshold = thr, quiet = TRUE))
  })
  # Higher threshold -> fewer (or equal) suggestions
  expect_true(all(diff(counts) <= 0),
              info = sprintf("counts: %s", paste(counts, collapse = ", ")))
})


test_that("every returned score is >= threshold and in [0, 1]", {
  df_x <- data.frame(
    species = c("Parus mejor", "Corvus cxrax"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("Parus major", "Corvus corax", "Turdus merula"),
    stringsAsFactors = FALSE
  )
  rec <- reconcile_data(df_x, df_y, x_species = "species",
                        y_species = "species",
                        authority = NULL, quiet = TRUE)

  for (thr in c(0.3, 0.5, 0.7, 0.85)) {
    out <- reconcile_suggest(rec, n = 5, threshold = thr, quiet = TRUE)
    if (nrow(out) > 0) {
      expect_true(all(out$score >= thr),
                  info = sprintf("threshold=%.2f: some scores below thr", thr))
      expect_true(all(out$score >= 0 & out$score <= 1))
    }
  }
})


test_that("output is sorted by unresolved asc, score desc", {
  df_x <- data.frame(
    species = c("Parus mejor", "Corvus cxrax", "Aquila chrisaetos"),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("Parus major", "Parus minor", "Parus ater",
                "Corvus corax", "Corvus corone",
                "Aquila chrysaetos", "Aquila heliaca"),
    stringsAsFactors = FALSE
  )
  rec <- reconcile_data(df_x, df_y, x_species = "species",
                        y_species = "species",
                        authority = NULL, quiet = TRUE)

  out <- reconcile_suggest(rec, n = 3, threshold = 0.5, quiet = TRUE)
  if (nrow(out) > 1) {
    # Within each unresolved group, score should be descending
    for (nm in unique(out$unresolved)) {
      sub <- out[out$unresolved == nm, ]
      expect_equal(sub$score, sort(sub$score, decreasing = TRUE))
    }
    # Across groups, unresolved should be ascending
    unique_order <- unique(out$unresolved)
    expect_equal(unique_order, sort(unique_order))
  }
})


test_that("impossibly high threshold returns empty tibble with right columns", {
  rec <- fx_rec_unresolved()
  out <- reconcile_suggest(rec, n = 5, threshold = 0.9999, quiet = TRUE)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("unresolved", "suggestion", "score"))
})


test_that("reconcile_suggest handles diacritic unresolved names without crashing", {
  df_x <- data.frame(
    species = c("Passer d\u00f6mesticus", "Turdus merula"),  # ö typo
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    species = c("Passer domesticus", "Turdus merula"),
    stringsAsFactors = FALSE
  )
  rec <- reconcile_data(df_x, df_y, x_species = "species",
                        y_species = "species",
                        authority = NULL, quiet = TRUE)

  expect_no_error(
    reconcile_suggest(rec, n = 3, threshold = 0.5, quiet = TRUE)
  )
})


test_that("reconcile_suggest with no y candidates returns empty", {
  # A reconciliation where y is empty relative to what's in x
  df_x <- data.frame(species = c("Abc xyz", "Def uvw"), stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("Abc xyz"), stringsAsFactors = FALSE)
  rec <- reconcile_data(df_x, df_y, x_species = "species",
                        y_species = "species",
                        authority = NULL, quiet = TRUE)
  # "Def uvw" has no plausible y — other y name is also "Abc xyz"
  out <- reconcile_suggest(rec, n = 3, threshold = 0.5, quiet = TRUE)
  expect_s3_class(out, "tbl_df")
  # Either 0 rows, or suggestions only for "Def uvw"
  if (nrow(out) > 0) {
    expect_true(all(out$unresolved == "Def uvw"))
  }
})
