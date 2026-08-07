# W1-W8: end-to-end workflow tests.
# These tests chain multiple functions together the way a real user would,
# verifying that handoffs between functions remain consistent. Each block is
# one workflow.

# --- W1. Data merge pipeline -----------------------------------------------
test_that("W1: reconcile_data -> summary -> mapping -> merge -> export", {
  asym <- fx_df_asymmetric(shared = 5, only_x = 3, only_y = 4)

  rec <- reconcile_data(asym$x, asym$y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # 1. summary as a structured object
  s <- reconcile_summary(rec, format = "data.frame")
  expect_s3_class(s, "reconciliation_summary")
  expect_true(is.list(s$tables))

  # 2. mapping
  mp <- reconcile_mapping(rec)
  expect_s3_class(mp, "data.frame")
  expect_true(all(c("name_x", "name_y", "match_type") %in% names(mp)))

  # 3. merge with how = "inner"
  merged <- reconcile_merge(rec, asym$x, asym$y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "inner")
  expect_s3_class(merged, "data.frame")
  expect_true("species_resolved" %in% names(merged))
  expect_equal(nrow(merged), 5L)  # only the 5 shared species

  # 4. export
  out_dir <- tempfile("w1_export")
  on.exit(unlink(out_dir, recursive = TRUE))

  # write merged frame manually as CSV and re-read; emulates what users do
  csv_path <- file.path(out_dir, "merged.csv")
  dir.create(out_dir, recursive = TRUE)
  utils::write.csv(merged, csv_path, row.names = FALSE)
  expect_true(file.exists(csv_path))
  re_read <- read.csv(csv_path, stringsAsFactors = FALSE)
  expect_equal(nrow(re_read), nrow(merged))
})


# --- W2. Tree augmentation pipeline ----------------------------------------
test_that("W2: reconcile_tree -> suggest -> override_batch -> augment -> apply", {
  df <- fx_df_for_congeners()
  tree <- fx_tree_with_congeners()

  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  # 1. suggestions for unresolved
  sug <- reconcile_suggest(rec, n = 3, threshold = 0.3, quiet = TRUE)
  expect_s3_class(sug, "tbl_df")

  # 2. apply override_batch with empty batch (no-op) — tests that the chain
  # survives an empty batch
  empty_batch <- data.frame(
    name_x = character(0),
    name_y = character(0),
    action = character(0),
    note   = character(0),
    stringsAsFactors = FALSE
  )
  rec_overridden <- suppressWarnings(
    reconcile_override_batch(rec, empty_batch, quiet = TRUE)
  )
  expect_s3_class(rec_overridden, "reconciliation")

  # 3. augment
  aug <- suppressMessages(
    reconcile_augment(rec_overridden, tree, where = "genus",
                      branch_length = "congener_median",
                      seed = 42, quiet = TRUE)
  )
  expect_equal(
    ape::Ntip(aug$tree),
    ape::Ntip(tree) + nrow(aug$augmented)
  )

  # 4. apply on the augmented tree
  applied <- reconcile_apply(rec_overridden,
                             data = df, tree = aug$tree,
                             species_col = "species",
                             drop_unresolved = FALSE)
  expect_s3_class(applied$tree, "phylo")
  expect_s3_class(applied$data, "data.frame")
})


# --- W3. Multi-dataset against one tree ------------------------------------
test_that("W3: reconcile_multi -> summary -> report writes successfully", {
  df1 <- data.frame(species = c("A b", "C d"), val1 = 1:2,
                    stringsAsFactors = FALSE)
  df2 <- data.frame(species = c("A b", "E f"), val2 = 1:2,
                    stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "((A_b:1,C_d:1):1,E_f:1);")

  rec <- reconcile_multi(list(d1 = df1, d2 = df2), tree,
                         species_cols = "species",
                         authority = NULL, quiet = TRUE)

  s <- reconcile_summary(rec, format = "data.frame")
  expect_s3_class(s, "reconciliation_summary")

  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)
  expect_no_error(
    suppressMessages(suppressWarnings(
      reconcile_report(rec, file = tmp, open = FALSE)
    ))
  )
  expect_true(file.exists(tmp))
})


# --- W4. Multi-tree against one dataset ------------------------------------
test_that("W4: reconcile_to_trees -> diff per pair -> merge per tree", {
  df <- data.frame(
    species = c("Parus major", "Corvus corax", "Turdus merula"),
    trait = 1:3,
    stringsAsFactors = FALSE
  )
  tree_a <- ape::read.tree(
    text = "((Parus_major:1,Corvus_corax:1):1,Turdus_merula:1);"
  )
  tree_b <- ape::read.tree(
    text = "((Parus_major:1,Corvus_corax:1):1,Falco_peregrinus:1);"
  )

  results <- reconcile_to_trees(df,
                                trees = list(a = tree_a, b = tree_b),
                                x_species = "species",
                                authority = NULL, quiet = TRUE)
  expect_equal(length(results), 2)

  # diff identical pair
  d_self <- reconcile_diff(results$a, results$a, quiet = TRUE)
  expect_equal(nrow(d_self$gained), 0L)
  expect_equal(nrow(d_self$lost), 0L)

  # diff across the two trees
  d_ab <- reconcile_diff(results$a, results$b, quiet = TRUE)
  expect_s3_class(d_ab$summary, "data.frame")

  # merge per tree
  m_a <- reconcile_merge(results$a, df, data.frame(species = tree_a$tip.label,
                                                    stringsAsFactors = FALSE),
                         species_col_x = "species",
                         species_col_y = "species",
                         how = "left")
  m_b <- reconcile_merge(results$b, df, data.frame(species = tree_b$tip.label,
                                                    stringsAsFactors = FALSE),
                         species_col_x = "species",
                         species_col_y = "species",
                         how = "left")
  expect_equal(nrow(m_a), 3L)
  expect_equal(nrow(m_b), 3L)
})


# --- W5. Splits-lumps workflow ---------------------------------------------
test_that("W5: reconcile_splits_lumps then reconcile_data on resolved names", {
  data(crosswalk_birdlife_birdtree, package = "prepR4pcm")

  # Build a tiny crosswalk-shaped reconciliation by reconciling two name lists
  df_a <- data.frame(
    species = head(unique(crosswalk_birdlife_birdtree$Species1), 20),
    stringsAsFactors = FALSE
  )
  df_b <- data.frame(
    species = head(unique(crosswalk_birdlife_birdtree$Species3), 20),
    stringsAsFactors = FALSE
  )
  rec <- reconcile_data(df_a, df_b,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # reconcile_splits_lumps should run without error
  sl <- suppressMessages(reconcile_splits_lumps(rec, quiet = TRUE))
  expect_true(is.list(sl) || is.data.frame(sl))
})


# --- W6. Full round trip with diacritics -----------------------------------
test_that("W6: diacritic data round-trips through reconcile_data and merge", {
  df_x <- fx_df_diacritics()
  df_y <- data.frame(
    species = c("Passer domesticus", "Turdus merula",
                "Corvus corone", "Parus major"),
    trait = seq_len(4),
    stringsAsFactors = FALSE
  )

  rec <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)

  # Should match all 4 via normalisation
  expect_equal(rec$counts$n_normalized + rec$counts$n_exact, 4L)

  merged <- reconcile_merge(rec, df_x, df_y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "inner")
  expect_equal(nrow(merged), 4L)
  # Original diacritic names preserved in the unsuffixed column or "_x" col
  if ("species_x" %in% names(merged)) {
    expect_true(any(grepl("M\u00fcller|Linn\u00e9|F\u00e4hse",
                          merged$species_x)))
  }
})


# --- W7. Asymmetric edge case (the #495 pattern) ----------------------------
test_that("W7: large asymmetric reconcile_data + merge runs in reasonable time", {
  skip_on_cran()
  asym <- fx_df_asymmetric(shared = 100, only_x = 10, only_y = 500)

  t0 <- Sys.time()
  rec <- reconcile_data(asym$x, asym$y,
                        x_species = "species", y_species = "species",
                        authority = NULL, quiet = TRUE)
  merged <- reconcile_merge(rec, asym$x, asym$y,
                            species_col_x = "species",
                            species_col_y = "species",
                            how = "left")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  # The classic regression: nrow(merged) must equal nrow(data_x) for left join
  expect_equal(nrow(merged), nrow(asym$x))
  expect_lt(elapsed, 30)
})


# --- W8. Override -> diff cycle ---------------------------------------------
test_that("W8: applying overrides shows up cleanly in reconcile_diff", {
  df <- data.frame(species = c("A b", "C d", "E f"),
                   stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "((A_b:1,C_d:1):1,G_h:1);")
  r_initial <- reconcile_tree(df, tree, x_species = "species",
                               authority = NULL, quiet = TRUE)

  r_overridden <- reconcile_override(r_initial,
                                     name_x = "E f",
                                     name_y = "G_h",
                                     action = "accept",
                                     note = "manual")

  d <- reconcile_diff(r_initial, r_overridden, quiet = TRUE)
  # Initial had no match for E f, overridden does — should appear in gained
  expect_true(d$summary$n_gained >= 1L)

  # And the gained row should be E f
  if (nrow(d$gained) > 0) {
    expect_true("E f" %in% d$gained$name_x)
  }
})
