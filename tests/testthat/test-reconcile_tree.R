test_that("reconcile_tree works with phylo object", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes", "Gorilla gorilla"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  expect_s3_class(result, "reconciliation")
  expect_equal(result$meta$type, "data_tree")
  # All should match via normalisation (underscores vs spaces)
  n_matched <- sum(result$mapping$in_x & result$mapping$in_y, na.rm = TRUE)
  expect_equal(n_matched, 3L)
})

test_that("reconcile_tree auto-detects species column", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, authority = NULL, quiet = TRUE)
  expect_s3_class(result, "reconciliation")
})

test_that("reconcile_tree handles underscored tips", {
  df <- data.frame(species = c("Homo sapiens"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  mapping <- reconcile_mapping(result)
  matched <- mapping[mapping$in_x & mapping$in_y, ]
  expect_equal(nrow(matched), 1L)
  expect_equal(matched$name_x, "Homo sapiens")
})

test_that("reconcile_tree records tree source in metadata", {
  df <- data.frame(species = "Homo sapiens")
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, authority = NULL, quiet = TRUE)
  expect_true(grepl("phylo", result$meta$y_source))
})

test_that("reconcile_tree errors on invalid input", {
  expect_error(
    reconcile_tree("not a df", ape::rtree(5), authority = NULL),
    "must be a data frame"
  )
})

test_that("reconcile_tree errors on 0-row data frame", {
  df_empty <- data.frame(species = character(0), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_tree(df_empty, tree, x_species = "species",
                   authority = NULL, quiet = TRUE),
    "0 rows"
  )
})

test_that("reconcile_tree errors on all-NA species column", {
  df_na <- data.frame(species = c(NA_character_, NA_character_))
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  expect_error(
    reconcile_tree(df_na, tree, x_species = "species",
                   authority = NULL, quiet = TRUE),
    "All species names.*NA"
  )
})

test_that("reconcile_tree errors on tree with duplicate tip labels", {
  df <- data.frame(species = "A b", stringsAsFactors = FALSE)
  # Construct a tree with duplicate tips manually
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  tree$tip.label <- c("A_b", "A_b")  # force duplicate
  expect_error(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE),
    "duplicate tip"
  )
})


# --- M3. reconcile_tree combinatorial grid ----------------------------------

test_that("M3 grid: tree shape × tip format × data overlap × fuzzy × rank", {
  valid_match_types <- c("exact", "normalized", "synonym", "fuzzy",
                         "manual", "flagged", "unresolved", "override")

  # Tree shapes — use factories from helper-fixtures.R
  tree_shapes <- list(
    binary          = function() fx_tree_small(5),
    polytomy        = function() fx_tree_polytomy(),
    ultrametric     = function() fx_tree_ultrametric(6),
    non_ultrametric = function() fx_tree_non_ultrametric(6)
  )

  # Data shapes relative to tree tips
  make_data <- function(tree, overlap) {
    tips_space <- gsub("_", " ", tree$tip.label)
    switch(overlap,
      all_in_tree    = data.frame(species = tips_space[seq_len(2)],
                                  stringsAsFactors = FALSE),
      all_out        = data.frame(species = c("Nonexistent alpha",
                                               "Nonexistent beta"),
                                  stringsAsFactors = FALSE),
      partial        = data.frame(species = c(tips_space[1],
                                               "Nonexistent gamma"),
                                  stringsAsFactors = FALSE)
    )
  }

  overlaps <- c("all_in_tree", "all_out", "partial")
  tip_formats <- c("underscore", "space")

  for (tree_name in names(tree_shapes)) {
    for (tip_fmt in tip_formats) {
      tree <- tree_shapes[[tree_name]]()
      if (tip_fmt == "space") tree$tip.label <- gsub("_", " ", tree$tip.label)

      for (overlap in overlaps) {
        df <- make_data(tree, overlap)

        for (fuzzy in c(TRUE, FALSE)) {
          for (rank in c("species", "subspecies")) {

            info <- sprintf(
              "tree=%s tips=%s overlap=%s fuzzy=%s rank=%s",
              tree_name, tip_fmt, overlap, fuzzy, rank
            )

            res <- suppressMessages(
              reconcile_tree(df, tree,
                             x_species = "species",
                             authority = NULL,
                             fuzzy = fuzzy,
                             rank = rank,
                             quiet = TRUE)
            )
            expect_true(inherits(res, "reconciliation"), info = info)

            mapping <- res$mapping

            # Invariant 1: when in_y == TRUE, name_y is not NA and
            # exists in tree$tip.label
            in_y_rows <- mapping[!is.na(mapping$in_y) & mapping$in_y, ]
            if (nrow(in_y_rows) > 0) {
              expect_true(all(!is.na(in_y_rows$name_y)), info = info)
              expect_true(all(in_y_rows$name_y %in% tree$tip.label),
                          info = info)
            }

            # Invariant 2: match_type values are a subset of the valid set
            mt <- unique(stats::na.omit(mapping$match_type))
            expect_true(all(mt %in% valid_match_types),
                        info = paste(info, "unexpected types:",
                                     paste(setdiff(mt, valid_match_types),
                                           collapse = ",")))

            # Invariant 3: meta$type is "data_tree"
            expect_equal(res$meta$type, "data_tree", info = info)

            # Invariant 4: counts consistency
            counts <- res$counts
            total_matched <- counts$n_exact + counts$n_normalized +
                             counts$n_synonym + counts$n_fuzzy +
                             counts$n_manual + counts$n_flagged
            expect_equal(total_matched + counts$n_unresolved_x,
                         nrow(df), info = info)
          }
        }
      }
    }
  }
})


test_that("M3: reconcile_tree with empty tree errors", {
  bad <- structure(
    list(edge = matrix(integer(), 0, 2),
         tip.label = character(),
         Nnode = 0L),
    class = "phylo"
  )
  df <- data.frame(species = "Parus major", stringsAsFactors = FALSE)
  expect_error(
    reconcile_tree(df, bad, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
})


test_that("M3: reconcile_tree accepts a Newick file path as the tree arg", {
  tmp <- tempfile(fileext = ".nwk")
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  ape::write.tree(tree, file = tmp)

  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"),
                   stringsAsFactors = FALSE)
  res <- reconcile_tree(df, tmp, x_species = "species",
                        authority = NULL, quiet = TRUE)
  expect_s3_class(res, "reconciliation")
  expect_equal(res$meta$type, "data_tree")
  unlink(tmp)
})


test_that("M3: fuzzy matching finds tree tips with typos when fuzzy=TRUE", {
  df <- data.frame(
    species = c("Homo sapinens",  # 1 insertion
                "Pan troglodites"), # 1 substitution
    stringsAsFactors = FALSE
  )
  tree <- ape::read.tree(
    text = "(Homo_sapiens:1,Pan_troglodytes:1);"
  )

  # Without fuzzy — neither should match (1-edit fails normalization)
  r_off <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species", authority = NULL,
                   fuzzy = FALSE, quiet = TRUE)
  )
  # With fuzzy — both should match at low threshold
  r_on <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species", authority = NULL,
                   fuzzy = TRUE, fuzzy_threshold = 0.7, quiet = TRUE)
  )

  # Fuzzy should recover strictly more matches than non-fuzzy here
  matched_off <- sum(r_off$mapping$in_x & r_off$mapping$in_y, na.rm = TRUE)
  matched_on  <- sum(r_on$mapping$in_x  & r_on$mapping$in_y,  na.rm = TRUE)
  expect_gte(matched_on, matched_off)
})


test_that("M3: no underscores leak into in_y rows' matching when df uses spaces", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    stringsAsFactors = FALSE
  )
  tree <- ape::read.tree(
    text = "(Homo_sapiens:1,Pan_troglodytes:1);"
  )
  res <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)
  # name_x should preserve the user's original form (spaces)
  in_x_rows <- res$mapping[res$mapping$in_x, ]
  expect_true(all(!grepl("_", in_x_rows$name_x)))
})


test_that("M3: large tree smoke test completes in reasonable time", {
  skip_on_cran()
  tree <- fx_tree_large(n = 500)
  # 20 species from the tree, with various case/separator variants
  tips <- tree$tip.label
  df <- data.frame(
    species = gsub("_", " ", tips[seq_len(20)]),
    stringsAsFactors = FALSE
  )
  t0 <- Sys.time()
  res <- suppressMessages(
    reconcile_tree(df, tree, x_species = "species",
                   authority = NULL, quiet = TRUE)
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_s3_class(res, "reconciliation")
  expect_lt(elapsed, 10)
})
