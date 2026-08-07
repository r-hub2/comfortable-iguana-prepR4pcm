# Round 16 regression tests for rtrees placement-status table (#74).
#
# rtrees::get_tree() may graft species at higher taxonomic ranks when
# they aren't in the mega-tree exactly: genus-level (tip suffix `*`)
# or family-level (`**`). Some species fall back to "skipped" (rtrees
# tells the user inline but doesn't put them in the tree). Round 16
# surfaces this distinction so users can choose to exclude grafted
# tips from their analysis.
#
# Contract: result$backend_meta$placement is a tibble (one row per
# unique input species) with columns:
#   input_name, tree_name, placement_status
# where placement_status is one of
#   exact, genus_added, family_added, skipped, unmatched.

rtrees_result_quiet <- function(...) {
  suppressWarnings(suppressMessages(pr_get_tree(...)))
}

rtrees_skip_unless_live <- function() {
  skip_on_cran()
  testthat::skip_if_not_installed("rtrees")
  skip_if_offline("api.github.com")
}


test_that("rtrees tip parser strips all markers and prioritises family grafts", {
  parsed <- prepR4pcm:::.pr_parse_rtrees_tip_labels(
    c("Exact_species", "Genus_species*", "Family_species**", "Odd_species***")
  )

  expect_equal(
    parsed$markerless_label,
    c("Exact_species", "Genus_species", "Family_species", "Odd_species")
  )
  expect_equal(
    parsed$placement_status,
    c("exact", "genus_added", "family_added", "family_added")
  )
})


test_that("rtrees placement parser retains marked tips and maps all graft ranks", {
  marked_tree <- ape::read.tree(
    text = "(Exact_species:1,Genus_species*:1,Family_species**:1);"
  )
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) TRUE,
    .package = "base"
  )
  testthat::local_mocked_bindings(
    .pr_rtrees_get_tree = function(...) marked_tree,
    .package = "prepR4pcm"
  )

  res <- prepR4pcm:::.pr_get_tree_rtrees(
    c("Exact species", "Genus species", "Family species", "Skipped species"),
    taxon = "bird"
  )

  expect_identical(res$tree$tip.label, marked_tree$tip.label)
  expect_equal(
    res$backend_meta$placement$placement_status,
    c("exact", "genus_added", "family_added", "skipped")
  )
  expect_equal(res$backend_meta$n_grafted, 2L)
})


test_that("rtrees placement preserves original input names", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rtrees = function(species, taxon = NULL, n_tree = 1L, ...) {
      tree <- ape::read.tree(text = "(Corvus_corax:1,Pica_pica:1);")
      list(
        tree = tree,
        in_query = rep(TRUE, length(species)),
        backend_meta = list(
          placement = tibble::tibble(
            input_name = species,
            tree_name = c("Corvus_corax", "Pica_pica"),
            placement_status = c("exact", "exact")
          )
        )
      )
    },
    .package = "prepR4pcm"
  )

  r <- pr_get_tree(
    c("Corvus_corax", "Pica_pica"),
    source = "rtrees",
    taxon = "bird",
    tnrs = "never"
  )
  expect_equal(
    r$backend_meta$placement$input_name,
    c("Corvus_corax", "Pica_pica")
  )
  expect_equal(
    r$mapping[, c("input_name", "tree_name", "placement_status")],
    tibble::tibble(
      input_name = c("Corvus_corax", "Pica_pica"),
      tree_name = c("Corvus_corax", "Pica_pica"),
      placement_status = c("exact", "exact")
    )
  )
})


test_that("rtrees backend: placement table exists with the documented columns", {
  rtrees_skip_unless_live()
  species <- c("Corvus corax", "Pica pica", "Turdus merula")
  r <- rtrees_result_quiet(
    species,
    source = "rtrees",
    taxon = "bird",
    tnrs = "never"
  )
  expect_true(
    is.data.frame(r$backend_meta$placement),
    info = "placement should be a data frame"
  )
  expect_setequal(
    names(r$backend_meta$placement),
    c("input_name", "tree_name", "placement_status")
  )
})


test_that("rtrees backend: every unique input has exactly one placement row", {
  rtrees_skip_unless_live()
  species <- c("Corvus corax", "Pica pica", "Turdus merula", "Corvus corax") # 1 dup
  r <- rtrees_result_quiet(
    species,
    source = "rtrees",
    taxon = "bird",
    tnrs = "never"
  )
  placement <- r$backend_meta$placement
  expect_equal(
    nrow(placement),
    length(unique(species)),
    info = "one row per unique input species"
  )
  expect_setequal(placement$input_name, unique(species))
})


test_that("rtrees backend: placement_status uses the documented enum", {
  rtrees_skip_unless_live()
  species <- c("Corvus corax", "Pica pica", "Turdus merula")
  r <- rtrees_result_quiet(
    species,
    source = "rtrees",
    taxon = "bird",
    tnrs = "never"
  )
  valid <- c("exact", "genus_added", "family_added", "skipped", "unmatched")
  expect_true(
    all(r$backend_meta$placement$placement_status %in% valid),
    info = "every status must be in the documented enum"
  )
})


test_that("rtrees backend: exact-match species are flagged as 'exact', not 'genus_added'", {
  rtrees_skip_unless_live()
  # All three species are real and should be in the bird mega-tree exactly.
  species <- c("Corvus corax", "Pica pica", "Turdus merula")
  r <- rtrees_result_quiet(
    species,
    source = "rtrees",
    taxon = "bird",
    tnrs = "never"
  )
  status <- r$backend_meta$placement$placement_status
  # At least the three real species should resolve exactly.
  expect_true(
    all(status[r$backend_meta$placement$input_name %in% species] == "exact"),
    info = "real species in the mega-tree should be flagged 'exact'"
  )
})


test_that("rtrees backend: a made-up species in a real genus is flagged 'genus_added'", {
  rtrees_skip_unless_live()
  # Corvus is a real bird genus; 'Corvus madeupensis' isn't a real
  # species but rtrees will graft it at the genus level. Include
  # additional real species so the resulting tree has >= 2 tips
  # (rtrees errors on 1-tip results).
  species <- c("Corvus corax", "Pica pica", "Corvus madeupensis")
  r <- rtrees_result_quiet(
    species,
    source = "rtrees",
    taxon = "bird",
    tnrs = "never"
  )
  placement <- r$backend_meta$placement
  row <- placement[placement$input_name == "Corvus madeupensis", ]
  expect_equal(nrow(row), 1L)
  expect_equal(
    row$placement_status,
    "genus_added",
    info = "made-up species in a real genus should be 'genus_added'"
  )
})


test_that("rtrees backend: a species in no recognised family is flagged 'skipped'", {
  rtrees_skip_unless_live()
  # "Madeupgenus" doesn't match any real family -> rtrees skips it.
  # Include >= 2 real species so the tree has >= 2 tips.
  species <- c("Corvus corax", "Pica pica", "Madeupgenus madeupspecies")
  r <- rtrees_result_quiet(
    species,
    source = "rtrees",
    taxon = "bird",
    tnrs = "never"
  )
  placement <- r$backend_meta$placement
  row <- placement[placement$input_name == "Madeupgenus madeupspecies", ]
  expect_equal(nrow(row), 1L)
  expect_equal(
    row$placement_status,
    "skipped",
    info = "a name in no recognised family is 'skipped' by rtrees"
  )
})


test_that("rtrees backend: skipped species appear in result$unmatched, not result$matched", {
  rtrees_skip_unless_live()
  species <- c("Corvus corax", "Pica pica", "Madeupgenus madeupspecies")
  r <- rtrees_result_quiet(
    species,
    source = "rtrees",
    taxon = "bird",
    tnrs = "never"
  )
  expect_true(
    "Madeupgenus madeupspecies" %in% r$unmatched,
    info = "skipped species should be in unmatched"
  )
  expect_false(
    "Madeupgenus madeupspecies" %in% r$matched,
    info = "skipped species must NOT be in matched"
  )
})
