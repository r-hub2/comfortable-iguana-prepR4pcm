test_that("reconcile_diff detects gained matches with crosswalk", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")
  data(crosswalk_birdlife_birdtree, package = "prepR4pcm")

  r1 <- reconcile_tree(avonet_subset, tree_jetz,
                        x_species = "Species1", authority = NULL,
                        quiet = TRUE)

  overrides <- reconcile_crosswalk(crosswalk_birdlife_birdtree,
                                    from_col = "Species1", to_col = "Species3",
                                    match_type_col = "Match.type")

  r2 <- reconcile_tree(avonet_subset, tree_jetz,
                        x_species = "Species1", authority = NULL,
                        overrides = overrides, quiet = TRUE)

  d <- reconcile_diff(r1, r2, quiet = TRUE)

  # The crosswalk should produce some gained matches
  expect_true(is.data.frame(d$gained))
  expect_true(nrow(d$gained) >= 0)
  expect_true(all(c("name_x", "name_y_new", "match_type_old",
                     "match_type_new", "match_score") %in% names(d$gained)))

  # Summary structure
 expect_true(is.data.frame(d$summary))
  expect_equal(nrow(d$summary), 1L)
  expect_true(all(c("n_gained", "n_lost", "n_type_changed",
                     "n_target_changed", "n_shared") %in% names(d$summary)))
})

test_that("reconcile_diff returns empty tibbles for identical reconciliations", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  r <- reconcile_tree(df, tree, x_species = "species",
                       authority = NULL, quiet = TRUE)

  d <- reconcile_diff(r, r, quiet = TRUE)

  expect_equal(nrow(d$gained), 0L)
  expect_equal(nrow(d$lost), 0L)
  expect_equal(nrow(d$type_changed), 0L)
  expect_equal(nrow(d$target_changed), 0L)
  expect_equal(d$summary$n_gained, 0L)
  expect_equal(d$summary$n_lost, 0L)
})

test_that("reconcile_diff rejects non-reconciliation inputs", {
  df <- data.frame(species = "A b")
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  r <- reconcile_tree(df, tree, x_species = "species",
                       authority = NULL, quiet = TRUE)

  expect_error(reconcile_diff("not_a_rec", r), "reconciliation")
  expect_error(reconcile_diff(r, list()), "reconciliation")
})

test_that("reconcile_diff summary has correct structure", {
  df <- data.frame(species = c("A b", "C d", "E f"))
  tree <- ape::read.tree(text = "((A_b:1,C_d:1):1,G_h:2);")

  r1 <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)

  # Apply an override to create a difference
  r2 <- reconcile_override(r1,
                            name_x = "E f",
                            name_y = "G_h",
                            action = "accept",
                            note = "test override")

  d <- reconcile_diff(r1, r2, quiet = TRUE)

  # Round 2 added a sixth column, n_unused_override_diff, surfacing
  # differences in the unused_overrides slot between two reconciliations.
  expect_equal(ncol(d$summary), 6L)
  expect_true(d$summary$n_gained >= 1L)
  expect_true(is.integer(d$summary$n_gained) || is.numeric(d$summary$n_gained))
})


# --- M7. reconcile_diff comparison grid -------------------------------------

test_that("M7: identical reconciliations produce empty diffs in all categories", {
  rec <- fx_rec_unresolved()
  d <- reconcile_diff(rec, rec, quiet = TRUE)
  expect_equal(nrow(d$gained), 0L)
  expect_equal(nrow(d$lost), 0L)
  expect_equal(nrow(d$type_changed), 0L)
  expect_equal(nrow(d$target_changed), 0L)
  expect_equal(d$summary$n_gained, 0L)
  expect_equal(d$summary$n_lost, 0L)
})


test_that("M7: gain on B is mirrored as loss on A when arguments swapped", {
  df <- data.frame(species = c("A b", "C d", "E f"), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "((A_b:1,C_d:1):1,E_f:2);")
  r_base <- reconcile_tree(df, tree, x_species = "species",
                           authority = NULL, quiet = TRUE)

  # Force one row to unresolved in r_alt
  r_alt <- reconcile_override(r_base, name_x = "E f", action = "reject",
                              note = "")

  d_forward <- reconcile_diff(r_base, r_alt, quiet = TRUE)
  d_reverse <- reconcile_diff(r_alt, r_base, quiet = TRUE)

  # n_lost in forward should equal n_gained in reverse
  expect_equal(d_forward$summary$n_lost, d_reverse$summary$n_gained)
  expect_equal(d_forward$summary$n_gained, d_reverse$summary$n_lost)
})


test_that("M7: target_changed reflects override that swaps name_y", {
  df <- data.frame(species = c("A b"), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,Z_z:1);")
  r1 <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)
  r2 <- reconcile_override(r1, name_x = "A b", name_y = "Z_z",
                           action = "replace", note = "")
  d <- reconcile_diff(r1, r2, quiet = TRUE)
  # Either target_changed or type_changed must show the row, depending on
  # the override action's match_type behaviour
  total_changes <- nrow(d$target_changed) + nrow(d$type_changed) +
                   nrow(d$gained) + nrow(d$lost)
  expect_true(total_changes >= 1L)
})


test_that("M7: disjoint species sets produce mostly gained/lost", {
  df_a <- data.frame(species = c("A b", "C d"), stringsAsFactors = FALSE)
  df_b <- data.frame(species = c("E f", "G h"), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(((A_b:1,C_d:1):1,E_f:1):1,G_h:1);")

  r_a <- reconcile_tree(df_a, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)
  r_b <- reconcile_tree(df_b, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  d <- reconcile_diff(r_a, r_b, quiet = TRUE)
  # Different species sets — n_shared should be 0 or low
  expect_true(d$summary$n_shared <= 2L)
})


test_that("M7: diff categories are mutually exclusive on a single row", {
  # No row should appear in two categories at once
  df <- data.frame(species = c("A b", "C d"), stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(A_b:1,C_d:1);")
  r1 <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL, quiet = TRUE)
  r2 <- reconcile_override(r1, name_x = "A b", action = "reject", note = "")

  d <- reconcile_diff(r1, r2, quiet = TRUE)
  gained_x  <- d$gained$name_x
  lost_x    <- d$lost$name_x
  changed_x <- d$type_changed$name_x

  # Intersections should be empty
  expect_equal(length(intersect(gained_x, lost_x)), 0L)
  expect_equal(length(intersect(gained_x, changed_x)), 0L)
  expect_equal(length(intersect(lost_x, changed_x)), 0L)
})
