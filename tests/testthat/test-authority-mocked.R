# M11: Authority parameter tested with a mocked pr_lookup_authority.
# The real implementation depends on `taxadb`, which hits the network and may
# require a ~500 MB database download. Tests here mock that internal lookup so
# they run offline, deterministic, and fast.
#
# The mock returns a tibble with the same columns the real function returns:
#   input, accepted_name, status, taxon_id, authority

mock_lookup <- function(table) {
  # `table` is a named character vector: names are inputs, values are
  # accepted names. Inputs not in `table` fall back to `not_found`.
  force(table)
  function(names, authority = "col", db_version = NULL) {
    tibble::tibble(
      input         = names,
      accepted_name = unname(table[names]),
      status        = ifelse(is.na(table[names]), "not_found", "synonym"),
      taxon_id      = ifelse(is.na(table[names]), NA_character_,
                             paste0("id_", match(names, names(table)))),
      authority     = authority
    )
  }
}


test_that("M11: both names are synonyms of the same accepted name", {
  # x has "Parus caeruleus" (old name), y has "Cyanistes caeruleus" (new name);
  # both map to "Cyanistes caeruleus" in the authority
  table <- c(
    "Parus caeruleus"     = "Cyanistes caeruleus",
    "Cyanistes caeruleus" = "Cyanistes caeruleus"
  )
  testthat::local_mocked_bindings(
    pr_lookup_authority = mock_lookup(table),
    .package = "prepR4pcm"
  )

  df_x <- data.frame(species = "Parus caeruleus", stringsAsFactors = FALSE)
  df_y <- data.frame(species = "Cyanistes caeruleus", stringsAsFactors = FALSE)

  res <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = "col", quiet = TRUE)

  expect_equal(res$counts$n_synonym, 1L)
  expect_equal(res$counts$n_unresolved_x, 0L)
  mapping <- res$mapping
  matched <- mapping[!is.na(mapping$name_x) & mapping$match_type == "synonym", ]
  expect_equal(nrow(matched), 1L)
  expect_equal(matched$name_x, "Parus caeruleus")
  expect_equal(matched$name_y, "Cyanistes caeruleus")
  expect_equal(matched$match_source, "col_synonym")
})


test_that("M11: x is accepted, y is a synonym of x", {
  table <- c(
    "Turdus migratorius" = "Turdus migratorius",  # accepted
    "Planesticus migratorius" = "Turdus migratorius"  # synonym
  )
  testthat::local_mocked_bindings(
    pr_lookup_authority = mock_lookup(table),
    .package = "prepR4pcm"
  )

  df_x <- data.frame(species = "Turdus migratorius", stringsAsFactors = FALSE)
  df_y <- data.frame(species = "Planesticus migratorius",
                     stringsAsFactors = FALSE)

  res <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = "col", quiet = TRUE)
  expect_equal(res$counts$n_synonym, 1L)
})


test_that("M11: y is accepted, x is a synonym of y", {
  table <- c(
    "Troglodytes troglodytes" = "Troglodytes troglodytes",
    "Troglodytes aedon"       = "Troglodytes troglodytes"  # wrong example but OK for mock
  )
  testthat::local_mocked_bindings(
    pr_lookup_authority = mock_lookup(table),
    .package = "prepR4pcm"
  )

  df_x <- data.frame(species = "Troglodytes aedon", stringsAsFactors = FALSE)
  df_y <- data.frame(species = "Troglodytes troglodytes",
                     stringsAsFactors = FALSE)

  res <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = "col", quiet = TRUE)
  expect_equal(res$counts$n_synonym, 1L)
})


test_that("M11: neither name found in authority", {
  table <- character(0)
  names(table) <- character(0)
  testthat::local_mocked_bindings(
    pr_lookup_authority = mock_lookup(table),
    .package = "prepR4pcm"
  )

  df_x <- data.frame(species = "Nonsenicus bogus", stringsAsFactors = FALSE)
  df_y <- data.frame(species = "Imaginarius fake", stringsAsFactors = FALSE)

  res <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = "col", quiet = TRUE)
  expect_equal(res$counts$n_synonym, 0L)
  expect_equal(res$counts$n_unresolved_x, 1L)
})


test_that("M11: authority is a no-op when names already match exactly", {
  # Mock should not affect exact-matching path; verify counts reflect exact
  # matches even though mock table is empty
  table <- character(0)
  names(table) <- character(0)
  testthat::local_mocked_bindings(
    pr_lookup_authority = mock_lookup(table),
    .package = "prepR4pcm"
  )

  df_x <- data.frame(species = c("Aaa bbb", "Ccc ddd"),
                     stringsAsFactors = FALSE)
  df_y <- data.frame(species = c("Aaa bbb", "Ccc ddd"),
                     stringsAsFactors = FALSE)

  res <- reconcile_data(df_x, df_y,
                        x_species = "species", y_species = "species",
                        authority = "col", quiet = TRUE)
  expect_equal(res$counts$n_exact, 2L)
  expect_equal(res$counts$n_synonym, 0L)
})


test_that("M11: multiple different authorities are reflected in match_source", {
  # Exercise that match_source prefix uses the supplied authority
  for (auth in c("col", "itis", "gbif", "ncbi")) {
    table <- c("Old name" = "New name", "New name" = "New name")
    testthat::local_mocked_bindings(
      pr_lookup_authority = mock_lookup(table),
      .package = "prepR4pcm"
    )

    df_x <- data.frame(species = "Old name", stringsAsFactors = FALSE)
    df_y <- data.frame(species = "New name", stringsAsFactors = FALSE)

    res <- reconcile_data(df_x, df_y,
                          x_species = "species", y_species = "species",
                          authority = auth, quiet = TRUE)
    mapping <- res$mapping
    matched <- mapping[!is.na(mapping$match_source), ]
    if (nrow(matched) > 0) {
      # every non-NA match_source should be either "exact",
      # "normalisation", or "<auth>_synonym"
      expect_true(all(
        matched$match_source %in% c("exact", "normalisation",
                                     paste0(auth, "_synonym"))
      ), info = sprintf("authority=%s", auth))
    }
  }
})


test_that("M11: authority lookup returning NULL (network error) does not crash", {
  # Simulate upstream taxadb error by having the mock return NULL-like empty
  null_lookup <- function(names, authority = "col", db_version = NULL) {
    tibble::tibble(
      input         = names,
      accepted_name = rep(NA_character_, length(names)),
      status        = rep("not_found", length(names)),
      taxon_id      = rep(NA_character_, length(names)),
      authority     = rep(authority, length(names))
    )
  }
  testthat::local_mocked_bindings(
    pr_lookup_authority = null_lookup,
    .package = "prepR4pcm"
  )

  df_x <- data.frame(species = "Some name", stringsAsFactors = FALSE)
  df_y <- data.frame(species = "Other name", stringsAsFactors = FALSE)

  expect_no_error(
    reconcile_data(df_x, df_y,
                   x_species = "species", y_species = "species",
                   authority = "col", quiet = TRUE)
  )
})


test_that("M11: mocked authority integrates with reconcile_tree", {
  table <- c(
    "Parus caeruleus"     = "Cyanistes caeruleus",
    "Cyanistes caeruleus" = "Cyanistes caeruleus"
  )
  testthat::local_mocked_bindings(
    pr_lookup_authority = mock_lookup(table),
    .package = "prepR4pcm"
  )

  df <- data.frame(species = "Parus caeruleus", stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(Cyanistes_caeruleus:1,Parus_major:1);")

  res <- reconcile_tree(df, tree, x_species = "species",
                        authority = "col", quiet = TRUE)
  expect_equal(res$counts$n_synonym, 1L)
  expect_equal(res$counts$n_unresolved_x, 0L)
})


# Bundle D ---------------------------------------------------------------
#
# Parameterised mock-test that loops over every entry in
# pr_valid_authorities() and asserts that the cascade integrates cleanly
# with each one. Catches the next "X claims to work but doesn't" bug
# the moment a new authority is added to the valid list, without
# requiring the live taxadb network round-trip in CI.

test_that("each authority on the valid list integrates cleanly with the cascade", {
  for (auth in pr_valid_authorities()) {
    table <- c(
      "Parus caeruleus"     = "Cyanistes caeruleus",
      "Cyanistes caeruleus" = "Cyanistes caeruleus"
    )
    testthat::local_mocked_bindings(
      pr_lookup_authority = mock_lookup(table),
      .package = "prepR4pcm"
    )

    df_x <- data.frame(species = "Parus caeruleus", stringsAsFactors = FALSE)
    df_y <- data.frame(species = "Cyanistes caeruleus",
                       stringsAsFactors = FALSE)

    res <- tryCatch(
      reconcile_data(df_x, df_y,
                     x_species = "species", y_species = "species",
                     authority = auth, quiet = TRUE),
      error = function(e) e
    )

    expect_false(
      inherits(res, "error"),
      info = sprintf(
        "reconcile_data() crashes for authority `%s`: %s",
        auth,
        if (inherits(res, "error")) conditionMessage(res) else ""
      )
    )

    if (!inherits(res, "error")) {
      expect_s3_class(res, "reconciliation")
      # Sanity: the synonym row was matched.
      expect_equal(res$counts$n_synonym, 1L,
                   label = sprintf("synonym match count for authority `%s`", auth))
      # Provenance: meta records the authority we asked for.
      expect_equal(res$meta$authority, auth)
    }
  }
})


test_that("removed authorities trigger the migration error, not a generic one", {
  # Locks in the helpful error introduced in Bundle C. If someone later
  # re-adds e.g. `iucn` to pr_valid_authorities() without checking it
  # really works, the live test catches it; if someone removes one of
  # `iucn`/`tpl`/`fb`/`slb`/`wd` from .pr_removed_authorities(), this
  # test catches that too because the migration path is no longer
  # exercised.
  for (bad in c("iucn", "tpl", "fb", "slb", "wd")) {
    err <- tryCatch(pr_validate_authority(bad), error = function(e) e)
    expect_s3_class(err, "error")
    msg <- conditionMessage(err)
    expect_true(grepl("not.*supported", msg, ignore.case = TRUE),
                info = sprintf("error for `%s` should say 'not supported'; got: %s",
                               bad, msg))
    expect_true(grepl("col|itis|gbif|ncbi|ott", msg, ignore.case = TRUE),
                info = sprintf("error for `%s` should suggest a working authority; got: %s",
                               bad, msg))
  }
})
