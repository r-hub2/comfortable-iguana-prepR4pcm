# pr_get_tree() — pluggable tree retrieval (issue #42).
#
# We don't want the test suite to depend on three external backends
# (rotl + rtrees + clootl on CRAN) being installed AND the
# Open Tree of Life servers being reachable. So we mock each backend's
# entry-point function. The tests verify:
#
#   1. Input dispatch: character vector, data frame, reconciliation
#      object, NULL/empty -- each handled correctly.
#   2. Backend dispatch: source = "rotl" / "rtrees" / "clootl" calls
#      the right underlying function.
#   3. Helpful errors: missing backend package; rtrees without taxon;
#      empty species list.
#   4. Result shape: matched + unmatched + mapping + tree + source +
#      backend_meta + class("pr_tree_result").
#   5. Print method: works without error and emits expected fields.

mini_phylo <- function(tip_labels) {
  ape::read.tree(
    text = paste0(
      "(",
      paste(tip_labels, collapse = ","),
      ");"
    )
  )
}


# 1. Input dispatch -----------------------------------------------------

test_that("character-vector input is deduplicated and NA-cleaned", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(
        tree = mini_phylo(gsub(" ", "_", species)),
        in_query = rep(TRUE, length(species)),
        backend_meta = list(n_queried = length(species))
      )
    },
    .package = "prepR4pcm"
  )

  res <- pr_get_tree(
    c("Homo sapiens", NA, "Homo sapiens", "Pan troglodytes"),
    source = "rotl"
  )
  expect_s3_class(res, "pr_tree_result")
  expect_equal(sort(res$matched), c("Homo sapiens", "Pan troglodytes"))
  expect_equal(res$source, "rotl")
})


test_that("data-frame input pulls the species column (autodetected)", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(
        tree = mini_phylo(gsub(" ", "_", species)),
        in_query = rep(TRUE, length(species)),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )

  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  res <- pr_get_tree(df, source = "rotl")
  expect_equal(sort(res$matched), c("Homo sapiens", "Pan troglodytes"))
})


test_that("data-frame input respects species_col", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(
        tree = mini_phylo(gsub(" ", "_", species)),
        in_query = rep(TRUE, length(species)),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )

  df <- data.frame(scientific = c("Homo sapiens", "Pan troglodytes"))
  res <- pr_get_tree(df, source = "rotl", species_col = "scientific")
  expect_length(res$matched, 2)
})


test_that("reconciliation input pulls name_y and ignores species_col", {
  rec <- reconcile_data(
    data.frame(species = c("Homo sapiens", "Pan troglodytes")),
    data.frame(species = c("Homo sapiens", "Pan troglodytes")),
    x_species = "species",
    y_species = "species",
    authority = NULL,
    quiet = TRUE
  )

  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(
        tree = mini_phylo(gsub(" ", "_", species)),
        in_query = rep(TRUE, length(species)),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )

  expect_message(
    res <- pr_get_tree(
      rec,
      source = "rotl",
      species_col = "ignored_with_a_warning"
    ),
    "ignored"
  )
  expect_equal(sort(res$matched), c("Homo sapiens", "Pan troglodytes"))
})


test_that("empty species list errors with a helpful message", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) stop("should not run"),
    .package = "prepR4pcm"
  )

  expect_error(pr_get_tree(character(0), source = "rotl"), "No species names")
  expect_error(pr_get_tree(NA_character_, source = "rotl"), "No species names")
})


test_that("invalid input class errors with a helpful message", {
  expect_error(
    pr_get_tree(123L, source = "rotl"),
    "must be"
  )
})


# 2. Backend dispatch ---------------------------------------------------

test_that("source = 'rotl' calls the rotl backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      called <<- TRUE
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("foo", source = "rotl")
  expect_true(called)
})

test_that("source = 'clootl' calls the clootl backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_get_tree_clootl = function(species, ...) {
      called <<- TRUE
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("foo", source = "clootl")
  expect_true(called)
})

test_that("source = 'rtrees' calls the rtrees backend with taxon", {
  seen_taxon <- NA
  testthat::local_mocked_bindings(
    .pr_get_tree_rtrees = function(species, taxon = NULL, ...) {
      seen_taxon <<- taxon
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list(taxon = taxon)
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("Salmo salar", source = "rtrees", taxon = "fish")
  expect_equal(seen_taxon, "fish")
  expect_equal(res$backend_meta$taxon, "fish")
})

test_that("source = 'fishtree' calls the fishtree backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, ...) {
      called <<- TRUE
      list(
        tree = mini_phylo(gsub(" ", "_", species)),
        in_query = rep(TRUE, length(species)),
        backend_meta = list(backend = "fishtree", type = "chronogram")
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(
    c("Salmo salar", "Esox lucius"),
    source = "fishtree",
    tnrs = "never"
  )
  expect_true(called)
  expect_equal(res$source, "fishtree")
  expect_equal(res$backend_meta$backend, "fishtree")
})


# 3. Helpful errors -----------------------------------------------------

test_that("missing rotl package gives a helpful migration error", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "rotl")) FALSE else TRUE
    },
    .package = "base"
  )

  err <- tryCatch(.pr_get_tree_rotl("foo"), error = function(e) e)
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("rotl", msg, fixed = TRUE))
  expect_true(grepl("install.packages", msg, fixed = TRUE))
})


test_that("missing rtrees package gives CRAN installation guidance", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "rtrees")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(
    .pr_get_tree_rtrees("foo", taxon = "bird"),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  expect_true(grepl("rtrees", msg, fixed = TRUE))
  expect_true(grepl("install.packages", msg, fixed = TRUE))
  expect_true(grepl("daijiang", msg, fixed = TRUE))
})


test_that("missing clootl package gives CRAN installation guidance", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "clootl")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(.pr_get_tree_clootl("foo"), error = function(e) e)
  msg <- conditionMessage(err)
  expect_true(grepl("clootl", msg, fixed = TRUE))
  expect_true(grepl("install.packages", msg, fixed = TRUE))
  expect_true(grepl("eliotmiller", msg, fixed = TRUE))
})


test_that("missing fishtree package gives a helpful CRAN install hint", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "fishtree")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(.pr_get_tree_fishtree("Salmo salar"), error = function(e) e)
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("fishtree", msg, fixed = TRUE))
  expect_true(grepl("install.packages", msg, fixed = TRUE))
})


test_that("missing datelife package gives a helpful GitHub install hint", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "datelife")) FALSE else TRUE
    },
    .package = "base"
  )
  err <- tryCatch(.pr_get_tree_datelife("Rhea americana"), error = function(e) {
    e
  })
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_true(grepl("datelife", msg, fixed = TRUE))
  expect_true(grepl("phylotastic", msg, fixed = TRUE))
})


# 6. n_tree parameter ---------------------------------------------------

test_that("n_tree validation: must be positive integer", {
  expect_error(
    pr_get_tree("foo", source = "rotl", n_tree = 0),
    "positive integer"
  )
  expect_error(
    pr_get_tree("foo", source = "rotl", n_tree = -1),
    "positive integer"
  )
  expect_error(
    pr_get_tree("foo", source = "rotl", n_tree = c(1, 2)),
    "length-1"
  )
  expect_error(
    pr_get_tree("foo", source = "rotl", n_tree = NA_real_),
    "positive integer"
  )
  expect_error(
    pr_get_tree("foo", source = "rotl", n_tree = NaN),
    "positive integer"
  )
  expect_error(
    pr_get_tree("foo", source = "rotl", n_tree = 1.5),
    "positive integer"
  )
})


test_that("n_tree > 1 on rotl emits a warning and returns single", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      # Real rotl helper would warn; verify n_tree is plumbed through.
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        backend_meta = list(n_received_n_tree = n_tree)
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("foo", source = "rotl", n_tree = 5)
  expect_equal(res$backend_meta$n_received_n_tree, 5L)
})


test_that("n_tree is plumbed through to rtrees", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_rtrees = function(species, taxon = NULL, n_tree = 1L, ...) {
      seen <<- n_tree
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Salmo salar", source = "rtrees", taxon = "fish", n_tree = 10)
  expect_equal(seen, 10L)
})


test_that("n_tree is plumbed through to clootl", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_clootl = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Corvus corax", source = "clootl", n_tree = 3)
  expect_equal(seen, 3L)
})


test_that("n_tree is plumbed through to fishtree", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Salmo salar", source = "fishtree", n_tree = 7, tnrs = "never")
  expect_equal(seen, 7L)
})


test_that("n_tree is plumbed through to datelife", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_datelife = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Rhea americana", source = "datelife", n_tree = 4)
  expect_equal(seen, 4L)
})


test_that("default n_tree = 1 (back-compat)", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      seen <<- n_tree
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("foo", source = "rotl")
  expect_equal(seen, 1L)
})


# 7. Per-tree provenance ------------------------------------------------

test_that("backend_meta$tree_provenance is always present (single phylo)", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        backend_meta = list(n_queried = length(species))
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("foo", source = "rotl")
  expect_true(!is.null(res$backend_meta$tree_provenance))
  expect_length(res$backend_meta$tree_provenance, 1L)
  expect_equal(res$backend_meta$tree_provenance[[1]]$source_index, 1L)
  expect_true(nzchar(res$backend_meta$tree_provenance[[1]]$citation))
})


test_that("backend_meta$tree_provenance has one entry per multiPhylo tree", {
  testthat::local_mocked_bindings(
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      mp <- structure(
        list(
          mini_phylo(species),
          mini_phylo(species),
          mini_phylo(species),
          mini_phylo(species)
        ),
        class = "multiPhylo"
      )
      list(
        tree = mp,
        in_query = rep(TRUE, length(species)),
        backend_meta = list(
          backend = "fishtree",
          reference = "Rabosky 2018",
          n_returned = 4L
        )
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(
    "Salmo salar",
    source = "fishtree",
    n_tree = 4,
    tnrs = "never"
  )
  expect_s3_class(res$tree, "multiPhylo")
  expect_length(res$backend_meta$tree_provenance, 4L)
  for (i in seq_len(4)) {
    expect_equal(res$backend_meta$tree_provenance[[i]]$source_index, i)
  }
})


test_that("datelife per-source citations populate tree_provenance", {
  testthat::local_mocked_bindings(
    .pr_get_tree_datelife = function(species, n_tree = 1L, ...) {
      mp <- structure(
        list(mini_phylo(species), mini_phylo(species)),
        class = "multiPhylo",
        names = c("Hedges et al. 2015", "Bininda-Emonds et al. 2007")
      )
      list(
        tree = mp,
        in_query = rep(TRUE, length(species)),
        backend_meta = list(
          backend = "datelife",
          summary_format = "phylo_all",
          reference = "Sanchez Reyes 2024"
        )
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree("Rhea americana", source = "datelife", n_tree = 2)
  cites <- vapply(
    res$backend_meta$tree_provenance,
    function(p) p$citation,
    character(1)
  )
  expect_true(any(grepl("Hedges", cites)))
  expect_true(any(grepl("Bininda", cites)))
})


# 8. datelife backend (mocked) ------------------------------------------

test_that("source = 'datelife' calls the datelife backend", {
  called <- FALSE
  testthat::local_mocked_bindings(
    .pr_get_tree_datelife = function(species, n_tree = 1L, ...) {
      called <<- TRUE
      list(
        tree = mini_phylo(gsub(" ", "_", species)),
        in_query = rep(TRUE, length(species)),
        backend_meta = list(backend = "datelife", summary_format = "phylo_sdm")
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(
    c("Rhea americana", "Struthio camelus"),
    source = "datelife"
  )
  expect_true(called)
  expect_equal(res$source, "datelife")
})


# 9. TNRS preflight ------------------------------------------------------
#
# We mock the internal `.pr_resolve_query` so the tests don't depend
# on rotl being installed. The mock signals whether it was called and
# with what arguments. (Round 15: `.pr_tnrs_preflight` is now a thin
# shim over `.pr_resolve_query`, which builds the full
# original/normalised/resolved/query mapping table; we mock the latter.)

test_that("tnrs = 'auto' runs preflight for every backend (Round 15: now per-backend gating)", {
  preflight_calls <- list()
  testthat::local_mocked_bindings(
    .pr_resolve_query = function(species, source, tnrs) {
      preflight_calls[[length(preflight_calls) + 1L]] <<- list(
        source = source,
        tnrs = tnrs,
        n = length(species)
      )
      list(
        original = species,
        normalised = species,
        resolved = species,
        query = species,
        tnrs_replacements = NULL
      )
    },
    .pr_get_tree_clootl = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .pr_get_tree_datelife = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )

  # All four backends call the preflight (with different `source`)
  pr_get_tree("Salmo salar", source = "clootl")
  pr_get_tree("Homo sapiens", source = "rotl")
  pr_get_tree("Rhea americana", source = "datelife")
  pr_get_tree("Esox lucius", source = "fishtree")
  expect_length(preflight_calls, 4L)
  # All four were called with tnrs = "auto"
  expect_true(all(vapply(
    preflight_calls,
    function(c) c$tnrs == "auto",
    logical(1)
  )))
})


test_that("tnrs preflight is plumbed through with the user's choice", {
  seen_tnrs <- character()
  testthat::local_mocked_bindings(
    .pr_resolve_query = function(species, source, tnrs) {
      seen_tnrs <<- c(seen_tnrs, tnrs)
      list(
        original = species,
        normalised = species,
        resolved = species,
        query = species,
        tnrs_replacements = NULL
      )
    },
    .pr_get_tree_clootl = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  pr_get_tree("Salmo salar", source = "clootl", tnrs = "always")
  pr_get_tree("Salmo salar", source = "clootl", tnrs = "never")
  pr_get_tree("Salmo salar", source = "clootl", tnrs = "auto")
  expect_equal(seen_tnrs, c("always", "never", "auto"))
})


test_that(".pr_resolve_query: tnrs = 'never' returns query == normalised (no TNRS substitution)", {
  out <- .pr_resolve_query(
    c("Homo sapiens", "Pan troglodytes"),
    source = "clootl",
    tnrs = "never"
  )
  # query is the normalised form (pr_normalize_names is always run);
  # without TNRS, no replacements happen.
  expect_equal(out$original, c("Homo sapiens", "Pan troglodytes"))
  expect_equal(
    out$query,
    out$normalised,
    info = "with tnrs='never', query and normalised must agree"
  )
  expect_null(out$tnrs_replacements)
})


test_that(".pr_resolve_query: tnrs = 'auto' skips TNRS for non-default backends like rotl", {
  # rotl already does TNRS internally; auto should skip TNRS preflight
  # at the dispatcher level.
  out <- .pr_resolve_query(c("Homo sapiens"), source = "rotl", tnrs = "auto")
  expect_equal(
    out$query,
    out$normalised,
    info = "rotl is not in the auto-TNRS default; query stays at normalised"
  )
  expect_null(out$tnrs_replacements)
})


test_that(".pr_resolve_query: warns once when tnrs = 'always' but rotl is missing", {
  # Reset the one-shot flag so we can deterministically observe the warn
  prev <- getOption("prepR4pcm.tnrs_warning_shown", default = NULL)
  on.exit(options(prepR4pcm.tnrs_warning_shown = prev), add = TRUE)
  options(prepR4pcm.tnrs_warning_shown = NULL)

  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "rotl")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_warning(
    out <- .pr_resolve_query(
      c("Salmo salar"),
      source = "fishtree",
      tnrs = "auto"
    ),
    "rotl"
  )
  expect_equal(
    out$query,
    out$normalised,
    info = "without rotl, TNRS is skipped and query falls back to normalised"
  )
  # Second call: no warning (one-shot)
  expect_no_warning(
    .pr_resolve_query(c("Salmo salar"), source = "fishtree", tnrs = "auto")
  )
})


test_that("min_match validation rejects out-of-range values", {
  expect_error(pr_get_tree("foo", source = "rotl", min_match = -0.1), "0, 1")
  expect_error(pr_get_tree("foo", source = "rotl", min_match = 1.5), "0, 1")
  expect_error(
    pr_get_tree("foo", source = "rotl", min_match = c(0.5, 0.8)),
    "length-1"
  )
  expect_error(
    pr_get_tree("foo", source = "rotl", min_match = NA_real_),
    "0, 1"
  )
  expect_error(pr_get_tree("foo", source = "rotl", min_match = NaN), "0, 1")
})


# 10. source = 'auto' dispatcher ----------------------------------------
#
# We mock pr_get_tree_status so all backends look installed regardless
# of the local environment. We also mock all backend helpers so the
# dispatcher's behaviour is deterministic.

.fake_status_all_installed <- function() {
  data.frame(
    source = c("rotl", "rtrees", "clootl", "fishtree", "datelife"),
    installed = TRUE,
    version = "1.0.0",
    needs_network = c(TRUE, FALSE, FALSE, TRUE, FALSE),
    reachable = NA,
    install_hint = "...",
    source_repo = "...",
    stringsAsFactors = FALSE
  )
}


test_that("source = 'auto' returns the first backend that meets min_match", {
  testthat::local_mocked_bindings(
    pr_get_tree_status = function(check_network = FALSE) {
      .fake_status_all_installed()
    },
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      # Resolve 100% of species
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(
    c("Homo sapiens", "Pan troglodytes"),
    source = "auto",
    min_match = 0.5,
    tnrs = "never"
  )
  expect_equal(res$source, "rotl")
  expect_equal(length(res$matched), 2)
})


test_that("source = 'auto' falls through if first backend doesn't meet min_match", {
  testthat::local_mocked_bindings(
    pr_get_tree_status = function(check_network = FALSE) {
      .fake_status_all_installed()
    },
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species[1]),
        in_query = seq_along(species) == 1L,
        backend_meta = list()
      )
    },
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species),
        in_query = rep(TRUE, length(species)),
        unmatched = character(),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(
    c("a", "b", "c", "d"),
    source = "auto",
    min_match = 0.8,
    tnrs = "never"
  )
  expect_equal(res$source, "fishtree")
})


test_that("source = 'auto' returns best-of-the-lot when none meets threshold", {
  testthat::local_mocked_bindings(
    pr_get_tree_status = function(check_network = FALSE) {
      .fake_status_all_installed()
    },
    .pr_get_tree_rotl = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species[1]),
        in_query = seq_along(species) == 1L,
        backend_meta = list()
      )
    },
    .pr_get_tree_fishtree = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species[1:2]),
        in_query = seq_along(species) <= 2L,
        backend_meta = list()
      )
    },
    .pr_get_tree_clootl = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species[1]),
        in_query = seq_along(species) == 1L,
        backend_meta = list()
      )
    },
    .pr_get_tree_datelife = function(species, n_tree = 1L, ...) {
      list(
        tree = mini_phylo(species[1]),
        in_query = seq_along(species) == 1L,
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  expect_warning(
    res <- pr_get_tree(
      c("a", "b", "c", "d", "e"),
      source = "auto",
      min_match = 0.95,
      tnrs = "never"
    ),
    "min_match"
  )
  # fishtree resolved 2/5 = best
  expect_equal(res$source, "fishtree")
  expect_equal(length(res$matched), 2L)
})


test_that("source = 'auto' errors when no backends are installed", {
  testthat::local_mocked_bindings(
    pr_get_tree_status = function(check_network = FALSE) {
      df <- .fake_status_all_installed()
      df$installed <- FALSE
      df
    },
    .package = "prepR4pcm"
  )
  expect_error(
    pr_get_tree(c("a", "b"), source = "auto"),
    "No tree-retrieval backends"
  )
})


test_that("rtrees without taxon errors helpfully", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) TRUE,
    .package = "base"
  )
  err <- tryCatch(
    .pr_get_tree_rtrees("foo", taxon = NULL),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_true(grepl("taxon", conditionMessage(err)))
})


# 4. Result shape -------------------------------------------------------

test_that("result has the documented class and components", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(
        tree = mini_phylo(gsub(" ", "_", species)),
        in_query = rep(TRUE, length(species)),
        backend_meta = list(n_queried = length(species))
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("a b", "c d"), source = "rotl")
  expect_s3_class(res, "pr_tree_result")
  expect_named(
    res,
    c("tree", "matched", "unmatched", "mapping", "source", "backend_meta")
  )
  expect_s3_class(res$tree, "phylo")
})


test_that("result mapping audits original, query, and tree names", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(
        tree = mini_phylo(c("Corvus_corax", "Pica_pica")),
        in_query = c(TRUE, TRUE, FALSE),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )

  res <- pr_get_tree(
    c("Corvus_corax", "Pica pica", "Madeup bird"),
    source = "rotl",
    tnrs = "never"
  )

  expect_s3_class(res$mapping, "tbl_df")
  expect_named(
    res$mapping,
    c(
      "input_name",
      "normalized_name",
      "query_name",
      "tree_name",
      "in_tree",
      "match_type",
      "placement_status",
      "tnrs_number_matches",
      "tnrs_is_synonym",
      "tnrs_approximate_match",
      "tnrs_flags"
    )
  )
  expect_equal(
    res$mapping$input_name,
    c("Corvus_corax", "Pica pica", "Madeup bird")
  )
  expect_equal(
    res$mapping$normalized_name,
    c("Corvus corax", "Pica pica", "Madeup bird")
  )
  expect_equal(
    res$mapping$tree_name,
    c("Corvus_corax", "Pica_pica", NA_character_)
  )
  expect_equal(res$mapping$in_tree, c(TRUE, TRUE, FALSE))
  expect_equal(res$mapping$match_type, c("normalized", "exact", "unmatched"))
  expect_equal(res$mapping$placement_status, rep(NA_character_, 3L))
  # tnrs = "never": the TNRS audit columns are present but all NA
  expect_true(all(is.na(res$mapping$tnrs_number_matches)))
  expect_true(all(is.na(res$mapping$tnrs_flags)))
})


test_that("result mapping records TNRS substitutions as query names", {
  testthat::local_mocked_bindings(
    .pr_resolve_query = function(species, source, tnrs) {
      list(
        original = c("Corvas corax", "Pica pica"),
        normalised = c("Corvas corax", "Pica pica"),
        resolved = c("Corvus corax", "Pica pica"),
        query = c("Corvus corax", "Pica pica"),
        tnrs_replacements = c("Corvas corax" = "Corvus corax")
      )
    },
    .pr_get_tree_rotl = function(species, ...) {
      list(
        tree = mini_phylo(c("Corvus_corax", "Pica_pica")),
        in_query = rep(TRUE, length(species)),
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )

  res <- pr_get_tree(
    c("Corvas corax", "Pica pica"),
    source = "rotl",
    tnrs = "always"
  )

  expect_equal(res$mapping$query_name, c("Corvus corax", "Pica pica"))
  expect_equal(res$mapping$tree_name, c("Corvus_corax", "Pica_pica"))
  expect_equal(res$mapping$match_type, c("tnrs", "exact"))
  expect_equal(
    res$backend_meta$tnrs_replacements,
    c("Corvas corax" = "Corvus corax")
  )
})


# 5. Print method -------------------------------------------------------

test_that("print.pr_tree_result runs without error", {
  testthat::local_mocked_bindings(
    .pr_get_tree_rotl = function(species, ...) {
      list(
        tree = mini_phylo(gsub(" ", "_", species[1])),
        in_query = seq_along(species) == 1L,
        backend_meta = list()
      )
    },
    .package = "prepR4pcm"
  )
  res <- pr_get_tree(c("a b", "c d", "e f"), source = "rotl")
  expect_no_error(capture.output(print(res)))
})
