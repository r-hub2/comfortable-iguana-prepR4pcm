# Round-2 backward-compat fixture --------------------------------------
#
# Round 1 of the package shipped a reconciliation S3 class with four
# slots: mapping, meta, counts, overrides. Round 2 adds a fifth slot,
# unused_overrides. Users may have saved reconciliations to disk
# (saveRDS) before this slot existed; their .rds files will load as
# four-slot objects after the upgrade. The accessors -- print(),
# summary(), reconcile_mapping(), reconcile_summary(),
# reconcile_export() -- must keep working on those legacy objects.

make_legacy_reconciliation <- function() {
  # Build a four-slot reconciliation by hand, deliberately omitting the
  # new unused_overrides slot. Mirrors what an .rds saved before
  # Round 2 would look like after readRDS().
  mapping <- tibble::tibble(
    name_x        = c("Homo sapiens", "Pan troglodytes"),
    name_y        = c("Homo_sapiens", "Pan_troglodytes"),
    name_resolved = NA_character_,
    match_type    = c("normalized", "normalized"),
    match_score   = c(1, 1),
    match_source  = c("normalisation", "normalisation"),
    in_x          = c(TRUE, TRUE),
    in_y          = c(TRUE, TRUE),
    notes         = c("'Homo sapiens' normalised to 'homo_sapiens'",
                      "'Pan troglodytes' normalised to 'pan_troglodytes'")
  )
  legacy <- list(
    mapping   = mapping,
    meta      = list(
      type             = "data_tree",
      timestamp        = Sys.time(),
      authority        = "none",
      db_version       = "latest",
      fuzzy            = FALSE,
      fuzzy_threshold  = NA_real_,
      fuzzy_method     = NA_character_,
      resolve          = "flag",
      prepR4pcm_version = "0.3.0",
      x_source         = "<legacy data>",
      y_source         = "<legacy tree>",
      rank             = "species"
    ),
    counts    = list(
      n_x            = 2L,
      n_y            = 2L,
      n_exact        = 0L,
      n_normalized   = 2L,
      n_synonym      = 0L,
      n_fuzzy        = 0L,
      n_manual       = 0L,
      n_unresolved_x = 0L,
      n_unresolved_y = 0L,
      n_flagged      = 0L
    ),
    overrides = tibble::tibble(
      name_x    = character(),
      name_y    = character(),
      action    = character(),
      user_note = character(),
      timestamp = as.POSIXct(character())
    )
    # NOTE: deliberately no unused_overrides slot -- this is the legacy
    # object shape from Round 1.
  )
  class(legacy) <- "reconciliation"
  legacy
}


test_that("print() works on a legacy reconciliation without unused_overrides", {
  legacy <- make_legacy_reconciliation()
  expect_no_error(capture.output(print(legacy)))
})


test_that("summary() / reconcile_summary() work on a legacy reconciliation", {
  legacy <- make_legacy_reconciliation()
  expect_no_error(capture.output(summary(legacy)))
  expect_no_error(capture.output(
    reconcile_summary(legacy, format = "console", detail = "brief")
  ))
})


test_that("reconcile_mapping() works on a legacy reconciliation", {
  legacy <- make_legacy_reconciliation()
  m <- reconcile_mapping(legacy)
  expect_s3_class(m, "data.frame")
  expect_equal(nrow(m), 2)
})


test_that("reconcile_mapping(include_unused_overrides = TRUE) is safe on legacy objects", {
  legacy <- make_legacy_reconciliation()
  m <- reconcile_mapping(legacy, include_unused_overrides = TRUE)
  # No unused_overrides slot -> no rows appended; output equals the
  # bare mapping.
  expect_equal(nrow(m), nrow(legacy$mapping))
})


test_that("legacy reconciliation round-trips through saveRDS / readRDS", {
  legacy <- make_legacy_reconciliation()
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(legacy, path)
  loaded <- readRDS(path)
  expect_s3_class(loaded, "reconciliation")
  expect_no_error(capture.output(print(loaded)))
  m <- reconcile_mapping(loaded)
  expect_equal(nrow(m), 2)
})
