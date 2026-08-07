# Issue #4: cli markup in taxadb-related error messages must be rendered,
# not printed literally. The previous code used rlang::abort() with
# {.pkg ...} / {.code ...} placeholders which only cli interprets;
# users saw the raw template strings.

test_that("pr_ensure_db uses cli::cli_abort, not bare rlang::abort (issue #4)", {
  # Static regression check: deparsed source must contain `cli_abort`
  # for both error paths, not bare `abort(`.
  src <- paste(deparse(pr_ensure_db), collapse = "\n")
  expect_true(grepl("cli::cli_abort", src, fixed = TRUE),
              info = "pr_ensure_db must use cli::cli_abort to render cli markup")
  # No bare rlang::abort inside the function body
  expect_false(grepl("\\babort\\(", gsub("cli::cli_abort", "", src),
                     perl = TRUE),
              info = "no remaining bare abort() calls in pr_ensure_db")
})

test_that("cli markup renders correctly in the missing-taxadb message (issue #4)", {
  # Verify the exact message template we use produces clean text when
  # passed through cli::cli_abort -- protects against regressions in
  # the message template itself.
  err <- tryCatch(
    cli::cli_abort(c(
      "Synonym resolution requires the {.pkg taxadb} package.",
      "i" = 'Install with: {.code install.packages("taxadb")}'
    )),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  expect_match(msg, "taxadb", fixed = TRUE)
  expect_match(msg, "install.packages", fixed = TRUE)
  expect_false(grepl("{.pkg",  msg, fixed = TRUE),
               info = "cli markup must not leak through as literal text")
  expect_false(grepl("{.code", msg, fixed = TRUE),
               info = "cli markup must not leak through as literal text")
})

test_that("missing-taxadb error fires when taxadb is unavailable (issue #4)", {
  # Skip when taxadb actually IS installed -- the error path doesn't
  # trigger and we can't fake it without recursive mocking.
  skip_if(requireNamespace("taxadb", quietly = TRUE),
          "taxadb is installed; cannot exercise the missing-taxadb branch")

  err <- tryCatch(pr_ensure_db("col"), error = function(e) e)
  expect_s3_class(err, "error")
  msg <- conditionMessage(err)
  expect_match(msg, "taxadb", fixed = TRUE)
  expect_false(grepl("{.pkg",  msg, fixed = TRUE))
  expect_false(grepl("{.code", msg, fixed = TRUE))
})

test_that("the supported-authority list matches taxadb's documented providers (issue #5)", {
  # Round 1 incorrectly removed `ott`; Round 2 restored it after we
  # found that taxadb::filter_name actually works for OTT, and that
  # the failure was at td_create() because the default schema set
  # included "common" (which OTT does not ship). Round 2 restricts
  # to schema = "dwc" -- see pr_ensure_db().
  #
  # The taxadb-backed entries now mirror ?taxadb::td_create's documented
  # providers exactly. Round 21 added `gnverifier` as a non-taxadb,
  # HTTP-backed authority (see .pr_lookup_gnverifier()); this test
  # checks the taxadb-served subset, not the full valid list.
  expected_taxadb <- c("col", "itis", "gbif", "ncbi", "ott", "itis_test")
  expect_setequal(
    setdiff(pr_valid_authorities(), "gnverifier"),
    expected_taxadb
  )
})

test_that("removed authorities produce a helpful migration error (issue #5 follow-up)", {
  # Anyone passing one of the previously-listed-but-broken authorities
  # should get a targeted message naming them, not the generic
  # "not a valid authority" message.
  for (bad in c("iucn", "tpl", "fb", "slb", "wd")) {
    err <- tryCatch(pr_validate_authority(bad), error = function(e) e)
    expect_s3_class(err, "error")
    msg <- conditionMessage(err)
    expect_true(grepl("not.*supported", msg, ignore.case = TRUE),
                info = sprintf("error message for `%s` does not mention 'not supported': %s", bad, msg))
  }
})
