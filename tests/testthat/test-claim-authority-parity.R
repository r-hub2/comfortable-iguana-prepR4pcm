# Bundle G #1 — Authority parity ----------------------------------------
#
# These tests catch the "documentation lies about supported authorities"
# bug we hit in May 2026, where pr_valid_authorities() listed `tpl`,
# `fb`, `slb`, `wd` -- none of which are taxadb providers at all -- and
# `iucn`, which broke at taxadb v22.12.
#
# Three angles on the same claim:
#
# 1. Every authority that the package claims to support
#    (pr_valid_authorities()) is documented in the user-facing
#    @param authority block of ?reconcile_data.
# 2. Every authority documented in ?reconcile_data is in
#    pr_valid_authorities() -- no aspirational claims.
# 3. Every authority in pr_valid_authorities() is genuinely supported
#    by the underlying taxadb dependency. (Live; skipped on CRAN /
#    offline / without taxadb installed.)
#
# Sub-tests 1 and 2 are documentation-vs-code parity. Sub-test 3 is
# code-vs-dependency parity. All three can drift independently, so
# all three are tested.

.read_rd_source <- function(name) {
  candidates <- c(
    test_path("..", "..", "man"),
    file.path(getwd(), "man"),
    file.path(getwd(), "..", "man"),
    file.path(getwd(), "..", "..", "man")
  )
  for (c in candidates) {
    if (dir.exists(c) &&
        file.exists(file.path(dirname(c), "DESCRIPTION"))) {
      p <- file.path(c, paste0(name, ".Rd"))
      if (file.exists(p)) {
        return(paste(readLines(p, warn = FALSE), collapse = "\n"))
      }
    }
  }
  NA_character_
}


test_that("every entry in pr_valid_authorities() is documented in ?reconcile_data", {
  skip_on_cran()
  rd_text <- .read_rd_source("reconcile_data")
  if (is.na(rd_text)) skip("Rd source not accessible during this run")

  # Search the whole Rd source for the authority code as a quoted
  # string. We search the full Rd rather than extracting the @param
  # block specifically, because Rd source uses \item{authority}{...}
  # nesting that's awkward to slice with a flat regex; quoted authority
  # codes are unlikely to appear elsewhere in a data-rich help page.
  for (auth in pr_valid_authorities()) {
    expect_match(
      rd_text, paste0('["`]', auth, '["`]'),
      perl = TRUE,
      info = sprintf(
        "authority `%s` is in pr_valid_authorities() but not documented anywhere in ?reconcile_data. Update either the @param authority block to mention it, or remove it from pr_valid_authorities().",
        auth
      )
    )
  }
})


test_that("every authority listed as supported in ?reconcile_data is in pr_valid_authorities()", {
  skip_on_cran()
  rd_text <- .read_rd_source("reconcile_data")
  if (is.na(rd_text)) skip("Rd source not accessible during this run")

  # The supported authorities appear as keys in a \describe{}{...}
  # list, each as `\item{\code{"X"} ...}{...}`. Mentions in narrative
  # prose (e.g. a deprecation note listing removed codes via
  # `\code{"iucn"}` etc.) are NOT in `\item{\code{"X"}`-key position
  # and so should not be treated as claims of support. We scan for
  # the key pattern specifically rather than slicing the \describe{}
  # block, which avoids brace-nesting fragility in the regex.
  candidate_authorities <- c(
    "col", "itis", "gbif", "ncbi", "ott", "iucn", "itis_test",
    "tpl", "fb", "slb", "wd"
  )

  key_pattern <- function(a) {
    # Matches `\item{\code{"a"}` (the key opener of an \item{} entry
    # in a \describe{} list), allowing optional whitespace.
    paste0("\\\\item\\{\\\\code\\{\"", a, "\"\\}")
  }

  documented <- candidate_authorities[
    vapply(candidate_authorities, function(a) {
      grepl(key_pattern(a), rd_text, perl = TRUE)
    }, logical(1))
  ]

  for (auth in documented) {
    expect_true(
      auth %in% pr_valid_authorities(),
      info = sprintf(
        "authority `%s` is listed as a \\describe{} key in ?reconcile_data (treated as a claim of support) but is missing from pr_valid_authorities() -- aspirational doc, not backed by code",
        auth
      )
    )
  }
})


test_that("every entry in pr_valid_authorities() actually works against taxadb (live)", {
  skip_on_cran()
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  testthat::skip_if_not_installed("taxadb")

  # Errors that indicate taxadb infrastructure problems (DB not yet
  # downloaded, schema mismatch, internal NULL/NA from a fresh cache,
  # network glitch on first download) rather than "this authority is
  # invalid". When these fire we SKIP -- because the claim under test
  # is "taxadb accepts this provider", not "taxadb's filter_name()
  # always works in any environment". A real authority-validity
  # error has the substring "provider" and an explicit list, e.g.
  # "Argument 'provider' must be one of ...".
  is_infrastructure_error <- function(msg) {
    grepl(
      paste(
        "character vector argument expected",  # internal NA/NULL from cache miss
        "could not connect", "Could not connect",
        "no such table",                       # SQL: schema not built yet
        "Connection failed",                   # DBI: dbConnect failed
        "Could not download",                  # download.file failed
        "Could not resolve host",              # DNS / offline
        "schema",                              # generic schema problems
        "Failed to download",
        "Cache error",
        sep = "|"
      ),
      msg, ignore.case = TRUE, perl = TRUE
    )
  }

  # The valid list also includes `gnverifier`, which is HTTP-backed
  # (not taxadb-served) — exclude it from this live taxadb parity check.
  taxadb_authorities <- setdiff(pr_valid_authorities(), "gnverifier")
  for (auth in taxadb_authorities) {
    result <- tryCatch(
      taxadb::filter_name("Homo sapiens", provider = auth),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      msg <- conditionMessage(result)
      if (is_infrastructure_error(msg)) {
        skip(sprintf(
          "taxadb infrastructure unavailable for `%s` (skipped, not failed): %s",
          auth, substr(msg, 1, 120)
        ))
      }
    }
    expect_false(
      inherits(result, "error"),
      info = sprintf(
        "taxadb::filter_name fails for authority `%s` (`pr_valid_authorities()` claims this works but it does not): %s",
        auth,
        if (inherits(result, "error")) conditionMessage(result) else ""
      )
    )
    if (!inherits(result, "error")) {
      expect_gt(
        nrow(result), 0,
        label = sprintf(
          "taxadb::filter_name(`Homo sapiens`, provider = `%s`) returned 0 rows; the authority is technically callable but cannot resolve the most basic species name",
          auth
        )
      )
    }
  }
})
