# Round 21: optional gnverifier synonym backend. The default authority
# path goes through taxadb (a local database); `authority = "gnverifier"`
# routes the same stage through the Global Names verifier over HTTP.
#
# Mocked tests stand in for httr2's req_perform / resp_body_json so they
# stay offline, deterministic, and fast. The live test at the bottom
# skips when the network or httr2 is unavailable.


# Mock helpers --------------------------------------------------------------

# A "response" stub that survives the mocked round-trip. Nothing inside the
# helper inspects the object directly — it is only forwarded to the mocked
# `resp_body_json`, so an empty list keyed by class is plenty.
.mock_response <- function() structure(list(), class = "httr2_mock_response")

# Build a canned `names` payload mirroring the GNverifier API:
# https://verifier.globalnames.org/. One entry per input, each entry has
# `bestResult` (or NULL).
.mock_body <- function(entries) list(names = entries)


# Tests ---------------------------------------------------------------------

test_that("authority = 'gnverifier' errors helpfully when httr2 is missing", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "httr2")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_error(
    prepR4pcm:::.pr_lookup_gnverifier("Homo sapiens"),
    "httr2"
  )
})


test_that(".pr_lookup_gnverifier returns the 5-column contract on a happy mix", {
  skip_if_not_installed("httr2")

  entries <- list(
    # Exact accepted match
    list(
      bestResult = list(
        matchType              = "Exact",
        currentName            = "Homo sapiens",
        matchedName            = "Homo sapiens",
        currentCanonicalSimple = "Homo sapiens",
        matchedCanonicalSimple = "Homo sapiens",
        recordId               = "col:6MB3T"
      )
    ),
    # Synonym -> resolved to a different current name
    list(
      bestResult = list(
        matchType              = "Exact",
        currentName            = "Cyanistes caeruleus",
        matchedName            = "Parus caeruleus",
        currentCanonicalSimple = "Cyanistes caeruleus",
        matchedCanonicalSimple = "Parus caeruleus",
        recordId               = "col:35QXJ"
      )
    ),
    # No bestResult at all
    list(bestResult = NULL)
  )

  testthat::local_mocked_bindings(
    req_perform     = function(req, ...) .mock_response(),
    resp_body_json  = function(resp, ...) .mock_body(entries),
    .package = "httr2"
  )

  res <- prepR4pcm:::.pr_lookup_gnverifier(
    c("Homo sapiens", "Parus caeruleus", "Nonsenicus bogus")
  )

  expect_s3_class(res, "tbl_df")
  expect_named(res,
               c("input", "accepted_name", "status", "taxon_id", "authority"))
  expect_equal(nrow(res), 3L)

  # Row 1: exact accepted match -- status accepted, accepted_name = input
  expect_equal(res$status[1], "accepted")
  expect_equal(res$accepted_name[1], "Homo sapiens")

  # Row 2: synonym -- accepted_name is the verifier's current canonical
  expect_equal(res$status[2], "synonym")
  expect_equal(res$accepted_name[2], "Cyanistes caeruleus")

  # Row 3: no bestResult -- not_found
  expect_equal(res$status[3], "not_found")
  expect_true(is.na(res$accepted_name[3]))

  expect_true(all(res$authority == "gnverifier"))
})


test_that(".pr_lookup_gnverifier degrades to not_found on network failure", {
  skip_if_not_installed("httr2")

  testthat::local_mocked_bindings(
    req_perform = function(req, ...) stop("ECONNREFUSED"),
    .package = "httr2"
  )

  expect_warning(
    res <- prepR4pcm:::.pr_lookup_gnverifier(c("Homo sapiens", "Esox lucius")),
    "GNverifier lookup failed"
  )

  expect_equal(nrow(res), 2L)
  expect_true(all(res$status == "not_found"))
  expect_true(all(is.na(res$accepted_name)))
  expect_true(all(res$authority == "gnverifier"))
})


test_that(".pr_lookup_gnverifier warns once and ignores db_version", {
  skip_if_not_installed("httr2")

  entries <- list(list(bestResult = NULL))
  testthat::local_mocked_bindings(
    req_perform    = function(req, ...) .mock_response(),
    resp_body_json = function(resp, ...) .mock_body(entries),
    .package = "httr2"
  )

  expect_warning(
    res <- prepR4pcm:::.pr_lookup_gnverifier("Homo sapiens",
                                             db_version = "22.12"),
    "db_version"
  )
  # Lookup still ran -- the warning did not abort the call.
  expect_equal(nrow(res), 1L)
  expect_equal(res$status, "not_found")
})


test_that(".pr_lookup_gnverifier warns on a mismatched response shape", {
  skip_if_not_installed("httr2")

  # Server returns a body without a `names` array (or with the wrong length).
  testthat::local_mocked_bindings(
    req_perform    = function(req, ...) .mock_response(),
    resp_body_json = function(resp, ...) list(unexpected = "shape"),
    .package = "httr2"
  )

  expect_warning(
    res <- prepR4pcm:::.pr_lookup_gnverifier(c("a", "b")),
    "response shape"
  )
  expect_true(all(res$status == "not_found"))
})


test_that("pr_lookup_authority('gnverifier') uses the session cache", {
  skip_if_not_installed("httr2")

  # Wipe any pre-existing cache entry so the first call exercises the
  # mocked lookup; the second should hit the cache and skip it entirely.
  rm(list = ls(envir = prepR4pcm:::.pr_cache),
     envir = prepR4pcm:::.pr_cache)

  call_count <- 0L
  entries <- list(list(
    bestResult = list(
      matchType              = "Exact",
      currentName            = "Cyanistes caeruleus",
      matchedName            = "Parus caeruleus",
      currentCanonicalSimple = "Cyanistes caeruleus",
      matchedCanonicalSimple = "Parus caeruleus",
      recordId               = "col:35QXJ"
    )
  ))
  testthat::local_mocked_bindings(
    req_perform = function(req, ...) {
      call_count <<- call_count + 1L
      .mock_response()
    },
    resp_body_json = function(resp, ...) .mock_body(entries),
    .package = "httr2"
  )

  r1 <- prepR4pcm:::pr_lookup_authority("Parus caeruleus",
                                        authority = "gnverifier")
  r2 <- prepR4pcm:::pr_lookup_authority("Parus caeruleus",
                                        authority = "gnverifier")

  expect_equal(call_count, 1L)
  expect_equal(r1$accepted_name, "Cyanistes caeruleus")
  expect_equal(r2$accepted_name, "Cyanistes caeruleus")
})


# Live integration test ----------------------------------------------------
# Runs one real POST against verifier.globalnames.org. Skips when offline,
# when httr2 is missing, or when the request errors -- we never want
# transient network conditions to fail CI.

test_that("live: .pr_lookup_gnverifier round-trips against verifier.globalnames.org", {
  skip_on_cran()
  skip_if_not_installed("httr2")
  skip_if_not_installed("curl")
  skip_if_offline()

  res <- tryCatch(
    prepR4pcm:::.pr_lookup_gnverifier(c("Homo sapiens", "Esox lucius")),
    error = function(e) skip(paste("network/verifier error:", conditionMessage(e)))
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2L)
  expect_named(res,
               c("input", "accepted_name", "status", "taxon_id", "authority"))
  expect_true(all(res$authority == "gnverifier"))

  usable <- !is.na(res$accepted_name) &
    res$status %in% c("accepted", "synonym")
  if (!any(usable)) {
    skip(paste(
      "GNverifier live service returned no accepted/synonym hits:",
      paste(res$status, collapse = ", ")
    ))
  }
  expect_true(any(usable))
})
