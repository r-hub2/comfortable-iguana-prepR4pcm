# Helper for claim-parity tests --------------------------------------------
#
# All test-claim-*.R tests inspect the package SOURCE tree (R/, man/,
# vignettes/, _pkgdown.yml, NAMESPACE, NEWS.md, etc.) rather than the
# installed package. They run in two situations:
#
#   1. devtools::test() during local development  -- the source tree is
#      one level up from tests/testthat, so test_path("..", "..") points
#      at it.
#
#   2. R CMD check during a fresh install -- the source tree is NOT
#      accessible (tests run inside a temp library install). In that
#      mode every claim-parity test should skip cleanly: these are
#      doc-vs-code drift gates, not user-facing tests.
#
# The helper below resolves the package root once per test file. Tests
# call .claim_root() and skip if it returns NA.

.claim_root <- function() {
  cands <- c(
    testthat::test_path("..", ".."),
    file.path(getwd()),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  )
  for (c in cands) {
    if (file.exists(file.path(c, "DESCRIPTION")) &&
        dir.exists(file.path(c, "R"))) {
      return(normalizePath(c))
    }
  }
  NA_character_
}
