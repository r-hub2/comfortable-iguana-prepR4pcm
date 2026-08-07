# Bundle G #5 -- NEWS top-of-file version matches DESCRIPTION
#
# Catches "bumped DESCRIPTION but forgot to add a NEWS section" and the
# inverse "added a NEWS section but forgot to bump DESCRIPTION".

.find_pkg_root <- function() {
  candidates <- c(
    test_path("..", ".."),
    file.path(getwd()),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", "..")
  )
  for (c in candidates) {
    if (file.exists(file.path(c, "DESCRIPTION")) &&
        file.exists(file.path(c, "NEWS.md"))) {
      return(normalizePath(c))
    }
  }
  NA_character_
}


test_that("the top NEWS.md header version is consistent with DESCRIPTION", {
  skip_on_cran()
  root <- .find_pkg_root()
  if (is.na(root)) skip("package root not found")

  desc_version <- unname(
    read.dcf(file.path(root, "DESCRIPTION"),
             fields = "Version")[1, "Version"]
  )
  expect_false(is.na(desc_version),
               info = "no Version field in DESCRIPTION")

  news_lines <- readLines(file.path(root, "NEWS.md"), warn = FALSE)
  first_h1 <- news_lines[grepl("^# ", news_lines)][1]
  expect_false(is.na(first_h1),
               info = "no top-level header in NEWS.md")

  # The header is one of:
  #   "# prepR4pcm <version>"
  #   "# prepR4pcm <version> (development version)"
  #   "# prepR4pcm (development version)"   <- legacy no-version form
  m <- regmatches(
    first_h1,
    regexpr("[0-9]+\\.[0-9]+\\.[0-9]+(\\.[0-9]+)?", first_h1)
  )
  if (length(m) == 0 || !nzchar(m)) {
    # Legacy header without an embedded version: only allowed if
    # DESCRIPTION's version is itself a development tick (.9000 suffix).
    expect_match(
      desc_version, "\\.9000$",
      info = paste0(
        "NEWS.md top header is '",
        first_h1,
        "' (no embedded version), but DESCRIPTION reports a release ",
        "version (",
        desc_version,
        "). Add the version to the NEWS header."
      )
    )
  } else {
    expect_equal(
      m, desc_version,
      info = paste0(
        "NEWS.md top header version (",
        m,
        ") does not match DESCRIPTION$Version (",
        desc_version,
        "). Update one of them."
      )
    )
  }
})
