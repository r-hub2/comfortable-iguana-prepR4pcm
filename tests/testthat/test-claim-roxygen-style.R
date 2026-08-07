# Bundle G #41 -- roxygen style consistency
#
# Two patterns the round-3 doc-pass eliminated, each documented
# pooherna+jimuelceleste cross-function feedback (issues #17-#21,
# #26, #30-#41):
#
#   1. `Character(N)` -- ambiguous between "single character" and
#      "length-N character vector". Replace with the unambiguous
#      "A length-N character vector".
#
#   2. `Logical. Verb-with-question-mark?` -- e.g.
#      `Logical. Suppress progress messages?`. Reads as a
#      yes/no quiz instead of a parameter description. Replace
#      with statement form: `Logical. Suppresses ... when TRUE.`
#
# Both patterns crept across many help pages without test coverage.
# These tests fail the build if anyone reintroduces either pattern in
# `R/*.R` roxygen blocks.

test_that("no `Character(N)` type strings in R/ roxygen", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  r_files <- list.files(file.path(root, "R"),
                         pattern = "\\.R$", full.names = TRUE)
  if (length(r_files) == 0) skip("R/ source not accessible")

  bad <- character()
  for (f in r_files) {
    src <- readLines(f, warn = FALSE)
    for (i in seq_along(src)) {
      line <- src[i]
      # Match only inside roxygen comment lines (`#'`).
      if (!grepl("^#'", line)) next
      if (grepl("Character\\(\\d+\\)", line, perl = TRUE)) {
        bad <- c(bad, sprintf("%s:%d: %s", basename(f), i, trimws(line)))
      }
    }
  }

  expect_equal(
    length(bad), 0,
    info = paste0(
      "Found `Character(N)` type strings in R/*.R roxygen blocks. ",
      "Replace each with 'A length-N character vector' for clarity ",
      "(@pooherna / @jimuelceleste convention from issues #17-#41):\n  ",
      paste(head(bad, 20), collapse = "\n  "),
      if (length(bad) > 20)
        sprintf("\n  ... and %d more", length(bad) - 20)
      else ""
    )
  )
})


test_that("no `Logical. <Verb>...?` question-style boolean param descriptions", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  r_files <- list.files(file.path(root, "R"),
                         pattern = "\\.R$", full.names = TRUE)
  if (length(r_files) == 0) skip("R/ source not accessible")

  # Pattern: a roxygen `@param X Logical. <Verb...>?` line, where the
  # text after "Logical." ends with `?` before any further sentence
  # terminator. We're flexible about whitespace and word count.
  bad <- character()
  for (f in r_files) {
    src <- readLines(f, warn = FALSE)
    for (i in seq_along(src)) {
      line <- src[i]
      if (!grepl("^#'\\s+@param\\s+\\w+\\s+Logical\\.", line, perl = TRUE)) {
        next
      }
      # Look for `?` ending a sentence within the line.
      # Allow lines that end with `Default ...`/`Defaults ...`/`when TRUE.`.
      tail <- sub("^.*Logical\\.\\s*", "", line)
      first_sentence <- sub("[.!?].*$", "\\0", tail)
      if (grepl("\\?$", first_sentence, perl = TRUE)) {
        bad <- c(bad, sprintf("%s:%d: %s", basename(f), i, trimws(line)))
      }
    }
  }

  expect_equal(
    length(bad), 0,
    info = paste0(
      "Found question-style boolean @param descriptions (e.g. ",
      "`Logical. Suppress messages?`). Rephrase as a statement: ",
      "`Logical. Suppresses messages when TRUE.` ",
      "(@pooherna / @jimuelceleste convention from issues #17-#41):\n  ",
      paste(head(bad, 20), collapse = "\n  "),
      if (length(bad) > 20)
        sprintf("\n  ... and %d more", length(bad) - 20)
      else ""
    )
  )
})
