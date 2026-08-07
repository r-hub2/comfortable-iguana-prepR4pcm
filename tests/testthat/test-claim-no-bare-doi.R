# Bundle G #40 -- no bare `doi:10.xxx` URIs in user-facing files
#
# Catches the bug shipped in PR #28 where vignettes used `doi:10.xxx`
# as plain text. Pandoc renders these as href="doi:10.xxx" which the
# browser treats as a custom URI scheme -- effectively dead links on
# the deployed site. Use markdown DOI links when the target is stable
# under automated checks, or plain `DOI 10.xxx` text when CRAN URL
# checks would hit publisher bot-blocks.
#
# This test scans every .Rmd / README.md for bare doi: outside of a
# completed markdown link and outside fenced code blocks. If any
# survive in prose, the deployed site has dead links. (A bare doi:
# inside a ``` fence is literal text -- pandoc does not auto-link
# inside code -- so those carry no dead-link risk.)

test_that("no bare doi: URIs in user-facing files (issue: PR #28 regression)", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  files <- c(
    file.path(root, "README.Rmd"),
    file.path(root, "README.md"),
    list.files(file.path(root, "vignettes"),
               pattern = "\\.Rmd$", full.names = TRUE)
  )
  files <- files[file.exists(files)]
  if (length(files) == 0) skip("no user-facing markdown files found")

  bad_lines <- character()
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    in_code_fence <- FALSE
    for (i in seq_along(src)) {
      line <- src[i]
      # Toggle in/out of fenced code blocks. A bare `doi:` inside a
      # ``` fence is literal text (pandoc does not auto-link inside
      # code), so it is not a dead-link risk and is skipped.
      if (grepl("^\\s*```", line)) {
        in_code_fence <- !in_code_fence
        next
      }
      if (in_code_fence) next
      # Look for a `doi:10.xxx` substring whose preceding char is
      # NOT `]`+`(` (markdown link) and NOT inside `[...]` brackets.
      # Cheap heuristic: every `doi:10.` must have a preceding `[`
      # OR be inside `()` brackets that are part of `](...)`.
      pos <- gregexpr("doi:10\\.", line, perl = TRUE)[[1]]
      if (pos[1] == -1) next
      for (p in pos) {
        # Inside a `[doi:...](url)` link, the doi: comes right after `[`.
        # Inside the URL, doi: doesn't appear -- doi.org/10. does.
        # So we accept a doi:10. ONLY if the char immediately before is `[`.
        before <- if (p == 1) "" else substr(line, p - 1, p - 1)
        if (before != "[") {
          bad_lines <- c(
            bad_lines,
            sprintf("%s:%d: %s", basename(f), i, trimws(line))
          )
        }
      }
    }
  }

  expect_equal(
    length(bad_lines), 0,
    info = paste0(
      "Bare `doi:10.xxx` URIs found (these render as dead links):\n  ",
      paste(head(bad_lines, 20), collapse = "\n  "),
      if (length(bad_lines) > 20)
        sprintf("\n  ... and %d more", length(bad_lines) - 20)
      else "",
      "\nUse a markdown DOI link or plain `DOI 10.xxx` text instead."
    )
  )
})


test_that("every `[doi:X](url/Y)` has X == Y (regression for PR #29 truncation)", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  files <- c(
    file.path(root, "README.Rmd"),
    file.path(root, "README.md"),
    list.files(file.path(root, "vignettes"),
               pattern = "\\.Rmd$", full.names = TRUE)
  )
  files <- files[file.exists(files)]
  if (length(files) == 0) skip("no user-facing markdown files found")

  mismatches <- character()
  for (f in files) {
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")
    matches <- regmatches(
      src,
      gregexpr("\\[doi:([^]]+)\\]\\(https://doi\\.org/([^)]+)\\)",
               src, perl = TRUE)
    )[[1]]
    for (m in matches) {
      parts <- regmatches(
        m,
        regexec("\\[doi:([^]]+)\\]\\(https://doi\\.org/([^)]+)\\)",
                m, perl = TRUE)
      )[[1]]
      if (length(parts) >= 3 && parts[2] != parts[3]) {
        mismatches <- c(
          mismatches,
          sprintf("%s: text=`%s` url=`%s`",
                  basename(f), parts[2], parts[3])
        )
      }
    }
  }

  expect_equal(
    length(mismatches), 0,
    info = paste0(
      "Markdown DOI links where the visible text and URL disagree (likely a regex truncation bug):\n  ",
      paste(mismatches, collapse = "\n  ")
    )
  )
})
