# Bundle G #42 -- deployed-site URL patterns
#
# When pkgdown sites are served at `<owner>.github.io/<repo>/`, the
# correct URL for an article is `/articles/<name>.html` -- NOT
# `/docs/articles/<name>.html`. The `/docs/` segment is the
# repository directory; GitHub Pages serves it from the root and the
# `/docs/` part is internal.
#
# Round-3 caught a README that linked to
# `https://itchyshin.github.io/prepR4pcm/docs/articles/...` -- those
# were 404s. This test fails the build if anyone reintroduces the
# pattern.

test_that("no `/docs/articles/` or `/docs/reference/` URLs in user-facing files", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  files <- c(
    file.path(root, "README.Rmd"),
    file.path(root, "README.md"),
    list.files(file.path(root, "vignettes"),
               pattern = "\\.Rmd$", full.names = TRUE),
    file.path(root, "_pkgdown.yml")
  )
  files <- files[file.exists(files)]
  if (length(files) == 0) skip("no user-facing files found")

  bad_lines <- character()
  pat <- "https?://[^/]*itchyshin\\.github\\.io/prepR4pcm/docs/(articles|reference)/"
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    for (i in seq_along(src)) {
      if (grepl(pat, src[i], perl = TRUE)) {
        bad_lines <- c(
          bad_lines,
          sprintf("%s:%d: %s", basename(f), i, trimws(src[i]))
        )
      }
    }
  }

  expect_equal(
    length(bad_lines), 0,
    info = paste0(
      "Found `/docs/articles/` or `/docs/reference/` URLs (these 404 on the deployed site).",
      " Use `/articles/...` and `/reference/...` instead:\n  ",
      paste(head(bad_lines, 20), collapse = "\n  "),
      if (length(bad_lines) > 20)
        sprintf("\n  ... and %d more", length(bad_lines) - 20)
      else ""
    )
  )
})
