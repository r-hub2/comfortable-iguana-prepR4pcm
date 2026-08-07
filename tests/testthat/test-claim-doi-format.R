# Bundle G #4 (extension) -- DOI format consistency
#
# Some claim-parity work overlaps with test-claim-data-source-dois.R
# which only inspects R/data.R Rd. This file extends the format check
# to inst/CITATION, README.Rmd, and every vignette. Doing so catches
# stale DOIs that drifted in user-facing prose (the bird workflow
# vignette previously had the wrong Chia et al. DOI; that bug class
# is now structurally caught).

test_that("every DOI in user-facing files is well-formed", {
  skip_on_cran()
  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible (running in installed-only mode)")

  files <- c(
    file.path(root, "README.Rmd"),
    file.path(root, "README.md"),
    file.path(root, "inst", "CITATION"),
    list.files(file.path(root, "vignettes"),
               pattern = "\\.(Rmd|bib)$", full.names = TRUE),
    list.files(file.path(root, "man"),
               pattern = "\\.Rd$", full.names = TRUE)
  )
  files <- files[file.exists(files)]

  doi_pattern <- "10\\.\\d{4,9}/[-._;()/:A-Z0-9a-z]+"
  doi_strict <- "^10\\.\\d{4,9}/[-._;()/:A-Z0-9a-z]+$"

  for (f in files) {
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")
    matches <- regmatches(src, gregexpr(doi_pattern, src, perl = TRUE))[[1]]
    if (length(matches) == 0) next
    matches <- gsub("[).,;}\"]+$", "", matches)
    matches <- unique(matches)
    for (doi in matches) {
      expect_match(
        doi, doi_strict,
        info = sprintf("malformed DOI in %s: %s",
                       sub(paste0("^", root, "/"), "", f),
                       doi)
      )
    }
  }
})
