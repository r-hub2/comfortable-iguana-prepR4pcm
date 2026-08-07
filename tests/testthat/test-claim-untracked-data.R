# Bundle G #30 -- bundled-data documentation completeness
#
# Every file in data/ has a corresponding @docType data block in R/;
# every documented dataset has an .rda file in data/. Catches:
#
#   * "shipped data without a help page"
#   * "documented a dataset that doesn't exist"
#
# Both have happened in past R packages and are silent failures.

test_that("every file in data/ has a corresponding documented object", {
  skip_on_cran()
  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible (running in installed-only mode)")

  rda_files <- list.files(file.path(root, "data"),
                          pattern = "\\.rda$", full.names = FALSE)
  if (length(rda_files) == 0) skip("no .rda files in data/")

  # Each .rda contains an object whose name should match the file
  # stem; the corresponding Rd is at man/<name>.Rd.
  for (f in rda_files) {
    name <- sub("\\.rda$", "", f)
    rd_path <- file.path(root, "man", paste0(name, ".Rd"))
    expect_true(
      file.exists(rd_path),
      info = sprintf(
        "data/%s has no corresponding man/%s.Rd. Add an `@docType data` block in R/data.R for this dataset, or remove the .rda.",
        f, name
      )
    )
  }
})


test_that("every documented dataset has a corresponding .rda in data/", {
  skip_on_cran()
  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible (running in installed-only mode)")

  # Datasets are documented with `\docType{data}` in their Rd source.
  rd_files <- list.files(file.path(root, "man"),
                         pattern = "\\.Rd$", full.names = TRUE)
  for (rd in rd_files) {
    src <- paste(readLines(rd, warn = FALSE), collapse = "\n")
    if (grepl("\\\\docType\\{data\\}", src)) {
      name <- sub("\\.Rd$", "", basename(rd))
      rda_path <- file.path(root, "data", paste0(name, ".rda"))
      expect_true(
        file.exists(rda_path),
        info = sprintf(
          "man/%s claims to document a dataset (\\\\docType{data}) but data/%s.rda does not exist.",
          basename(rd), name
        )
      )
    }
  }
})
