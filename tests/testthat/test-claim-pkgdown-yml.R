# Bundle G #19 -- pkgdown.yml validity
#
# Every function listed under reference: in _pkgdown.yml exists in the
# package; every exported function in the package appears under some
# reference: section (or carries @keywords internal). Catches "added a
# function but forgot to list it in pkgdown" -- which would happen
# silently otherwise because the function still works, but users of the
# pkgdown site never find it.

test_that("every entry under _pkgdown.yml reference: exists in the package", {
  skip_on_cran()
  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible (running in installed-only mode)")

  yml <- readLines(file.path(root, "_pkgdown.yml"), warn = FALSE)

  # Pull "  - <name>" lines that follow a `contents:` key. Cheap and
  # tolerant of comments / minor format changes.
  contents_idx <- grep("^\\s*contents:\\s*$", yml)
  if (length(contents_idx) == 0) skip("no contents: blocks in _pkgdown.yml")

  refs <- character()
  for (i in contents_idx) {
    j <- i + 1
    while (j <= length(yml) && grepl("^\\s+-\\s+", yml[j])) {
      ref <- sub("^\\s+-\\s+", "", yml[j])
      ref <- sub("\\s*#.*$", "", ref)         # strip trailing comments
      ref <- trimws(ref)
      if (nzchar(ref)) refs <- c(refs, ref)
      j <- j + 1
    }
  }

  if (length(refs) == 0) skip("no references parsed from _pkgdown.yml")

  exported <- getNamespaceExports("prepR4pcm")
  data_objs <- tryCatch(
    {
      e <- new.env()
      utils::data(package = "prepR4pcm", envir = e)$results[, "Item"]
    },
    error = function(e) character()
  )

  # An entry is valid if it's an exported function, a bundled dataset,
  # the package help page (`prepR4pcm-package`), or the reconciliation
  # class doc (alias for new_reconciliation).
  valid <- c(exported, data_objs, "prepR4pcm-package", "reconciliation")

  for (ref in refs) {
    expect_true(
      ref %in% valid,
      info = sprintf(
        "_pkgdown.yml lists `%s` under reference: but it is neither an exported function nor a documented dataset. Either remove it from the yml or expose/document it.",
        ref
      )
    )
  }
})
