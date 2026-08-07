# Bundle G #20 -- NAMESPACE export completeness
#
# Every R/ function that carries an `@export` roxygen tag has a
# corresponding `export(...)` line in NAMESPACE. devtools::document()
# normally keeps these in sync; this test catches stale builds and
# manual NAMESPACE edits that drop exports silently.

test_that("every @export'd function in R/ is exported in NAMESPACE", {
  skip_on_cran()
  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible (running in installed-only mode)")

  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  # Plain `export(name)` entries.
  ns_exports <- regmatches(
    ns,
    regexpr("(?<=^export\\()[^)]+(?=\\))", ns, perl = TRUE)
  )
  # S3 method registrations: `S3method(generic, class)` registers
  # `generic.class` as an S3 method. Functions tagged `@export` for
  # an S3 method (e.g. `#' @export` above `print.reconciliation`)
  # land here, not in `export()`. Treat both as equivalent.
  ns_s3 <- regmatches(
    ns,
    regexpr("(?<=^S3method\\()[^)]+(?=\\))", ns, perl = TRUE)
  )
  ns_s3_methods <- vapply(
    strsplit(ns_s3, "\\s*,\\s*"),
    function(parts) paste(parts, collapse = "."),
    character(1)
  )
  ns_all <- c(ns_exports, ns_s3_methods)

  r_files <- list.files(file.path(root, "R"),
                        pattern = "\\.R$", full.names = TRUE)
  if (length(r_files) == 0) skip("no R/*.R files found")

  declared_exports <- character()
  for (f in r_files) {
    src <- readLines(f, warn = FALSE)
    # Find `#' @export` lines and the function definition that follows.
    export_idx <- grep("^#'\\s*@export\\b", src)
    for (i in export_idx) {
      # Walk forward to the next non-roxygen line; that should be the
      # function declaration `name <- function(...)` or similar.
      j <- i + 1
      while (j <= length(src) && grepl("^#'", src[j])) j <- j + 1
      if (j > length(src)) next
      m <- regmatches(src[j], regexpr("^[A-Za-z_.][A-Za-z0-9_.]*", src[j]))
      if (length(m) == 1 && nzchar(m)) {
        declared_exports <- c(declared_exports, m)
      }
    }
  }
  declared_exports <- unique(declared_exports)
  if (length(declared_exports) == 0) skip("no @export'd functions found in R/")

  missing <- setdiff(declared_exports, ns_all)
  expect_equal(
    length(missing), 0,
    info = sprintf(
      "Functions tagged `@export` in R/ but missing from NAMESPACE: %s. Run devtools::document() to regenerate NAMESPACE.",
      paste(missing, collapse = ", ")
    )
  )
})
