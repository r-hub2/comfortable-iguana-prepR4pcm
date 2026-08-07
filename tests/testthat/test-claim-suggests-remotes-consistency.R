# Bundle G #43 -- CRAN dependency metadata consistency
#
# CRAN does not accept DESCRIPTION `Remotes:` fields. Optional GitHub-only
# integrations therefore must either stay out of package dependency fields
# and be guarded at runtime, or be resolvable through a working
# `Additional_repositories:` entry.
#
# We approximate "is on CRAN" via a static allowlist of known CRAN
# packages used by this project, since hitting the CRAN API on every
# test is slow and CI-fragile. If a NEW non-CRAN dependency is added
# that isn't on this allowlist, the test fails -- forcing the
# developer to either confirm it's on CRAN or keep it out of
# DESCRIPTION dependency fields.

# Packages we know are on CRAN (any release year). When adding a new
# Suggests / Imports package that's on CRAN, append it here.
.cran_allowlist <- c(
  "ape", "cli", "rlang", "tibble",
  "caper", "clootl", "digest", "dplyr", "fishtree", "httr2", "knitr",
  "MCMCglmm", "phytools", "piggyback", "pkgdown", "readr", "rgnparser",
  "rmarkdown", "rotl", "rtrees", "spelling", "stringr", "taxadb",
  "testthat",
  # standard packages bundled with R
  "stats", "tools", "utils", "graphics", "grDevices", "methods"
)



test_that("DESCRIPTION has no CRAN-incompatible Remotes field", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  expect_false(
    "Remotes" %in% colnames(desc),
    info = "CRAN incoming checks flag `Remotes:` as an unknown DESCRIPTION field."
  )
})


test_that("every declared package dependency is CRAN/mainstream", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  parse_field <- function(name) {
    if (!name %in% colnames(desc)) return(character())
    v <- desc[1, name]
    if (is.na(v)) return(character())
    parts <- trimws(strsplit(v, ",")[[1]])
    parts <- gsub("\\s*\\([^)]*\\)\\s*$", "", parts)  # strip "(>= ...)"
    parts <- gsub(",\\s*$", "", parts)
    parts[nzchar(parts)]
  }

  imports  <- parse_field("Imports")
  suggests <- parse_field("Suggests")
  depends  <- parse_field("Depends")
  # `Depends` may include `R (>= ...)` -- strip `R` itself
  depends <- depends[depends != "R"]
  declared <- c(imports, suggests, depends)

  unaccounted <- setdiff(declared, .cran_allowlist)
  expect_equal(
    length(unaccounted), 0,
    info = paste0(
      "Imports / Suggests / Depends entries that are neither on the ",
      "CRAN allowlist. Either add them to the allowlist (in this test ",
      "file) after confirming they are in a mainstream repository, or ",
      "remove them from DESCRIPTION and keep them as guarded runtime-only ",
      "integrations:\n  ",
      paste(unaccounted, collapse = "\n  ")
    )
  )
})


test_that("GitHub-only optional integrations stay out of dependency fields", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  parse_field <- function(name) {
    if (!name %in% colnames(desc)) return(character())
    v <- desc[1, name]
    if (is.na(v)) return(character())
    parts <- trimws(strsplit(v, ",")[[1]])
    parts <- gsub("\\s*\\([^)]*\\)\\s*$", "", parts)
    parts <- gsub(",\\s*$", "", parts)
    parts[nzchar(parts)]
  }

  dependency_fields <- c(
    parse_field("Imports"),
    parse_field("Suggests"),
    parse_field("Enhances")
  )
  github_only <- c("datelife", "U.PhyloMaker", "V.PhyloMaker", "V.PhyloMaker2")

  expect_equal(
    intersect(github_only, dependency_fields),
    character(),
    info = paste(
      "GitHub-only optional integrations should be guarded at runtime,",
      "not declared in CRAN dependency fields."
    )
  )
})
