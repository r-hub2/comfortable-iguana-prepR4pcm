# Bundle G #6 -- README and every vignette agree on the install command
#
# Catches the original issue #6 (Ayumi): pak::pak("...") in the README
# but remotes::install_github("...") in the getting-started vignette,
# silently confusing new users about which command to run.
#
# Static check: scan README.Rmd and every .Rmd in vignettes/ for any
# install_github / pak::pak / remotes::install_github / install.packages
# calls that target the package itself, and assert they all use the
# same command.

.find_install_lines <- function(file) {
  if (!file.exists(file)) return(character())
  src <- readLines(file, warn = FALSE)
  patterns <- c(
    'pak::pak\\("itchyshin/prepR4pcm"\\)',
    'remotes::install_github\\("itchyshin/prepR4pcm"\\)',
    'devtools::install_github\\("itchyshin/prepR4pcm"\\)'
  )
  hits <- character()
  for (p in patterns) {
    m <- grep(p, src, perl = TRUE)
    if (length(m) > 0) {
      hits <- c(hits, regmatches(src[m], regexpr(p, src[m], perl = TRUE)))
    }
  }
  hits
}


test_that("README and every vignette agree on the install command for prepR4pcm", {
  skip_on_cran()
  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible (running in installed-only mode)")

  # README and all vignettes
  files <- c(
    file.path(root, "README.Rmd"),
    list.files(file.path(root, "vignettes"),
               pattern = "\\.Rmd$", full.names = TRUE)
  )
  files <- files[file.exists(files)]
  if (length(files) == 0) skip("no README.Rmd or vignettes found")

  all_hits <- list()
  for (f in files) {
    hits <- unique(.find_install_lines(f))
    if (length(hits) > 0) {
      all_hits[[basename(f)]] <- hits
    }
  }

  if (length(all_hits) == 0) {
    skip("no install commands found in README/vignettes (nothing to check)")
  }

  # Every command across every file must be identical.
  flat <- unique(unlist(all_hits, use.names = FALSE))
  expect_equal(
    length(flat), 1L,
    info = paste0(
      "README and vignettes use inconsistent install commands. Found:\n",
      paste(
        sprintf("  %s -> %s",
                names(all_hits),
                vapply(all_hits, paste, character(1), collapse = ", ")),
        collapse = "\n"
      )
    )
  )
})
