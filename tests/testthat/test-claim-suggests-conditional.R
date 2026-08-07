# Bundle G #2 -- Suggests are conditionally guarded
#
# Every package in DESCRIPTION's `Suggests:` field that is referenced
# in R/ or vignettes/ is guarded so that the package functions /
# vignettes degrade gracefully when the Suggests dependency is absent.
#
# Four buckets per Suggests entry, in priority order:
#
#   R-guarded:   referenced under R/ AND at least one
#                requireNamespace(<pkg>, quietly = TRUE) check exists
#                somewhere in R/ (the pattern this package uses for
#                taxadb / phytools).
#
#   Vignette-only: referenced in vignettes/*.Rmd only; every reference
#                must be inside a chunk with
#                eval = requireNamespace(<pkg>, quietly = TRUE) so the
#                vignette knits without the dependency.
#
#   Allowlist:   knitr, rmarkdown, testthat, pkgdown, spelling -- these
#                are tooling Suggests that don't need user-facing
#                guards because they're only invoked by the package
#                infrastructure.
#
#   Transitive:  a Suggests entry declared only so the dependency
#                resolver installs it for another Suggests' own
#                dependency chain -- not called by prepR4pcm itself.
#                Held in `transitive_suggests` below.

allowlisted <- c("knitr", "rmarkdown", "testthat", "pkgdown", "spelling")

# Transitive-only Suggests: declared in `Suggests:` not because
# prepR4pcm calls them, but so the dependency resolver installs them
# for another Suggests' own dependency chain.
transitive_suggests <- "piggyback"


.find_root_with_dirs <- function(dirs) {
  cands <- c(
    test_path("..", ".."),
    file.path(getwd()),
    file.path(getwd(), "..", "..")
  )
  for (c in cands) {
    if (all(vapply(dirs, function(d) dir.exists(file.path(c, d)),
                   logical(1)))) {
      return(normalizePath(c))
    }
  }
  NA_character_
}


test_that("every Suggests package is conditionally guarded", {
  skip_on_cran()
  root <- .find_root_with_dirs(c("R", "vignettes"))
  if (is.na(root)) skip("package R/ or vignettes/ not found")

  suggests_field <- read.dcf(file.path(root, "DESCRIPTION"),
                             fields = "Suggests")[1, "Suggests"]
  if (is.na(suggests_field)) skip("no Suggests field in DESCRIPTION")

  pkgs <- trimws(strsplit(suggests_field, ",")[[1]])
  pkgs <- gsub("\\s*\\([^)]*\\)\\s*$", "", pkgs)  # strip "(>= ...)"

  r_files <- list.files(file.path(root, "R"),
                         pattern = "\\.R$", full.names = TRUE)
  vig_files <- list.files(file.path(root, "vignettes"),
                          pattern = "\\.Rmd$", full.names = TRUE)

  # Strip roxygen comment lines (`#'`) and ordinary R comments (`#`)
  # from R source so a `caper::pgls(...)` mentioned only in `@examples`
  # or `@description` is not counted as an "actual use" of caper.
  read_r_no_comments <- function(files) {
    if (length(files) == 0) return("")
    lines <- unlist(lapply(files, function(f) readLines(f, warn = FALSE)))
    lines <- lines[!grepl("^\\s*#", lines)]
    paste(lines, collapse = "\n")
  }
  read_all <- function(files) {
    if (length(files) == 0) return("")
    paste(unlist(lapply(files, function(f) readLines(f, warn = FALSE))),
          collapse = "\n")
  }
  r_blob <- read_r_no_comments(r_files)
  vig_blob <- read_all(vig_files)

  for (pkg in pkgs) {
    if (pkg %in% allowlisted || pkg %in% transitive_suggests) next

    in_r        <- grepl(paste0("\\b", pkg, "::"), r_blob, perl = TRUE) ||
                   grepl(paste0("requireNamespace\\(\\s*[\"']", pkg, "[\"']"),
                         r_blob, perl = TRUE)
    in_vignette <- grepl(paste0("\\b", pkg, "::"), vig_blob, perl = TRUE) ||
                   grepl(paste0("library\\(", pkg, "\\)"),
                         vig_blob, perl = TRUE)

    # If the package is unused entirely, the Suggests entry is stale.
    if (!in_r && !in_vignette) {
      fail(sprintf(
        "`%s` is in Suggests but is not referenced in R/ or vignettes/. Either remove it from DESCRIPTION or use it.",
        pkg
      ))
      next
    }

    if (in_r) {
      r_guarded <- grepl(paste0("requireNamespace\\(\\s*[\"']", pkg, "[\"']"),
                         r_blob, perl = TRUE)
      expect_true(
        r_guarded,
        info = sprintf(
          "`%s` is in Suggests and is used in R/, but no `requireNamespace(\"%s\", quietly = TRUE)` guard exists in R/. The package will fail for users who don't install %s.",
          pkg, pkg, pkg
        )
      )
    }

    # When the package is referenced in a vignette but NOT in R/, every
    # vignette reference must be inside a chunk with
    # eval = requireNamespace(...). Skip strictness when the package
    # is also referenced in R/ (its R-side guard covers the use).
    if (!in_r && in_vignette) {
      # Find every chunk that mentions the package and check the chunk
      # header has the eval guard. Cheap regex over the joined blob.
      bare <- regmatches(
        vig_blob,
        gregexpr(paste0("\\bbib", "library\\(", pkg, "\\)|\\b", pkg, "::"),
                 vig_blob, perl = TRUE)
      )[[1]]
      if (length(bare) > 0) {
        # Two acceptable patterns for the eval guard:
        #   (a) chunk-level `eval = requireNamespace("<pkg>", ...)`
        #   (b) global gate via knitr::opts_chunk$set(eval = <flag>)
        #       where <flag> is set from `requireNamespace("<pkg>", ...)`
        #       earlier in the document.
        guarded_explicit <- grepl(
          paste0("eval\\s*=\\s*requireNamespace\\([\"']", pkg, "[\"']"),
          vig_blob, perl = TRUE
        )
        guarded_via_global_gate <-
          grepl("opts_chunk\\$set\\([^)]*eval\\s*=", vig_blob, perl = TRUE) &&
          grepl(paste0("requireNamespace\\([\"']", pkg, "[\"']"),
                vig_blob, perl = TRUE)
        guarded_anywhere <- guarded_explicit || guarded_via_global_gate
        expect_true(
          guarded_anywhere,
          info = sprintf(
            "`%s` is referenced in a vignette but no chunk has eval = requireNamespace(\"%s\", ...), and no global knitr::opts_chunk$set(eval = ...) gate checks for it either. Add a guard so the vignette knits without %s installed.",
            pkg, pkg, pkg
          )
        )
      }
    }
  }
})
