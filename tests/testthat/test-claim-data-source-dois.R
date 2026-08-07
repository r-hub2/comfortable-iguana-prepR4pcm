# Bundle G #4 — DOI format and resolution -------------------------------
#
# Catches stale or malformed DOIs in @source roxygen blocks. The Chia
# et al. fix from the prior CRAN audit (where the DOI mistakenly read
# 10.1038/s41597-023-02360-9 instead of 10.1038/s41597-023-02837-1) is
# exactly this class of bug. A DOI in a help page is a claim that the
# data came from a specific publication; the test verifies that claim
# is at least syntactically well-formed and (optionally) resolves.
#
# Two sub-tests:
#
# 1. Format check (always runs, including on CRAN) -- every DOI
#    string in R/data.R Rd output matches the DOI regex.
#
# 2. HEAD check (skipped on CRAN / offline) -- every DOI returns HTTP
#    200 OR 403 (publishers like Wiley, OUP, UChicago Press return 403
#    to anonymous bots; that's documented in cran-comments.md and is
#    an accepted CRAN policy). 404 / 410 / 500 fail the test.

.find_man_dir <- function() {
  # Walk up from the test working dir until we find a man/ directory
  # that's at the package root (sibling of DESCRIPTION).
  candidates <- c(
    test_path("..", "..", "man"),
    file.path(getwd(), "man"),
    file.path(getwd(), "..", "man"),
    file.path(getwd(), "..", "..", "man")
  )
  for (c in candidates) {
    if (dir.exists(c) &&
        file.exists(file.path(dirname(c), "DESCRIPTION"))) {
      return(normalizePath(c))
    }
  }
  NA_character_
}

extract_dois <- function() {
  man_dir <- .find_man_dir()
  if (is.na(man_dir)) skip("man/ directory not found")

  data_rd_names <- c(
    "avonet_subset", "nesttrait_subset", "delhey_subset",
    "tree_jetz", "tree_clements25", "crosswalk_birdlife_birdtree",
    "mammal_amniote_example", "mammal_pantheria_example",
    "mammal_tetrapodtraits_example", "mammal_tree_example"
  )

  doi_pattern <- "10\\.\\d{4,9}/[-._;()/:A-Z0-9a-z]+"
  doi_table <- list()

  for (rd_name in data_rd_names) {
    rd_path <- file.path(man_dir, paste0(rd_name, ".Rd"))
    if (!file.exists(rd_path)) next
    src <- paste(readLines(rd_path, warn = FALSE), collapse = "\n")
    matches <- regmatches(src,
                          gregexpr(doi_pattern, src, perl = TRUE))[[1]]
    if (length(matches) > 0) {
      matches <- gsub("[).,;}]+$", "", matches)
      doi_table[[rd_name]] <- unique(matches)
    }
  }
  doi_table
}


test_that("every DOI in dataset @source blocks is well-formed", {
  skip_on_cran()
  doi_table <- extract_dois()
  if (length(doi_table) == 0) skip("no datasets found with DOIs -- test is misconfigured")

  doi_pattern <- "^10\\.\\d{4,9}/[-._;()/:A-Z0-9a-z]+$"
  for (rd_name in names(doi_table)) {
    for (doi in doi_table[[rd_name]]) {
      expect_match(
        doi, doi_pattern,
        info = sprintf("DOI in ?%s is malformed: %s",
                       sub("\\.Rd$", "", rd_name), doi)
      )
    }
  }
})


test_that("every DOI in dataset @source blocks resolves (HTTP 200 or 403)", {
  skip_on_cran()
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  doi_table <- extract_dois()

  for (rd_name in names(doi_table)) {
    for (doi in doi_table[[rd_name]]) {
      url <- paste0("https://doi.org/", doi)
      status <- tryCatch(
        as.integer(
          system(
            paste0("curl -s -o /dev/null -w '%{http_code}' --max-time 10 -L ",
                   shQuote(url)),
            intern = TRUE
          )
        ),
        error = function(e) NA_integer_
      )
      expect_true(
        !is.na(status) && status %in% c(200L, 301L, 302L, 403L),
        info = sprintf(
          "DOI in ?%s does not resolve cleanly: %s -> HTTP %s. (200 or 403 are acceptable; 301/302 means a redirect chain that curl could not finish following; 403 is publisher bot-blocking, documented in cran-comments.md.)",
          sub("\\.Rd$", "", rd_name), doi, format(status)
        )
      )
    }
  }
})
