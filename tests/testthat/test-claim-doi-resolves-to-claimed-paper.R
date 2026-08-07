# Bundle G #44 -- DOI resolves to a paper whose title plausibly
# matches what the help page claims.
#
# Caught in PR #28: our `delhey_subset` Rd cited Delhey 2019
# *American Naturalist* 194:13-27 with `doi:10.1086/703588`. The
# DOI was wrong -- it resolves to a 2018 book review of
# *Synthetic Biology: A Very Short Introduction* in *Quarterly
# Review of Biology*. The format-only DOI test passed because the
# DOI string was syntactically valid; the live HEAD-check passed
# because the URL existed. Neither caught the title mismatch.
#
# This test queries Crossref's API for each `@source`/`@references`
# DOI in the package and asserts that the returned title contains
# at least one keyword from the dataset's claimed citation. Catches
# the next "DOI points at a different paper" bug.

# Live test: skip on CRAN, skip if offline.

# A small allowlist of {DOI -> keywords that must appear in the
# Crossref title} entries. Keep the keyword list short (1-3 words);
# the test passes if ANY of the listed keywords appear (case-
# insensitive) in the resolved title.
.doi_keyword_map <- list(
  # Bird datasets
  `10.1111/ele.13898`         = c("AVONET", "ecological"),       # Tobias 2022
  `10.1038/s41597-023-02837-1`= c("nest", "global"),              # Chia 2023
  `10.1111/ele.13233`         = c("ecogeographical", "rainfall"), # Delhey 2019
  `10.1038/nature11631`       = c("global diversity", "birds"),   # Jetz 2012
  # taxadb / ape
  `10.1111/2041-210X.13440`   = c("taxadb", "taxonomic"),         # Norman 2020
  `10.1093/bioinformatics/bty633` = c("ape 5.0", "phylogenetics"), # Paradis & Schliep 2019
  # Mammal datasets
  `10.1890/15-0846R.1`        = c("amniote", "life-history"),     # Myhrvold 2015
  `10.1890/08-1494.1`         = c("PanTHERIA", "mammals"),        # Jones 2009
  `10.1371/journal.pbio.3002658` = c("tetrapod", "traits"),       # Moura 2024
  # Mammal phylogeny references
  `10.1371/journal.pbio.3000494` = c("mammal", "tree"),           # Upham 2019
  `10.1016/j.ympev.2014.11.001`  = c("mammal", "extinct"),        # Faurby & Svenning 2015
  `10.1038/nature05634`          = c("delayed", "mammals")        # Bininda-Emonds 2007
)


test_that("each DOI in the @source allowlist resolves to a paper whose title matches the expected topic", {
  skip_on_cran()
  testthat::skip_if_offline()

  for (doi in names(.doi_keyword_map)) {
    keywords <- .doi_keyword_map[[doi]]
    url <- paste0("https://api.crossref.org/works/", doi)
    title <- tryCatch(
      {
        raw <- system(
          paste0("curl -s --max-time 15 ", shQuote(url)),
          intern = TRUE
        )
        if (length(raw) == 0 || !nzchar(paste(raw, collapse = ""))) {
          return(NA_character_)
        }
        # Cheap JSON title extraction: find "title":["..."] pattern
        joined <- paste(raw, collapse = " ")
        m <- regmatches(joined,
                        regexpr('"title":\\s*\\[\\s*"[^"]+"', joined,
                                perl = TRUE))
        if (length(m) == 0 || !nzchar(m)) return(NA_character_)
        sub('^"title":\\s*\\[\\s*"', "", m)
      },
      error = function(e) NA_character_
    )

    if (is.na(title)) {
      skip(sprintf("Crossref unavailable for DOI %s", doi))
    }

    matched <- any(vapply(
      keywords,
      function(k) grepl(k, title, ignore.case = TRUE, fixed = FALSE),
      logical(1)
    ))
    expect_true(
      matched,
      info = sprintf(
        "DOI %s resolves to paper titled '%s' but the help page citation claims a topic matching one of: %s",
        doi, substr(title, 1, 100),
        paste(keywords, collapse = ", ")
      )
    )
  }
})


test_that("every DOI cited in R/data.R appears in the keyword allowlist", {
  skip_on_cran()

  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible")

  data_rd <- file.path(root, "R", "data.R")
  if (!file.exists(data_rd)) skip("R/data.R not present")

  src <- paste(readLines(data_rd, warn = FALSE), collapse = "\n")
  doi_pattern <- "10\\.\\d{4,9}/[-._;()/:A-Z0-9a-z]+"
  matches <- regmatches(src, gregexpr(doi_pattern, src, perl = TRUE))[[1]]
  matches <- gsub("[).,;}]+$", "", matches)
  matches <- unique(matches)

  unallowlisted <- setdiff(matches, names(.doi_keyword_map))
  expect_equal(
    length(unallowlisted), 0,
    info = paste0(
      "DOIs cited in R/data.R but missing from .doi_keyword_map ",
      "(in test-claim-doi-resolves-to-claimed-paper.R). Add each ",
      "to the map with a 1-3 word keyword list so the resolution ",
      "test exercises it:\n  ",
      paste(unallowlisted, collapse = "\n  ")
    )
  )
})
