# Bundle G #33 -- LICENSE / DESCRIPTION License field consistency
#
# Catches accidental licence drift (DESCRIPTION says MIT, LICENSE.md
# is GPL, etc.) and ensures the LICENSE / LICENSE.md files exist.

test_that("LICENSE files exist and the DESCRIPTION License field matches", {
  skip_on_cran()
  root <- .claim_root()
  if (is.na(root)) skip("source tree not accessible (running in installed-only mode)")

  desc_license <- unname(
    read.dcf(file.path(root, "DESCRIPTION"),
             fields = "License")[1, "License"]
  )
  expect_false(is.na(desc_license),
               info = "no License field in DESCRIPTION")

  if (grepl("file LICENSE", desc_license, fixed = TRUE)) {
    expect_true(
      file.exists(file.path(root, "LICENSE")),
      info = "DESCRIPTION says License: ... + file LICENSE, but LICENSE file is missing."
    )
  }

  # If LICENSE.md exists, its content should declare the same family
  # as DESCRIPTION's License: field. We do a loose substring check
  # because LICENSE.md often has full prose ("MIT License") while
  # DESCRIPTION uses short forms ("MIT + file LICENSE").
  lic_md <- file.path(root, "LICENSE.md")
  if (file.exists(lic_md)) {
    lic_md_text <- paste(readLines(lic_md, warn = FALSE), collapse = " ")
    short_form <- sub("\\s*\\+.*$", "", desc_license)
    short_form <- sub("\\s*\\(.*$", "", short_form)
    expect_match(
      lic_md_text,
      short_form,
      info = sprintf(
        "DESCRIPTION License: %s, but LICENSE.md does not mention `%s`. Make sure they describe the same licence.",
        desc_license, short_form
      )
    )
  }
})
