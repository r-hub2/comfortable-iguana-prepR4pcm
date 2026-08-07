test_that("exact matches are found", {
  result <- pr_run_cascade(
    names_x = c("Homo sapiens", "Pan troglodytes"),
    names_y = c("Homo sapiens", "Gorilla gorilla"),
    authority = NULL
  )
  exact <- result[result$match_type == "exact", ]
  expect_equal(nrow(exact), 1)
  expect_equal(exact$name_x, "Homo sapiens")
})

test_that("normalised matches are found", {
  result <- pr_run_cascade(
    names_x = c("Homo_sapiens", "Pan troglodytes"),
    names_y = c("Homo sapiens", "Gorilla gorilla"),
    authority = NULL
  )
  norm <- result[result$match_type == "normalized", ]
  expect_equal(nrow(norm), 1)
  expect_equal(norm$name_x, "Homo_sapiens")
  expect_equal(norm$name_y, "Homo sapiens")
})

test_that("unresolved names are reported", {
  result <- pr_run_cascade(
    names_x = c("Homo sapiens", "Missing species"),
    names_y = c("Homo sapiens", "Other species"),
    authority = NULL
  )
  unresolved <- result[result$match_type == "unresolved", ]
  expect_true(nrow(unresolved) >= 2)
})

test_that("manual overrides take priority", {
  overrides <- data.frame(
    name_x = "Custom name",
    name_y = "Target species",
    user_note = "Test override",
    stringsAsFactors = FALSE
  )
  result <- pr_run_cascade(
    names_x = c("Custom name", "Homo sapiens"),
    names_y = c("Target species", "Homo sapiens"),
    authority = NULL,
    overrides = overrides
  )
  manual <- result[result$match_type == "manual", ]
  expect_equal(nrow(manual), 1)
  expect_equal(manual$name_x, "Custom name")
})

test_that("manual overrides can preempt exact matches", {
  overrides <- data.frame(
    name_x = "Species alpha",
    name_y = "Species beta",
    user_note = "Taxonomy crosswalk",
    stringsAsFactors = FALSE
  )

  result <- pr_run_cascade(
    names_x = c("Species alpha", "Species beta"),
    names_y = c("Species alpha", "Species beta"),
    authority = NULL,
    overrides = overrides
  )

  manual <- result[
    !is.na(result$name_x) &
      result$name_x == "Species alpha",
  ]
  exact <- result[
    !is.na(result$name_x) &
      result$name_x == "Species beta",
  ]

  expect_equal(manual$match_type, "manual")
  expect_equal(manual$name_y, "Species beta")
  expect_equal(exact$match_type, "unresolved")
})

test_that("cascade does not double-match names", {
  result <- pr_run_cascade(
    names_x = c("Homo sapiens"),
    names_y = c("Homo sapiens"),
    authority = NULL
  )
  matched <- result[result$in_x & result$in_y, ]
  expect_equal(nrow(matched), 1)
  expect_equal(matched$match_type, "exact")
})

test_that("empty inputs produce empty mapping", {
  result <- pr_run_cascade(
    names_x = character(),
    names_y = character(),
    authority = NULL
  )
  expect_equal(nrow(result), 0)
})
