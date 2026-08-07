test_that("fuzzy matching catches obvious typos", {
  result <- pr_fuzzy_match(
    names_x = c("Homo sapiens", "Parus mejor"),
    names_y = c("Homo sapiens", "Parus major"),
    threshold = 0.8
  )
  # "Parus mejor" vs "Parus major" should match (small typo)
  expect_true(nrow(result) >= 1)
  typo_match <- result[result$name_x == "Parus mejor", ]
  expect_equal(nrow(typo_match), 1)
  expect_equal(typo_match$name_y, "Parus major")
  expect_true(typo_match$score >= 0.8)
})

test_that("fuzzy matching respects threshold", {
  result_high <- pr_fuzzy_match(
    names_x = c("Abc def"),
    names_y = c("Xyz ghi"),
    threshold = 0.99
  )
  expect_equal(nrow(result_high), 0)
})

test_that("fuzzy matching handles empty inputs", {
  result <- pr_fuzzy_match(
    names_x = character(),
    names_y = c("Homo sapiens"),
    threshold = 0.9
  )
  expect_equal(nrow(result), 0)

  result2 <- pr_fuzzy_match(
    names_x = c("Homo sapiens"),
    names_y = character(),
    threshold = 0.9
  )
  expect_equal(nrow(result2), 0)
})

test_that("fuzzy matching returns correct columns", {
  result <- pr_fuzzy_match(
    names_x = c("Parus mejor"),
    names_y = c("Parus major"),
    threshold = 0.8
  )
  expect_true(all(c("name_x", "name_y", "score", "notes") %in% names(result)))
})

test_that("fuzzy matching ignores single-word names", {
  result <- pr_fuzzy_match(
    names_x = c("Homo"),
    names_y = c("Homo sapiens"),
    threshold = 0.5
  )
  expect_equal(nrow(result), 0)
})

test_that("cascade runs fuzzy stage when enabled", {
  result <- pr_run_cascade(
    names_x = c("Parus mejor", "Homo sapiens"),
    names_y = c("Parus major", "Homo sapiens"),
    authority = NULL,
    fuzzy = TRUE,
    fuzzy_threshold = 0.8
  )
  fuzzy_rows <- result[result$match_type %in% c("fuzzy", "flagged"), ]
  expect_true(nrow(fuzzy_rows) >= 1)
  expect_equal(fuzzy_rows$name_x[1], "Parus mejor")
  expect_equal(fuzzy_rows$name_y[1], "Parus major")
})

test_that("cascade skips fuzzy stage when disabled", {
  result <- pr_run_cascade(
    names_x = c("Parus mejor"),
    names_y = c("Parus major"),
    authority = NULL,
    fuzzy = FALSE
  )
  fuzzy_rows <- result[result$match_type == "fuzzy", ]
  expect_equal(nrow(fuzzy_rows), 0)
  # Should be unresolved instead
  unresolved <- result[result$match_type == "unresolved", ]
  expect_true(nrow(unresolved) >= 1)
})
