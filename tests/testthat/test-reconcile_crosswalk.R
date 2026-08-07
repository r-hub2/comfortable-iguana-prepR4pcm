test_that("reconcile_crosswalk produces overrides table", {
  xw <- data.frame(
    from = c("Species A", "Species B", "Species C"),
    to = c("Species A", "Species B_alt", "Species C"),
    type = c("1BL to 1BT", "Many BL to 1BT", "1BL to 1BT"),
    stringsAsFactors = FALSE
  )

  result <- reconcile_crosswalk(xw, "from", "to", match_type_col = "type")

  expect_true(is.data.frame(result))
  expect_true(all(c("name_x", "name_y", "user_note") %in% names(result)))
  # Only Species B has different from/to names, so only 1 override
  expect_equal(nrow(result), 1L)
  expect_equal(result$name_x, "Species B")
})

test_that("reconcile_crosswalk one_to_one_only filters correctly", {
  xw <- data.frame(
    from = c("Sp A", "Sp B", "Sp C"),
    to = c("Sp A2", "Sp B2", "Sp C2"),
    type = c("1BL to 1BT", "Many BL to 1BT", "1BL to many BT"),
    stringsAsFactors = FALSE
  )

  result <- reconcile_crosswalk(
    xw,
    "from",
    "to",
    match_type_col = "type",
    one_to_one_only = TRUE
  )

  expect_equal(nrow(result), 1L)
  expect_equal(result$name_x, "Sp A")
})

test_that("reconcile_crosswalk warns when complex rows are kept automatically", {
  xw <- data.frame(
    from = c("Sp A", "Sp B", "Sp C"),
    to = c("Sp A2", "Sp B2", "Sp C2"),
    type = c("1BL to 1BT", "Many BL to 1BT", "1BL to many BT"),
    stringsAsFactors = FALSE
  )

  expect_message(
    result <- reconcile_crosswalk(xw, "from", "to", match_type_col = "type"),
    "non-one-to-one crosswalk"
  )

  expect_equal(nrow(result), 3L)
})

test_that("reconcile_crosswalk reads from CSV path", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  xw <- data.frame(
    name_from = c("Alpha beta", "Gamma delta"),
    name_to = c("Alpha beta", "Gamma epsilon"),
    stringsAsFactors = FALSE
  )
  write.csv(xw, tmp, row.names = FALSE)

  result <- reconcile_crosswalk(tmp, "name_from", "name_to")
  expect_equal(nrow(result), 1L)
})

test_that("reconcile_crosswalk errors on missing columns", {
  xw <- data.frame(a = "x", b = "y", stringsAsFactors = FALSE)
  expect_error(
    reconcile_crosswalk(xw, "nonexistent", "b"),
    "not found"
  )
})

test_that("reconcile_crosswalk removes empty rows", {
  xw <- data.frame(
    from = c("Sp A", "", NA),
    to = c("Sp B", "Sp C", "Sp D"),
    stringsAsFactors = FALSE
  )

  result <- reconcile_crosswalk(xw, "from", "to")
  expect_equal(nrow(result), 1L)
})

# Issue #8b: file-format support beyond CSV ------------------------

test_that("reconcile_crosswalk reads a CSV file path", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  write.csv(
    data.frame(
      from = c("Sp A", "Sp B"),
      to = c("Sp A2", "Sp B2"),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )
  result <- reconcile_crosswalk(path, "from", "to")
  expect_equal(nrow(result), 2L)
  expect_equal(result$name_x, c("Sp A", "Sp B"))
})

test_that("reconcile_crosswalk reads a TSV file path (issue #8b)", {
  path <- tempfile(fileext = ".tsv")
  on.exit(unlink(path), add = TRUE)
  write.table(
    data.frame(
      from = c("Sp A", "Sp B"),
      to = c("Sp A2", "Sp B2"),
      stringsAsFactors = FALSE
    ),
    path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  result <- reconcile_crosswalk(path, "from", "to")
  expect_equal(nrow(result), 2L)
  expect_equal(result$name_x, c("Sp A", "Sp B"))
})

test_that("reconcile_crosswalk reads a TXT (tab-delimited) file path (issue #8b)", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  write.table(
    data.frame(
      from = c("Sp A", "Sp B"),
      to = c("Sp A2", "Sp B2"),
      stringsAsFactors = FALSE
    ),
    path,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  result <- reconcile_crosswalk(path, "from", "to")
  expect_equal(nrow(result), 2L)
})

test_that("reconcile_crosswalk errors on unsupported file extension", {
  path <- tempfile(fileext = ".xls")
  on.exit(unlink(path), add = TRUE)
  writeLines("from\tto\nSpA\tSpA2", path)
  expect_error(
    reconcile_crosswalk(path, "from", "to"),
    "Unsupported"
  )
})
