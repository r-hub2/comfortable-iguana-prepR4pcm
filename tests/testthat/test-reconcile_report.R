test_that("reconcile_report() generates a valid HTML file", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")

  result <- reconcile_tree(avonet_subset, tree_jetz,
                           x_species = "Species1", authority = NULL,
                           quiet = TRUE)

  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  out <- reconcile_report(result, file = f, open = FALSE)

  expect_true(file.exists(f))
  expect_identical(out, f)

  html <- readLines(f)
  content <- paste(html, collapse = "\n")

  # Basic structure

  expect_true(grepl("<!DOCTYPE html>", content, fixed = TRUE))
  expect_true(grepl("<title>", content, fixed = TRUE))
  expect_true(grepl("Match summary", content, fixed = TRUE))

  # Contains match type rows

  expect_true(grepl("Exact", content, fixed = TRUE))
  expect_true(grepl("Normalized", content, fixed = TRUE))

  # Has metadata section
  expect_true(grepl("data vs tree", content, fixed = TRUE))
})

test_that("reconcile_report() includes normalised and unresolved sections", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")

  result <- reconcile_tree(avonet_subset, tree_jetz,
                           x_species = "Species1", authority = NULL,
                           quiet = TRUE)

  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  reconcile_report(result, file = f, open = FALSE)
  content <- paste(readLines(f), collapse = "\n")

  # Should have normalised matches section (spaces vs underscores)
  expect_true(grepl("Normalized matches", content, fixed = TRUE))

  # Should have unresolved sections
  expect_true(grepl("Unresolved", content, fixed = TRUE))
})

test_that("reconcile_report() uses custom title", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")

  result <- reconcile_tree(avonet_subset, tree_jetz,
                           x_species = "Species1", authority = NULL,
                           quiet = TRUE)

  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  reconcile_report(result, file = f, title = "My Custom Title", open = FALSE)
  content <- paste(readLines(f), collapse = "\n")

  expect_true(grepl("My Custom Title", content, fixed = TRUE))
})

test_that("reconcile_report() rejects non-.html file path", {
  data(avonet_subset, package = "prepR4pcm")
  data(tree_jetz, package = "prepR4pcm")

  result <- reconcile_tree(avonet_subset, tree_jetz,
                           x_species = "Species1", authority = NULL,
                           quiet = TRUE)

  expect_error(reconcile_report(result, file = "report.pdf"),
               "must end in.*\\.html")
})

test_that("reconcile_report() rejects non-reconciliation input", {
  expect_error(reconcile_report(list(a = 1), file = "out.html"),
               "reconciliation")
})

test_that("reconcile_report() works with all-exact reconciliation", {
  # Create a reconciliation where everything matches exactly
  df <- data.frame(species = c("Corvus_corax", "Corvus_corone"),
                   trait = 1:2, stringsAsFactors = FALSE)
  result <- reconcile_data(df, df,
                           x_species = "species", y_species = "species",
                           authority = NULL, quiet = TRUE)

  f <- tempfile(fileext = ".html")
  on.exit(unlink(f), add = TRUE)

  reconcile_report(result, file = f, open = FALSE)
  expect_true(file.exists(f))

  content <- paste(readLines(f), collapse = "\n")
  expect_true(grepl("Match summary", content, fixed = TRUE))
})
