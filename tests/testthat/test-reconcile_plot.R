test_that("reconcile_plot produces a bar chart without error", {
  df <- data.frame(species = c("Homo sapiens", "Pan_troglodytes", "Missing sp"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f))
  grDevices::pdf(f)
  expect_invisible(reconcile_plot(result))
  grDevices::dev.off()

  expect_true(file.exists(f))
})

test_that("reconcile_plot produces a pie chart without error", {
  df <- data.frame(species = c("Homo sapiens", "Pan_troglodytes", "Missing sp"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f))
  grDevices::pdf(f)
  expect_invisible(reconcile_plot(result, type = "pie"))
  grDevices::dev.off()

  expect_true(file.exists(f))
})

test_that("reconcile_plot rejects non-reconciliation input", {
  expect_error(reconcile_plot(list(a = 1)), "reconciliation")
  expect_error(reconcile_plot("not an object"), "reconciliation")
})

test_that("reconcile_plot works when all matches are exact", {
  df <- data.frame(species = c("Homo_sapiens", "Pan_troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # Verify all are exact (no unresolved)
  expect_equal(result$counts$n_unresolved_x, 0)
  expect_equal(result$counts$n_unresolved_y, 0)

  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f))
  grDevices::pdf(f)
  expect_invisible(reconcile_plot(result))
  grDevices::dev.off()

  expect_true(file.exists(f))
})

test_that("reconcile_plot returns input invisibly for piping", {
  df <- data.frame(species = c("Homo_sapiens", "Pan_troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  f <- tempfile(fileext = ".pdf")
  on.exit(unlink(f))
  grDevices::pdf(f)
  out <- reconcile_plot(result)
  grDevices::dev.off()

  expect_identical(out, result)
})
