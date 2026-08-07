test_that("reconcile_summary prints without error (via print method)", {
  df <- data.frame(species = c("Homo sapiens", "Missing sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # As of issue #12 the report renders via print(), not as a side
  # effect of the function body. Verify both paths.
  s <- reconcile_summary(result)
  expect_output(print(s), "Reconciliation Report")
})

test_that("reconcile_summary returns data.frame format", {
  df <- data.frame(species = c("Homo sapiens", "Pan_troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  summ <- reconcile_summary(result, format = "data.frame")
  expect_s3_class(summ, "reconciliation_summary")
  expect_true("tables" %in% names(summ))
  expect_true("by_type" %in% names(summ$tables))
})

test_that("reconcile_summary detail levels work", {
  df <- data.frame(species = c("Homo sapiens", "Pan_troglodytes", "Missing sp"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # As of issue #12 the report renders via print(), not the function body.
  expect_output(print(reconcile_summary(result, detail = "brief")),
                "Match Summary")
  expect_output(print(reconcile_summary(result, detail = "full")),
                "Reconciliation Report")
  expect_output(print(reconcile_summary(result, detail = "mismatches_only")),
                "Reconciliation Report")
})

test_that("reconcile_summary writes to file", {
  df <- data.frame(species = c("Homo sapiens"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))

  reconcile_summary(result, file = tmp)
  expect_true(file.exists(tmp))
  content <- readLines(tmp)
  expect_true(any(grepl("Reconciliation Report", content)))
})


# Issue #12 (Ayumi): assignment should not print the report.
test_that("reconcile_summary does not print when assigned (issue #12)", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # The function body itself should produce no console output;
  # printing happens via the print method on auto-print only.
  out_chars <- capture.output({
    summary_obj <- reconcile_summary(result)
  })
  expect_equal(length(out_chars), 0L)

  # The assigned object still carries the formatted report
  expect_s3_class(summary_obj, "reconciliation_summary")
  expect_true(nzchar(summary_obj$formatted_text))
  expect_true(grepl("Reconciliation Report", summary_obj$formatted_text))

  # Explicit print() shows the report
  expect_output(print(summary_obj), "Reconciliation Report")
})


test_that("reconcile_summary returns visibly so the prompt still prints", {
  df <- data.frame(species = c("Homo sapiens"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)
  # withVisible mimics R's REPL auto-print decision
  res <- withVisible(reconcile_summary(result))
  expect_true(res$visible)
  expect_s3_class(res$value, "reconciliation_summary")
})
