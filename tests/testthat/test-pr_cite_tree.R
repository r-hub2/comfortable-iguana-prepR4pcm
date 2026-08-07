# pr_cite_tree() formatting tests
#
# Check that for each backend, the text/markdown/bibtex outputs:
#   1. Contain the package and underlying-paper citation strings
#   2. Mention per-tree source citations when result has multiPhylo
#   3. Reject non-pr_tree_result inputs
#   4. Bibtex output is parseable enough to surface @misc entries

# Helper: build a fake pr_tree_result with one or many trees.
fake_result <- function(source = "fishtree", n = 1L,
                         per_tree_citations = NULL) {
  tip_labels <- c("Salmo_salar", "Esox_lucius")
  one <- ape::read.tree(text = paste0("(",
                                       paste(tip_labels, collapse = ","),
                                       ");"))
  tree <- if (n == 1L) {
    one
  } else {
    structure(replicate(n, one, simplify = FALSE), class = "multiPhylo")
  }

  prov <- vector("list", n)
  for (i in seq_len(n)) {
    prov[[i]] <- list(
      source_index       = i,
      citation           = if (!is.null(per_tree_citations)) {
        per_tree_citations[[i]]
      } else {
        sprintf("Tree-%d source citation", i)
      },
      calibration_method = "test-calibration",
      n_tips             = ape::Ntip(one)
    )
  }

  out <- list(
    tree         = tree,
    matched      = tip_labels,
    unmatched    = character(),
    source       = source,
    backend_meta = list(
      backend         = source,
      tree_provenance = prov
    )
  )
  class(out) <- "pr_tree_result"
  out
}


test_that("pr_cite_tree rejects non-pr_tree_result input", {
  expect_error(pr_cite_tree(list(tree = NULL)),
               "pr_tree_result")
  expect_error(pr_cite_tree("foo"),
               "pr_tree_result")
})


test_that("pr_cite_tree text output mentions backend and paper", {
  res <- fake_result("fishtree", n = 1L)
  out <- capture.output(invisible(pr_cite_tree(res, format = "text")))
  out <- paste(out, collapse = "\n")
  expect_true(grepl("fishtree", out))
  expect_true(grepl("Rabosky", out))
})


test_that("pr_cite_tree markdown has heading + bullets", {
  res <- fake_result("fishtree", n = 1L)
  out <- capture.output(invisible(pr_cite_tree(res, format = "markdown")))
  out <- paste(out, collapse = "\n")
  expect_true(grepl("###", out))
  expect_true(grepl("^- |\n- ", out))
  expect_true(grepl("Rabosky", out))
})


test_that("pr_cite_tree bibtex emits @misc entries", {
  res <- fake_result("fishtree", n = 1L)
  out <- capture.output(invisible(pr_cite_tree(res, format = "bibtex")))
  out <- paste(out, collapse = "\n")
  expect_true(grepl("@misc", out, fixed = TRUE))
  expect_true(grepl("rabosky2018fishtree", out, fixed = TRUE))
})


test_that("pr_cite_tree per-tree section shown when n > 1", {
  res <- fake_result("datelife", n = 3L,
                    per_tree_citations = c("Hedges 2015",
                                            "Bininda 2007",
                                            "Upham 2019"))
  out <- capture.output(invisible(pr_cite_tree(res, format = "text")))
  out <- paste(out, collapse = "\n")
  expect_true(grepl("Per-tree", out))
  expect_true(grepl("Hedges 2015", out))
  expect_true(grepl("Bininda 2007", out))
  expect_true(grepl("Upham 2019", out))
})


test_that("pr_cite_tree per-tree section omitted when n == 1", {
  res <- fake_result("rotl", n = 1L)
  out <- capture.output(invisible(pr_cite_tree(res, format = "text")))
  out <- paste(out, collapse = "\n")
  expect_false(grepl("Per-tree", out))
})


test_that("pr_cite_tree handles all five backend keys without erroring", {
  for (src in c("rotl", "rtrees", "clootl", "fishtree", "datelife")) {
    res <- fake_result(src, n = 1L)
    expect_no_error(capture.output(pr_cite_tree(res, format = "text")))
    expect_no_error(capture.output(pr_cite_tree(res, format = "markdown")))
    expect_no_error(capture.output(pr_cite_tree(res, format = "bibtex")))
  }
})
