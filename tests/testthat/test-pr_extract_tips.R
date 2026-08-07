test_that("pr_extract_tips returns tip labels from phylo object", {
  tree <- ape::read.tree(text = "(A:1,B:1,(C:1,D:1):1);")
  tips <- pr_extract_tips(tree)
  expect_equal(sort(tips), c("A", "B", "C", "D"))
})

test_that("pr_extract_tips reads a Newick file", {
  tmp <- tempfile(fileext = ".nwk")
  ape::write.tree(
    ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);"),
    file = tmp
  )
  tips <- pr_extract_tips(tmp)
  expect_equal(sort(tips), c("Homo_sapiens", "Pan_troglodytes"))
  unlink(tmp)
})

test_that("pr_extract_tips returns labels with underscores as-is", {
  tree <- ape::read.tree(text = "(Genus_sp1:1,Genus_sp2:1);")
  tips <- pr_extract_tips(tree)
  expect_true("Genus_sp1" %in% tips)
  expect_true("Genus_sp2" %in% tips)
})

test_that("pr_extract_tips errors on invalid file path", {
  expect_error(pr_extract_tips("/nonexistent/path/tree.nwk"),
               "not found")
})

test_that("pr_extract_tips errors on non-phylo non-path input", {
  expect_error(pr_extract_tips(42), "phylo")
})


# --- M13. pr_extract_tips combinatorial grid --------------------------------

test_that("pr_extract_tips reads a Nexus file", {
  tmp <- tempfile(fileext = ".nex")
  tr <- ape::read.tree(text = "(A:1,(B:1,C:1):1);")
  ape::write.nexus(tr, file = tmp)
  tips <- pr_extract_tips(tmp)
  expect_equal(sort(tips), c("A", "B", "C"))
  unlink(tmp)
})


test_that("pr_extract_tips unwraps multiPhylo and warns", {
  trees <- fx_tree_multiphylo(k = 3, n = 5)
  expect_message(
    tips <- pr_extract_tips(trees),
    "multiPhylo"
  )
  expect_equal(sort(tips), sort(trees[[1]]$tip.label))
})


test_that("pr_extract_tips unwraps multiPhylo from a Newick file with multiple trees", {
  # write.nexus requires identical tips across trees, so use Newick instead
  tmp <- tempfile(fileext = ".nwk")
  trees <- c(
    ape::read.tree(text = "(A:1,B:1);"),
    ape::read.tree(text = "(C:1,D:1);")
  )
  class(trees) <- "multiPhylo"
  ape::write.tree(trees, file = tmp)

  expect_message(
    tips <- pr_extract_tips(tmp),
    "contains.*trees"
  )
  # Should have returned tips from first tree
  expect_equal(sort(tips), c("A", "B"))
  unlink(tmp)
})


test_that("pr_extract_tips errors on a tree with 0 tips", {
  bad <- structure(
    list(edge = matrix(integer(), 0, 2),
         tip.label = character(),
         Nnode = 0L),
    class = "phylo"
  )
  expect_error(pr_extract_tips(bad), "no tips")
})


test_that("pr_extract_tips errors on a tree with duplicate tip labels", {
  bad <- fx_tree_duplicated_tips()
  expect_error(pr_extract_tips(bad), "duplicate")
})


test_that("pr_extract_tips handles a single-tip tree", {
  tr <- fx_tree_single()
  tips <- pr_extract_tips(tr)
  expect_length(tips, 1)
  expect_equal(tips, "Parus major")
})


test_that("pr_extract_tips errors on malformed tree file", {
  tmp <- tempfile(fileext = ".nwk")
  # Unclosed parenthesis, no semicolon
  writeLines("((A,B,C,", tmp)
  suppressWarnings(
    expect_error(pr_extract_tips(tmp), "Newick|Nexus|Failed|phylo")
  )
  unlink(tmp)
})


test_that("pr_extract_tips errors on NULL input", {
  expect_error(pr_extract_tips(NULL), "phylo")
})


test_that("pr_extract_tips errors on character vector of length > 1", {
  expect_error(pr_extract_tips(c("a.nwk", "b.nwk")), "phylo")
})
