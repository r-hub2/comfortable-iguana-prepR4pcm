# Health probe tests. The default check_network = FALSE path is purely
# local --- it just calls requireNamespace() per backend. We mock that
# to test both installed and missing branches.

test_that("pr_get_tree_status() returns a row per backend with the right shape", {
  status <- pr_get_tree_status()
  expect_s3_class(status, "data.frame")
  expect_setequal(
    names(status),
    c("source", "installed", "version", "needs_network",
      "reachable", "install_hint", "source_repo")
  )
  expect_setequal(status$source,
                   c("rotl", "rtrees", "clootl", "fishtree", "datelife"))
})


test_that("pr_get_tree_status() reports installed = FALSE for missing pkgs", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = TRUE) {
      if (identical(package, "rotl")) FALSE else TRUE
    },
    .package = "base"
  )
  status <- pr_get_tree_status()
  rotl_row <- status[status$source == "rotl", ]
  expect_false(rotl_row$installed)
  expect_true(is.na(rotl_row$version))
  expect_true(grepl("install.packages", rotl_row$install_hint))
})


test_that("pr_get_tree_status() reports needs_network correctly", {
  status <- pr_get_tree_status()
  # rotl + fishtree need network; rtrees + clootl + datelife don't
  expect_true(status$needs_network[status$source == "rotl"])
  expect_true(status$needs_network[status$source == "fishtree"])
  expect_false(status$needs_network[status$source == "rtrees"])
  expect_false(status$needs_network[status$source == "clootl"])
  expect_false(status$needs_network[status$source == "datelife"])
})


test_that("pr_get_tree_status() gives CRAN guidance for rtrees", {
  status <- pr_get_tree_status()
  rtrees_row <- status[status$source == "rtrees", ]
  expect_equal(rtrees_row$install_hint, 'install.packages("rtrees")')
  expect_equal(rtrees_row$source_repo, "CRAN")
})


test_that("pr_get_tree_status() gives CRAN guidance for clootl", {
  status <- pr_get_tree_status()
  clootl_row <- status[status$source == "clootl", ]
  expect_equal(clootl_row$install_hint, 'install.packages("clootl")')
  expect_equal(clootl_row$source_repo, "CRAN")
})


test_that("reachable is NA when check_network = FALSE", {
  status <- pr_get_tree_status(check_network = FALSE)
  expect_true(all(is.na(status$reachable)))
})
