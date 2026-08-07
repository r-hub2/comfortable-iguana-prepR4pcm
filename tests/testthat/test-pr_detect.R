test_that("detects common column names", {
  df <- data.frame(
    species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  expect_equal(
    pr_detect_species_column(df),
    "species"
  )
})

test_that("detects case-insensitive column names", {
  df <- data.frame(
    Species = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  expect_equal(
    pr_detect_species_column(df),
    "Species"
  )
})

test_that("detects binomial column name", {
  df <- data.frame(
    binomial = c("Homo sapiens", "Pan troglodytes"),
    mass = c(70, 50)
  )
  expect_equal(
    pr_detect_species_column(df),
    "binomial"
  )
})

test_that("falls back to content heuristic", {
  df <- data.frame(
    taxon_name = c("Homo sapiens", "Pan troglodytes", "Gorilla gorilla"),
    mass = c(70, 50, 150)
  )
  expect_equal(
    pr_detect_species_column(df),
    "taxon_name"
  )
})

test_that("errors when no species column found", {
  df <- data.frame(x = 1:3, y = 4:6)
  expect_error(
    pr_detect_species_column(df),
    "Could not detect"
  )
})

test_that("errors when multiple candidates found", {
  df <- data.frame(
    col1 = c("Homo sapiens", "Pan troglodytes", "Gorilla gorilla"),
    col2 = c("Mus musculus", "Rattus norvegicus", "Canis lupus")
  )
  expect_error(
    pr_detect_species_column(df),
    "Multiple columns"
  )
})
