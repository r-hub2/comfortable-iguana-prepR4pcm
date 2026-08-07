test_that("crosswalk supplement preserves baseline exact matches", {
  x <- data.frame(species = "Remiz pendulinus")
  y <- data.frame(species = c("Remiz macronyx", "Remiz pendulinus"))
  crosswalk <- data.frame(
    from = "Remiz pendulinus",
    to = "Remiz macronyx",
    type = "1BL to 1BT",
    stringsAsFactors = FALSE
  )

  baseline <- reconcile_data(
    x,
    y,
    x_species = "species",
    y_species = "species",
    authority = NULL,
    fuzzy = FALSE,
    quiet = TRUE
  )
  supplemented <- reconcile_crosswalk_supplement(
    baseline,
    crosswalk,
    from_col = "from",
    to_col = "to",
    match_type_col = "type",
    quiet = TRUE
  )

  remiz <- reconcile_mapping(supplemented)
  remiz <- remiz[
    !is.na(remiz$name_x) &
      remiz$name_x == "Remiz pendulinus",
  ]

  expect_equal(remiz$match_type, "exact")
  expect_equal(remiz$name_y, "Remiz pendulinus")
  expect_equal(supplemented$meta$crosswalk_supplement$n_applied, 0L)
})

test_that("crosswalk supplement applies one-to-one rows only to unresolved names", {
  x <- data.frame(species = c("Species old", "Species exact"))
  y <- data.frame(species = c("Species new", "Species exact"))
  crosswalk <- data.frame(
    from = "Species old",
    to = "Species new",
    type = "1BL to 1BT",
    stringsAsFactors = FALSE
  )

  baseline <- reconcile_data(
    x,
    y,
    x_species = "species",
    y_species = "species",
    authority = NULL,
    fuzzy = FALSE,
    quiet = TRUE
  )
  supplemented <- reconcile_crosswalk_supplement(
    baseline,
    crosswalk,
    from_col = "from",
    to_col = "to",
    match_type_col = "type",
    quiet = TRUE
  )

  mapping <- reconcile_mapping(supplemented)
  old <- mapping[mapping$name_x == "Species old", ]
  exact <- mapping[mapping$name_x == "Species exact", ]

  expect_equal(old$match_type, "manual")
  expect_equal(old$name_y, "Species new")
  expect_equal(exact$match_type, "exact")
  expect_equal(supplemented$meta$crosswalk_supplement$n_applied, 1L)
})

test_that("crosswalk supplement skips ambiguous duplicate target candidates", {
  x <- data.frame(species = c("Accipiter hiogaster", "Accipiter sylvestris"))
  y <- data.frame(species = "Accipiter novaehollandiae")
  crosswalk <- data.frame(
    from = c("Accipiter hiogaster", "Accipiter sylvestris"),
    to = c("Accipiter novaehollandiae", "Accipiter novaehollandiae"),
    type = c("Many BL to 1BT", "Many BL to 1BT"),
    stringsAsFactors = FALSE
  )

  baseline <- reconcile_data(
    x,
    y,
    x_species = "species",
    y_species = "species",
    authority = NULL,
    fuzzy = FALSE,
    quiet = TRUE
  )
  supplemented <- suppressMessages(
    reconcile_crosswalk_supplement(
      baseline,
      crosswalk,
      from_col = "from",
      to_col = "to",
      match_type_col = "type",
      one_to_one_only = FALSE
    )
  )

  mapping <- reconcile_mapping(supplemented)
  expect_equal(sum(mapping$match_type == "manual"), 0L)
  expect_equal(supplemented$meta$crosswalk_supplement$n_ambiguous, 2L)
  expect_equal(supplemented$meta$crosswalk_supplement$n_applied, 0L)
})
