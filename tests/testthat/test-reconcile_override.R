test_that("reconcile_override accept action adds manual match", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"))
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  result2 <- reconcile_override(
    result,
    name_x = "Unknown sp",
    name_y = "Pan_troglodytes",
    action = "accept",
    note = "Known to be Pan"
  )

  mapping <- reconcile_mapping(result2)
  manual <- mapping[mapping$match_type == "manual", ]
  expect_equal(nrow(manual), 1L)
  expect_equal(manual$name_x, "Unknown sp")
  expect_equal(manual$name_y, "Pan_troglodytes")
})

test_that("reconcile_override reject action marks as unresolved", {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  # Reject the Pan match
  result2 <- reconcile_override(
    result,
    name_x = "Pan troglodytes",
    action = "reject",
    note = "Not confident in this match"
  )

  mapping <- reconcile_mapping(result2)
  pan_row <- mapping[mapping$name_x == "Pan troglodytes" & !is.na(mapping$name_x), ]
  expect_equal(pan_row$match_type, "unresolved")
})

test_that("reconcile_override errors without name_y for accept", {
  df <- data.frame(species = "Homo sapiens")
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  expect_error(
    reconcile_override(result, name_x = "Homo sapiens", name_y = NULL,
                        action = "accept"),
    "Must provide"
  )
})

test_that("reconcile_override records in overrides table", {
  df <- data.frame(species = c("A sp", "B sp"))
  tree <- ape::read.tree(text = "(A_sp:1,C_sp:1);")

  result <- reconcile_tree(df, tree, x_species = "species",
                            authority = NULL, quiet = TRUE)

  result2 <- reconcile_override(
    result,
    name_x = "B sp",
    name_y = "C_sp",
    action = "replace",
    note = "Lab convention"
  )

  expect_equal(nrow(result2$overrides), 1L)
  expect_equal(result2$overrides$action, "replace")
})


# --- M6. reconcile_override action × target-state grid ---------------------

test_that("M6 grid: every combination of action × target-state", {
  # Build a reconciliation with one matched row (Homo sapiens) and one
  # unresolved row (Unknown species).
  df <- data.frame(species = c("Homo sapiens", "Unknown species"),
                   stringsAsFactors = FALSE)
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  # Case A: accept on unresolved -> becomes manual
  r_a <- reconcile_override(rec, name_x = "Unknown species",
                            name_y = "Pan_troglodytes",
                            action = "accept", note = "A")
  ma <- reconcile_mapping(r_a)
  unknown_row <- ma[ma$name_x == "Unknown species" & !is.na(ma$name_x), ]
  expect_equal(unknown_row$match_type, "manual")

  # Case B: replace on matched -> switches target
  r_b <- reconcile_override(rec, name_x = "Homo sapiens",
                            name_y = "Gorilla_gorilla",
                            action = "replace", note = "B")
  mb <- reconcile_mapping(r_b)
  homo_row <- mb[mb$name_x == "Homo sapiens" & !is.na(mb$name_x), ]
  expect_equal(homo_row$name_y, "Gorilla_gorilla")

  # Case C: reject on matched -> becomes unresolved
  r_c <- reconcile_override(rec, name_x = "Homo sapiens",
                            action = "reject", note = "C")
  mc <- reconcile_mapping(r_c)
  homo_c <- mc[mc$name_x == "Homo sapiens" & !is.na(mc$name_x), ]
  expect_equal(homo_c$match_type, "unresolved")

  # Case D: reject on unresolved -> stays unresolved but records override
  r_d <- reconcile_override(rec, name_x = "Unknown species",
                            action = "reject", note = "D")
  md <- reconcile_mapping(r_d)
  unk_d <- md[md$name_x == "Unknown species" & !is.na(md$name_x), ]
  expect_equal(unk_d$match_type, "unresolved")
  expect_equal(nrow(r_d$overrides), 1L)
})


test_that("M6: override is idempotent when applied twice with same values", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"),
                   stringsAsFactors = FALSE)
  tree <- ape::read.tree(text = "(Homo_sapiens:1,Pan_troglodytes:1);")
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  r1 <- reconcile_override(rec, name_x = "Unknown sp",
                           name_y = "Pan_troglodytes",
                           action = "accept", note = "same")
  r2 <- reconcile_override(r1, name_x = "Unknown sp",
                           name_y = "Pan_troglodytes",
                           action = "accept", note = "same")

  # Mapping rows for Unknown sp should match between r1 and r2
  m1 <- reconcile_mapping(r1)
  m2 <- reconcile_mapping(r2)
  r1_row <- m1[m1$name_x == "Unknown sp" & !is.na(m1$name_x), ]
  r2_row <- m2[m2$name_x == "Unknown sp" & !is.na(m2$name_x), ]
  expect_equal(r1_row$match_type, r2_row$match_type)
  expect_equal(r1_row$name_y, r2_row$name_y)
})


test_that("M6: conflicting overrides — second overwrites first", {
  df <- data.frame(species = c("Homo sapiens", "Unknown sp"),
                   stringsAsFactors = FALSE)
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )
  rec <- reconcile_tree(df, tree, x_species = "species",
                         authority = NULL, quiet = TRUE)

  r1 <- reconcile_override(rec, name_x = "Unknown sp",
                           name_y = "Pan_troglodytes",
                           action = "accept", note = "first")
  r2 <- reconcile_override(r1, name_x = "Unknown sp",
                           name_y = "Gorilla_gorilla",
                           action = "accept", note = "second")
  m2 <- reconcile_mapping(r2)
  unk <- m2[m2$name_x == "Unknown sp" & !is.na(m2$name_x), ]
  expect_equal(unk$name_y, "Gorilla_gorilla")
})
