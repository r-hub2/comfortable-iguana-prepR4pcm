# Bundle G #12 — Match-type vocabulary -----------------------------------
#
# Catches new match_type enum values introduced in code without a
# corresponding doc update (or vice versa). The match_type column on
# the mapping tibble is documented as one of a fixed set of strings;
# downstream code (reconcile_summary(), reconcile_apply(),
# reconcile_mapping(include_unused_overrides = TRUE), users' filter
# calls) all branch on these values. Drift produces silent miscounts.
#
# Documented vocabulary (in ?reconciliation -- the "mapping" sub-list):
#
#   exact           -- name found in both x and y exactly as given
#   normalized      -- match after pr_normalize_names() applied to one
#                      or both sides
#   synonym         -- match resolved via taxadb authority lookup
#   fuzzy           -- match from fuzzy-matching stage
#                      (>= flag_threshold)
#   manual          -- match introduced via override
#                      (action = "accept" or "replace")
#   unresolved      -- name in one side but not the other
#   flagged         -- low-confidence fuzzy match (< flag_threshold)
#                      OR indirect synonym (both sides required lookup)
#   override_unused -- override row exposed via reconcile_mapping(
#                      include_unused_overrides = TRUE) (added Round 2)

documented_vocabulary <- c(
  "exact", "normalized", "synonym", "fuzzy", "manual",
  "unresolved", "flagged", "override_unused"
)


test_that("every match_type value emitted by the cascade is in the documented vocabulary", {
  skip_on_cran()
  # Build a reconciliation that exercises every realistic path:
  # exact, normalized (case/underscore), unresolved, manual override.
  df_x <- data.frame(
    species = c(
      "Homo sapiens",        # exact
      "Pan_troglodytes",     # normalized vs `Pan troglodytes`
      "Bogus species"        # unresolved
    )
  )
  df_y <- data.frame(
    species = c(
      "Homo sapiens",
      "Pan troglodytes",
      "Gorilla gorilla"
    )
  )
  rec <- reconcile_data(
    df_x, df_y,
    x_species = "species", y_species = "species",
    authority = NULL, quiet = TRUE
  )

  observed <- unique(rec$mapping$match_type)
  bad <- setdiff(observed, documented_vocabulary)
  expect_equal(
    length(bad), 0,
    info = sprintf(
      "mapping$match_type contains undocumented values: %s. Either add them to the documented vocabulary in ?reconciliation, or fix the cascade.",
      paste(bad, collapse = ", ")
    )
  )
})


test_that("the documented vocabulary is reflected in ?reconcile_mapping", {
  skip_on_cran()
  # The match_type vocabulary is documented inside reconcile_mapping.Rd
  # (the @return block enumerates the possible values). reconciliation.Rd
  # delegates to reconcile_mapping.Rd for column-level details, so we
  # check the latter.
  candidates <- c(
    test_path("..", "..", "man"),
    file.path(getwd(), "man"),
    file.path(getwd(), "..", "man"),
    file.path(getwd(), "..", "..", "man")
  )
  rd_text <- NA_character_
  for (c in candidates) {
    if (dir.exists(c) &&
        file.exists(file.path(dirname(c), "DESCRIPTION"))) {
      p <- file.path(c, "reconcile_mapping.Rd")
      if (file.exists(p)) {
        rd_text <- paste(readLines(p, warn = FALSE), collapse = "\n")
        break
      }
    }
  }
  if (is.na(rd_text)) skip("reconcile_mapping.Rd not present")

  # Each documented value should appear (quoted) somewhere in the Rd.
  # We're not strict about location to keep this test tolerant of
  # legitimate doc rephrasing.
  for (v in documented_vocabulary) {
    expect_match(
      rd_text, paste0('["`]', v, '["`]'),
      perl = TRUE,
      info = sprintf(
        "match_type value `%s` is in the documented vocabulary list inside this test, but is not mentioned in ?reconciliation. Update the Rd or this test.",
        v
      )
    )
  }
})


test_that("override_unused only appears when explicitly requested", {
  skip_on_cran()
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(
    text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);"
  )

  bad <- data.frame(
    name_x = "Homo sapiens",
    name_y = "Not_in_tree",
    user_note = "y missing"
  )
  rec <- reconcile_tree(df, tree, x_species = "species",
                        authority = NULL,
                        overrides = bad, quiet = TRUE)
  default_mapping <- reconcile_mapping(rec)
  expect_false(
    "override_unused" %in% default_mapping$match_type,
    info = "reconcile_mapping() default should not include override_unused rows"
  )
})
