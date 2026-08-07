# Bundle G #11 — Mapping table schema integrity -------------------------
#
# Catches column-rename drift between the documented mapping schema
# (in ?reconciliation, helper docs, vignettes) and what the code
# actually produces. The mapping tibble is the central data structure
# of the package; any silent change in its columns -- or their types --
# breaks downstream user code.
#
# Required columns (from the reconciliation class doc):
#   name_x        chr   -- name from source x
#   name_y        chr   -- name from source y (or NA)
#   name_resolved chr   -- accepted name from authority lookup (or NA)
#   match_type    chr   -- enum (see test-claim-match-type-vocab.R)
#   match_score   dbl   -- in [0, 1] or NA for unresolved
#   match_source  chr   -- audit trail: where the match came from
#   in_x          lgl   -- present in source x?
#   in_y          lgl   -- present in source y?
#   notes         chr   -- free-text annotation

required_columns <- c(
  "name_x", "name_y", "name_resolved", "match_type",
  "match_score", "match_source", "in_x", "in_y", "notes"
)

required_types <- c(
  name_x        = "character",
  name_y        = "character",
  name_resolved = "character",
  match_type    = "character",
  match_score   = "double",
  match_source  = "character",
  in_x          = "logical",
  in_y          = "logical",
  notes         = "character"
)


make_minimal_data_data_reconciliation <- function() {
  df_x <- data.frame(species = c("Homo sapiens", "Pan troglodytes",
                                  "Unknown sp"))
  df_y <- data.frame(species = c("Homo sapiens", "Pan troglodytes",
                                  "Gorilla gorilla"))
  reconcile_data(
    df_x, df_y,
    x_species = "species", y_species = "species",
    authority = NULL, quiet = TRUE
  )
}

make_minimal_data_tree_reconciliation <- function() {
  df <- data.frame(species = c("Homo sapiens", "Pan troglodytes"))
  tree <- ape::read.tree(text = "((Homo_sapiens:1,Pan_troglodytes:1):1,Gorilla_gorilla:2);")
  reconcile_tree(df, tree, x_species = "species",
                 authority = NULL, quiet = TRUE)
}


test_that("every reconciliation $mapping has the documented columns", {
  skip_on_cran()
  for (mk in list(make_minimal_data_data_reconciliation,
                  make_minimal_data_tree_reconciliation)) {
    rec <- mk()
    cols <- names(rec$mapping)
    missing <- setdiff(required_columns, cols)
    extra <- setdiff(cols, required_columns)

    expect_equal(
      length(missing), 0,
      info = sprintf(
        "mapping is missing documented columns: %s. Either add the column or update the reconciliation class doc.",
        paste(missing, collapse = ", ")
      )
    )
    expect_equal(
      length(extra), 0,
      info = sprintf(
        "mapping has undocumented columns: %s. Either remove the column or document it in ?reconciliation.",
        paste(extra, collapse = ", ")
      )
    )
  }
})


test_that("every mapping column has the documented type", {
  skip_on_cran()
  rec <- make_minimal_data_data_reconciliation()
  for (col in required_columns) {
    if (col %in% names(rec$mapping)) {
      actual <- typeof(rec$mapping[[col]])
      # Tibbles store doubles as "double" but logicals as "logical";
      # treat character/character, double/double, logical/logical
      # equivalents.
      expect_equal(
        actual, required_types[[col]],
        info = sprintf(
          "mapping$%s should have type %s but has type %s",
          col, required_types[[col]], actual
        )
      )
    }
  }
})


test_that("an empty reconciliation still has the documented columns", {
  skip_on_cran()
  # Edge case: zero overlap between x and y. Mapping should still
  # have all columns, just with zero rows for matches and rows for
  # unresolved-on-each-side.
  df_x <- data.frame(species = "Homo sapiens")
  df_y <- data.frame(species = "Tyrannosaurus rex")
  rec <- reconcile_data(
    df_x, df_y,
    x_species = "species", y_species = "species",
    authority = NULL, quiet = TRUE
  )
  cols <- names(rec$mapping)
  missing <- setdiff(required_columns, cols)
  expect_equal(
    length(missing), 0,
    info = sprintf("zero-overlap mapping is missing columns: %s",
                   paste(missing, collapse = ", "))
  )
})
