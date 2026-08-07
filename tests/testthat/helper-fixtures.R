# Shared test fixtures ------------------------------------------------------
#
# testthat automatically sources any file named `helper-*.R` before each test
# run, so every factory below is visible inside test_that() blocks without an
# explicit source() call.
#
# These fixtures are *deterministic*: they use fixed seeds wherever randomness
# is involved. Tests that need reproducibility should rely on the fixtures and
# avoid calling rtree()/sample() directly.
#
# Convention:
#   fx_df_*   — data frame factories
#   fx_tree_* — phylo factories
#   fx_rec_*  — reconciliation object factories built from the above
#
# Fixtures that intentionally produce invalid inputs (empty, all-NA, duplicate
# tips) exist so tests can exercise the error paths — not so tests can feed
# them through valid workflows.

# ---------------------------------------------------------------------------
# Data-frame fixtures
# ---------------------------------------------------------------------------

#' Clean binomial data frame with matching species column
#' @keywords internal
fx_df_clean <- function(n = 10, col = "species") {
  set.seed(42)
  genera   <- rep(c("Parus", "Corvus", "Turdus", "Falco", "Aquila"),
                  length.out = n)
  epithets <- paste0("sp", sprintf("%03d", seq_len(n)))
  df <- data.frame(
    x = paste(genera, epithets),
    trait = rnorm(n),
    mass  = runif(n, 5, 500),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Zero-row data frame with a species column of correct type
#' @keywords internal
fx_df_empty <- function(col = "species") {
  df <- data.frame(
    x = character(),
    trait = numeric(),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Data frame where every species cell is NA
#' @keywords internal
fx_df_all_na <- function(n = 5, col = "species") {
  df <- data.frame(
    x = rep(NA_character_, n),
    trait = seq_len(n),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Data frame with a single species, single row
#' @keywords internal
fx_df_single <- function(col = "species") {
  df <- data.frame(
    x = "Parus major",
    trait = 1.0,
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Data frame where species column is a factor (not character)
#' @keywords internal
fx_df_factor <- function(n = 5, col = "species") {
  df <- fx_df_clean(n, col)
  df[[col]] <- as.factor(df[[col]])
  df
}

#' Data frame with underscore-delimited binomials
#' @keywords internal
fx_df_underscore <- function(n = 5, col = "species") {
  df <- fx_df_clean(n, col)
  df[[col]] <- gsub(" ", "_", df[[col]])
  df
}

#' Data frame with mixed-case names
#' @keywords internal
fx_df_mixed_case <- function(col = "species") {
  data.frame(
    species = c("parus MAJOR", "PARUS major", "Parus Major",
                "corvus corax", "CORVUS CORAX"),
    trait = seq_len(5),
    stringsAsFactors = FALSE
  ) |>
    (\(d) { names(d)[1] <- col; d })()
}

#' Data frame whose species names carry diacritics and authority strings
#'
#' Used to verify that Unicode author names (Müller, Linné) round-trip through
#' normalisation without mojibake.
#' @keywords internal
fx_df_diacritics <- function(col = "species") {
  df <- data.frame(
    x = c(
      "Passer domesticus M\u00fcller, 1776",    # Müller
      "Turdus merula Linn\u00e9, 1758",          # Linné
      "Corvus corone (F\u00e4hse, 1871)",        # Fähse
      "Parus major Linnaeus, 1758"               # ASCII control
    ),
    trait = seq_len(4),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Data frame with parenthetical and non-parenthetical authority strings
#' @keywords internal
fx_df_authority_strings <- function(col = "species") {
  df <- data.frame(
    x = c(
      "Turdus merula (Linnaeus, 1758)",
      "Parus major Linnaeus, 1758",
      "Corvus corax Blyth & Tegetmeier 1881",
      "Homo sapiens L."
    ),
    trait = seq_len(4),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Data frame with leading/trailing/internal irregular whitespace
#' @keywords internal
fx_df_whitespace <- function(col = "species") {
  df <- data.frame(
    x = c(
      "  Parus major  ",
      "Parus\tmajor",
      "Parus  major",
      "\nParus major\n"
    ),
    trait = seq_len(4),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Data frame with each species duplicated (e.g. male + female rows)
#' @keywords internal
fx_df_duplicates <- function(col = "species") {
  df <- data.frame(
    x = rep(c("Parus major", "Corvus corax", "Turdus merula"), each = 2),
    sex = rep(c("M", "F"), 3),
    mass = c(18, 16, 1200, 1100, 100, 95),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Data frame with single-word (monotypic genus) names
#' @keywords internal
fx_df_monotypic <- function(col = "species") {
  df <- data.frame(
    x = c("Tyrannosaurus", "Homo sapiens", "Velociraptor"),
    trait = seq_len(3),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Large synthetic dataset for performance smoke tests
#' @keywords internal
fx_df_large <- function(n = 2000, col = "species") {
  set.seed(42)
  genera <- paste0("Genus", sprintf("%04d", seq_len(ceiling(n / 5))))
  genera <- rep(genera, each = 5)[seq_len(n)]
  epithets <- paste0("sp", sprintf("%05d", seq_len(n)))
  df <- data.frame(
    x = paste(genera, epithets),
    trait = rnorm(n),
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

#' Configurable asymmetric pair of data frames
#'
#' Reproduces the #495 pattern: small `data_x`, large `data_y`, few shared.
#' Returns a list with `x` and `y`. Accepts any `shared`/`only_x`/`only_y`
#' combination including zeros.
#' @keywords internal
fx_df_asymmetric <- function(shared = 5, only_x = 5, only_y = 95,
                             col = "species") {
  # Handle zero lengths explicitly — R's paste recycles scalars against
  # empty vectors, which produces length-1 output where we want length-0.
  make_names <- function(prefix, n) {
    if (n == 0) return(character(0))
    paste0(prefix, " sp", sprintf("%04d", seq_len(n)))
  }

  shared_names <- if (shared == 0) character(0) else {
    if (shared <= length(letters)) {
      paste0("Shared ", letters[seq_len(shared)])
    } else {
      make_names("Shared", shared)
    }
  }
  x_only_names <- make_names("Xonly", only_x)
  y_only_names <- make_names("Yonly", only_y)

  df_x <- data.frame(
    x = c(shared_names, x_only_names),
    val_x = seq_len(shared + only_x),
    stringsAsFactors = FALSE
  )
  df_y <- data.frame(
    x = c(shared_names, y_only_names),
    val_y = seq_len(shared + only_y),
    stringsAsFactors = FALSE
  )
  names(df_x)[1] <- col
  names(df_y)[1] <- col

  list(x = df_x, y = df_y, shared = shared,
       only_x = only_x, only_y = only_y)
}

#' Data frame with a user-specified species column name
#'
#' Useful for auto-detection tests.
#' @keywords internal
fx_df_custom_col <- function(colname = "Scientific_Name") {
  df <- fx_df_clean(5)
  names(df)[1] <- colname
  df
}

#' Data frame of deliberate typos (1-, 2-, and 3-edit distance)
#'
#' The "correct" names are the same set as in `fx_df_clean`; each row is a
#' typo of `fx_df_clean(5)$species`.
#' @keywords internal
fx_df_typos <- function(col = "species") {
  clean <- fx_df_clean(5)$species  # "Parus sp001", "Corvus sp002", ...
  typos <- c(
    gsub("Parus",  "Parus",  clean[1]),                        # identical
    gsub("Corvus", "Corvvs", clean[2]),                        # 1 edit
    gsub("Turdus", "Turdos", clean[3]),                        # 1 edit
    gsub("Falco",  "Falcon", clean[4]),                        # 1 insertion
    gsub("Aquila", "Aqila",  clean[5])                         # 1 deletion
  )
  df <- data.frame(
    x = typos,
    canonical = clean,
    stringsAsFactors = FALSE
  )
  names(df)[1] <- col
  df
}

# ---------------------------------------------------------------------------
# Tree fixtures
# ---------------------------------------------------------------------------

#' Small binary tree with space-delimited binomial tip labels
#' @keywords internal
fx_tree_small <- function(n = 5) {
  set.seed(42)
  tips <- c("Parus major", "Parus caeruleus", "Corvus corax",
            "Corvus corone", "Falco peregrinus", "Aquila chrysaetos",
            "Turdus merula", "Turdus philomelos")[seq_len(n)]
  ape::rtree(n, tip.label = tips)
}

#' Tree with underscored binomial tips (Jetz-style format)
#' @keywords internal
fx_tree_underscored <- function(n = 5) {
  tr <- fx_tree_small(n)
  tr$tip.label <- gsub(" ", "_", tr$tip.label)
  tr
}

#' Non-binary tree (contains a polytomy)
#' @keywords internal
fx_tree_polytomy <- function() {
  set.seed(42)
  tr <- ape::rtree(6, tip.label = c(
    "Parus major", "Parus caeruleus", "Parus ater",
    "Corvus corax", "Corvus corone", "Corvus monedula"
  ))
  # Collapse short edges into a polytomy
  ape::di2multi(tr, tol = max(tr$edge.length) * 0.9)
}

#' Ultrametric tree (smoothed via chronos)
#' @keywords internal
fx_tree_ultrametric <- function(n = 6) {
  set.seed(42)
  tips <- c("Parus major", "Parus caeruleus", "Corvus corax",
            "Corvus corone", "Falco peregrinus",
            "Aquila chrysaetos")[seq_len(n)]
  tr <- ape::rtree(n, tip.label = tips)
  # chronos() is noisy — suppress
  suppressWarnings(suppressMessages(
    ape::chronos(tr, quiet = TRUE)
  ))
}

#' Non-ultrametric tree (rtree default)
#' @keywords internal
fx_tree_non_ultrametric <- function(n = 6) {
  fx_tree_small(n)
}

#' Tree with all zero-length branches
#' @keywords internal
fx_tree_zero_branches <- function(n = 5) {
  tr <- fx_tree_small(n)
  tr$edge.length <- rep(0, length(tr$edge.length))
  tr
}

#' Tree with a duplicated tip label (invalid — for error tests)
#' @keywords internal
fx_tree_duplicated_tips <- function() {
  tr <- fx_tree_small(5)
  tr$tip.label[2] <- tr$tip.label[1]
  tr
}

#' Single-tip tree (minimal valid input)
#' @keywords internal
fx_tree_single <- function() {
  tr <- list(
    edge        = matrix(c(2L, 1L), 1, 2),
    tip.label   = "Parus major",
    edge.length = 1,
    Nnode       = 1L
  )
  class(tr) <- "phylo"
  tr
}

#' multiPhylo of k trees
#' @keywords internal
fx_tree_multiphylo <- function(k = 3, n = 5) {
  trees <- lapply(seq_len(k), function(i) fx_tree_small(n))
  class(trees) <- "multiPhylo"
  trees
}

#' Large synthetic tree for performance tests
#' @keywords internal
fx_tree_large <- function(n = 2000) {
  set.seed(42)
  tips <- paste0("Genus",
                 rep(sprintf("%04d", seq_len(ceiling(n / 5))), each = 5)[seq_len(n)],
                 " sp",
                 sprintf("%05d", seq_len(n)))
  ape::rtree(n, tip.label = tips)
}

#' Tree designed for reconcile_augment tests
#'
#' Contains multiple congeners in two genera (Parus, Corvus) and a singleton
#' (Falco). Paired with a data frame that has species with and without
#' congeners in the tree.
#' @keywords internal
fx_tree_with_congeners <- function() {
  set.seed(42)
  ape::rtree(5, tip.label = c(
    "Parus major", "Parus caeruleus",
    "Corvus corax", "Corvus corone",
    "Falco peregrinus"
  ))
}

# Companion data frame for fx_tree_with_congeners:
#   - 5 species that match tree tips
#   - 2 species with congeners in tree (Parus ater, Corvus monedula)
#   - 1 species with no congener (Aquila chrysaetos)
#' @keywords internal
fx_df_for_congeners <- function() {
  data.frame(
    species = c("Parus major", "Parus caeruleus",
                "Corvus corax", "Corvus corone", "Falco peregrinus",
                "Parus ater", "Corvus monedula",
                "Aquila chrysaetos"),
    trait = seq_len(8),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Reconciliation fixtures
# ---------------------------------------------------------------------------

#' Pre-built data-vs-data reconciliation
#' @keywords internal
fx_rec_data <- function(shared = 5, only_x = 3, only_y = 4) {
  asym <- fx_df_asymmetric(shared = shared, only_x = only_x,
                           only_y = only_y)
  reconcile_data(asym$x, asym$y,
                 x_species = "species", y_species = "species",
                 authority = NULL, quiet = TRUE)
}

#' Pre-built data-vs-tree reconciliation
#' @keywords internal
fx_rec_tree <- function() {
  reconcile_tree(fx_df_for_congeners(), fx_tree_with_congeners(),
                 x_species = "species",
                 authority = NULL, quiet = TRUE)
}

#' Pre-built data-vs-multiPhylo reconciliation
#' @keywords internal
fx_rec_trees <- function(k = 3) {
  reconcile_to_trees(fx_df_for_congeners(), fx_tree_multiphylo(k = k, n = 5),
                     x_species = "species",
                     authority = NULL, quiet = TRUE)
}

#' Pre-built reconciliation guaranteed to have matched + unresolved rows
#' @keywords internal
fx_rec_unresolved <- function() {
  fx_rec_data(shared = 3, only_x = 4, only_y = 5)
}
