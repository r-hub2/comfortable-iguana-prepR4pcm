#' Reconcile species names between two datasets
#'
#' Match the species column of one data frame (`x`) to the species column
#' of another (`y`), returning a [reconciliation] object that records how
#' every name was resolved. Use this when combining trait datasets, range
#' datasets, or any other species-level tables that may use slightly
#' different taxonomies or spellings.
#'
#' @details
#' Names are passed through a four-stage matching cascade, and the first
#' stage that returns a match is recorded in `match_type`:
#'
#' \enumerate{
#'   \item \strong{exact} --- verbatim string equality.
#'   \item \strong{normalized} --- after stripping underscores, authority
#'     strings (*"Corvus corax Linnaeus, 1758"*), diacritics, and
#'     case/whitespace differences.
#'   \item \strong{synonym} --- lookup in a local taxonomic database via
#'     \pkg{taxadb} (Catalogue of Life, GBIF, ITIS, NCBI, ...). Skipped if
#'     `authority = NULL`.
#'   \item \strong{fuzzy} --- character-level similarity (opt-in via
#'     `fuzzy = TRUE`). Uses a genus-weighted Levenshtein score
#'     (60% genus, 40% specific epithet) with a genus pre-filter so that
#'     only plausibly similar genera are compared.
#' }
#'
#' Names that survive all four stages are labelled `unresolved`. Any
#' entries supplied through `overrides` take precedence over the cascade.
#' This is intentional for locked manual decisions, but it also means a
#' crosswalk row can preempt an otherwise exact or normalised match. If you
#' want a crosswalk to supplement, rather than replace, the baseline cascade,
#' use [reconcile_crosswalk_supplement()] or run the baseline reconciliation
#' first and apply reviewed overrides only to still-unresolved names.
#'
#' \strong{After the call.} A `reconciliation` object is the input to
#' most other functions in the package. Common next steps:
#'
#' \itemize{
#'   \item [reconcile_summary()] --- human-readable breakdown of matches.
#'   \item [reconcile_plot()] --- one-glance bar/pie of match composition.
#'   \item [reconcile_mapping()] --- extract the full per-name tibble.
#'   \item [reconcile_suggest()] --- near-miss candidates for unresolved
#'     names.
#'   \item [reconcile_merge()] --- join the two datasets using the
#'     reconciliation as the species key.
#'   \item [reconcile_report()] --- shareable HTML audit trail.
#' }
#'
#' @param x A data frame whose species will be matched \emph{from}.
#' @param y A data frame whose species will be matched \emph{to}
#'   (typically the "reference" taxonomy or the dataset you want to
#'   merge with).
#' @param x_species A length-1 character vector. Name of the column in
#'   `x` containing scientific names. Auto-detected (e.g. `species`,
#'   `Species1`, `scientific_name`) when `NULL`.
#' @param y_species A length-1 character vector. Name of the column in
#'   `y` containing scientific names. Auto-detected when `NULL`.
#' @param authority A length-1 character vector, or `NULL`. Taxonomic
#'   authority used for synonym resolution (stage 3 of the cascade).
#'   One of:
#'   \describe{
#'     \item{`"col"` (default)}{Catalogue of Life --- broad, curated,
#'       frequently updated. A sensible default for most taxa.}
#'     \item{`"itis"`}{Integrated Taxonomic Information System ---
#'       strong for North American vertebrates and plants.}
#'     \item{`"gbif"`}{Global Biodiversity Information Facility
#'       backbone. Wider coverage; includes more recent synonymy.}
#'     \item{`"ncbi"`}{NCBI Taxonomy --- best when working with
#'       sequence data.}
#'     \item{`"ott"`}{Open Tree of Life synthetic taxonomy. Useful when
#'       your downstream phylogeny is from the Open Tree synthesis.}
#'     \item{`"itis_test"`}{A small bundled subset of ITIS, cached
#'       locally with \pkg{taxadb} for testing. Intended for examples
#'       and unit tests; not for analysis.}
#'     \item{`"gnverifier"`}{HTTP-backed verification against ~100
#'       sources via the Global Names verifier; no local database
#'       download. See `vignette("getting-started")` for the
#'       trade-off (wider coverage, requires network and the
#'       \pkg{httr2} package).}
#'     \item{`NULL`}{Skip the synonym stage entirely. Useful for quick
#'       checks or when \pkg{taxadb} is unavailable. Stages 1, 2 and 4
#'       still run.}
#'   }
#'
#'   Five authority codes that earlier versions of the package
#'   advertised --- `"iucn"`, `"tpl"`, `"fb"`, `"slb"`, `"wd"` --- are
#'   no longer accepted. Empirical testing against \pkg{taxadb} v22.12
#'   showed that `iucn` errors with a schema mismatch and the others
#'   are not \pkg{taxadb} providers at all. Passing one of those
#'   values now produces a helpful migration error.
#' @param rank A length-1 character vector. Controls how trinomials
#'   are handled during normalisation:
#'   \describe{
#'     \item{`"species"` (default)}{Strip infraspecific epithets so
#'       that `"Parus major major"` becomes `"Parus major"` before
#'       matching.}
#'     \item{`"subspecies"`}{Keep trinomials intact. Use this when
#'       your analysis operates at subspecies level.}
#'   }
#' @param overrides Optional pre-built corrections. Either a data
#'   frame with at least columns `name_x` and `name_y` (plus an
#'   optional `user_note` column), or a file path to a CSV with the
#'   same columns. Any name listed here bypasses the cascade and is
#'   recorded as `match_type = "manual"`. Use this for locked decisions.
#'   For published crosswalks (see [reconcile_crosswalk()]), prefer
#'   [reconcile_crosswalk_supplement()] or `one_to_one_only = TRUE` for
#'   automatic use and review split/lump cases before applying them.
#' @param db_version A length-1 character vector. \pkg{taxadb}
#'   database snapshot to use (e.g. `"22.12"`). `NULL` (default) uses
#'   the latest available.
#' @param fuzzy Logical. Enables the fuzzy-matching stage when
#'   `TRUE`. Default `FALSE`. Turn this on to catch likely typos
#'   (*Corvus brachyrhnchos* -> *Corvus brachyrhynchos*). When
#'   `FALSE`, stages 1--3 still run.
#' @param fuzzy_threshold Numeric in \[0, 1\]. Minimum genus-weighted
#'   similarity score for a fuzzy match to be accepted. Default `0.9`
#'   (roughly "no more than ~10% of characters differ"). Lower values
#'   (e.g. `0.7`) are more permissive but produce more false
#'   positives; always review fuzzy matches with [reconcile_suggest()]
#'   or [reconcile_review()] before trusting them.
#' @param flag_threshold Numeric in \[0, 1\]. When `resolve = "flag"`,
#'   fuzzy matches with a score below this value are recorded as
#'   `match_type = "flagged"` rather than `"fuzzy"`, marking them for
#'   manual review. Default `0.95`. Must be >= `fuzzy_threshold` to
#'   have any effect.
#' @param resolve A length-1 character vector. What to do with
#'   borderline matches:
#'   \describe{
#'     \item{`"flag"` (default)}{Mark low-confidence fuzzy matches
#'       (score below `flag_threshold`) and names with indirect
#'       taxadb synonymy as `match_type = "flagged"` so you can audit
#'       them with [reconcile_review()] or [reconcile_suggest()].}
#'     \item{`"first"`}{Accept the highest-scoring candidate silently,
#'       without flagging. Faster but riskier; use only when you have
#'       already reviewed the ambiguities.}
#'   }
#' @param quiet Logical. Suppresses progress messages when `TRUE`.
#'   Default `FALSE`.
#' @param x_label A length-1 character vector or `NULL`. Human-readable label for source `x`
#'   stored in the reconciliation metadata and shown in `print()` / `format()`.
#'   Defaults to the expression passed as `x` (via `deparse(substitute())`).
#'   Set this explicitly when calling `reconcile_data()` inside another function
#'   so the label reflects the real data source rather than the local argument
#'   name.
#' @param y_label A length-1 character vector or `NULL`. Same as `x_label`, for source `y`.
#'
#' @return A [reconciliation] object. The accompanying mapping tibble,
#'   match-type counts, provenance metadata, and applied / unused
#'   override slots are documented in [reconciliation]. See the
#'   "After the call" section above for the most common next steps.
#'
#' @family reconciliation functions
#' @seealso [reconcile_tree()] for matching against a phylogenetic tree;
#'   [reconcile_to_trees()] / [reconcile_trees()] / [reconcile_multi()]
#'   for multi-input workflows.
#'
#' @references
#' Norman, K.E., Chamberlain, S. & Boettiger, C. (2020) taxadb: A
#' high-performance local taxonomic database interface.
#' \emph{Methods in Ecology and Evolution} 11:1153--1159.
#' \doi{10.1111/2041-210X.13440}
#'
#' @examples
#' # Merge AVONET morphology with nest-site data. Both datasets use
#' # slightly different taxonomies; authority = NULL keeps the example
#' # offline (no taxadb download).
#' data(avonet_subset)
#' data(nesttrait_subset)
#'
#' rec <- reconcile_data(avonet_subset, nesttrait_subset,
#'                       x_species = "Species1",
#'                       y_species = "Scientific_name",
#'                       authority = NULL)
#' rec                      # concise print method
#' reconcile_summary(rec)   # full breakdown
#'
#' # Join the two datasets on the reconciled species key
#' merged <- reconcile_merge(rec, avonet_subset, nesttrait_subset,
#'                           species_col_x = "Species1",
#'                           species_col_y = "Scientific_name")
#' head(merged[, c("species_resolved", "Family1", "Common_name")])
#'
#' @export
reconcile_data <- function(
  x,
  y,
  x_species = NULL,
  y_species = NULL,
  authority = "col",
  rank = c("species", "subspecies"),
  overrides = NULL,
  db_version = NULL,
  fuzzy = FALSE,
  fuzzy_threshold = 0.9,
  flag_threshold = 0.95,
  resolve = c("flag", "first"),
  quiet = FALSE,
  x_label = NULL,
  y_label = NULL
) {
  # Capture source labels before any modifications to x/y
  x_source <- x_label %||% deparse(substitute(x))
  y_source <- y_label %||% deparse(substitute(y))

  rank <- match.arg(rank)
  resolve <- match.arg(resolve)

  # Validate inputs
  if (!is.data.frame(x)) {
    abort("`x` must be a data frame.", call = caller_env())
  }
  if (!is.data.frame(y)) {
    abort("`y` must be a data frame.", call = caller_env())
  }

  authority <- pr_validate_authority(authority)

  # Detect species columns
  if (is.null(x_species)) {
    x_species <- pr_detect_species_column(x, "x_species")
  }
  if (is.null(y_species)) {
    y_species <- pr_detect_species_column(y, "y_species")
  }

  if (!x_species %in% names(x)) {
    abort(
      paste0("Column '", x_species, "' not found in `x`."),
      call = caller_env()
    )
  }
  if (!y_species %in% names(y)) {
    abort(
      paste0("Column '", y_species, "' not found in `y`."),
      call = caller_env()
    )
  }

  # Input guards: empty data, all-NA species, factor columns
  if (nrow(x) == 0) {
    abort("`x` has 0 rows.", call = caller_env())
  }
  if (nrow(y) == 0) {
    abort("`y` has 0 rows.", call = caller_env())
  }

  if (is.factor(x[[x_species]])) {
    cli_alert_warning(
      "Converting factor column '{x_species}' in `x` to character."
    )
    x[[x_species]] <- as.character(x[[x_species]])
  }
  if (is.factor(y[[y_species]])) {
    cli_alert_warning(
      "Converting factor column '{y_species}' in `y` to character."
    )
    y[[y_species]] <- as.character(y[[y_species]])
  }

  names_x <- as.character(x[[x_species]])
  names_y <- as.character(y[[y_species]])

  if (all(is.na(names_x))) {
    abort("All species names in `x` are NA.", call = caller_env())
  }
  if (all(is.na(names_y))) {
    abort("All species names in `y` are NA.", call = caller_env())
  }

  # Load overrides
  overrides_df <- pr_load_overrides(overrides)

  if (!quiet) {
    cli_alert_info(
      "Reconciling {length(unique(names_x))} names (x) vs {length(unique(names_y))} names (y)"
    )
  }

  # Run cascade
  mapping <- pr_run_cascade(
    names_x = names_x,
    names_y = names_y,
    authority = authority,
    db_version = db_version,
    rank = rank,
    overrides = overrides_df,
    fuzzy = fuzzy,
    fuzzy_threshold = fuzzy_threshold,
    flag_threshold = flag_threshold,
    resolve = resolve,
    quiet = quiet
  )

  # Build metadata
  meta <- list(
    call = match.call(),
    type = "data_data",
    timestamp = Sys.time(),
    authority = authority %||% "none",
    db_version = db_version %||% "latest",
    fuzzy = fuzzy,
    fuzzy_threshold = if (fuzzy) fuzzy_threshold else NA_real_,
    fuzzy_method = if (fuzzy) "component_levenshtein" else NA_character_,
    resolve = resolve,
    prepR4pcm_version = as.character(utils::packageVersion("prepR4pcm")),
    x_source = x_source,
    y_source = y_source,
    rank = rank
  )

  result <- new_reconciliation(mapping = mapping, meta = meta)

  # Surface unused overrides (issue #8a).
  if (!quiet && nrow(result$unused_overrides) > 0) {
    pr_warn_unused_overrides(result$unused_overrides)
  }

  if (!quiet) {
    n_matched <- sum(mapping$in_x & mapping$in_y, na.rm = TRUE)
    n_total <- result$counts$n_x
    cli_alert_success("Matched {n_matched}/{n_total} names from x")
  }

  result
}
