#' Merge two reconciled datasets
#'
#' After reconciling two datasets with [reconcile_data()], use this function
#' to join them into a single analysis-ready data frame. The reconciliation
#' mapping table provides the species-level join key, so names that differ
#' between the two datasets (due to formatting, synonyms, or typos) are
#' correctly linked.
#'
#' @details
#' **One row per species.** `reconcile_merge()` works best when each dataset
#' has exactly one row per species. If a species appears in multiple rows
#' (e.g., sex-specific measurements, repeated populations), the merge
#' produces all pairwise combinations for that species---the same behaviour
#' as base [merge()]. To avoid unexpected row expansion, aggregate to one
#' row per species before merging, or be aware that the output will contain
#' more rows than either input.
#'
#' **Asymmetric datasets.** When `data_y` contains many more species than
#' `data_x` (common when merging against a large reference database), use
#' `how = "inner"` or `how = "left"`. Inner joins keep only the species
#' present in both datasets; left joins keep all `data_x` rows and fill
#' `data_y` columns with `NA` for unmatched species. Use `how = "full"`
#' only when you need to retain species unique to either side.
#'
#' **Recommended workflow for multi-row data.** Reconcile using a
#' species-level summary (one row per species), inspect the mapping with
#' [reconcile_mapping()], then join the mapping back to your full dataset
#' using the species column as key.
#'
#' @param reconciliation A [reconciliation] object (typically from
#'   [reconcile_data()]).
#' @param data_x The first data frame (source x in the reconciliation).
#' @param data_y The second data frame (source y in the reconciliation).
#' @param species_col_x A length-1 character vector. Species column in `data_x`.
#'   Auto-detected if `NULL`.
#' @param species_col_y A length-1 character vector. Species column in `data_y`.
#'   Auto-detected if `NULL`.
#' @param how A length-1 character vector. Join type:
#'   - `"inner"` (default): keep only species matched in both datasets.
#'   - `"left"`: keep all species from `data_x`.
#'   - `"full"`: keep all species from both datasets.
#' @param suffix A length-2 character vector. Suffixes to disambiguate columns with the
#'   same name in both datasets. Default `c("_x", "_y")`.
#' @param drop_unresolved Logical. If `TRUE`, rows where `species_resolved`
#'   is `NA` (i.e., species that could not be reconciled) are removed from
#'   the final result. Default `FALSE` (keep all rows, fill unmatched
#'   columns with `NA`). Only relevant for `how = "left"` or `how = "full"`;
#'   inner joins drop unmatched rows by definition.
#'
#' @return A data frame with a `species_resolved` column as the join
#'   key, plus all columns from both datasets (with suffixes added when
#'   column names collide).
#'
#' @family reconciliation functions
#' @seealso [reconcile_data()] to build the reconciliation;
#'   [reconcile_apply()] when you want aligned data + tree instead of a
#'   single merged data frame.
#'
#' @examples
#' data(avonet_subset)
#' data(nesttrait_subset)
#'
#' rec <- reconcile_data(avonet_subset, nesttrait_subset,
#'                       x_species = "Species1",
#'                       y_species = "Scientific_name",
#'                       authority = NULL, quiet = TRUE)
#'
#' merged <- reconcile_merge(rec, avonet_subset, nesttrait_subset,
#'                           species_col_x = "Species1",
#'                           species_col_y = "Scientific_name")
#' cat(sprintf("Merged: %d rows, %d cols\n", nrow(merged), ncol(merged)))
#' head(merged[, c("species_resolved", "Family1", "Common_name")])
#'
#' @export
reconcile_merge <- function(reconciliation, data_x, data_y,
                            species_col_x = NULL,
                            species_col_y = NULL,
                            how = c("inner", "left", "full"),
                            suffix = c("_x", "_y"),
                            drop_unresolved = FALSE) {

  validate_reconciliation(reconciliation)
  how <- match.arg(how)

  if (!is.data.frame(data_x)) abort("`data_x` must be a data frame.", call = caller_env())
  if (!is.data.frame(data_y)) abort("`data_y` must be a data frame.", call = caller_env())

  if (length(suffix) != 2) {
    abort("`suffix` must be a character vector of length 2.", call = caller_env())
  }

  # Detect species columns
  if (is.null(species_col_x)) {
    species_col_x <- pr_detect_species_column(data_x, "species_col_x")
  }
  if (is.null(species_col_y)) {
    species_col_y <- pr_detect_species_column(data_y, "species_col_y")
  }

  if (!species_col_x %in% names(data_x)) {
    abort(paste0("Column '", species_col_x, "' not found in `data_x`."),
          call = caller_env())
  }
  if (!species_col_y %in% names(data_y)) {
    abort(paste0("Column '", species_col_y, "' not found in `data_y`."),
          call = caller_env())
  }

  if ("..join_key.." %in% names(data_x) || "..join_key.." %in% names(data_y)) {
    abort(
      c(
        "Input data contains a reserved column name '..join_key..'.",
        "i" = "Rename that column before calling `reconcile_merge()`."
      ),
      call = caller_env()
    )
  }

  mapping <- reconciliation$mapping

  # Build the join key from the mapping table
  matched <- mapping[mapping$in_x & mapping$in_y, ]

  # Create a lookup: name_x -> resolved name (use name_y as the resolved key)
  join_key <- data.frame(
    name_x           = matched$name_x,
    name_y           = matched$name_y,
    species_resolved = ifelse(!is.na(matched$name_resolved),
                              matched$name_resolved,
                              matched$name_y),
    stringsAsFactors = FALSE
  )

  # Attach the join key to each dataset
  data_x$..join_key.. <- join_key$species_resolved[
    match(as.character(data_x[[species_col_x]]), join_key$name_x)
  ]

  data_y$..join_key.. <- join_key$species_resolved[
    match(as.character(data_y[[species_col_y]]), join_key$name_y)
  ]

  # Warn about duplicate species (can cause pairwise row expansion)
  keys_x <- data_x[["..join_key.."]]
  keys_y <- data_y[["..join_key.."]]
  n_dup_x <- sum(duplicated(keys_x[!is.na(keys_x)]))
  n_dup_y <- sum(duplicated(keys_y[!is.na(keys_y)]))
  if (n_dup_x > 0 || n_dup_y > 0) {
    cli_alert_warning(
      paste0(
        "Duplicate species detected: {n_dup_x} in data_x, {n_dup_y} in data_y. ",
        "Merge will produce pairwise row combinations for duplicates."
      )
    )
  }

  # --- Prevent NA-to-NA cartesian explosion ---
  # Base R merge() treats NA == NA as TRUE, so unkeyed rows from both sides

  # cross-join to produce a massive cartesian product. Prevent this by
  # removing or isolating NA-keyed rows BEFORE the merge.
  na_x <- is.na(data_x[["..join_key.."]])
  na_y <- is.na(data_y[["..join_key.."]])

  if (how == "inner") {
    # Inner join: NA rows would be dropped anyway; remove early
    data_x <- data_x[!na_x, , drop = FALSE]
    data_y <- data_y[!na_y, , drop = FALSE]

  } else if (how == "left") {
    # Left join: keep all data_x rows (including NA-keyed), but remove
    # NA-keyed data_y rows so they don't cross-match with NA x-rows.
    # Unmatched x-rows get NAs for all y-columns (correct left-join semantics).
    data_y <- data_y[!na_y, , drop = FALSE]

  } else {
    # Full join: assign unique sentinel values to NA keys on both sides so
    # they don't cross-match. Replace sentinels with NA after merge.
    if (any(na_x)) {
      data_x[["..join_key.."]][na_x] <- paste0(
        "..unmatched_x_", seq_len(sum(na_x)), ".."
      )
    }
    if (any(na_y)) {
      data_y[["..join_key.."]][na_y] <- paste0(
        "..unmatched_y_", seq_len(sum(na_y)), ".."
      )
    }
  }

  # Handle duplicate column names with suffixes
  common_cols <- intersect(names(data_x), names(data_y))
  common_cols <- setdiff(common_cols, "..join_key..")

  if (length(common_cols) > 0) {
    for (col in common_cols) {
      names(data_x)[names(data_x) == col] <- paste0(col, suffix[1])
      names(data_y)[names(data_y) == col] <- paste0(col, suffix[2])
    }
  }

  # Perform the merge
  all_x <- how %in% c("left", "full")
  all_y <- how == "full"

  merged <- merge(data_x, data_y,
                  by = "..join_key..",
                  all.x = all_x,
                  all.y = all_y)

  # Rename join key to species_resolved
  names(merged)[names(merged) == "..join_key.."] <- "species_resolved"

  # For full joins, replace sentinel values with NA
  if (how == "full") {
    sentinel <- grepl("^\\.\\.unmatched_[xy]_\\d+\\.\\.$",
                      merged$species_resolved)
    merged$species_resolved[sentinel] <- NA_character_
  }

  # Drop unresolved species if requested
  if (drop_unresolved && how != "inner") {
    na_resolved <- is.na(merged$species_resolved)
    n_drop <- sum(na_resolved)
    if (n_drop > 0) {
      merged <- merged[!na_resolved, , drop = FALSE]
      cli_alert_warning(
        "Dropped {n_drop} unresolved species from merge result (drop_unresolved = TRUE)"
      )
    }
  }

  # Reorder: species_resolved first
  col_order <- c("species_resolved",
                 setdiff(names(merged), "species_resolved"))
  merged <- merged[, col_order, drop = FALSE]

  n_merged <- sum(!is.na(merged$species_resolved))
  cli_alert_success("Merged {n_merged} species ({how} join)")

  merged
}
