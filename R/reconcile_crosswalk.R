#' Convert a published taxonomy crosswalk into an overrides table
#'
#' Turn a curated species-name crosswalk (e.g. the BirdLife--BirdTree
#' crosswalk bundled as [crosswalk_birdlife_birdtree], or Clements
#' updates released each year) into a data frame that can be passed
#' straight to the `overrides` argument of [reconcile_tree()],
#' [reconcile_data()] and friends.
#'
#' @details
#' The returned table is an override table, not an extra matching stage.
#' Overrides are applied before the exact -> normalised -> synonym -> fuzzy
#' cascade, so a crosswalk row can preempt a name pair that would otherwise
#' match exactly or after normalisation. For unattended workflows, restrict
#' automatic crosswalk use to clean one-to-one rows with
#' `one_to_one_only = TRUE`. Treat split/lump rows such as
#' `"1BL to many BT"` and `"Many BL to 1BT"` as review candidates, because
#' they represent competing species concepts rather than formatting fixes. If
#' you keep competing rows as overrides, they are tried sequentially and the
#' first applicable row wins.
#'
#' Using a crosswalk is preferable to automated synonym resolution when
#' an authoritative mapping exists --- it is reproducible, does not
#' depend on \pkg{taxadb} being available, and you can point to the
#' published source in the methods section of your paper.
#'
#' @param crosswalk A data frame, or a file path. File format is
#'   inferred from the extension: `.csv` (comma-separated), `.tsv`
#'   (tab-separated), or `.txt` (tab-separated). For other delimited
#'   formats, read the file yourself with `read.delim()` or
#'   `read.table()` and pass the resulting data frame.
#' @param from_col A length-1 character vector. Column name for source names (e.g.,
#'   `"Species1"` for BirdLife names).
#' @param to_col A length-1 character vector. Column name for target names (e.g.,
#'   `"Species3"` for BirdTree names).
#' @param match_type_col A length-1 character vector or `NULL`. Name
#'   of an optional column in `crosswalk` that classifies each row's
#'   relationship between the two taxonomies --- e.g. `"1BL to 1BT"`
#'   (one BirdLife species mapped to one BirdTree species; a clean
#'   one-to-one match), `"Many BL to 1BT"` (a lump: several BirdLife
#'   species mapped to a single BirdTree species), `"1BL to many BT"`
#'   (a split). When supplied, the contents of this column are
#'   appended to each override's `user_note` so the audit trail
#'   records the relationship; if you also pass
#'   `one_to_one_only = TRUE`, only the rows whose match type starts
#'   `"1...to 1..."` are kept. Pass `NULL` (default) when your
#'   crosswalk has no such classification column --- every row is
#'   then kept and notes carry no provenance label.
#' @param notes_col A length-1 character vector or NULL. Column containing additional
#'   notes.
#' @param one_to_one_only Logical. If `TRUE`, keeps only one-to-one
#'   matches (e.g., `"1BL to 1BT"`). Default `FALSE` for backwards
#'   compatibility; `TRUE` is recommended for automatic workflows.
#'
#' @return A data frame with columns `name_x`, `name_y`, and
#'   `user_note`, ready to be passed as the `overrides` argument.
#'
#' @family reconciliation functions
#' @seealso [reconcile_override_batch()] for applying this table
#'   directly to an existing reconciliation; [crosswalk_birdlife_birdtree]
#'   for the bundled example.
#'
#' @examples
#' data(crosswalk_birdlife_birdtree)
#' overrides <- reconcile_crosswalk(
#'   crosswalk_birdlife_birdtree,
#'   from_col = "Species1",
#'   to_col = "Species3",
#'   match_type_col = "Match.type",
#'   one_to_one_only = TRUE
#' )
#' head(overrides)
#'
#' @export
reconcile_crosswalk <- function(
  crosswalk,
  from_col,
  to_col,
  match_type_col = NULL,
  notes_col = NULL,
  one_to_one_only = FALSE
) {
  # Load from file if needed (issue #8b: support CSV, TSV, TXT).
  if (is.character(crosswalk) && length(crosswalk) == 1) {
    if (!file.exists(crosswalk)) {
      abort(
        c("Crosswalk file not found.", "x" = paste0("Path: ", crosswalk)),
        call = caller_env()
      )
    }
    ext <- tolower(tools::file_ext(crosswalk))
    crosswalk <- switch(
      ext,
      csv = utils::read.csv(crosswalk, stringsAsFactors = FALSE),
      tsv = ,
      txt = utils::read.delim(crosswalk, sep = "\t", stringsAsFactors = FALSE),
      cli::cli_abort(
        c(
          "Unsupported crosswalk file extension: {.val {ext}}.",
          "i" = "Use {.file .csv}, {.file .tsv}, or {.file .txt}, or pass a data frame."
        ),
        call = caller_env()
      )
    )
  }

  if (!is.data.frame(crosswalk)) {
    abort(
      "`crosswalk` must be a data frame, or a path to a .csv, .tsv, or .txt file.",
      call = caller_env()
    )
  }

  # Validate columns
  if (!from_col %in% names(crosswalk)) {
    abort(
      c(
        paste0("Column '", from_col, "' not found in crosswalk."),
        "i" = paste0("Available: ", paste(names(crosswalk), collapse = ", "))
      ),
      call = caller_env()
    )
  }
  if (!to_col %in% names(crosswalk)) {
    abort(
      c(
        paste0("Column '", to_col, "' not found in crosswalk."),
        "i" = paste0("Available: ", paste(names(crosswalk), collapse = ", "))
      ),
      call = caller_env()
    )
  }

  # Build overrides
  result <- data.frame(
    name_x = as.character(crosswalk[[from_col]]),
    name_y = as.character(crosswalk[[to_col]]),
    stringsAsFactors = FALSE
  )

  # Build notes from match_type and notes columns
  note_parts <- rep("crosswalk", nrow(result))
  if (!is.null(match_type_col) && match_type_col %in% names(crosswalk)) {
    note_parts <- paste0("crosswalk [", crosswalk[[match_type_col]], "]")
  }
  if (!is.null(notes_col) && notes_col %in% names(crosswalk)) {
    extra <- as.character(crosswalk[[notes_col]])
    has_extra <- !is.na(extra) & nchar(extra) > 0
    note_parts[has_extra] <- paste0(
      note_parts[has_extra],
      ": ",
      extra[has_extra]
    )
  }
  result$user_note <- note_parts

  # Remove rows where from or to is NA/empty
  keep <- !is.na(result$name_x) &
    nchar(result$name_x) > 0 &
    !is.na(result$name_y) &
    nchar(result$name_y) > 0
  result <- result[keep, ]

  # Filter to one-to-one if requested
  if (
    one_to_one_only &&
      !is.null(match_type_col) &&
      match_type_col %in% names(crosswalk)
  ) {
    types <- crosswalk[[match_type_col]][keep]
    # Identify 1:1 matches — match type contains "1" on both sides
    # Common patterns: "1BL to 1BT", "1-to-1", etc.
    is_1to1 <- grepl("^1.*to.*1", types, ignore.case = TRUE)
    n_removed <- sum(!is_1to1)
    if (n_removed > 0) {
      cli_alert_warning(
        "Removed {n_removed} non-one-to-one entries from crosswalk"
      )
    }
    result <- result[is_1to1, ]
  }

  # Warn about many-to-one and one-to-many when they are being kept as
  # automatic overrides. These rows are often legitimate taxonomy splits/lumps,
  # but they should be reviewed before being allowed to preempt the cascade.
  if (!is.null(match_type_col) && match_type_col %in% names(crosswalk)) {
    types <- crosswalk[[match_type_col]][keep]
    n_m2o <- sum(grepl("Many.*to.*1", types, ignore.case = TRUE))
    n_o2m <- sum(grepl("1.*to.*many", types, ignore.case = TRUE))
    if (!one_to_one_only && (n_m2o > 0 || n_o2m > 0)) {
      cli_alert_warning(
        "{n_m2o + n_o2m} non-one-to-one crosswalk entr{?y/ies} kept as overrides."
      )
      cli_alert_info(
        "Review split/lump rows before applying them automatically: {n_m2o} many-to-one, {n_o2m} one-to-many. Competing override rows are tried sequentially; the first applicable row wins."
      )
    }
  }

  # Remove identical name pairs (no reconciliation needed)
  different <- result$name_x != result$name_y
  n_same <- sum(!different)
  result_filtered <- result[different, ]

  cli_alert_success(
    "Crosswalk: {nrow(result_filtered)} overrides ({n_same} identical pairs skipped)"
  )

  rownames(result_filtered) <- NULL
  result_filtered
}
