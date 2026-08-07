#' Print a reconciliation summary to the console
#'
#' Produce a human-readable breakdown of a [reconciliation] object:
#' how many names matched exactly, how many were rescued by
#' normalisation, synonymy, or fuzzy matching, and which names remain
#' unresolved. Usually the second function you call after
#' [reconcile_tree()] or [reconcile_data()].
#'
#' @param reconciliation A [reconciliation] object returned by
#'   [reconcile_tree()], [reconcile_data()], or a related matcher.
#' @param detail A length-1 character vector. How much to show:
#'   \describe{
#'     \item{`"full"` (default)}{Every match category, with the names
#'       belonging to each category listed out.}
#'     \item{`"brief"`}{Counts only --- a one-screen overview.}
#'     \item{`"mismatches_only"`}{Non-exact matches and unresolved
#'       names. Useful once the easy cases are out of the way and you
#'       want to focus on what still needs review.}
#'   }
#' @param format A length-1 character vector. Where the summary goes:
#'   \describe{
#'     \item{`"console"` (default)}{Pretty-printed to the screen.}
#'     \item{`"data.frame"`}{Returns a list of tibbles silently; useful
#'       when writing a report or table in a larger script.}
#'   }
#' @param file A length-1 character vector or `NULL`. If non-`NULL`, writes the
#'   console report to this file path in addition to printing it.
#' @param ... Additional arguments (currently unused).
#'
#' @return A `reconciliation_summary` object. The formatted report
#'   is attached to the object and rendered by
#'   [print.reconciliation_summary()]. R's REPL auto-printing means
#'   that calling the function at the prompt without assignment shows
#'   the full report; assigning the result to a variable shows
#'   nothing until you `print(x)` (or auto-print `x`). Use
#'   `invisible(reconcile_summary(rec))` to suppress display at the
#'   prompt entirely.
#'
#' @family reconciliation functions
#' @seealso [reconcile_plot()] for a visual summary;
#'   [reconcile_report()] for a shareable HTML audit trail;
#'   [reconcile_mapping()] for the full per-name tibble.
#'
#' @examples
#' data(avonet_subset)
#' data(tree_jetz)
#' rec <- reconcile_tree(avonet_subset, tree_jetz,
#'                       x_species = "Species1", authority = NULL)
#' reconcile_summary(rec, detail = "brief")
#' reconcile_summary(rec, detail = "mismatches_only")
#'
#' @export
reconcile_summary <- function(reconciliation,
                              detail = c("full", "brief", "mismatches_only"),
                              format = c("console", "data.frame"),
                              file = NULL,
                              ...) {

  validate_reconciliation(reconciliation)
  detail <- match.arg(detail)
  format <- match.arg(format)

  mapping <- reconciliation$mapping
  meta    <- reconciliation$meta
  counts  <- reconciliation$counts

  # Build sub-tables
  by_type <- tibble(
    match_type = c("exact", "normalized", "synonym", "fuzzy", "manual",
                   "unresolved", "flagged"),
    count = c(counts$n_exact, counts$n_normalized, counts$n_synonym,
              counts$n_fuzzy, counts$n_manual,
              counts$n_unresolved_x + counts$n_unresolved_y,
              counts$n_flagged)
  )

  unresolved <- mapping[mapping$match_type == "unresolved", ]
  flagged    <- mapping[mapping$match_type == "flagged", ]
  synonyms   <- mapping[mapping$match_type == "synonym", ]
  normalized <- mapping[mapping$match_type == "normalized", ]
  fuzzy_m    <- mapping[mapping$match_type == "fuzzy", ]
  manual_m   <- mapping[mapping$match_type == "manual", ]

  # Console output
  if (format == "console") {
    lines <- character()

    lines <- c(lines, "")
    lines <- c(lines, "=== Reconciliation Report ===")
    lines <- c(lines, sprintf("Type: %s", meta$type))
    lines <- c(lines, sprintf("Timestamp: %s",
                               format(meta$timestamp, "%Y-%m-%d %H:%M:%S")))
    lines <- c(lines, sprintf("Package: prepR4pcm %s", meta$prepR4pcm_version))
    lines <- c(lines, sprintf("Authority: %s (version: %s)",
                               toupper(meta$authority), meta$db_version))
    lines <- c(lines, sprintf("Rank: %s", meta$rank %||% "species"))
    lines <- c(lines, "")

    lines <- c(lines, "--- Match Summary ---")
    lines <- c(lines, sprintf("  Exact:       %d / %d", counts$n_exact, counts$n_x))
    lines <- c(lines, sprintf("  Normalized:  %d / %d", counts$n_normalized, counts$n_x))
    lines <- c(lines, sprintf("  Synonym:     %d / %d", counts$n_synonym, counts$n_x))
    lines <- c(lines, sprintf("  Fuzzy:       %d / %d", counts$n_fuzzy, counts$n_x))
    lines <- c(lines, sprintf("  Manual:      %d / %d", counts$n_manual, counts$n_x))
    lines <- c(lines, sprintf("  Unresolved:  %d (x only) + %d (y only)",
                               counts$n_unresolved_x, counts$n_unresolved_y))
    n_unused_ov <- if (!is.null(reconciliation$unused_overrides))
                     nrow(reconciliation$unused_overrides) else 0L
    if (n_unused_ov > 0) {
      lines <- c(lines, sprintf("  Overrides unused: %d", n_unused_ov))
    }
    lines <- c(lines, "")

    if (detail != "brief") {
      # Normalised matches
      if (nrow(normalized) > 0 && detail != "brief") {
        lines <- c(lines, sprintf("--- Normalized Matches (%d) ---", nrow(normalized)))
        for (i in seq_len(min(nrow(normalized), 20))) {
          lines <- c(lines, sprintf('  "%s" -> "%s"  [%s]',
                                     normalized$name_x[i],
                                     normalized$name_y[i],
                                     normalized$notes[i]))
        }
        if (nrow(normalized) > 20) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(normalized) - 20))
        }
        lines <- c(lines, "")
      }

      # Synonym matches
      if (nrow(synonyms) > 0) {
        lines <- c(lines, sprintf("--- Synonym Matches (%d) ---", nrow(synonyms)))
        for (i in seq_len(min(nrow(synonyms), 20))) {
          lines <- c(lines, sprintf('  "%s" -> "%s"  [%s]',
                                     synonyms$name_x[i],
                                     synonyms$name_y[i],
                                     synonyms$notes[i]))
        }
        if (nrow(synonyms) > 20) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(synonyms) - 20))
        }
        lines <- c(lines, "")
      }

      # Fuzzy matches
      if (nrow(fuzzy_m) > 0) {
        lines <- c(lines, sprintf("--- Fuzzy Matches (%d) ---", nrow(fuzzy_m)))
        for (i in seq_len(min(nrow(fuzzy_m), 20))) {
          lines <- c(lines, sprintf('  "%s" -> "%s"  [score: %.3f]  [%s]',
                                     fuzzy_m$name_x[i],
                                     fuzzy_m$name_y[i],
                                     fuzzy_m$match_score[i],
                                     fuzzy_m$notes[i]))
        }
        if (nrow(fuzzy_m) > 20) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(fuzzy_m) - 20))
        }
        lines <- c(lines, "")
      }

      # Manual overrides
      if (nrow(manual_m) > 0) {
        lines <- c(lines, sprintf("--- Manual Overrides (%d) ---", nrow(manual_m)))
        for (i in seq_len(min(nrow(manual_m), 20))) {
          lines <- c(lines, sprintf('  "%s" -> "%s"  [%s]',
                                     manual_m$name_x[i],
                                     manual_m$name_y[i],
                                     manual_m$notes[i]))
        }
        lines <- c(lines, "")
      }

      # Unused overrides (issue #8a) -- overrides supplied by the user
      # but not applied because of missing names or prior matches.
      unused_ov <- reconciliation$unused_overrides
      if (!is.null(unused_ov) && nrow(unused_ov) > 0) {
        lines <- c(lines,
                   sprintf("--- Unused Overrides (%d) ---", nrow(unused_ov)))
        show_n <- min(nrow(unused_ov), 20)
        for (i in seq_len(show_n)) {
          lines <- c(lines, sprintf('  "%s" -> "%s"  [%s]',
                                     unused_ov$name_x[i],
                                     unused_ov$name_y[i],
                                     unused_ov$reason[i]))
        }
        if (nrow(unused_ov) > 20) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(unused_ov) - 20))
        }
        lines <- c(lines, "")
      }

      # Unresolved (x only)
      unres_x <- unresolved[unresolved$in_x & !unresolved$in_y, ]
      if (nrow(unres_x) > 0) {
        lines <- c(lines, sprintf("--- Unresolved: In x But Not In y (%d) ---",
                                   nrow(unres_x)))
        show_n <- min(nrow(unres_x), 30)
        for (i in seq_len(show_n)) {
          lines <- c(lines, sprintf("  %s", unres_x$name_x[i]))
        }
        if (nrow(unres_x) > 30) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(unres_x) - 30))
        }
        lines <- c(lines, "")
      }

      # Unresolved (y only)
      unres_y <- unresolved[!unresolved$in_x & unresolved$in_y, ]
      if (nrow(unres_y) > 0) {
        lines <- c(lines, sprintf("--- Unresolved: In y But Not In x (%d) ---",
                                   nrow(unres_y)))
        show_n <- min(nrow(unres_y), 30)
        for (i in seq_len(show_n)) {
          lines <- c(lines, sprintf("  %s", unres_y$name_y[i]))
        }
        if (nrow(unres_y) > 30) {
          lines <- c(lines, sprintf("  ... and %d more", nrow(unres_y) - 30))
        }
        lines <- c(lines, "")
      }
    }

    # Capture the formatted report text. Don't print directly --- the
    # cat() lives in print.reconciliation_summary() so assignment
    # doesn't trigger a side effect (issue #12).
    text <- paste(lines, collapse = "\n")

    if (!is.null(file)) {
      writeLines(text, file)
      cli_alert_success("Report written to {.path {file}}")
    }
  } else {
    text <- ""
  }

  # Build summary object. The formatted text is attached so the print
  # method can render it.
  summary_obj <- structure(
    list(
      tables = list(
        by_type    = by_type,
        unresolved = unresolved,
        flagged    = flagged,
        synonyms   = synonyms,
        normalized = normalized
      ),
      meta            = meta,
      formatted_text  = text,
      file_written_to = file
    ),
    class = "reconciliation_summary"
  )

  if (format == "data.frame") {
    return(summary_obj)
  }

  # NOTE: returning visibly (not invisibly) so R's REPL auto-print
  # fires print.reconciliation_summary() when the user calls this at
  # the prompt without assignment. When the user assigns the result
  # to a variable, no auto-print happens and the summary stays quiet.
  # See issue #12.
  summary_obj
}


#' Print a reconciliation summary
#'
#' Renders the formatted report attached to the object. Triggered
#' automatically by R's REPL when the object is auto-printed (i.e.
#' when `reconcile_summary(rec)` is called without assignment).
#'
#' @param x A `reconciliation_summary` from [reconcile_summary()].
#' @param ... Additional arguments (currently unused).
#' @return The object, invisibly.
#' @keywords internal
#' @export
print.reconciliation_summary <- function(x, ...) {
  if (!is.null(x$formatted_text) && nzchar(x$formatted_text)) {
    cat(x$formatted_text, "\n")
  } else {
    # Fallback when format = "data.frame": show the brief summary.
    cat("Reconciliation summary object\n")
    cat(sprintf("  Match types: %s\n",
                paste(x$tables$by_type$match_type[x$tables$by_type$count > 0],
                      collapse = ", ")))
    cat(sprintf("  Unresolved: %d names\n", nrow(x$tables$unresolved)))
    cat("Use $tables to access individual match tables.\n")
  }
  invisible(x)
}
