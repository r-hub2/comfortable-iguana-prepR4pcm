# Matching cascade ---------------------------------------------------------

#' Run the matching cascade
#'
#' The central engine behind all `reconcile_*` functions. Manual overrides
#' are applied first, then the remaining names pass through matching stages in
#' strict order of decreasing confidence: exact -> normalised -> synonym ->
#' fuzzy. Each stage only operates on names not yet matched.
#'
#' @param names_x Character vector. Names from source x.
#' @param names_y Character vector. Names from source y.
#' @param authority A length-1 character vector, or `NULL`. Taxonomic
#'   authority for the synonym-resolution stage. One of `"col"`,
#'   `"itis"`, `"gbif"`, `"ncbi"`, `"ott"`, or `"itis_test"`. `NULL`
#'   skips stage 3.
#' @param db_version A length-1 character vector or NULL.
#' @param rank A length-1 character vector. `"species"` or `"subspecies"`.
#' @param overrides A data.frame with columns `name_x` and `name_y` for
#'   pre-built overrides, or NULL. Overrides are pre-cascade decisions:
#'   they can preempt otherwise exact or normalised matches.
#' @param fuzzy Logical. Enables the fuzzy-matching stage when `TRUE`. Default `FALSE`.
#' @param fuzzy_threshold Numeric. Minimum similarity (0--1) for fuzzy
#'   matches. Default `0.9` (conservative).
#' @param resolve A length-1 character vector. How to handle low-confidence matches:
#'   `"flag"` (default) marks fuzzy matches below 0.95 and indirect
#'   synonym matches as `match_type = "flagged"` for manual review.
#'   `"first"` accepts all matches at face value.
#' @param multi_x Logical. Allow multiple x names to resolve to the
#'   same y? Default `FALSE` (each y is matched at most once).
#'   `reconcile_multi()` sets this to `TRUE` because the same species
#'   may legitimately appear in different formats (e.g. underscore
#'   vs space) across datasets, and both formats should resolve to
#'   the same tree tip. With `multi_x = TRUE`, the normalised and
#'   synonym stages do not exclude already-matched y's from their
#'   lookup space.
#' @param quiet Logical.
#'
#' @return A tibble with the full mapping table.
#' @keywords internal
pr_run_cascade <- function(
  names_x,
  names_y,
  authority = "col",
  db_version = NULL,
  rank = "species",
  overrides = NULL,
  fuzzy = FALSE,
  fuzzy_threshold = 0.9,
  flag_threshold = 0.95,
  resolve = "flag",
  multi_x = FALSE,
  quiet = FALSE
) {
  # Deduplicate inputs, warning about NAs
  n_na_x <- sum(is.na(names_x))
  n_na_y <- sum(is.na(names_y))
  if (!quiet && (n_na_x > 0 || n_na_y > 0)) {
    cli_alert_warning(
      "Removed {n_na_x} NA name{?s} from x and {n_na_y} from y before matching."
    )
  }
  unique_x <- unique(names_x[!is.na(names_x)])
  unique_y <- unique(names_y[!is.na(names_y)])

  # Show progress for large datasets. We renumber Stage labels so they
  # reflect only the active stages -- Stage 1/N is always exact,
  # Stage 2/N is always normalised, Stage 3/N is synonym (only if
  # authority is supplied), Stage 4/N is fuzzy (only if fuzzy = TRUE).
  # Without this dynamic numbering we would print "Stage 3/4: Synonym
  # resolution (...)" even with authority = NULL, which previously
  # suggested matches were being made at the synonym stage when in
  # reality stages 1+2 had already matched them. (Issue #13.)
  use_progress <- !quiet && length(unique_x) > 500
  do_synonym <- !is.null(authority)
  do_fuzzy <- isTRUE(fuzzy)
  n_stages <- 2L + do_synonym + do_fuzzy
  stage_no <- 0L
  next_stage_label <- function(name) {
    stage_no <<- stage_no + 1L
    sprintf("Stage %d/%d: %s", stage_no, n_stages, name)
  }
  if (use_progress) {
    cli_alert_info(
      "Matching {length(unique_x)} x {length(unique_y)} names through {n_stages} stage{?s}..."
    )
  }

  # Track which names are matched at each stage
  matched_x <- character()
  matched_y <- character()
  rows <- list()

  # Track overrides that could not be applied (issue #8a). Each row has
  # `name_x`, `name_y`, and `reason` -- one of:
  #   "name_x_not_in_data"   : ox not present in the input names
  #   "name_y_not_in_target" : oy not present in the target names
  #   "already_matched"      : ox or oy was already used by a prior override
  unused_overrides <- list()

  # --- Pre-stage: Apply manual overrides ---
  if (!is.null(overrides) && nrow(overrides) > 0) {
    # Normalise for comparison so overrides with spaces match targets
    # with underscores (and vice versa)
    norm_ux <- as.character(pr_normalize_names(unique_x, rank = rank))
    norm_uy <- as.character(pr_normalize_names(unique_y, rank = rank))
    lookup_orig_ux <- stats::setNames(unique_x, norm_ux)
    lookup_orig_uy <- stats::setNames(unique_y, norm_uy)

    for (i in seq_len(nrow(overrides))) {
      ox <- overrides$name_x[i]
      oy <- overrides$name_y[i]
      ox_norm <- as.character(pr_normalize_names(ox, rank = rank))
      oy_norm <- as.character(pr_normalize_names(oy, rank = rank))

      # Match override names to input names via normalised form
      ox_orig <- lookup_orig_ux[ox_norm]
      oy_orig <- lookup_orig_uy[oy_norm]

      ox_missing <- is.na(ox_orig)
      oy_missing <- is.na(oy_orig)

      if (
        !ox_missing &&
          !oy_missing &&
          !(ox_orig %in% matched_x) &&
          !(oy_orig %in% matched_y)
      ) {
        rows[[length(rows) + 1]] <- tibble(
          name_x = unname(ox_orig),
          name_y = unname(oy_orig),
          name_resolved = NA_character_,
          match_type = "manual",
          match_score = 1.0,
          match_source = "user_override",
          in_x = TRUE,
          in_y = TRUE,
          notes = overrides$user_note[i] %||% "Manual override"
        )
        matched_x <- c(matched_x, unname(ox_orig))
        matched_y <- c(matched_y, unname(oy_orig))
      } else {
        # Record why this override could not be applied. Multiple reasons
        # can apply at once (e.g. both names absent); we report the most
        # informative one in priority order.
        reason <- if (ox_missing && oy_missing) {
          "name_x_not_in_data"
        } else if (ox_missing) {
          "name_x_not_in_data"
        } else if (oy_missing) {
          "name_y_not_in_target"
        } else {
          "already_matched"
        }
        unused_overrides[[length(unused_overrides) + 1]] <- tibble(
          name_x = ox,
          name_y = oy,
          reason = reason
        )
      }
    }
  }

  # --- Stage 1: Exact match ---
  if (use_progress) {
    cli_alert_info(next_stage_label("Exact matching..."))
  }
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  exact_both <- intersect(remaining_x, remaining_y)

  if (length(exact_both) > 0) {
    rows[[length(rows) + 1]] <- tibble(
      name_x = exact_both,
      name_y = exact_both,
      name_resolved = NA_character_,
      match_type = "exact",
      match_score = 1.0,
      match_source = "exact_string",
      in_x = TRUE,
      in_y = TRUE,
      notes = ""
    )
    matched_x <- c(matched_x, exact_both)
    matched_y <- c(matched_y, exact_both)
  }

  # --- Stage 2: Normalised match ---
  if (use_progress) {
    cli_alert_info(
      paste0(
        next_stage_label("Normalised matching"),
        " ({length(matched_x)} matched so far)..."
      )
    )
  }
  remaining_x <- setdiff(unique_x, matched_x)
  # When multi_x = TRUE, allow already-matched y's to be matched to
  # additional x's via normalisation. This handles the case where the
  # same species appears in multiple formats across datasets (e.g.
  # `Homo_sapiens` from one dataset and `Homo sapiens` from another)
  # and both should resolve to the same tree tip. Issue #10 (Ayumi).
  remaining_y <- if (multi_x) unique_y else setdiff(unique_y, matched_y)

  if (length(remaining_x) > 0 && length(remaining_y) > 0) {
    norm_x <- pr_normalize_names(remaining_x, rank = rank)
    norm_y <- pr_normalize_names(remaining_y, rank = rank)

    # Build lookup: normalised -> original
    # Warn if multiple originals normalise to the same string (only first matches)
    dup_norm_y <- norm_y[duplicated(norm_y) & !is.na(norm_y)]
    if (length(dup_norm_y) > 0 && !quiet) {
      cli_alert_warning(
        "{length(dup_norm_y)} name{?s} in y have identical normalised forms; only the first of each will be matched."
      )
    }
    lookup_y <- stats::setNames(remaining_y, norm_y)

    # Vectorised: find all remaining_x whose normalised form hits a normalised y
    hit_mask <- norm_x %in% names(lookup_y)
    if (any(hit_mask)) {
      x_hits <- remaining_x[hit_mask]
      nx_hits <- as.character(norm_x[hit_mask])
      y_hits <- unname(lookup_y[nx_hits])

      # If multiple x names normalise to the same y, keep only the first
      # x in single-source mode. With multi_x = TRUE we keep every x ->
      # y mapping so the per-dataset join works.
      if (!multi_x) {
        dup_y <- duplicated(y_hits)
        x_hits <- x_hits[!dup_y]
        nx_hits <- nx_hits[!dup_y]
        y_hits <- y_hits[!dup_y]
      }

      rows[[length(rows) + 1]] <- tibble(
        name_x = x_hits,
        name_y = y_hits,
        name_resolved = NA_character_,
        match_type = "normalized",
        match_score = 1.0,
        match_source = "normalisation",
        in_x = TRUE,
        in_y = TRUE,
        notes = sprintf("'%s' normalised to '%s'", x_hits, nx_hits)
      )
      matched_x <- c(matched_x, x_hits)
      matched_y <- c(matched_y, y_hits)
    }
  }

  # --- Stage 3: Synonym resolution (skipped if authority = NULL) ---
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  if (do_synonym) {
    if (use_progress) {
      cli_alert_info(
        paste0(
          next_stage_label("Synonym resolution"),
          " ({length(matched_x)} matched so far)..."
        )
      )
    }
  }

  if (do_synonym && length(remaining_x) > 0 && length(remaining_y) > 0) {
    # Normalise remaining names before synonym lookup
    norm_remaining_x <- as.character(pr_normalize_names(
      remaining_x,
      rank = rank
    ))
    norm_remaining_y <- as.character(pr_normalize_names(
      remaining_y,
      rank = rank
    ))

    syn_matches <- pr_resolve_synonyms(
      unmatched_x = norm_remaining_x,
      unmatched_y = norm_remaining_y,
      authority = authority,
      db_version = db_version,
      quiet = quiet
    )

    if (nrow(syn_matches) > 0) {
      # Map normalised names back to originals
      lookup_orig_x <- stats::setNames(remaining_x, norm_remaining_x)
      lookup_orig_y <- stats::setNames(remaining_y, norm_remaining_y)

      for (i in seq_len(nrow(syn_matches))) {
        orig_x <- lookup_orig_x[syn_matches$name_x[i]]
        orig_y <- lookup_orig_y[syn_matches$name_y[i]]

        if (
          !is.na(orig_x) &&
            !is.na(orig_y) &&
            !(orig_x %in% matched_x) &&
            !(orig_y %in% matched_y)
        ) {
          # Indirect synonyms (both names required a lookup) are flagged regardless
          # of score: the chain of inference is longer and warrants manual review.
          is_indirect <- grepl("^Both synonyms", syn_matches$notes[i])
          mtype <- if (resolve == "flag" && is_indirect) {
            "flagged"
          } else {
            "synonym"
          }

          rows[[length(rows) + 1]] <- tibble(
            name_x = unname(orig_x),
            name_y = unname(orig_y),
            name_resolved = syn_matches$name_resolved[i],
            match_type = mtype,
            match_score = 0.95,
            match_source = syn_matches$match_source[i],
            in_x = TRUE,
            in_y = TRUE,
            notes = syn_matches$notes[i]
          )
          matched_x <- c(matched_x, unname(orig_x))
          matched_y <- c(matched_y, unname(orig_y))
        }
      }
    }
  }

  # --- Stage 4: Fuzzy matching (skipped if fuzzy = FALSE) ---
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  if (do_fuzzy) {
    if (use_progress) {
      cli_alert_info(
        paste0(
          next_stage_label("Fuzzy matching"),
          " ({length(matched_x)} matched so far)..."
        )
      )
    }
  }

  if (do_fuzzy && length(remaining_x) > 0 && length(remaining_y) > 0) {
    if (!quiet) {
      cli_alert_info(
        "Running fuzzy matching on {length(remaining_x)} x {length(remaining_y)} remaining names..."
      )
    }

    fuzzy_matches <- pr_fuzzy_match(
      remaining_x,
      remaining_y,
      threshold = fuzzy_threshold,
      rank = rank
    )

    if (nrow(fuzzy_matches) > 0) {
      for (i in seq_len(nrow(fuzzy_matches))) {
        fx <- fuzzy_matches$name_x[i]
        fy <- fuzzy_matches$name_y[i]
        if (!(fx %in% matched_x) && !(fy %in% matched_y)) {
          # Flag low-confidence fuzzy matches when resolve = "flag"
          fscore <- fuzzy_matches$score[i]
          mtype <- if (resolve == "flag" && fscore < flag_threshold) {
            "flagged"
          } else {
            "fuzzy"
          }

          rows[[length(rows) + 1]] <- tibble(
            name_x = fx,
            name_y = fy,
            name_resolved = NA_character_,
            match_type = mtype,
            match_score = fscore,
            match_source = "fuzzy_match",
            in_x = TRUE,
            in_y = TRUE,
            notes = fuzzy_matches$notes[i]
          )
          matched_x <- c(matched_x, fx)
          matched_y <- c(matched_y, fy)
        }
      }
    }
  }

  # --- Unresolved names ---
  remaining_x <- setdiff(unique_x, matched_x)
  remaining_y <- setdiff(unique_y, matched_y)

  if (length(remaining_x) > 0) {
    rows[[length(rows) + 1]] <- tibble(
      name_x = remaining_x,
      name_y = NA_character_,
      name_resolved = NA_character_,
      match_type = "unresolved",
      match_score = NA_real_,
      match_source = NA_character_,
      in_x = TRUE,
      in_y = FALSE,
      notes = "No match found in source y"
    )
  }

  if (length(remaining_y) > 0) {
    rows[[length(rows) + 1]] <- tibble(
      name_x = NA_character_,
      name_y = remaining_y,
      name_resolved = NA_character_,
      match_type = "unresolved",
      match_score = NA_real_,
      match_source = NA_character_,
      in_x = FALSE,
      in_y = TRUE,
      notes = "No match found in source x"
    )
  }

  # Build the unused-overrides table (issue #8a). Always present, even
  # when no overrides were given, so downstream code can rely on its
  # shape.
  if (length(unused_overrides) == 0) {
    unused_tbl <- tibble(
      name_x = character(),
      name_y = character(),
      reason = character()
    )
  } else {
    unused_tbl <- do.call(rbind, unused_overrides)
  }

  # Combine all rows
  if (length(rows) == 0) {
    mapping <- tibble(
      name_x = character(),
      name_y = character(),
      name_resolved = character(),
      match_type = character(),
      match_score = numeric(),
      match_source = character(),
      in_x = logical(),
      in_y = logical(),
      notes = character()
    )
  } else {
    mapping <- do.call(rbind, rows)
  }

  attr(mapping, "unused_overrides") <- unused_tbl
  mapping
}


#' Warn the user that some overrides could not be applied
#'
#' Emits a `cli_alert_warning` summarising why each rejected override
#' was skipped. Pointer to the full table on the result object.
#'
#' @param unused A tibble produced by `pr_run_cascade()` with columns
#'   `name_x`, `name_y`, `reason`.
#' @return Invisibly `NULL`.
#' @keywords internal
pr_warn_unused_overrides <- function(unused) {
  if (is.null(unused) || nrow(unused) == 0) {
    return(invisible(NULL))
  }

  reason_counts <- table(unused$reason)
  reason_strs <- vapply(
    names(reason_counts),
    function(r) sprintf("%d %s", reason_counts[[r]], r),
    character(1)
  )

  n <- nrow(unused)
  cli_alert_warning(
    "{n} override{?s} could not be applied: {paste(reason_strs, collapse = '; ')}."
  )
  cli_alert_info("See {.code result$unused_overrides} for details.")
  invisible(NULL)
}
