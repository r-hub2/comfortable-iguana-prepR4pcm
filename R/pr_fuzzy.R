# Fuzzy matching -----------------------------------------------------------

#' Fuzzy-match two sets of species names
#'
#' Uses component-based similarity: the genus and epithet are matched
#' separately, then combined with weights (genus 0.6, epithet 0.4) to
#' reflect that genus-level errors are more informative. Uses base R
#' `utils::adist()` for Levenshtein distance — no extra dependencies.
#'
#' Genus pre-filtering is applied: only names whose genus is within 2
#' edits of each other are compared. This reduces the number of pairwise
#' comparisons dramatically for large datasets.
#'
#' @param names_x Character vector.
#' @param names_y Character vector.
#' @param threshold Numeric (0--1). Minimum similarity score. Default 0.9.
#' @param rank Character. `"species"` or `"subspecies"`.
#'
#' @return A tibble with columns: `name_x`, `name_y`, `score`, `notes`.
#' @keywords internal
pr_fuzzy_match <- function(names_x, names_y, threshold = 0.9,
                            rank = "species") {

  # Normalise for comparison
  norm_x <- as.character(pr_normalize_names(names_x, rank = rank))
  norm_y <- as.character(pr_normalize_names(names_y, rank = rank))

  # Parse into genus + epithet
  parse_binomial <- function(nms) {
    parts <- strsplit(nms, "\\s+")
    genus   <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
    epithet <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))
    list(genus = genus, epithet = epithet)
  }

  px <- parse_binomial(norm_x)
  py <- parse_binomial(norm_y)

  # Filter out entries without valid binomials
  valid_x <- !is.na(px$genus) & !is.na(px$epithet)
  valid_y <- !is.na(py$genus) & !is.na(py$epithet)

  n_skip_x <- sum(!valid_x)
  n_skip_y <- sum(!valid_y)
  if (n_skip_x > 0 || n_skip_y > 0) {
    cli_alert_warning(
      paste0(
        "{n_skip_x} non-binomial name{?s} in x and {n_skip_y} in y ",
        "excluded from fuzzy matching."
      )
    )
  }

  idx_x <- which(valid_x)
  idx_y <- which(valid_y)

  if (length(idx_x) == 0 || length(idx_y) == 0) {
    return(tibble(
      name_x = character(),
      name_y = character(),
      score  = numeric(),
      notes  = character()
    ))
  }

  # Build genus-level index: for each genus in y, store indices
  genus_y_groups <- split(idx_y, py$genus[idx_y])

  # Compute genus edit-distance matrix (unique genera only)
  unique_genus_x <- unique(px$genus[idx_x])
  unique_genus_y <- names(genus_y_groups)

  # Vectorised genus distance: adist can take vectors
  genus_dist_mat <- utils::adist(unique_genus_x, unique_genus_y)

  # For each x genus, find y genera within 2 edits (allows fuzzy genus match)
  max_genus_edits <- 2L
  genus_x_lookup <- stats::setNames(seq_along(unique_genus_x), unique_genus_x)

  results <- list()

  for (ii in seq_along(idx_x)) {
    i <- idx_x[ii]
    gx <- px$genus[i]
    ex <- px$epithet[i]
    gx_idx <- genus_x_lookup[gx]

    # Find candidate y genera (within max_genus_edits)
    close_genus_mask <- genus_dist_mat[gx_idx, ] <= max_genus_edits
    candidate_genera <- unique_genus_y[close_genus_mask]

    if (length(candidate_genera) == 0) next

    # Collect all y indices from candidate genera
    cand_y <- unlist(genus_y_groups[candidate_genera], use.names = FALSE)
    if (length(cand_y) == 0) next

    # Vectorised epithet distance
    epithets_y <- py$epithet[cand_y]
    ep_dists <- utils::adist(ex, epithets_y)[1, ]
    ep_maxlen <- pmax(nchar(ex), nchar(epithets_y))
    ep_sim <- ifelse(ep_maxlen == 0, 1, 1 - ep_dists / ep_maxlen)

    # Genus similarity for candidates
    genera_y <- py$genus[cand_y]
    g_dists <- utils::adist(gx, genera_y)[1, ]
    g_maxlen <- pmax(nchar(gx), nchar(genera_y))
    g_sim <- ifelse(g_maxlen == 0, 1, 1 - g_dists / g_maxlen)

    # Weighted score
    scores <- 0.6 * g_sim + 0.4 * ep_sim

    best_k <- which.max(scores)
    best_score <- scores[best_k]

    if (best_score >= threshold) {
      best_j <- cand_y[best_k]
      results[[length(results) + 1]] <- tibble(
        name_x = names_x[i],
        name_y = names_y[best_j],
        score  = round(best_score, 4),
        notes  = sprintf("Fuzzy match (score %.3f): '%s' ~ '%s'",
                          best_score, norm_x[i], norm_y[best_j])
      )
    }
  }

  if (length(results) == 0) {
    return(tibble(
      name_x = character(),
      name_y = character(),
      score  = numeric(),
      notes  = character()
    ))
  }

  do.call(rbind, results)
}
