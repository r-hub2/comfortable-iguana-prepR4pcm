# Split / lump detection ----------------------------------------------------

#' Detect taxonomic splits and lumps in a reconciliation mapping
#'
#' Examines a reconciliation mapping for cases where:
#' - **Splits**: one name in x maps to multiple names in y (or vice versa)
#'   via synonym resolution, indicating a taxonomic split.
#' - **Lumps**: multiple names in x map to a single name in y (or vice versa)
#'   via synonym resolution, indicating a taxonomic lump.
#'
#' Detection uses the `name_resolved` column: when multiple rows share
#' the same accepted name but differ in the original names, the accepted
#' name has been split or lumped between the two sources.
#'
#' @param mapping A mapping tibble from a `reconciliation` object
#'   (i.e., `result$mapping`).
#'
#' @return A list with two tibbles:
#'   \describe{
#'     \item{splits}{Cases where one name in x maps to multiple names in y
#'       (or one resolved name covers multiple y names).}
#'     \item{lumps}{Cases where multiple names in x map to one name in y
#'       (or multiple x names share one resolved name).}
#'   }
#'   Each tibble has columns: `name_resolved`, `names_x`, `names_y`,
#'   `n_x`, `n_y`, `type`.
#' @keywords internal
pr_detect_splits_lumps <- function(mapping) {

  # Only look at rows that were matched via synonym resolution
  syn_rows <- mapping[mapping$match_type == "synonym" & !is.na(mapping$name_resolved), ]

  if (nrow(syn_rows) == 0) {
    empty <- tibble(
      name_resolved = character(),
      names_x       = list(),
      names_y       = list(),
      n_x           = integer(),
      n_y           = integer(),
      type          = character()
    )
    return(list(splits = empty, lumps = empty))
  }

  # Group by resolved name
  resolved_names <- unique(syn_rows$name_resolved)
  results <- list()

  for (rn in resolved_names) {
    rows <- syn_rows[syn_rows$name_resolved == rn, ]
    ux <- unique(rows$name_x[!is.na(rows$name_x)])
    uy <- unique(rows$name_y[!is.na(rows$name_y)])

    if (length(ux) > 1 || length(uy) > 1) {
      sl_type <- if (length(uy) > 1 && length(ux) == 1) {
        "split"
      } else if (length(ux) > 1 && length(uy) == 1) {
        "lump"
      } else {
        "split_lump"
      }

      results[[length(results) + 1]] <- tibble(
        name_resolved = rn,
        names_x       = list(ux),
        names_y       = list(uy),
        n_x           = length(ux),
        n_y           = length(uy),
        type          = sl_type
      )
    }
  }

  if (length(results) == 0) {
    empty <- tibble(
      name_resolved = character(),
      names_x       = list(),
      names_y       = list(),
      n_x           = integer(),
      n_y           = integer(),
      type          = character()
    )
    return(list(splits = empty, lumps = empty))
  }

  all_sl <- do.call(rbind, results)
  splits <- all_sl[all_sl$type %in% c("split", "split_lump"), ]
  lumps  <- all_sl[all_sl$type %in% c("lump", "split_lump"), ]

  list(splits = splits, lumps = lumps)
}


#' Flag taxonomic splits and lumps in a reconciliation
#'
#' Taxonomic revisions often split a single species into several or
#' lump several into one. When your data and your reference taxonomy
#' disagree on such cases, the reconciliation mapping will show one
#' name in one source linked to multiple accepted names in the other.
#' `reconcile_splits_lumps()` scans a [reconciliation] for these cases
#' and returns them as two tibbles, one for splits and one for lumps,
#' so you can decide how to handle each before running your PCM
#' (e.g. keep only one of the split taxa, pool traits across a lumped
#' set, or exclude them entirely).
#'
#' Detection relies on the `name_resolved` column populated by
#' synonym resolution --- so `authority` must have been set (i.e. not
#' `NULL`) when building the reconciliation.
#'
#' @param reconciliation A [reconciliation] object built with a
#'   non-`NULL` `authority` argument. The function inspects the
#'   `name_resolved` column, which is only populated when synonym
#'   resolution was performed.
#' @param quiet Logical. Suppresses the console summary when `TRUE`.
#'   Default `FALSE`.
#'
#' @return Invisibly, a list with two tibbles:
#'   \describe{
#'     \item{`splits`}{Cases where one name in source `x` corresponds
#'       to multiple accepted names in source `y`.}
#'     \item{`lumps`}{Cases where several names in source `x` share a
#'       single accepted name in source `y`.}
#'   }
#'
#' @family reconciliation functions
#' @seealso [reconcile_diff()] for comparing two reconciliations,
#'   which surfaces the same splits/lumps across taxonomy versions.
#'
#' @examples
#' # `reconcile_splits_lumps()` only surfaces rows that synonym lookup
#' # resolved (`match_type == "synonym"`), which requires `authority`
#' # to be non-NULL when building the reconciliation. The bundled-data
#' # call below uses `authority = NULL` for speed, so the output is
#' # empty:
#' data(avonet_subset)
#' data(tree_jetz)
#' rec <- reconcile_tree(avonet_subset, tree_jetz,
#'                       x_species = "Species1", authority = NULL,
#'                       quiet = TRUE)
#' sl <- reconcile_splits_lumps(rec, quiet = TRUE)
#' nrow(sl$splits); nrow(sl$lumps)   # 0 and 0
#'
#' # To show what the output looks like when splits and lumps DO turn
#' # up, we hand-build a tiny reconciliation. In practice you would
#' # obtain this by calling reconcile_tree(..., authority = "col").
#' #
#' #   * Acanthiza pusilla (data) was split in CoL into A. pusilla and
#' #     A. apicalis  (1 x-name -> 2 y-names  ==>  split).
#' #   * Parus caeruleus and Cyanistes caeruleus (data: old + new names)
#' #     both map to Cyanistes caeruleus in CoL
#' #                 (2 x-names -> 1 y-name  ==>  lump).
#' demo_mapping <- tibble::tibble(
#'   name_x        = c("Acanthiza pusilla", "Acanthiza pusilla",
#'                     "Parus caeruleus",   "Cyanistes caeruleus"),
#'   name_y        = c("Acanthiza pusilla", "Acanthiza apicalis",
#'                     "Cyanistes caeruleus", "Cyanistes caeruleus"),
#'   name_resolved = c("Acanthiza pusilla", "Acanthiza pusilla",
#'                     "Cyanistes caeruleus", "Cyanistes caeruleus"),
#'   match_type    = "synonym",
#'   match_score   = 1,
#'   match_source  = "col",
#'   in_x          = TRUE,
#'   in_y          = TRUE,
#'   notes         = NA_character_
#' )
#' rec_demo <- structure(
#'   list(mapping   = demo_mapping,
#'        meta      = list(type = "data_tree", authority = "col"),
#'        counts    = list(),
#'        overrides = tibble::tibble()),
#'   class = "reconciliation"
#' )
#' sl <- reconcile_splits_lumps(rec_demo, quiet = TRUE)
#' sl$splits     # 1 row: Acanthiza pusilla split into 2 taxa
#' sl$lumps      # 1 row: Parus + Cyanistes lumped into 1 taxon
#'
#' @export
reconcile_splits_lumps <- function(reconciliation, quiet = FALSE) {

  validate_reconciliation(reconciliation)

  sl <- pr_detect_splits_lumps(reconciliation$mapping)

  if (!quiet) {
    n_splits <- nrow(sl$splits)
    n_lumps  <- nrow(sl$lumps)

    if (n_splits == 0 && n_lumps == 0) {
      cli_alert_info("No taxonomic splits or lumps detected.")
    } else {
      if (n_splits > 0) {
        cli_alert_warning("{n_splits} potential split(s) detected:")
        for (i in seq_len(n_splits)) {
          xs <- paste(sl$splits$names_x[[i]], collapse = ", ")
          ys <- paste(sl$splits$names_y[[i]], collapse = ", ")
          cli_bullets(c(
            " " = "  {sl$splits$name_resolved[i]}: [{xs}] -> [{ys}]"
          ))
        }
      }
      if (n_lumps > 0) {
        cli_alert_warning("{n_lumps} potential lump(s) detected:")
        for (i in seq_len(n_lumps)) {
          xs <- paste(sl$lumps$names_x[[i]], collapse = ", ")
          ys <- paste(sl$lumps$names_y[[i]], collapse = ", ")
          cli_bullets(c(
            " " = "  {sl$lumps$name_resolved[i]}: [{xs}] -> [{ys}]"
          ))
        }
      }
    }
  }

  invisible(sl)
}
