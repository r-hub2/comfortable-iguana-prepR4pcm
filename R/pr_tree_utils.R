# Tree utilities -----------------------------------------------------------

pr_namespace_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

.pr_rtrees_get_tree <- function(...) {
  rtrees::get_tree(...)
}

.pr_parse_rtrees_tip_labels <- function(tip_labels) {
  placement_status <- ifelse(
    grepl("\\*\\*+$", tip_labels),
    "family_added",
    ifelse(grepl("\\*+$", tip_labels), "genus_added", "exact")
  )

  tibble::tibble(
    tip_label = tip_labels,
    markerless_label = sub("\\*+$", "", tip_labels),
    placement_status = placement_status
  )
}


#' Extract tip labels from a phylogenetic tree
#'
#' Return the tip labels of a tree as a character vector, whether the
#' tree is already an `ape::phylo` object in memory or lives in a
#' Newick or Nexus file on disk. Convenience wrapper around
#' `tree$tip.label` that also handles file input and multi-tree files
#' (returns the tips of the first tree).
#'
#' @param tree An `ape::phylo` object, or a length-1 character vector
#'   giving the path to a Newick (`.nwk`, `.tre`, `.tree`, `.newick`)
#'   or Nexus (`.nex`, `.nexus`) file. Format is auto-detected.
#'
#' @return A character vector of tip labels (one element per tip).
#'
#' @family name utilities
#' @seealso [pr_normalize_names()] for cleaning tip labels before
#'   joining against a data frame.
#'
#' @examples
#' data(tree_jetz)
#' head(pr_extract_tips(tree_jetz))
#'
#' @export
pr_extract_tips <- function(tree) {
  tree <- pr_load_tree(tree)
  tree$tip.label
}


#' Load a phylogenetic tree
#'
#' If `tree` is already a `phylo` object, returns it. If it is a file path,
#' attempts to read it as Newick first, then Nexus.
#'
#' @param tree An `ape::phylo` object or a character(1) file path.
#' @return An `ape::phylo` object.
#' @keywords internal
pr_load_tree <- function(tree) {
  if (inherits(tree, "phylo")) {
    return(pr_validate_tree(tree))
  }
  if (inherits(tree, "multiPhylo")) {
    cli_alert_warning("Received multiPhylo with {length(tree)} trees; using the first.")
    return(pr_validate_tree(tree[[1]]))
  }

  if (is.character(tree) && length(tree) == 1) {
    if (!file.exists(tree)) {
      abort(
        c("Tree file not found.", "x" = paste0("Path: ", tree)),
        call = caller_env()
      )
    }

    ext <- tolower(tools::file_ext(tree))

    # Try Nexus for .nex/.nexus files
    if (ext %in% c("nex", "nexus")) {
      tryCatch({
        result <- ape::read.nexus(tree)
        if (inherits(result, "multiPhylo")) {
          cli_alert_warning(
            "File contains {length(result)} trees; using the first."
          )
          result <- result[[1]]
        }
        return(pr_validate_tree(result))
      },
        error = function(e) {
          abort(
            c("Failed to read Nexus tree file.",
              "x" = conditionMessage(e)),
            call = caller_env()
          )
        }
      )
    }

    # Try Newick for everything else
    tryCatch({
      result <- ape::read.tree(tree)
      if (inherits(result, "multiPhylo")) {
        cli_alert_warning(
          "File contains {length(result)} trees; using the first."
        )
        result <- result[[1]]
      }
      return(pr_validate_tree(result))
    },
      error = function(e) {
        # If Newick fails, try Nexus as fallback
        tryCatch({
          result <- ape::read.nexus(tree)
          if (inherits(result, "multiPhylo")) {
            cli_alert_warning(
              "File contains {length(result)} trees; using the first."
            )
            result <- result[[1]]
          }
          return(pr_validate_tree(result))
        },
          error = function(e2) {
            abort(
              c("Failed to read tree file as Newick or Nexus.",
                "x" = paste0("Newick error: ", conditionMessage(e)),
                "x" = paste0("Nexus error: ", conditionMessage(e2))),
              call = caller_env()
            )
          }
        )
      }
    )
  }

  abort(
    c("`tree` must be an ape::phylo object or a file path.",
      "i" = paste0("Got: ", class(tree)[1])),
    call = caller_env()
  )
}


#' Align a tree to a reconciliation mapping
#'
#' Renames and/or prunes tip labels according to the reconciliation mapping.
#'
#' @param tree An `ape::phylo` object.
#' @param mapping A mapping tibble from a reconciliation object.
#' @param drop_unresolved Logical. Drop tips with no match? Default `FALSE`.
#'
#' @return A modified `ape::phylo` object.
#' @keywords internal
pr_align_tree <- function(tree, mapping, drop_unresolved = FALSE) {
  tips <- tree$tip.label

  # Build a lookup: normalised tip -> matched name
  tip_norm <- pr_normalize_names(tips)
  matched <- mapping[mapping$in_x & mapping$in_y, ]

  # Rename tips where the y-name (tree side) differs from x-name (data side)
  for (i in seq_len(nrow(matched))) {
    y_name <- matched$name_y[i]
    x_name <- matched$name_x[i]
    if (!is.na(y_name) && !is.na(x_name)) {
      # Find the tip that corresponds to y_name (original or normalised)
      idx <- which(tips == y_name | tip_norm == pr_normalize_names(y_name))
      if (length(idx) > 0) {
        tree$tip.label[idx[1]] <- x_name
      }
    }
  }

  # Drop unresolved tips
  if (drop_unresolved) {
    unresolved_y <- mapping$name_y[mapping$match_type == "unresolved" &
                                     !mapping$in_x & mapping$in_y]
    unresolved_y <- unresolved_y[!is.na(unresolved_y)]
    if (length(unresolved_y) > 0) {
      # Match against current tip labels
      to_drop <- intersect(tree$tip.label, unresolved_y)
      if (length(to_drop) > 0) {
        tree <- ape::drop.tip(tree, to_drop)
      }
    }
  }

  tree
}


#' Validate a phylo object
#'
#' Checks for 0 tips and duplicate tip labels.
#'
#' @param tree An `ape::phylo` object.
#' @return The tree (unchanged) if valid.
#' @keywords internal
pr_validate_tree <- function(tree) {
  if (!inherits(tree, "phylo")) {
    abort("Expected a phylo object.", call = caller_env())
  }
  tips <- tree$tip.label
  if (length(tips) == 0) {
    abort("Tree has no tips.", call = caller_env())
  }
  dup_tips <- tips[duplicated(tips)]
  if (length(dup_tips) > 0) {
    shown <- paste(utils::head(unique(dup_tips), 5), collapse = ", ")
    msg <- paste0(
      "Tree has ", length(dup_tips), " duplicate tip label(s): ", shown
    )
    if (length(unique(dup_tips)) > 5) msg <- paste0(msg, ", ...")
    abort(msg, call = caller_env())
  }
  tree
}
