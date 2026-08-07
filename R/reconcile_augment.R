# Tree augmentation --------------------------------------------------------

#' Graft missing species onto a phylogenetic tree (genus-level placement)
#'
#' When a reconciliation identifies species that are present in your data
#' but missing from the tree, `reconcile_augment()` attaches each missing
#' species as sister to a congener --- i.e., a species in the same genus
#' already present in the tree. The result is a tree that contains every
#' species in your dataset, at the cost of making a strong assumption
#' about where the new tips sit.
#'
#' @section When to use this:
#' Tip-grafting is an \emph{exploratory} convenience, not a substitute
#' for a properly inferred phylogeny. Both source modes (see below)
#' make strong placement assumptions that are often wrong in detail.
#' Use it to keep exploratory PCMs running while you decide how to
#' handle orphan species, and always:
#' \enumerate{
#'   \item Report exactly which species were augmented (see `$augmented`
#'     in the return value).
#'   \item Run sensitivity analyses with and without the augmented tips.
#'   \item Prefer a published imputed phylogeny (e.g. the PhyloMaker or
#'     TACT approaches) when grafting many species.
#' }
#'
#' @section Choosing a source:
#' \describe{
#'   \item{\code{"internal"} (default)}{Genus-level placement using
#'     only your tree (no external dependencies). Each missing
#'     species is attached as sister to a congener (or at the
#'     congeneric MRCA). Fast and reproducible, but only works when
#'     the genus is already represented in the tree, and assumes the
#'     new tip diverged in roughly the same way as its congeners.}
#'   \item{\code{"rtrees"}}{Delegates the grafting to the
#'     \code{rtrees} mega-tree machinery via
#'     \code{rtrees::get_tree(tree_by_user = TRUE)}. Uses your tree
#'     as the backbone and lets \code{rtrees} place each missing
#'     species using genus / family information from a taxon-specific
#'     reference tree. Requires \code{taxon} and the CRAN
#'     \code{rtrees} package (install with
#'     \code{install.packages("rtrees")})
#'     (\url{https://daijiang.github.io/rtrees/}). Helpful when the
#'     genus is absent from your tree but present in \code{rtrees}'
#'     reference --- which the internal mode would skip.}
#'   \item{\code{"vphylomaker"}}{Plant-only alternative to
#'     \code{"rtrees"} via either of the GitHub packages
#'     \pkg{V.PhyloMaker2}
#'     (\url{https://github.com/jinyizju/V.PhyloMaker2}, preferred
#'     when installed; updated and enlarged version) or
#'     \pkg{V.PhyloMaker}
#'     (\url{https://github.com/jinyizju/V.PhyloMaker}, used as a
#'     fallback; original 2019 version). Calls
#'     \code{phylo.maker(sp.list, tree, scenarios = ...)} with your
#'     tree as the backbone. Use this when you want explicit control
#'     over the V.PhyloMaker placement scenario (\code{"S1"},
#'     \code{"S2"}, or \code{"S3"} --- see Jin & Qian 2019/2022);
#'     otherwise \code{"rtrees"} with \code{taxon = "plant"} is
#'     simpler.}
#'   \item{\code{"uphylomaker"}}{Universal (plants + animals) variant
#'     of V.PhyloMaker, via the GitHub package \pkg{U.PhyloMaker}
#'     (\url{https://github.com/jinyizju/U.PhyloMaker}). Same
#'     \code{phylo.maker} convention but takes a `gen.list` (a
#'     genus-family lookup) so it can graft non-plant taxa as well
#'     as plants. Use this when your tree spans multiple kingdoms
#'     and you want the V.PhyloMaker placement strategy.}
#' }
#' Use [pr_get_tree()] when you have only a species list and need a
#' candidate tree from scratch (rotl, clootl, or rtrees). Use
#' `reconcile_augment()` when you already have a tree and want to fill
#' the gaps.
#'
#' @param reconciliation A [reconciliation] object, typically from
#'   [reconcile_tree()].
#' @param tree An `ape::phylo` object. Must be the same tree used to
#'   build `reconciliation` (or a tree with the same tip set). For
#'   `source = "rtrees"`, this is passed to \code{rtrees} as the
#'   user-supplied backbone (`tree_by_user = TRUE`).
#' @param where A length-1 character vector. Where to attach each new tip
#'   (only used when `source = "internal"`; ignored otherwise):
#'   \describe{
#'     \item{`"genus"` (default)}{Attach as sister to a single congener
#'       chosen at random from the genus. Recommended when the
#'       genus has only one or two representatives in the tree, or when
#'       you want variation across runs for sensitivity analyses.}
#'     \item{`"near"`}{Attach at the most recent common ancestor (MRCA)
#'       of all congeners in the tree. Better when the genus is
#'       well-represented, because the new tip is not arbitrarily tied
#'       to one sister taxon.}
#'   }
#' @param branch_length A length-1 character vector. How to set the terminal branch
#'   length of each newly added tip (only used when `source = "internal"`;
#'   ignored otherwise --- \code{rtrees} sets its own branch lengths):
#'   \describe{
#'     \item{`"congener_median"` (default)}{Median terminal branch length
#'       of the species' congeners. Uses the average "how long since
#'       this group diverged" for the genus. Recommended for
#'       time-calibrated trees because it preserves approximate
#'       branch-length scale.}
#'     \item{`"half_terminal"`}{Half the sister tip's terminal branch.
#'       A conservative alternative that places the new tip as a recent
#'       split from its sister. Useful when the genus is sparsely
#'       sampled and the median is unreliable.}
#'     \item{`"zero"`}{Zero-length branch, producing a polytomy with the
#'       sister taxon (or MRCA). Use for exploratory sensitivity checks
#'       where you want to see the effect of adding species without
#'       assuming any divergence time.}
#'   }
#'   When the input tree is ultrametric, each grafted tip's terminal
#'   edge is adjusted after placement so the augmented tree stays
#'   ultrametric --- a requirement of phylogenetic comparative
#'   methods. `branch_length` then governs the initial graft only;
#'   `"zero"` is exempt, since it asks for a polytomy by construction.
#' @param seed A length-1 integer or `NULL`. When non-`NULL` and
#'   `source = "internal"`, a fixed seed for the random congener
#'   choice when `where = "genus"`, making the call reproducible.
#'   When `NULL` (default), the session's current RNG state is used
#'   so results vary across runs --- useful for sensitivity analyses
#'   that explore the variation introduced by the random choice. Set
#'   to a fixed integer in real analyses so results are reproducible.
#'   The seed is scoped to this call: the session RNG state is saved
#'   before and restored after, so subsequent random draws in your
#'   script are unaffected. Default `NULL`. (For
#'   `source = "rtrees"`, set the seed in your script before calling
#'   `reconcile_augment()`; \code{rtrees} does not accept a seed
#'   argument.)
#' @param quiet Logical. Suppress progress messages? Default `FALSE`.
#' @param source A length-1 character vector. Which grafting backend
#'   to use. One of `"internal"` (default), `"rtrees"`,
#'   `"vphylomaker"`, or `"uphylomaker"`. See \dQuote{Choosing a source}.
#' @param taxon A length-1 character vector. Required when
#'   `source = "rtrees"`. One of `"bird"`, `"mammal"`, `"fish"`,
#'   `"amphibian"`, `"reptile"`, `"plant"`, `"shark_ray"`, `"bee"`,
#'   `"butterfly"`. Ignored for `"internal"`, `"vphylomaker"`, and
#'   `"uphylomaker"`.
#' @param check_ultrametric Logical. After grafting, check that the
#'   result is ultrametric and warn if not. Default `TRUE`. The
#'   `"rtrees"`, `"vphylomaker"`, and `"uphylomaker"` backends produce
#'   ultrametric trees by design; the `"internal"` backend does too
#'   when the input tree was ultrametric and `branch_length` is
#'   `"congener_median"` or `"half_terminal"`, but not when
#'   `branch_length = "zero"` (which produces zero-length tip edges
#'   that break ultrametricity by construction).
#' @param ... Additional arguments forwarded to the chosen backend:
#'   `rtrees::get_tree()` for `source = "rtrees"` (e.g. `scenario`,
#'   `n_tree`); `V.PhyloMaker2::phylo.maker()` for
#'   `source = "vphylomaker"` (e.g. `scenarios = "S3"`,
#'   `nodes.type`); `U.PhyloMaker::phylo.maker()` for
#'   `source = "uphylomaker"` (e.g. `gen.list`, `scenario`).
#'   Ignored when `source = "internal"`.
#'
#' @return A list with:
#'   \describe{
#'     \item{tree}{The augmented `phylo` object (or `multiPhylo`
#'       when `source = "rtrees"` returns a posterior sample).}
#'     \item{original}{The original (unmodified) `phylo` object, for
#'       easy comparison.}
#'     \item{augmented}{A tibble documenting each added species:
#'       `species`, `genus`, `placed_near` (sister tip / MRCA node /
#'       \code{rtrees} placement note), `branch_length`, `method`,
#'       `n_congeners`. For `source = "rtrees"`, `branch_length` and
#'       `n_congeners` are `NA` because the backend chooses them.}
#'     \item{skipped}{A tibble of species that could not be placed,
#'       with the reason (e.g. "No congener in tree", "rtrees did
#'       not place this species").}
#'     \item{meta}{Provenance metadata: source, placement strategy,
#'       branch length rule, counts; for `source = "rtrees"` includes
#'       a `backend_meta` sub-list with the taxon and the number of
#'       grafted tips.}
#'   }
#'
#' @family reconciliation functions
#' @seealso [reconcile_tree()] for the reconciliation step;
#'   [reconcile_apply()] for the non-augmenting alternative (prune data
#'   and tree to the intersection); [pr_get_tree()] for retrieving a
#'   candidate tree from external resources when you don't have a tree
#'   yet; [pr_date_tree()] for time-calibrating an existing topology;
#'   [pr_cite_tree()] for formatting tree provenance citations. The
#'   companion package
#'   \href{https://itchyshin.github.io/pigauto/}{pigauto} consumes the
#'   resulting tree (or `multiPhylo`) directly via
#'   `multi_impute_trees()` for posterior-tree PCMs.
#'
#' @references
#' Paradis, E. & Schliep, K. (2019). ape 5.0: an environment for modern
#' phylogenetics and evolutionary analyses in R. \emph{Bioinformatics}
#' 35: 526--528. \doi{10.1093/bioinformatics/bty633}
#'
#' Augmentation backends:
#'
#' Jin, Y. & Qian, H. (2019). V.PhyloMaker: an R package that can
#' generate very large phylogenies for vascular plants.
#' \emph{Ecography} 42(8): 1353--1359. \doi{10.1111/ecog.04434}
#' (`source = "vphylomaker"`, fallback path.)
#'
#' Jin, Y. & Qian, H. (2022). V.PhyloMaker2: an updated and enlarged R
#' package that can generate very large phylogenies for vascular plants.
#' \emph{Plant Diversity} 44(4): 335--339.
#' \doi{10.1016/j.pld.2022.05.005}
#' (`source = "vphylomaker"`, preferred path.)
#'
#' Jin, Y. & Qian, H. (2023). U.PhyloMaker: an R package that can
#' generate large phylogenetic trees for plants and animals.
#' \emph{Plant Diversity} 45(3): 347--352.
#' \doi{10.1016/j.pld.2022.12.007}
#' (`source = "uphylomaker"`.)
#'
#' @examples
#' # --- Example 1: genus-level placement with congener_median branch lengths ---
#' x <- data.frame(species = c("A a", "A missing", "B c", "C absent"))
#' tree <- ape::read.tree(text = "((A_a:1,A_b:1):1,B_c:2);")
#' result <- reconcile_tree(x, tree, x_species = "species",
#'                          authority = NULL, quiet = TRUE)
#'
#' aug <- reconcile_augment(result, tree, seed = 42, quiet = TRUE)
#'
#' # Compare original vs augmented tree
#' cat("Original tips:", ape::Ntip(tree), "\n")
#' cat("Augmented tips:", ape::Ntip(aug$tree), "\n")
#' cat("Added:", nrow(aug$augmented), "| Skipped:", nrow(aug$skipped), "\n")
#'
#' # Inspect which species were added and where they were placed
#' head(aug$augmented[, c("species", "genus", "placed_near",
#'                        "branch_length", "n_congeners")])
#'
#' # Species skipped (no congener in tree)
#' head(aug$skipped)
#'
#' # --- Example 2: MRCA placement with zero-length branches ---
#' aug_near <- reconcile_augment(result, tree,
#'                               where = "near",
#'                               branch_length = "zero",
#'                               seed = 42, quiet = TRUE)
#'
#' cat("\nMRCA placement (zero branches):\n")
#' cat("  Added:", nrow(aug_near$augmented), "\n")
#' # Compare: MRCA placement shows genus-level context
#' head(aug_near$augmented[, c("species", "placed_near", "method")])
#'
#' \donttest{
#'   # --- Example 3: delegate grafting to rtrees ---
#'   # Useful when the genus is missing from your tree but present in
#'   # the rtrees taxon-specific reference tree.
#'   if (requireNamespace("rtrees", quietly = TRUE)) {
#'     aug_rt <- try(
#'       reconcile_augment(result, tree,
#'                         source = "rtrees",
#'                         taxon  = "bird",
#'                         quiet  = TRUE),
#'       silent = TRUE
#'     )
#'     if (!inherits(aug_rt, "try-error")) {
#'       nrow(aug_rt$augmented)              # how many were placed
#'       aug_rt$meta$backend_meta$n_grafted  # how many at higher rank
#'     }
#'   }
#' }
#'
#' @export
reconcile_augment <- function(reconciliation,
                               tree,
                               where = c("genus", "near"),
                               branch_length = c("congener_median",
                                                  "half_terminal",
                                                  "zero"),
                               seed = NULL,
                               quiet = FALSE,
                               source = c("internal", "rtrees",
                                           "vphylomaker", "uphylomaker"),
                               taxon = NULL,
                               check_ultrametric = TRUE,
                               ...) {

  validate_reconciliation(reconciliation)
  source <- match.arg(source)
  where <- match.arg(where)
  branch_length <- match.arg(branch_length)
  tree <- pr_load_tree(tree)
  original_tree <- tree

  # Find unresolved species (in data, not in tree)
  mapping <- reconciliation$mapping
  unresolved <- mapping[mapping$match_type == "unresolved" &
                          mapping$in_x & !mapping$in_y, ]
  species_to_add <- unresolved$name_x
  species_to_add <- species_to_add[!is.na(species_to_add)]

  if (length(species_to_add) == 0) {
    if (!quiet) cli_alert_info("No unresolved species to augment.")
    return(list(
      tree      = tree,
      original  = original_tree,
      augmented = tibble(species = character(), genus = character(),
                         placed_near = character(), branch_length = numeric(),
                         method = character(), n_congeners = integer()),
      skipped   = tibble(species = character(), genus = character(),
                         reason = character()),
      meta      = list(n_augmented = 0L, n_skipped = 0L,
                        where = where, branch_length_method = branch_length,
                        source = source)
    ))
  }

  # --- rtrees dispatch -----------------------------------------------------
  # When source = "rtrees", hand the species list and our tree to
  # rtrees::get_tree(tree_by_user = TRUE) and let it do the grafting.
  if (source == "rtrees") {
    if (!quiet) {
      cli_alert_info(
        "Augmenting tree with {length(species_to_add)} unresolved species via {.pkg rtrees}..."
      )
    }
    res <- .pr_augment_rtrees(species_to_add,
                              tree    = tree,
                              taxon   = taxon,
                              quiet   = quiet,
                              ...)
    new_n_tips <- if (inherits(res$tree, "multiPhylo")) {
      ape::Ntip(res$tree[[1]])
    } else {
      ape::Ntip(res$tree)
    }
    if (!quiet) {
      cli_alert_success(
        "Added {nrow(res$augmented)} species via rtrees ({nrow(res$skipped)} could not be placed)"
      )
      if (nrow(res$skipped) > 0) {
        cli_alert_warning(
          "Skipped species could not be placed by rtrees. See $skipped for details."
        )
      }
    }
    if (isTRUE(check_ultrametric)) {
      .pr_check_tree_ultrametric(res$tree, source = "rtrees")
    }
    return(list(
      tree      = res$tree,
      original  = original_tree,
      augmented = res$augmented,
      skipped   = res$skipped,
      meta      = list(
        n_augmented          = nrow(res$augmented),
        n_skipped            = nrow(res$skipped),
        where                = NA_character_,
        branch_length_method = NA_character_,
        seed                 = seed,
        source               = "rtrees",
        backend_meta         = res$backend_meta,
        original_n_tips      = ape::Ntip(original_tree),
        augmented_n_tips     = new_n_tips
      )
    ))
  }

  # --- V.PhyloMaker / U.PhyloMaker dispatch ------------------------------
  # Both call the same phylo.maker convention; we dispatch on `source`.
  if (source %in% c("vphylomaker", "uphylomaker")) {
    pkg_label <- if (source == "vphylomaker") "V.PhyloMaker" else "U.PhyloMaker"
    if (!quiet) {
      cli_alert_info(
        "Augmenting tree with {length(species_to_add)} unresolved species via {.pkg {pkg_label}}..."
      )
    }
    res <- if (source == "vphylomaker") {
      .pr_augment_vphylomaker(species_to_add, tree, quiet = quiet, ...)
    } else {
      .pr_augment_uphylomaker(species_to_add, tree, quiet = quiet, ...)
    }
    new_n_tips <- if (inherits(res$tree, "multiPhylo")) {
      ape::Ntip(res$tree[[1]])
    } else {
      ape::Ntip(res$tree)
    }
    if (!quiet) {
      cli_alert_success(
        "Added {nrow(res$augmented)} species via {pkg_label} ({nrow(res$skipped)} could not be placed)"
      )
    }
    if (isTRUE(check_ultrametric)) {
      .pr_check_tree_ultrametric(res$tree, source = source)
    }
    return(list(
      tree      = res$tree,
      original  = original_tree,
      augmented = res$augmented,
      skipped   = res$skipped,
      meta      = list(
        n_augmented          = nrow(res$augmented),
        n_skipped            = nrow(res$skipped),
        where                = NA_character_,
        branch_length_method = NA_character_,
        seed                 = seed,
        source               = source,
        backend_meta         = res$backend_meta,
        original_n_tips      = ape::Ntip(original_tree),
        augmented_n_tips     = new_n_tips
      )
    ))
  }

  # --- internal genus-level placement (default) ---------------------------
  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }
    on.exit(
      if (!is.null(old_seed)) {
        assign(".Random.seed", old_seed, envir = globalenv())
      },
      add = TRUE
    )
    set.seed(seed)
  }

  if (!quiet) {
    cli_alert_info(
      "Augmenting tree with {length(species_to_add)} unresolved species..."
    )
  }

  # Extract genera from species names (normalise underscores to match tree tips)
  genera <- pr_extract_genus(gsub("_", " ", species_to_add))

  # Pre-compute genus for every tip once; updated incrementally as tips are added
  tip_genera <- stats::setNames(
    pr_extract_genus(gsub("_", " ", tree$tip.label)),
    tree$tip.label
  )

  augmented_rows <- list()
  skipped_rows <- list()

  for (i in seq_along(species_to_add)) {
    sp <- species_to_add[i]
    genus <- genera[i]

    if (is.na(genus) || nchar(genus) == 0) {
      skipped_rows[[length(skipped_rows) + 1]] <- tibble(
        species = sp, genus = NA_character_,
        reason = "Could not extract genus from name"
      )
      next
    }

    # O(1) congener lookup from pre-computed vector
    congener_tips <- names(tip_genera)[!is.na(tip_genera) & tip_genera == genus]

    if (length(congener_tips) == 0) {
      skipped_rows[[length(skipped_rows) + 1]] <- tibble(
        species = sp, genus = genus,
        reason = "No congener in tree"
      )
      next
    }

    # Calculate branch length
    bl <- pr_calc_augment_bl(tree, congener_tips, branch_length)

    if (bl < .Machine$double.eps && branch_length != "zero") {
      cli_alert_warning(
        "Branch length is 0 for '{sp}'; added tip creates a polytomy."
      )
    }

    # Add species to tree
    sp_label <- gsub(" ", "_", sp)
    result <- pr_bind_species(tree, sp_label, congener_tips, where, bl)
    tree <- result$tree

    # Incrementally update the genus lookup so subsequent iterations see this tip
    tip_genera[[sp_label]] <- genus

    augmented_rows[[length(augmented_rows) + 1]] <- tibble(
      species       = sp,
      genus         = genus,
      placed_near   = gsub("_", " ", result$placed_near),
      branch_length = bl,
      method        = paste0(where, "/", branch_length),
      n_congeners   = length(congener_tips)
    )
  }

  # Build result tibbles
  augmented <- if (length(augmented_rows) > 0) {
    do.call(rbind, augmented_rows)
  } else {
    tibble(species = character(), genus = character(),
           placed_near = character(), branch_length = numeric(),
           method = character(), n_congeners = integer())
  }

  skipped <- if (length(skipped_rows) > 0) {
    do.call(rbind, skipped_rows)
  } else {
    tibble(species = character(), genus = character(),
           reason = character())
  }

  if (!quiet) {
    cli_alert_success(
      "Added {nrow(augmented)} species to tree ({nrow(skipped)} skipped)"
    )
    if (nrow(skipped) > 0) {
      cli_alert_warning(
        "Skipped species had no congener in the tree. See $skipped for details."
      )
    }
  }

  # Preserve ultrametricity. Each new tip's terminal edge is set from
  # `branch_length` independently of where it attached, so a grafted
  # tip generally does not land at the present and the tree stops
  # being ultrametric -- which comparative methods (PGLS, phylogenetic
  # meta-analysis) require. When the input tree was ultrametric, and
  # the user did not ask for zero-length (polytomy) grafts, adjust each
  # grafted tip's terminal edge so it reaches the common root-to-tip
  # depth. Adjusting one terminal edge moves only that tip, so a single
  # depth snapshot suffices even for several (or chained) grafts.
  if (branch_length != "zero" &&
      nrow(augmented) > 0L &&
      !is.null(original_tree$edge.length) &&
      isTRUE(tryCatch(ape::is.ultrametric(original_tree, tol = 1e-6),
                      error = function(e) FALSE))) {
    grafted_tips <- setdiff(tree$tip.label, original_tree$tip.label)
    depths <- ape::node.depth.edgelength(tree)
    target <- max(depths[match(original_tree$tip.label, tree$tip.label)])
    for (gt in grafted_tips) {
      tip_idx  <- match(gt, tree$tip.label)
      edge_idx <- match(tip_idx, tree$edge[, 2])
      if (!is.na(edge_idx)) {
        tree$edge.length[edge_idx] <-
          tree$edge.length[edge_idx] + (target - depths[tip_idx])
      }
    }
  }

  # Ultrametric check: skip when branch_length = "zero" because that
  # produces zero-length tip edges by design (the user asked for a
  # polytomy / sensitivity check); warn for the other branch-length
  # rules where ultrametricity is the expected output.
  if (isTRUE(check_ultrametric) && branch_length != "zero") {
    .pr_check_tree_ultrametric(tree, source = "internal")
  }

  list(
    tree      = tree,
    original  = original_tree,
    augmented = augmented,
    skipped   = skipped,
    meta      = list(
      n_augmented          = nrow(augmented),
      n_skipped            = nrow(skipped),
      where                = where,
      branch_length_method = branch_length,
      seed                 = seed,
      source               = "internal",
      original_n_tips      = ape::Ntip(original_tree),
      augmented_n_tips     = ape::Ntip(tree)
    )
  )
}


# Internal helpers ---------------------------------------------------------

#' Extract genus from binomial species names
#'
#' Takes the first word of each name as the genus.
#'
#' @param names Character vector of species names.
#' @return Character vector of genus names.
#' @keywords internal
pr_extract_genus <- function(names) {
  vapply(strsplit(names, "\\s+"), function(parts) {
    if (length(parts) >= 1 && nchar(parts[1]) > 0) parts[1] else NA_character_
  }, character(1))
}


#' Calculate branch length for an augmented tip
#'
#' @param tree phylo object.
#' @param congener_tips Character vector of congener tip labels.
#' @param method Branch length strategy.
#' @return Numeric(1) branch length.
#' @keywords internal
pr_calc_augment_bl <- function(tree, congener_tips, method) {
  switch(method,
    congener_median = {
      # Get terminal branch lengths of congeners
      tip_idx <- match(congener_tips, tree$tip.label)
      tip_idx <- tip_idx[!is.na(tip_idx)]
      if (length(tip_idx) == 0) return(0)
      edge_idx <- match(tip_idx, tree$edge[, 2])
      edge_idx <- edge_idx[!is.na(edge_idx)]
      if (length(edge_idx) == 0 || is.null(tree$edge.length)) return(0)
      bls <- tree$edge.length[edge_idx]
      stats::median(bls, na.rm = TRUE)
    },
    half_terminal = {
      # Half the first congener's terminal branch
      tip_idx <- match(congener_tips[1], tree$tip.label)
      if (is.na(tip_idx) || is.null(tree$edge.length)) return(0)
      edge_idx <- match(tip_idx, tree$edge[, 2])
      if (is.na(edge_idx)) return(0)
      tree$edge.length[edge_idx] / 2
    },
    zero = 0,
    0
  )
}


#' Bind a species to a tree as sister to a congener
#'
#' Uses `phytools::bind.tip()` if available, otherwise falls back to a
#' pure-ape implementation using `ape::bind.tree()`.
#'
#' @param tree phylo object.
#' @param sp_label A length-1 character vector. Tip label to add (underscore format).
#' @param congener_tips Character vector of congener tip labels.
#' @param where Placement strategy.
#' @param bl Branch length for the new tip.
#' @return List with `tree` and `placed_near`.
#' @keywords internal
pr_bind_species <- function(tree, sp_label, congener_tips, where, bl) {

  if (where == "genus") {
    # Pick a random congener as sister
    sister <- sample(congener_tips, 1)
    sister_idx <- which(tree$tip.label == sister)
    edge_idx <- match(sister_idx, tree$edge[, 2])

    # Attachment point: midway along the sister's terminal branch
    sister_bl <- if (!is.null(tree$edge.length) && !is.na(edge_idx)) {
      tree$edge.length[edge_idx]
    } else {
      1
    }
    attach_point <- sister_bl / 2

    tree <- pr_bind_tip(tree, sp_label,
                        where = sister_idx,
                        position = attach_point,
                        edge.length = bl)

    return(list(tree = tree, placed_near = sister))
  }

  if (where == "near") {
    if (length(congener_tips) == 1) {
      # Monotypic genus — same as "genus" strategy
      return(pr_bind_species(tree, sp_label, congener_tips, "genus", bl))
    }

    # Attach at MRCA of all congeners
    congener_idx <- match(congener_tips, tree$tip.label)
    congener_idx <- congener_idx[!is.na(congener_idx)]
    mrca_node <- ape::getMRCA(tree, congener_idx)

    if (is.null(mrca_node)) {
      # Fallback to genus strategy
      return(pr_bind_species(tree, sp_label, congener_tips, "genus", bl))
    }

    tree <- pr_bind_tip(tree, sp_label,
                        where = mrca_node,
                        position = 0,
                        edge.length = bl)

    return(list(tree = tree,
                placed_near = paste("MRCA of",
                                    paste(utils::head(congener_tips, 3),
                                          collapse = ", "))))
  }

  abort(paste0("Unknown placement strategy: '", where, "'"),
        call = caller_env())
}


#' Internal: delegate grafting to rtrees::get_tree(tree_by_user = TRUE)
#'
#' @param species_to_add Character vector of binomials present in data
#'   but absent from the tree.
#' @param tree The user's backbone phylo.
#' @param taxon One of rtrees' supported taxa.
#' @param quiet Logical.
#' @param ... Forwarded to `rtrees::get_tree()`.
#' @return A list with `tree`, `augmented`, `skipped`, `backend_meta`.
#' @keywords internal
.pr_augment_rtrees <- function(species_to_add, tree, taxon = NULL,
                                quiet = FALSE, ...) {
  if (!requireNamespace("rtrees", quietly = TRUE)) {
    cli::cli_abort(
      c("{.code source = \"rtrees\"} requires the {.pkg rtrees} package.",
        "i" = 'Install with: {.code install.packages("rtrees")}.',
        ">" = "See {.url https://daijiang.github.io/rtrees/} for details.")
    )
  }
  if (is.null(taxon) || !nzchar(taxon)) {
    cli::cli_abort(
      c("{.code source = \"rtrees\"} requires a {.arg taxon} argument.",
        "i" = "One of: {.val bird}, {.val mammal}, {.val fish}, {.val amphibian}, {.val reptile}, {.val plant}, {.val shark_ray}, {.val bee}, {.val butterfly}.",
        ">" = "Example: {.code reconcile_augment(rec, tree, source = \"rtrees\", taxon = \"bird\")}.")
    )
  }

  # rtrees::get_tree expects sp_list as either character or a data.frame
  # with cols `species`, `genus`, `family`. We pass character; rtrees
  # parses the genus from the binomial.
  augmented_tree <- .pr_rtrees_get_tree(
    sp_list      = species_to_add,
    tree         = tree,
    taxon        = taxon,
    tree_by_user = TRUE,
    show_grafted = TRUE,
    ...
  )

  # rtrees may return phylo (single best-guess tree) or multiPhylo
  # (posterior sample). All trees in a multiPhylo share the tip set so
  # the first one is enough for matching.
  is_multi <- inherits(augmented_tree, "multiPhylo")
  ref_tips <- if (is_multi) {
    augmented_tree[[1]]$tip.label
  } else {
    augmented_tree$tip.label
  }

  # rtrees flags genus and family grafts with one and two trailing stars.
  # Keep those labels in the returned tree; use markerless labels only for
  # matching requested species.
  tip_info <- .pr_parse_rtrees_tip_labels(ref_tips)
  norm_req <- pr_normalize_names(species_to_add)
  norm_tip <- pr_normalize_names(tip_info$markerless_label)
  in_tree  <- norm_req %in% norm_tip
  added_species   <- species_to_add[in_tree]
  skipped_species <- species_to_add[!in_tree]

  # Identify placement rank for every requested species retained in the tree.
  status_by_clean <- stats::setNames(tip_info$placement_status, norm_tip)
  added_norm    <- pr_normalize_names(added_species)
  placement_status <- unname(status_by_clean[added_norm])
  was_grafted <- placement_status != "exact"
  n_exact <- sum(placement_status == "exact")
  n_genus_added <- sum(placement_status == "genus_added")
  n_family_added <- sum(placement_status == "family_added")
  n_skipped <- length(skipped_species)

  augmented <- if (length(added_species) > 0) {
    tibble(
      species       = added_species,
      genus         = pr_extract_genus(added_species),
      placed_near   = ifelse(
        placement_status == "family_added",
        "rtrees: grafted at family-level node",
        ifelse(
          placement_status == "genus_added",
          "rtrees: grafted at genus-level node",
          "rtrees: placed at species level"
        )
      ),
      branch_length = NA_real_,        # rtrees decides
      method        = paste0(
        "rtrees/", taxon,
        ifelse(was_grafted, "/grafted", "/placed")
      ),
      n_congeners   = NA_integer_      # not applicable
    )
  } else {
    tibble(species = character(), genus = character(),
           placed_near = character(), branch_length = numeric(),
           method = character(), n_congeners = integer())
  }

  skipped <- if (length(skipped_species) > 0) {
    tibble(
      species = skipped_species,
      genus   = pr_extract_genus(skipped_species),
      reason  = "rtrees did not place this species"
    )
  } else {
    tibble(species = character(), genus = character(),
           reason = character())
  }

  list(
    tree         = augmented_tree,
    augmented    = augmented,
    skipped      = skipped,
    backend_meta = list(
      backend   = "rtrees",
      taxon     = taxon,
      n_exact = n_exact,
      n_genus_added = n_genus_added,
      n_family_added = n_family_added,
      n_skipped = n_skipped,
      n_grafted = n_genus_added + n_family_added,
      n_returned = if (is_multi) length(augmented_tree) else 1L
    )
  )
}


#' Internal: delegate grafting to V.PhyloMaker2::phylo.maker()
#'
#' Plant-only alternative to the rtrees backend. Wraps
#' `V.PhyloMaker2::phylo.maker()` so the user can pick a specific
#' V.PhyloMaker scenario (S1 / S2 / S3, see Jin & Qian 2019).
#'
#' @param species_to_add Character vector of binomials to graft.
#' @param tree The user's backbone phylo.
#' @param scenarios Character. One of "S1", "S2", "S3" (default
#'   "S3"). Forwarded to `V.PhyloMaker2::phylo.maker()`.
#' @param quiet Logical.
#' @param ... Forwarded to `V.PhyloMaker2::phylo.maker()`.
#' @return A list with `tree`, `augmented`, `skipped`, `backend_meta`.
#'
#' @references
#' Jin, Y. & Qian, H. (2019). V.PhyloMaker: an R package that can
#' generate very large phylogenies for vascular plants.
#' \emph{Ecography} 42(8): 1353--1359. \doi{10.1111/ecog.04434}
#'
#' Jin, Y. & Qian, H. (2022). V.PhyloMaker2: an updated and enlarged R
#' package that can generate very large phylogenies for vascular plants.
#' \emph{Plant Diversity} 44(4): 335--339.
#' \doi{10.1016/j.pld.2022.05.005}
#'
#' @keywords internal
.pr_augment_vphylomaker <- function(species_to_add, tree,
                                      scenarios = "S3",
                                      quiet = FALSE, ...) {
  # Prefer V.PhyloMaker2 when available; fall back to V.PhyloMaker
  # (the original). Both packages expose `phylo.maker()` with the
  # same calling convention, so the dispatch is just choosing the
  # namespace.
  pkg <- if (pr_namespace_available("V.PhyloMaker2")) {
    "V.PhyloMaker2"
  } else if (pr_namespace_available("V.PhyloMaker")) {
    "V.PhyloMaker"
  } else {
    cli::cli_abort(
      c("{.code source = \"vphylomaker\"} requires either {.pkg V.PhyloMaker2} or {.pkg V.PhyloMaker}.",
        "i" = 'Install V2 (preferred) with: {.code pak::pak("jinyizju/V.PhyloMaker2")}.',
        "i" = 'Or install V1 with: {.code pak::pak("jinyizju/V.PhyloMaker")}.',
        ">" = "See {.url https://github.com/jinyizju/V.PhyloMaker2} for details.")
    )
  }
  if (!quiet) {
    cli::cli_alert_info("Using {.pkg {pkg}} (preferred when both are installed: V2).")
  }

  # V.PhyloMaker (both 2 and 3) wants a sp.list data.frame with cols
  # `species`, `genus`, `family`. We provide species + genus from the
  # binomial; family is left NA (V.PhyloMaker fills it from its
  # taxonomy).
  sp_list <- data.frame(
    species = species_to_add,
    genus   = pr_extract_genus(species_to_add),
    family  = NA_character_,
    stringsAsFactors = FALSE
  )

  phylo_maker_fn <- get("phylo.maker", envir = asNamespace(pkg))
  pm <- phylo_maker_fn(
    sp.list   = sp_list,
    tree      = tree,
    scenarios = scenarios,
    ...
  )
  # phylo.maker returns a list; the dated tree lives at $scenario.<name>
  # for scenarios S1/S2 and at $species.list, $scenario.3 for S3.
  # Pull the first phylo we find.
  augmented_tree <- if (inherits(pm, "phylo")) {
    pm
  } else if (is.list(pm)) {
    cand <- pm[vapply(pm, inherits, logical(1), what = "phylo")]
    if (length(cand) == 0L) {
      cli::cli_abort(c(
        "Unexpected return type from {.code V.PhyloMaker2::phylo.maker()}.",
        "i" = "Got names: {.val {names(pm)}}.")
      )
    }
    cand[[1]]
  } else {
    cli::cli_abort(
      "Unexpected return type from {.code V.PhyloMaker2::phylo.maker()}: {.cls {class(pm)[1]}}."
    )
  }

  # Determine matched / unmatched by intersecting against the new tip set
  ref_tips <- augmented_tree$tip.label
  norm_req <- pr_normalize_names(species_to_add)
  norm_tip <- pr_normalize_names(ref_tips)
  in_tree  <- norm_req %in% norm_tip

  added_species   <- species_to_add[in_tree]
  skipped_species <- species_to_add[!in_tree]

  augmented <- if (length(added_species) > 0) {
    tibble(
      species       = added_species,
      genus         = pr_extract_genus(added_species),
      placed_near   = paste0("V.PhyloMaker (", scenarios, ")"),
      branch_length = NA_real_,
      method        = paste0("vphylomaker/", scenarios),
      n_congeners   = NA_integer_
    )
  } else {
    tibble(species = character(), genus = character(),
           placed_near = character(), branch_length = numeric(),
           method = character(), n_congeners = integer())
  }

  skipped <- if (length(skipped_species) > 0) {
    tibble(
      species = skipped_species,
      genus   = pr_extract_genus(skipped_species),
      reason  = "V.PhyloMaker did not place this species"
    )
  } else {
    tibble(species = character(), genus = character(),
           reason = character())
  }

  list(
    tree         = augmented_tree,
    augmented    = augmented,
    skipped      = skipped,
    backend_meta = list(
      backend    = "vphylomaker",
      package    = pkg,
      scenarios  = scenarios,
      references = c(
        v2 = "Jin Y & Qian H (2022) V.PhyloMaker2: an updated and enlarged R package that can generate very large phylogenies for vascular plants. Plant Diversity 44:335-339. doi:10.1016/j.pld.2022.05.005",
        v1 = "Jin Y & Qian H (2019) V.PhyloMaker: an R package that can generate very large phylogenies for vascular plants. Ecography 42:1353-1359. doi:10.1111/ecog.04434"
      )
    )
  )
}


#' Internal: delegate grafting to U.PhyloMaker::phylo.maker()
#'
#' Universal (plants + animals) variant of the V.PhyloMaker grafting
#' strategy. Wraps `U.PhyloMaker::phylo.maker()` so the user can pick
#' a specific scenario.
#'
#' @param species_to_add Character vector of binomials to graft.
#' @param tree The user's backbone phylo.
#' @param gen.list A data.frame mapping genus -> family. Required by
#'   U.PhyloMaker. If `NULL`, the function attempts to load the
#'   bundled `U.PhyloMaker::nodes.info.1` lookup; if that fails,
#'   errors with an instructive message.
#' @param scenario Character. One of "S1", "S2", "S3". Default "S3".
#' @param quiet Logical.
#' @param ... Forwarded to `U.PhyloMaker::phylo.maker()`.
#' @return A list with `tree`, `augmented`, `skipped`, `backend_meta`.
#'
#' @references
#' Jin, Y. & Qian, H. (2023). U.PhyloMaker: an R package that can
#' generate large phylogenetic trees for plants and animals.
#' \emph{Plant Diversity} 45(3): 347--352.
#' \doi{10.1016/j.pld.2022.12.007}
#'
#' @keywords internal
.pr_augment_uphylomaker <- function(species_to_add, tree,
                                      gen.list = NULL,
                                      scenario = "S3",
                                      quiet = FALSE, ...) {
  if (!pr_namespace_available("U.PhyloMaker")) {
    cli::cli_abort(
      c("{.code source = \"uphylomaker\"} requires the {.pkg U.PhyloMaker} package.",
        "i" = 'Install with: {.code pak::pak("jinyizju/U.PhyloMaker")} (GitHub-only).',
        ">" = "See {.url https://github.com/jinyizju/U.PhyloMaker} for details.")
    )
  }

  # U.PhyloMaker wants sp.list with cols species, genus, family, and a
  # gen.list mapping genus -> family. We provide species + genus from
  # the binomial; users can pass a custom `gen.list` via ..., otherwise
  # we try to use U.PhyloMaker's bundled lookup.
  sp_list <- data.frame(
    species = species_to_add,
    genus   = pr_extract_genus(species_to_add),
    family  = NA_character_,
    species.relative = NA_character_,
    genus.relative   = NA_character_,
    stringsAsFactors = FALSE
  )

  if (is.null(gen.list)) {
    gen.list <- tryCatch(
      get("nodes.info.1", envir = asNamespace("U.PhyloMaker")),
      error = function(e) NULL
    )
    if (is.null(gen.list)) {
      cli::cli_abort(c(
        "{.fn U.PhyloMaker::phylo.maker} requires a {.arg gen.list} argument.",
        "i" = "Pass a data.frame mapping genus -> family via {.code ...} (e.g. {.code gen.list = my_gen_list}).",
        ">" = "See {.url https://github.com/jinyizju/U.PhyloMaker} for the expected format."
      ))
    }
  }

  if (!quiet) {
    cli::cli_alert_info("Calling {.code U.PhyloMaker::phylo.maker()} with scenario = {.val {scenario}}.")
  }

  phylo_maker_fn <- get("phylo.maker", envir = asNamespace("U.PhyloMaker"))
  pm <- phylo_maker_fn(
    sp.list  = sp_list,
    tree     = tree,
    gen.list = gen.list,
    scenario = scenario,
    ...
  )
  augmented_tree <- if (inherits(pm, "phylo")) {
    pm
  } else if (is.list(pm)) {
    cand <- pm[vapply(pm, inherits, logical(1), what = "phylo")]
    if (length(cand) == 0L) {
      cli::cli_abort(c(
        "Unexpected return type from {.code U.PhyloMaker::phylo.maker()}.",
        "i" = "Got names: {.val {names(pm)}}.")
      )
    }
    cand[[1]]
  } else {
    cli::cli_abort(
      "Unexpected return type from {.code U.PhyloMaker::phylo.maker()}: {.cls {class(pm)[1]}}."
    )
  }

  ref_tips <- augmented_tree$tip.label
  norm_req <- pr_normalize_names(species_to_add)
  norm_tip <- pr_normalize_names(ref_tips)
  in_tree  <- norm_req %in% norm_tip

  added_species   <- species_to_add[in_tree]
  skipped_species <- species_to_add[!in_tree]

  augmented <- if (length(added_species) > 0) {
    tibble(
      species       = added_species,
      genus         = pr_extract_genus(added_species),
      placed_near   = paste0("U.PhyloMaker (", scenario, ")"),
      branch_length = NA_real_,
      method        = paste0("uphylomaker/", scenario),
      n_congeners   = NA_integer_
    )
  } else {
    tibble(species = character(), genus = character(),
           placed_near = character(), branch_length = numeric(),
           method = character(), n_congeners = integer())
  }
  skipped <- if (length(skipped_species) > 0) {
    tibble(
      species = skipped_species,
      genus   = pr_extract_genus(skipped_species),
      reason  = "U.PhyloMaker did not place this species"
    )
  } else {
    tibble(species = character(), genus = character(),
           reason = character())
  }

  list(
    tree         = augmented_tree,
    augmented    = augmented,
    skipped      = skipped,
    backend_meta = list(
      backend   = "uphylomaker",
      scenario  = scenario,
      reference = "Jin Y & Qian H (2023) U.PhyloMaker: an R package that can generate large phylogenetic trees for plants and animals. Plant Diversity 45:347-352. doi:10.1016/j.pld.2022.12.007"
    )
  )
}


#' Bind a tip to a tree
#'
#' Wrapper that uses `phytools::bind.tip()` if available, otherwise
#' uses a pure-ape implementation via `ape::bind.tree()`.
#'
#' @param tree phylo object.
#' @param tip_label A length-1 character vector. Label for the new tip.
#' @param where Integer. Node or tip index to bind near.
#' @param position Numeric. How far back from the node to attach.
#' @param edge.length Numeric. Branch length of the new tip.
#' @return A modified phylo object.
#' @keywords internal
pr_bind_tip <- function(tree, tip_label, where, position = 0,
                        edge.length = 0) {
  # Try phytools first
  if (requireNamespace("phytools", quietly = TRUE)) {
    return(phytools::bind.tip(tree, tip_label,
                              where = where,
                              position = position,
                              edge.length = edge.length))
  }

  # Pure-ape fallback: create a 1-tip tree and bind it
  new_tip <- list(
    edge        = matrix(c(2L, 1L), 1, 2),
    tip.label   = tip_label,
    edge.length = edge.length,
    Nnode       = 1L
  )
  class(new_tip) <- "phylo"

  # Find the edge leading to `where`
  edge_row <- which(tree$edge[, 2] == where)
  if (length(edge_row) == 0) {
    # `where` is the root — bind at root
    tree <- ape::bind.tree(tree, new_tip)
    return(tree)
  }

  parent_bl <- tree$edge.length[edge_row]
  if (is.null(parent_bl)) parent_bl <- 1

  if (position > 0 && position < parent_bl) {
    # ape::bind.tree() handles splitting the edge and adjusting the
    # original branch length; pre-shortening here would shorten it twice.
    tree <- ape::bind.tree(tree, new_tip, where = where, position = position)
  } else {
    # Binding a zero-length new tip directly onto a zero-length terminal
    # edge can replace the existing tip. Attach at the parent node instead,
    # giving a zero-length polytomy while preserving the sister tip.
    bind_where <- if (
      position <= 0 &&
        where <= length(tree$tip.label) &&
        length(edge_row) > 0
    ) {
      tree$edge[edge_row[1], 1]
    } else {
      where
    }
    tree <- ape::bind.tree(tree, new_tip, where = bind_where)
  }

  tree
}
