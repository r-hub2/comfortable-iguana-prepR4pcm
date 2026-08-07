#' prepR4pcm: Reconcile species names for phylogenetic comparative methods
#'
#' Species names in your dataset rarely match the tip labels of your
#' phylogenetic tree. Formatting differences (`Homo_sapiens` vs
#' `Homo sapiens`), taxonomic synonymy (*Corvus brachyrhynchos* splits and
#' lumps), and simple spelling mistakes silently drop species from PGLS,
#' phylogenetic mixed models, and other phylogenetic comparative methods
#' (PCMs). `prepR4pcm` is a toolkit for ecologists and evolutionary
#' biologists to detect and resolve these mismatches, audit every decision,
#' and produce aligned data-tree pairs ready for downstream analysis.
#'
#' @section Typical workflow:
#' A minimal end-to-end pipeline looks like this:
#'
#' ```r
#' # 1. Match your data frame to a tree
#' rec <- reconcile_tree(
#'   avonet_subset, tree_jetz,
#'   x_species = "Species1",
#'   fuzzy     = TRUE          # enable typo correction
#' )
#'
#' # 2. Review what matched, what is flagged, what is unresolved
#' reconcile_summary(rec)
#' reconcile_plot(rec)
#' reconcile_suggest(rec)      # suggest near-misses for unresolved names
#'
#' # 3. Correct any unresolved or flagged cases by hand
#' rec <- reconcile_override(rec,
#'         name_x = "Corvus brachyrhnchos",  # typo in data
#'         name_y = "Corvus_brachyrhynchos")
#'
#' # 4. Produce an aligned dataset and pruned tree
#' aligned <- reconcile_apply(rec,
#'                            data = avonet_subset, tree = tree_jetz,
#'                            species_col = "Species1",
#'                            drop_unresolved = TRUE)
#'
#' # 5. aligned$data and aligned$tree are ready for downstream PCM tools
#' ```
#'
#' @section Key concepts:
#' \describe{
#'
#'   \item{Reconciliation object}{The central data structure. Contains a
#'     `mapping` tibble (one row per source name, with match type and
#'     score), a `meta` list (reproducibility provenance), a `counts`
#'     summary, an `overrides` log of applied manual corrections, and an
#'     `unused_overrides` audit trail of overrides that could not be
#'     applied (e.g. when `name_y` is missing from the target). Returned
#'     by all `reconcile_*` matching functions. Inspect with
#'     [reconcile_summary()], extract the table with [reconcile_mapping()],
#'     and act on it with [reconcile_apply()], [reconcile_merge()], or
#'     [reconcile_export()].}
#'
#'   \item{Four-stage matching cascade}{Names are resolved in this order,
#'     and the first stage that produces a match is recorded as
#'     `match_type`:
#'     \enumerate{
#'       \item \strong{exact} --- verbatim string equality.
#'       \item \strong{normalized} --- after removing underscores, fixing
#'         case, stripping authority strings (*Corvus corax* Linnaeus 1758),
#'         and applying diacritic folding.
#'       \item \strong{synonym} --- via a local taxonomic database
#'         (see \pkg{taxadb}) such as Catalogue of Life or GBIF.
#'       \item \strong{fuzzy} --- character-level similarity on the remaining
#'         unmatched names (opt-in via `fuzzy = TRUE`).
#'     }
#'     Any additional `overrides` or manual edits are applied on top as
#'     `match_type = "manual"`.}
#'
#'   \item{Provenance}{Every decision is logged in the mapping table
#'     (`match_type`, `match_score`, `match_source`) and in `meta` (package
#'     version, timestamp, taxonomic authority, fuzzy threshold, etc.).
#'     Use [reconcile_report()] to produce a shareable HTML audit trail
#'     for supplementary materials or collaborators.}
#'
#'   \item{Splits and lumps}{Taxonomic revisions often split one species
#'     into several, or lump several into one. [reconcile_splits_lumps()]
#'     flags these cases so you can decide how to handle them before
#'     analysis.}
#'
#'   \item{Tree augmentation}{When unresolved species have congeners in the
#'     tree, [reconcile_augment()] can graft them in as sister taxa at
#'     genus level. This is an \emph{exploratory} aid: always run
#'     sensitivity analyses with and without augmented tips.}
#' }
#'
#' @section Function families:
#' \describe{
#'   \item{Match names}{[reconcile_tree()], [reconcile_data()],
#'     [reconcile_to_trees()], [reconcile_trees()], [reconcile_multi()]}
#'   \item{Apply and export}{[reconcile_apply()], [reconcile_merge()],
#'     [reconcile_export()]}
#'   \item{Inspect and audit}{[reconcile_summary()], [reconcile_mapping()],
#'     [reconcile_plot()], [reconcile_suggest()], [reconcile_diff()],
#'     [reconcile_report()], [reconcile_review()]}
#'   \item{Corrections and crosswalks}{[reconcile_override()],
#'     [reconcile_override_batch()], [reconcile_crosswalk()]}
#'   \item{Advanced}{[reconcile_augment()], [reconcile_splits_lumps()]}
#'   \item{Name utilities}{[pr_normalize_names()], [pr_extract_tips()]}
#' }
#'
#' @section Getting started:
#' \itemize{
#'   \item `vignette("getting-started", package = "prepR4pcm")` --- core
#'     concepts with a minimal worked example.
#'   \item `vignette("bird-workflow", package = "prepR4pcm")` --- a
#'     realistic multi-dataset bird pipeline ending in PGLS and
#'     phylogenetic GLMM fits.
#'   \item `vignette("db-assembly-workflow_mammals", package = "prepR4pcm")`
#'     --- assembling a mammal trait database from three sources
#'     (Amniote, PanTHERIA, TetrapodTraits), reconciling the unique
#'     species names against a mammal phylogeny, and producing a
#'     model-ready species-level data frame.
#' }
#'
#' @references
#' Mizuno, A., Drobniak, S.M., Williams, C., Lagisz, M. & Nakagawa, S.
#' (2025) Promoting the use of phylogenetic multinomial generalised
#' mixed-effects model to understand the evolution of discrete traits.
#' \emph{Journal of Evolutionary Biology} 38:1699--1715.
#' \doi{10.1093/jeb/voaf116}
#'
#' Norman, K.E., Chamberlain, S. & Boettiger, C. (2020) taxadb: A
#' high-performance local taxonomic database interface.
#' \emph{Methods in Ecology and Evolution} 11:1153--1159.
#' \doi{10.1111/2041-210X.13440}
#'
#' Paradis, E. & Schliep, K. (2019) ape 5.0: an environment for modern
#' phylogenetics and evolutionary analyses in R. \emph{Bioinformatics}
#' 35:526--528. \doi{10.1093/bioinformatics/bty633}
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang abort inform warn check_required caller_env
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning
#' @importFrom cli cli_alert_danger cli_h1 cli_h2 cli_bullets
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom tibble tibble as_tibble
## usethis namespace: end
NULL
