# Pluggable tree retrieval ------------------------------------------------
#
# `pr_get_tree()` connects a reconciled species list to an external
# phylogenetic resource and returns a pruned candidate tree plus a small
# matching report. Designed to slot in between `reconcile_*` and any
# downstream PCM workflow:
#
#     rec    <- reconcile_data(my_data, ref, ...)
#     tree   <- pr_get_tree(rec, source = "rotl")
#     align  <- reconcile_apply(rec, my_data, tree$tree)
#
# Issue #42 (Ayumi Mizuno).

#' Retrieve a candidate phylogeny for a species list
#'
#' Connects reconciled species names to an external phylogenetic resource
#' and returns a pruned candidate tree plus a report of which species
#' were matched and which were dropped. Intended as the bridge between
#' the package's reconciliation cascade and any downstream comparative
#' analysis: feed the result of `reconcile_data()` / `reconcile_tree()`
#' (or any character vector of cleaned names) into `pr_get_tree()` and
#' get back a `phylo` ready for `reconcile_apply()`.
#'
#' Each backend is provided by an external R package that we list in
#' `Suggests` rather than `Imports`, so installing **prepR4pcm** does
#' not pull them in automatically. The error message tells you what
#' to install if you ask for a backend you don't have.
#'
#' @param x One of:
#'   \describe{
#'     \item{a `reconciliation` object}{returned by [reconcile_tree()]
#'       or [reconcile_data()]; species are taken from the reconciled
#'       `name_y` column with `NA`s and unresolved entries dropped.}
#'     \item{a character vector}{used directly after deduplication and
#'       NA removal.}
#'     \item{a data frame}{`species_col` must name a character column;
#'       its unique non-NA values are used.}
#'   }
#' @param source A length-1 character vector. Which external backend
#'   to use. One of:
#'   \describe{
#'     \item{\code{"rotl"}}{Open Tree of Life synthesis tree, via the
#'       CRAN package \code{rotl}. Universal taxonomic coverage; calls
#'       \code{tnrs_match_names()} to resolve names to OTT ids and
#'       then \code{tol_induced_subtree()}.}
#'     \item{\code{"rtrees"}}{Taxon-specific mega-trees (bird, mammal,
#'       fish, amphibian, reptile, plant, shark/ray, bee, butterfly)
#'       via the CRAN package \code{rtrees}
#'       (\url{https://daijiang.github.io/rtrees/}). Requires
#'       \code{taxon = "<group>"}. Calls \code{get_tree()}. Install
#'       with \code{install.packages("rtrees")}. The project website
#'       links to upstream documentation.
#'       \strong{Grafting behaviour:} when an input species is not in
#'       the chosen mega-tree, \code{rtrees::get_tree()} grafts it at
#'       the genus level (tip suffix \verb{*}) or family level
#'       (\verb{**}); if no co-family species is in the mega-tree, the
#'       species is dropped. The placement of every input species is
#'       reported per-row in \code{result$backend_meta$placement} (a
#'       tibble with columns \code{input_name}, \code{tree_name},
#'       \code{placement_status} where \code{placement_status} is one
#'       of \code{"exact"}, \code{"genus_added"}, \code{"family_added"},
#'       \code{"skipped"}, or \code{"unmatched"}). The grafting itself
#'       cannot be disabled at the wrapper level:
#'       \code{rtrees::get_tree()} has no exact-only switch. To exclude
#'       grafted tips from a downstream analysis,
#'       filter the placement table on \code{placement_status == "exact"}
#'       and prune the tree to those tip labels. See
#'       \code{?rtrees::get_tree} for upstream control (\code{scenario}
#'       \emph{where} a graft is placed, but not \emph{whether}).}
#'     \item{\code{"clootl"}}{Bird-only phylogenies in current
#'       Clements taxonomy, via the CRAN package \code{clootl}
#'       (\url{https://github.com/eliotmiller/clootl}). Calls
#'       \code{extractTree()}. Install with
#'       \code{install.packages("clootl")}.}
#'     \item{\code{"fishtree"}}{Fish-only time-calibrated phylogeny
#'       (Rabosky et al. 2018), via the CRAN package \code{fishtree}.
#'       Calls \code{fishtree_phylogeny()} (single tree) or
#'       \code{fishtree_complete_phylogeny()} (multi-tree posterior;
#'       triggered by \code{n_tree > 1}). Requires exact name
#'       matches against the Fish Tree of Life taxonomy --- pre-clean
#'       with [reconcile_data()] (with a `taxadb` authority) for best
#'       results.}
#'     \item{\code{"datelife"}}{Universal database of pre-computed
#'       chronograms (Sanchez Reyes et al. 2024, *Syst. Biol.*
#'       73:470), via the GitHub package \code{datelife}
#'       (\url{https://github.com/phylotastic/datelife}). Returns a
#'       single SDM-summary chronogram by default; with
#'       \code{n_tree > 1}, returns a multiPhylo of up to that many
#'       per-source candidate chronograms. **Install before use**
#'       with \code{pak::pak("phylotastic/datelife")} --- the package
#'       is GitHub-only (archived from CRAN in 2024 with a heavy
#'       transitive dep tree pak can't auto-resolve), so \pkg{prepR4pcm}
#'       does NOT pull it in via `Suggests`.}
#'     \item{\code{"auto"}}{Fall-through dispatcher: try installed
#'       backends in priority order (rtrees if `taxon` provided,
#'       then rotl, fishtree, clootl, datelife), return the first
#'       result that resolves at least \code{min_match} of the
#'       species. Useful for first-pass exploration when you don't
#'       yet know which backend covers your taxa.}
#'   }
#' @param species_col A length-1 character vector. Required when `x`
#'   is a data frame; ignored otherwise.
#' @param taxon A length-1 character vector. Required when
#'   `source = "rtrees"`. One of `"bird"`, `"mammal"`, `"fish"`,
#'   `"amphibian"`, `"reptile"`, `"plant"`, `"shark_ray"`, `"bee"`,
#'   `"butterfly"` (see the \code{rtrees} package help for
#'   \code{get_tree}). Ignored for other backends.
#' @param n_tree A length-1 positive integer. How many trees to
#'   request from the backend. Default `1L` (single phylo for
#'   back-compat). Each backend negotiates this differently:
#'   \describe{
#'     \item{`"rotl"`}{Always returns 1 (the synthesis tree). A
#'       one-shot warning is emitted if `n_tree > 1`.}
#'     \item{`"rtrees"`}{**`n_tree` is informational only here.**
#'       `rtrees::get_tree()` does not have an `n_tree` argument;
#'       the multi-tree count is fixed by which mega-tree was
#'       selected. Reference trees rtrees uses internally:
#'       birds = Jetz et al. 2012
#'       (\url{https://birdtree.org}, 100 posterior trees);
#'       mammals = Upham et al. 2019 (VertLife, 100 by default;
#'       set `mammal_tree = "phylacine"` for the PHYLACINE set);
#'       amphibians + squamates = VertLife; fish = Rabosky et al. 2018
#'       (also wrapped by `source = "fishtree"`); plants =
#'       V.PhyloMaker; bees = Bee Tree of Life. Override which
#'       mega-tree is used via `...` (e.g. `bee_tree = "bootstrap"`
#'       for 100 bee trees instead of the single ML tree).
#'       Requires `taxon`.}
#'     \item{`"clootl"`}{`n_tree = 1` calls `clootl::extractTree()`
#'       and works out of the box with the v1.6 / 2025 taxonomy
#'       bundled in the `clootl` package. `n_tree > 1` calls
#'       `clootl::sampleTrees(count = n_tree)` (capped at 100
#'       upstream) and **requires the AvesData repo to be set up
#'       once via `clootl::get_avesdata_repo(".")` first**;
#'       otherwise it errors with `AvesData repo not found`.}
#'     \item{`"fishtree"`}{Single phylo via `fishtree_phylogeny()`
#'       when `n_tree = 1`; switches to
#'       `fishtree_complete_phylogeny()` returning a multiPhylo of
#'       stochastically polytomy-resolved trees when `n_tree > 1`.}
#'     \item{`"datelife"`}{`summary_format = "phylo_sdm"` (single
#'       summary chronogram) when `n_tree = 1`; switches to
#'       `summary_format = "phylo_all"` (one chronogram per source,
#'       capped at `n_tree`) when `n_tree > 1`.}
#'   }
#'   When the request returns a multiPhylo, the result's `tree` slot
#'   is `multiPhylo`; otherwise `phylo`.
#' @param cache Logical. Cache the result on disk and reuse it on
#'   subsequent identical calls? Default `FALSE`. When `TRUE`, the
#'   request is keyed by `(species, source, n_tree, taxon, tnrs, ...)` and
#'   stored at [pr_tree_cache_dir()]. See [pr_tree_cache_status()]
#'   and [pr_tree_cache_clear()] for inspecting / wiping the cache.
#' @param tnrs A length-1 character vector. Run a TNRS preflight
#'   (Open Tree of Life name resolution via `rotl::tnrs_match_names`)
#'   on the species list before calling the backend? One of:
#'   \describe{
#'     \item{`"auto"` (default)}{Run TNRS only for `fishtree`, where
#'       OTL-resolved names tend to improve the match rate. **Not
#'       run for `clootl` by default**: clootl uses the eBird /
#'       Clements taxonomy, so OTL-resolved names are often
#'       different from clootl's preferred names; the network call
#'       is also the dominant cost for large requests (~15 min for
#'       10k species before this change). Pass `tnrs = "always"` if
#'       you want it for clootl anyway.}
#'     \item{`"always"`}{Run TNRS regardless of backend.}
#'     \item{`"never"`}{Skip TNRS even when the backend would benefit.}
#'   }
#'   When `rotl` is not installed, TNRS is silently skipped with a
#'   one-shot warning.
#' @param min_match A length-1 numeric in `[0, 1]`. Only used when
#'   `source = "auto"`. The minimum fraction of input species a
#'   backend must resolve for the dispatcher to accept its result;
#'   if no backend meets the threshold, the best available is
#'   returned with a warning. Default `0.8`.
#' @param check_ultrametric Logical. After producing the tree, check
#'   that it's ultrametric (all tips equidistant from the root) and
#'   warn if not. Default `TRUE`. Only enforced for backends that
#'   normally return chronograms (`rtrees`, `clootl`, `fishtree`,
#'   `datelife`); `rotl` returns a topology without real branch
#'   lengths, so the check is skipped. To force ultrametricity on a
#'   non-ultrametric result, use `phytools::force.ultrametric()` or
#'   `ape::chronos()` directly --- prepR4pcm does not modify the
#'   tree silently.
#' @param resolve_polytomies Logical. After retrieval, resolve
#'   any polytomies via [ape::multi2di()] with `random = TRUE`?
#'   Default `FALSE` (back-compat; topology preserved). Useful for
#'   phylogenetic meta-analysis, where a strictly bifurcating tree
#'   is required for [pr_phylo_cor()] / [ape::vcv()] to produce a
#'   full-rank correlation matrix.
#' @param branch_lengths A length-1 character vector or `NULL`. After
#'   retrieval (and after polytomy resolution if requested), assign
#'   branch lengths via the named method? Default `NULL` (no
#'   transformation; backend's branch lengths are kept as-is). Other
#'   values:
#'   \describe{
#'     \item{`"grafen"`}{Grafen's (1989) method via
#'       [ape::compute.brlen()] with `method = "Grafen"`. The
#'       canonical choice for phylogenetic meta-analysis when the
#'       topology comes from `rotl` (whose edge lengths are
#'       unit-length placeholders). See Cinar et al. (2022)
#'       *Methods Ecol. Evol.* 13:383, who use this exact
#'       pattern.}
#'     \item{`"compute.brlen"`}{Same as `"grafen"` --- Grafen is
#'       `ape::compute.brlen()`'s default method. Provided as an
#'       alias for users who think in terms of the underlying
#'       function name.}
#'     \item{`"unit"`}{Set every edge length to 1. The crudest
#'       option; useful only for sensitivity-analysis comparisons.}
#'   }
#' @param ... Backend-specific arguments forwarded to the underlying
#'   call. See the help page of the underlying function in the
#'   relevant backend package (\code{tol_induced_subtree} in
#'   \code{rotl}, \code{extractTree} in \code{clootl},
#'   \code{get_tree} in \code{rtrees}, \code{fishtree_phylogeny} /
#'   \code{fishtree_complete_phylogeny} in \code{fishtree},
#'   \code{datelife_search} in \code{datelife}) for the full list.
#'
#' @return A list with class `pr_tree_result` and components:
#' \describe{
#'   \item{`tree`}{A `phylo` (single) or `multiPhylo` (posterior) object
#'     from the chosen backend, pruned to the matched species.}
#'   \item{`matched`}{Character vector of names from the user's
#'     **original input** (preserving the input format, including any
#'     underscores) that resolved to a tip in `tree`. The dispatcher
#'     enforces that matched names are a subset of `unique(input)` ---
#'     TNRS substitution,
#'     normalisation, and backend-internal name juggling cannot leak
#'     intermediate names into this slot.}
#'   \item{`unmatched`}{Character vector of names from the **original
#'     input** that did not resolve. Disjoint from `matched`;
#'     `length(matched) + length(unmatched) == length(unique(input))`
#'     always holds. Inspect these and consider running them back
#'     through [reconcile_suggest()] / a manual override.}
#'   \item{`mapping`}{A tibble with one row per unique input species.
#'     Core columns: `input_name`, `normalized_name`, `query_name`,
#'     `tree_name`, `in_tree`, `match_type`, and `placement_status`.
#'     This is the audit trail for name handling: `input_name` is what
#'     the user supplied, `normalized_name` is the result of
#'     [pr_normalize_names()], `query_name` is the backend query after
#'     optional TNRS, `tree_name` is the actual returned tip label, and
#'     `match_type` is one of `"exact"`, `"normalized"`, `"tnrs"`, or
#'     `"unmatched"`. For `source = "rtrees"`, `placement_status`
#'     carries the grafting status from `backend_meta$placement`;
#'     otherwise it is `NA`. Four further columns record what `rotl`'s
#'     TNRS resolver reported for each name: `tnrs_number_matches`,
#'     `tnrs_is_synonym`, `tnrs_approximate_match`, and `tnrs_flags`.
#'     These are `NA` for backends or `tnrs` settings where TNRS did
#'     not run. `tnrs_number_matches > 1` flags a homonym, meaning the
#'     resolved name is only one of several candidate taxa.}
#'   \item{`source`}{The backend that produced the tree.}
#'   \item{`backend_meta`}{A named list of diagnostic information.
#'     Standard fields populated by the dispatcher:
#'     \describe{
#'       \item{`n_queried`}{Unique input species count.}
#'       \item{`n_requested`}{The `n_tree` argument the user passed.}
#'       \item{`n_returned`}{Number of trees in `tree` (1 for `phylo`).}
#'       \item{`n_matched`}{Equal to `length(matched)`.}
#'       \item{`tnrs_replacements`}{When TNRS ran (`tnrs = "always"`,
#'         or `tnrs = "auto"` for `fishtree`) and `rotl` is installed:
#'         a named character vector mapping original input to the
#'         TNRS-resolved name, for names that TNRS changed. `NULL`
#'         when no TNRS or no replacements occurred. A one-shot `cli`
#'         warning lists the first three substitutions on the call,
#'         so silent name correction is impossible.}
#'       \item{`tip_set_consistent`}{Logical. For `multiPhylo` returns:
#'         `TRUE` if every tree shares the same tip set.}
#'       \item{`dropped_per_tree`}{For `multiPhylo` returns where
#'         `tip_set_consistent = FALSE`: a list of character vectors,
#'         per tree, listing species missing from each tree relative
#'         to the union of all trees. `NULL` otherwise.}
#'       \item{`tree_provenance`}{A list with one entry per returned
#'         tree (so `tree[[i]]` pairs with
#'         `backend_meta$tree_provenance[[i]]` when `tree` is a
#'         `multiPhylo`).}
#'     }
#'     Backend-specific fields (e.g. `taxon`, `n_grafted`,
#'     `grafted_tips`, `placement` for `rtrees`; `backend`, `type`,
#'     `tnrs_table` for `fishtree` / `rotl`; `summary_format`,
#'     `source_citations`, `reference` for `datelife`) are merged in
#'     at the top level by the wrapper that called the backend. The
#'     `rtrees`-specific `placement` slot is a tibble with one row per
#'     unique input species and columns `input_name`, `tree_name`,
#'     `placement_status` (`"exact"`, `"genus_added"`,
#'     `"family_added"`, `"skipped"`, or `"unmatched"`).}
#' }
#'
#' @details
#' **Name handling.** Input names are run through
#' [pr_normalize_names()] before the backend is queried --- underscores
#' become spaces, leading/trailing whitespace is trimmed, OTT-id
#' suffixes (e.g. `ott770315`) and authority strings (e.g.
#' `(Linnaeus, 1758)`) are stripped, and hybrid signs are
#' standardised. The `matched` and `unmatched` slots in the result use
#' the *original* input format (as you typed it), not the normalised
#' form.
#'
#' When TNRS substitutes a name (only when `tnrs = "always"`, or for
#' the `fishtree` backend under `tnrs = "auto"`), the replacement is
#' recorded in `result$backend_meta$tnrs_replacements` as a named
#' character vector `(original = resolved)`. A one-shot `cli` warning
#' lists the first few substitutions on the call itself.
#'
#' TNRS also returns structured match metadata. `pr_get_tree()` records
#' it per name in the `mapping` tibble: `tnrs_number_matches`,
#' `tnrs_is_synonym`, `tnrs_approximate_match`, and `tnrs_flags`. When a
#' name resolves to more than one taxon (`tnrs_number_matches > 1`, a
#' homonym), a one-shot `cli` warning names the affected species, since
#' the resolved name is then only one of several candidates.
#'
#' @seealso [reconcile_tree()] / [reconcile_data()] for producing the
#'   reconciled species list that feeds this function;
#'   [reconcile_apply()] for combining the returned `phylo` with the
#'   data frame ready for analysis;
#'   [reconcile_augment()] for filling gaps in an existing tree
#'   (a tree-aware alternative to retrieving a fresh tree);
#'   [pr_date_tree()] for time-calibrating an existing topology;
#'   [pr_cite_tree()] for formatting citations for a tree result;
#'   [pr_tree_compare()] for comparing two or more retrieved trees;
#'   [pr_get_tree_status()] for checking which backends are installed
#'   and reachable;
#'   [pr_tree_cache_dir()] / [pr_tree_cache_status()] /
#'   [pr_tree_cache_clear()] for managing the on-disk cache.
#'   The companion package
#'   \href{https://itchyshin.github.io/pigauto/}{pigauto} consumes a
#'   `multiPhylo` directly via `multi_impute_trees()` for posterior-
#'   tree PCMs --- request a posterior sample with `n_tree > 1`.
#'
#' @references
#' Backend reference trees:
#'
#' Jetz, W., Thomas, G. H., Joy, J. B., Hartmann, K., & Mooers, A. O.
#' (2012). The global diversity of birds in space and time.
#' *Nature* 491: 444--448. \doi{10.1038/nature11631}
#' (Used by `rtrees` for `taxon = "bird"` and by BirdTree.)
#'
#' Rabosky, D. L., Chang, J., Title, P. O., Cowman, P. F., Sallan, L.,
#' Friedman, M., Kaschner, K., Garilao, C., Near, T. J., Coll, M., &
#' Alfaro, M. E. (2018). An inverse latitudinal gradient in speciation
#' rate for marine fishes. *Nature* 559: 392--395.
#' \doi{10.1038/s41586-018-0273-1}
#' (Fish Tree of Life; used by `source = "fishtree"` and by `rtrees`
#' for `taxon = "fish"`.)
#'
#' Upham, N. S., Esselstyn, J. A., & Jetz, W. (2019). Inferring the
#' mammal tree: Species-level sets of phylogenies for questions in
#' ecology, evolution, and conservation. *PLOS Biology* 17(12):
#' e3000494. \doi{10.1371/journal.pbio.3000494}
#' (VertLife mammal posterior; used by `rtrees` for `taxon = "mammal"`
#' with `mammal_tree = "vertlife"`.)
#'
#' Jin, Y. & Qian, H. (2019). V.PhyloMaker: an R package that can
#' generate very large phylogenies for vascular plants.
#' *Ecography* 42(8): 1353--1359. \doi{10.1111/ecog.04434}
#' (Vascular-plant mega-tree used by `rtrees` for `taxon = "plant"`;
#' also the basis for the `source = "vphylomaker"` augmentation
#' backend in [reconcile_augment()].)
#'
#' Sanchez Reyes, L. L., O'Meara, B. C., Brown, J. W., & McTavish, E.
#' J. (2024). DateLife: Leveraging databases and analytical tools to
#' reveal the dated Tree of Life. *Systematic Biology* 73(2):
#' 470--485. \doi{10.1093/sysbio/syae015}
#' (Used by `source = "datelife"` and by `pr_date_tree()`.)
#'
#' Methodology:
#'
#' Chang, J., Rabosky, D. L., & Alfaro, M. E. (2019). Estimating
#' diversification rates on incompletely sampled phylogenies:
#' Theoretical concerns and practical solutions. *Systematic Biology*
#' 69(3): 602--611. \doi{10.1093/sysbio/syz081}
#' (Stochastic polytomy resolution behind `fishtree_complete_phylogeny()`
#' for `n_tree > 1`.)
#'
#' Michonneau, F., Brown, J. W., & Winter, D. J. (2016). rotl: an R
#' package to interact with the Open Tree of Life data. *Methods in
#' Ecology and Evolution* 7(12): 1476--1481.
#' \doi{10.1111/2041-210X.12593}
#' (TNRS preflight and `source = "rotl"`.)
#'
#' @examples
#' if (interactive()) {
#'   # Example 1: birds via clootl (Clements taxonomy). Uses the
#'   # bundled AVONET subset (657 species placed in the Clements tree).
#'   data(avonet_subset)
#'   if (requireNamespace("clootl", quietly = TRUE)) {
#'     res <- pr_get_tree(avonet_subset, species_col = "Species1",
#'                        source = "clootl")
#'     ape::Ntip(res$tree)        # species placed in the tree
#'     head(res$unmatched)        # names clootl could not resolve
#'   }
#'
#'   # Example 2: fish via fishtree (Rabosky et al. 2018, time-calibrated)
#'   if (requireNamespace("fishtree", quietly = TRUE)) {
#'     res <- pr_get_tree(c("Salmo salar", "Esox lucius", "Gadus morhua"),
#'                        source = "fishtree")
#'     res$tree
#'   }
#'
#'   # Example 3: anything via rotl (universal, network)
#'   if (requireNamespace("rotl", quietly = TRUE)) {
#'     res <- pr_get_tree(c("Homo sapiens", "Pan troglodytes",
#'                          "Mus musculus"),
#'                        source = "rotl")
#'     res$tree
#'   }
#'
#'   # Example 4: posterior of fish trees (50 trees, for multi-tree PCMs)
#'   if (requireNamespace("fishtree", quietly = TRUE)) {
#'     res <- pr_get_tree(c("Salmo salar", "Esox lucius"),
#'                        source = "fishtree", n_tree = 50)
#'     class(res$tree)            # "multiPhylo"
#'   }
#' }
#'
#' @export
pr_get_tree <- function(
  x,
  source = c("rotl", "rtrees", "clootl", "fishtree", "datelife", "auto"),
  species_col = NULL,
  taxon = NULL,
  n_tree = 1L,
  cache = FALSE,
  tnrs = c("auto", "always", "never"),
  min_match = 0.8,
  check_ultrametric = TRUE,
  resolve_polytomies = FALSE,
  branch_lengths = NULL,
  ...
) {
  source <- match.arg(source)
  tnrs <- match.arg(tnrs)
  if (!is.null(branch_lengths)) {
    branch_lengths <- match.arg(
      branch_lengths,
      choices = c("grafen", "compute.brlen", "unit")
    )
  }
  n_tree <- .pr_validate_positive_integer(n_tree, "n_tree")
  if (
    !is.numeric(min_match) ||
      length(min_match) != 1L ||
      is.na(min_match) ||
      !is.finite(min_match) ||
      min_match < 0 ||
      min_match > 1
  ) {
    cli::cli_abort(
      c(
        "{.arg min_match} must be a length-1 numeric in [0, 1].",
        "i" = "Got: {.val {min_match}}."
      )
    )
  }
  species <- .pr_extract_species_for_tree(x, species_col)

  if (length(species) == 0) {
    cli::cli_abort(
      c(
        "No species names available to query the backend.",
        "i" = "If you passed a {.cls reconciliation} object, ensure {.code mapping$name_y} contains resolved names."
      )
    )
  }

  # source = "auto" --- try backends in priority order, return the
  # first one that resolves >= min_match * length(species). Drops to
  # the fall-through dispatcher.
  if (source == "auto") {
    return(.pr_get_tree_auto(
      species,
      taxon = taxon,
      n_tree = n_tree,
      cache = cache,
      tnrs = tnrs,
      min_match = min_match,
      ...
    ))
  }

  # Build the original -> normalised -> resolved -> query mapping. This
  # is the spine of Round 15: it is the ONLY name resolution that
  # happens before the backend, and it tracks every transformation so
  # matched/unmatched can later be reported against the user's
  # original input. (See Ayumi #72/#73/#75.)
  resolved <- .pr_resolve_query(species, source, tnrs)
  species_query <- resolved$query

  # Cache lookup ------------------------------------------------------
  if (isTRUE(cache)) {
    key <- .pr_tree_cache_key(
      resolved$original,
      source = source,
      n_tree = n_tree,
      taxon = taxon,
      tnrs = tnrs,
      query = species_query,
      ...
    )
    cached <- .pr_tree_cache_get(key, source)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  # Backend wrappers now return list(tree, in_query, citations). The
  # dispatcher does all matched/unmatched accounting against the
  # ORIGINAL input below.
  result <- switch(
    source,
    rotl = .pr_get_tree_rotl(species_query, n_tree = n_tree, ...),
    rtrees = .pr_get_tree_rtrees(
      species_query,
      n_tree = n_tree,
      taxon = taxon,
      ...
    ),
    clootl = .pr_get_tree_clootl(species_query, n_tree = n_tree, ...),
    fishtree = .pr_get_tree_fishtree(species_query, n_tree = n_tree, ...),
    datelife = .pr_get_tree_datelife(species_query, n_tree = n_tree, ...)
  )

  # Map backend's per-query in/out flags back to original input.
  # `in_query[i]` says whether species_query[i] resolved at the backend;
  # we map that to "is original[i] matched?" -- same index, since
  # .pr_resolve_query preserved order.
  in_query <- result$in_query
  if (!is.logical(in_query) || length(in_query) != length(resolved$original)) {
    cli::cli_abort(c(
      "Internal: backend wrapper returned malformed {.code in_query}.",
      "i" = "Expected {.cls logical} of length {length(resolved$original)}; got {.cls {class(in_query)[1]}} of length {length(in_query)}."
    ))
  }
  matched <- resolved$original[in_query]
  unmatched <- resolved$original[!in_query]

  # Multi-tree reporting (#76) ---------------------------------------
  is_multi <- inherits(result$tree, "multiPhylo")
  n_returned <- if (is_multi) length(result$tree) else 1L

  tip_set_consistent <- TRUE
  dropped_per_tree <- NULL
  if (is_multi && length(result$tree) > 1L) {
    tip_sets <- lapply(result$tree, function(t) sort(t$tip.label))
    tip_set_consistent <- all(vapply(
      tip_sets[-1L],
      function(s) identical(s, tip_sets[[1L]]),
      logical(1L)
    ))
    if (!tip_set_consistent) {
      union_tips <- Reduce(union, tip_sets)
      dropped_per_tree <- lapply(tip_sets, function(s) setdiff(union_tips, s))
    }
  }

  # Accounting invariants -- these CANNOT fail under correct backend
  # behaviour. If they do, it's a bug in this dispatcher or a backend
  # wrapper, not user error. (#73)
  stopifnot(
    "matched must be a subset of unique input" = all(
      matched %in% resolved$original
    ),
    "unmatched must be a subset of unique input" = all(
      unmatched %in% resolved$original
    ),
    "matched + unmatched must cover unique input" = length(matched) +
      length(unmatched) ==
      length(resolved$original),
    "matched and unmatched must be disjoint" = length(intersect(
      matched,
      unmatched
    )) ==
      0L
  )

  # Build backend_meta. Start with whatever the wrapper returned (its
  # backend-specific fields like `taxon`, `n_grafted`, `backend`,
  # `type`, `reference`, `tnrs_table`, etc.) and OVERLAY the
  # dispatcher's standard fields on top. Wrapper-set fields stay
  # accessible at the top level of backend_meta for back-compat.
  wrapper_meta <- if (is.list(result$backend_meta)) {
    result$backend_meta
  } else {
    list()
  }
  if (
    identical(source, "rtrees") &&
      is.data.frame(wrapper_meta$placement) &&
      nrow(wrapper_meta$placement) == length(resolved$original)
  ) {
    wrapper_meta$placement$input_name <- resolved$original
  }
  backend_meta <- utils::modifyList(
    wrapper_meta,
    list(
      n_queried = length(resolved$original),
      n_requested = as.integer(n_tree),
      n_returned = n_returned,
      n_matched = length(matched),
      tnrs_replacements = resolved$tnrs_replacements,
      tip_set_consistent = tip_set_consistent,
      dropped_per_tree = dropped_per_tree
    )
  )

  # Ensure backend_meta$tree_provenance is always present as a list with
  # one entry per returned tree, so downstream consumers (e.g. pigauto)
  # can pair tree[[i]] with backend_meta$tree_provenance[[i]].
  backend_meta <- .pr_ensure_tree_provenance(
    result$tree,
    backend_meta,
    source
  )

  # Post-process for the meta-analysis path -----------------------
  # `resolve_polytomies = TRUE`: ape::multi2di() with random
  # resolution. Done before branch_lengths so the BL transform sees
  # a strictly bifurcating tree.
  # `branch_lengths`: post-hoc branch length assignment via
  #   ape::compute.brlen(method = "Grafen") or method = NULL.
  result$tree <- .pr_post_process_tree(
    result$tree,
    resolve_polytomies = resolve_polytomies,
    branch_lengths = branch_lengths
  )
  mapping <- .pr_build_tree_mapping(
    input_name = resolved$original,
    normalized_name = resolved$normalised,
    query_name = resolved$query,
    in_tree = in_query,
    tree = result$tree,
    backend_meta = backend_meta,
    tnrs_audit = resolved$tnrs_audit
  )

  out <- list(
    tree = result$tree,
    matched = matched,
    unmatched = unmatched,
    mapping = mapping,
    source = source,
    backend_meta = backend_meta
  )
  class(out) <- "pr_tree_result"

  # Cache write ------------------------------------------------------
  if (isTRUE(cache)) {
    .pr_tree_cache_put(key, source, out)
  }

  # Ultrametric sanity check ----------------------------------------
  # Backends that normally return chronograms should produce
  # ultrametric trees. Warn (don't force) if not. Skipped when the
  # user has applied a branch_lengths transform that defines its
  # own scale (Grafen output IS ultrametric, so the check still
  # fires usefully there).
  if (isTRUE(check_ultrametric)) {
    .pr_check_tree_ultrametric(out$tree, source)
  }

  out
}


# Internal: shared scalar positive-integer validation ---------------------

.pr_validate_positive_integer <- function(x, arg) {
  bad <- !is.numeric(x) ||
    length(x) != 1L ||
    is.na(x) ||
    !is.finite(x) ||
    x < 1L ||
    x != as.integer(x)
  if (bad) {
    cli::cli_abort(c(
      "`{arg}` must be a length-1 positive integer.",
      "i" = "Got: {.val {x}}."
    ))
  }
  as.integer(x)
}


# Internal: per-input mapping table for pr_tree_result ---------------------

.pr_build_tree_mapping <- function(
  input_name,
  normalized_name,
  query_name,
  in_tree,
  tree,
  backend_meta = list(),
  tnrs_audit = NULL
) {
  input_name <- as.character(input_name)
  normalized_name <- as.character(normalized_name)
  query_name <- as.character(query_name)
  in_tree <- as.logical(in_tree)
  in_tree[is.na(in_tree)] <- FALSE

  tree_name <- .pr_lookup_tree_tip_names(query_name, tree)
  placement_status <- rep(NA_character_, length(input_name))

  placement <- backend_meta$placement
  if (is.data.frame(placement)) {
    placement_idx <- match(input_name, placement$input_name)
    has_placement <- !is.na(placement_idx)

    if ("tree_name" %in% names(placement)) {
      tree_name[has_placement] <- as.character(
        placement$tree_name[placement_idx[has_placement]]
      )
    }
    if ("placement_status" %in% names(placement)) {
      placement_status[has_placement] <- as.character(
        placement$placement_status[placement_idx[has_placement]]
      )
    }
  }

  tree_name[!in_tree] <- NA_character_

  tnrs_match <- in_tree &
    !is.na(query_name) &
    !is.na(normalized_name) &
    query_name != normalized_name
  normalized_match <- in_tree &
    !tnrs_match &
    !is.na(normalized_name) &
    normalized_name != input_name

  match_type <- rep("unmatched", length(input_name))
  match_type[in_tree] <- "exact"
  match_type[normalized_match] <- "normalized"
  match_type[tnrs_match] <- "tnrs"

  # TNRS match-quality columns. Present (as NA) even for backends or
  # `tnrs` settings where TNRS did not run, so the mapping schema is
  # stable across every call.
  n_rows <- length(input_name)
  has_audit <- is.data.frame(tnrs_audit) && nrow(tnrs_audit) == n_rows
  tnrs_number_matches <- if (has_audit) {
    as.integer(tnrs_audit$tnrs_number_matches)
  } else {
    rep(NA_integer_, n_rows)
  }
  tnrs_is_synonym <- if (has_audit) {
    as.logical(tnrs_audit$tnrs_is_synonym)
  } else {
    rep(NA, n_rows)
  }
  tnrs_approximate_match <- if (has_audit) {
    as.logical(tnrs_audit$tnrs_approximate_match)
  } else {
    rep(NA, n_rows)
  }
  tnrs_flags <- if (has_audit) {
    as.character(tnrs_audit$tnrs_flags)
  } else {
    rep(NA_character_, n_rows)
  }

  tibble::tibble(
    input_name = input_name,
    normalized_name = normalized_name,
    query_name = query_name,
    tree_name = tree_name,
    in_tree = in_tree,
    match_type = match_type,
    placement_status = placement_status,
    tnrs_number_matches = tnrs_number_matches,
    tnrs_is_synonym = tnrs_is_synonym,
    tnrs_approximate_match = tnrs_approximate_match,
    tnrs_flags = tnrs_flags
  )
}


.pr_lookup_tree_tip_names <- function(query_name, tree) {
  out <- rep(NA_character_, length(query_name))
  tip_labels <- .pr_first_tree_tip_labels(tree)
  if (length(tip_labels) == 0L) {
    return(out)
  }

  clean_tips <- sub("\\*+$", "", tip_labels)
  normalized_query <- pr_normalize_names(query_name)
  normalized_tips <- pr_normalize_names(clean_tips)
  tip_by_normalized <- stats::setNames(tip_labels, normalized_tips)
  hit <- normalized_query %in% names(tip_by_normalized)
  out[hit] <- unname(tip_by_normalized[normalized_query[hit]])
  out
}


.pr_first_tree_tip_labels <- function(tree) {
  if (inherits(tree, "multiPhylo")) {
    if (length(tree) == 0L) {
      return(character())
    }
    return(tree[[1]]$tip.label)
  }
  tree$tip.label
}


# Internal: post-process a tree for the meta-analysis pattern --------
#
# Handles bifurcation (ape::multi2di with random = TRUE) and
# branch-length assignment (ape::compute.brlen with the requested
# method). Idempotent on multiPhylo (applies element-wise).

.pr_post_process_tree <- function(
  tree,
  resolve_polytomies = FALSE,
  branch_lengths = NULL
) {
  if (!isTRUE(resolve_polytomies) && is.null(branch_lengths)) {
    return(tree)
  }
  apply_one <- function(t) {
    if (isTRUE(resolve_polytomies)) {
      t <- ape::multi2di(t, random = TRUE)
    }
    if (!is.null(branch_lengths)) {
      method <- switch(
        branch_lengths,
        grafen = "Grafen",
        compute.brlen = "Grafen", # ape's default
        unit = NULL,
        branch_lengths
      )
      if (identical(branch_lengths, "unit")) {
        t$edge.length <- rep_len(1, nrow(t$edge))
      } else {
        t <- ape::compute.brlen(t, method = method)
      }
    }
    t
  }
  if (inherits(tree, "multiPhylo")) {
    out <- lapply(tree, apply_one)
    class(out) <- "multiPhylo"
    out
  } else {
    apply_one(tree)
  }
}


# Internal: warn when a backend that should produce ultrametric trees
# does not. Skipped for `rotl` (synthesis topology, no real branch
# lengths) and `fishtree` with `type = "phylogram"`.

.pr_check_tree_ultrametric <- function(tree, source) {
  # Don't bother for backends that don't pretend to produce
  # ultrametric output.
  if (source == "rotl") {
    return(invisible())
  }
  ut <- .pr_is_tree_ultrametric(tree)
  if (isTRUE(ut)) {
    return(invisible())
  }
  if (is.na(ut)) {
    return(invisible())
  } # no edge lengths, can't check
  cli::cli_warn(c(
    "Tree returned by {.val {source}} is not strictly ultrametric.",
    "i" = "Most PCM methods (PGLS, BM, OU, etc.) assume ultrametric trees.",
    ">" = "To force: {.code phytools::force.ultrametric(result$tree)} or {.code ape::chronos(result$tree)}.",
    "*" = "To suppress this check: pass {.code check_ultrametric = FALSE}."
  ))
}


# Internal: tolerant ultrametric check that handles multiPhylo.
# Returns TRUE/FALSE, or NA if we can't tell (no edge lengths).

.pr_is_tree_ultrametric <- function(tree) {
  if (inherits(tree, "multiPhylo")) {
    # All trees in a multiPhylo are tested; return TRUE only if every
    # tree is ultrametric.
    res <- vapply(tree, .pr_is_tree_ultrametric, logical(1))
    return(all(res))
  }
  if (is.null(tree$edge.length)) {
    return(NA)
  }
  tryCatch(
    isTRUE(ape::is.ultrametric(tree, tol = 1e-6)),
    error = function(e) NA
  )
}


# Internal: build the original -> normalised -> resolved -> query
# mapping that drives all matched/unmatched accounting in
# pr_get_tree(). Returns a list:
#   $original          : character -- unique input, in original form
#   $normalised        : character -- pr_normalize_names(original)
#   $resolved          : character -- TNRS resolved where applicable
#                                     (else == normalised)
#   $query             : character -- what gets passed to the backend
#                                     (currently identical to resolved)
#   $tnrs_replacements : named char (orig -> resolved) for *changed*
#                        names, or NULL when no TNRS or no changes
#
# Order is preserved through every transformation so the dispatcher can
# map per-query in/out flags back to original input by index.
#
# When tnrs = "auto", TNRS runs only for backends that empirically
# benefit from it (currently `fishtree`). `clootl` uses the eBird /
# Clements taxonomy and is harmed (Ayumi #72); rotl/datelife do their
# own internal name resolution; rtrees has its own genus/family
# fall-back. When tnrs = "always" we run TNRS for any backend; when
# "never" we skip entirely.
#
# Every TNRS substitution is recorded explicitly in
# `tnrs_replacements` and a one-shot cli warning is emitted, so silent
# name correction is impossible. (Ayumi #72/#73.)

# Internal: compat shim retained for tests / mocks that locked onto the
# pre-Round-15 internal API. Returns the resolved query vector only;
# new code (and pr_get_tree itself) calls .pr_resolve_query() to get
# the full mapping table.
.pr_tnrs_preflight <- function(species, source, tnrs) {
  .pr_resolve_query(species, source, tnrs)$query
}


# Internal: pull TNRS's structured match-quality columns
# (number_matches, is_synonym, approximate_match, flags) out of a
# tnrs_match_names() result and realign them to `normalised` via the
# `row` index. rotl always returns these columns; the NULL guard means
# a test mock or an upstream rotl change degrades to NA rather than
# erroring. Returns a tibble with one row per element of `row`.
.pr_tnrs_audit_table <- function(tnrs_res, row) {
  n <- length(row)
  realign <- function(name, na_value, coerce) {
    v <- tnrs_res[[name]]
    if (is.null(v)) {
      return(rep(na_value, n))
    }
    if (is.list(v)) {
      v <- vapply(v, function(x) paste(x, collapse = ","), character(1))
    }
    coerce(v[row])
  }
  tibble::tibble(
    tnrs_number_matches = realign("number_matches", NA_integer_, as.integer),
    tnrs_is_synonym = realign("is_synonym", NA, as.logical),
    tnrs_approximate_match = realign("approximate_match", NA, as.logical),
    tnrs_flags = realign("flags", NA_character_, as.character)
  )
}


# Internal: emit a one-shot cli warning when TNRS reports more than one
# taxonomic match for an input name (a homonym). The resolved name is
# then only one of several candidate taxa, so the user should inspect
# `result$mapping`. `input` and `number_matches` are index-aligned.
.pr_warn_tnrs_homonyms <- function(input, number_matches) {
  homonym <- !is.na(number_matches) & number_matches > 1L
  if (!any(homonym)) {
    return(invisible())
  }
  n_hom <- sum(homonym)
  examples_n <- min(3L, n_hom)
  examples <- paste(
    sprintf(
      "'%s' (%d matches)",
      input[homonym][seq_len(examples_n)],
      number_matches[homonym][seq_len(examples_n)]
    ),
    collapse = "; "
  )
  cli::cli_warn(c(
    "TNRS found multiple taxonomic matches for {n_hom} input name{?s} (possible homonym{?s}).",
    "i" = "The resolved name is only one candidate taxon; check {.code result$mapping$tnrs_number_matches} and {.code result$mapping$tnrs_flags}.",
    ">" = "e.g. {examples}"
  ))
}


.pr_resolve_query <- function(species, source, tnrs) {
  original <- unique(stats::na.omit(as.character(species)))
  normalised <- pr_normalize_names(original)

  needs_tnrs_default <- source %in% c("fishtree")
  do_tnrs <- switch(
    tnrs,
    auto = needs_tnrs_default,
    always = TRUE,
    never = FALSE
  )

  resolved <- normalised
  tnrs_replacements <- NULL
  tnrs_audit <- NULL

  if (do_tnrs && requireNamespace("rotl", quietly = TRUE)) {
    # `rotl::tnrs_match_names()` de-duplicates (and may reorder) its
    # input, so its rows do NOT line up 1:1 with `normalised` when two
    # distinct inputs normalise to the same string (e.g. "Esox lucius"
    # and "Esox_lucius" both normalise to "Esox lucius"). Query the
    # unique normalised names, then realign each result back to every
    # element of `normalised` by the lowercased `search_string` that
    # rotl echoes. Comparing `tnrs_res$unique_name` against `normalised`
    # directly recycles a short vector against a long one and silently
    # mis-matches species.
    uniq_norm <- unique(normalised)
    tnrs_res <- tryCatch(
      rotl::tnrs_match_names(uniq_norm),
      error = function(e) NULL
    )
    if (!is.null(tnrs_res) && !is.null(tnrs_res$unique_name)) {
      row <- match(tolower(normalised), tnrs_res$search_string)
      matched_name <- tnrs_res$unique_name[row]
      # `tnrs_match_names()`'s `unique_name` carries Open Tree homonym /
      # rank qualifiers (e.g. "Oncorhynchus mykiss (species in domain
      # Eukaryota)"). Run it back through pr_normalize_names() so the
      # backend query is a clean binomial; otherwise the qualifier
      # leaks into the query and the species silently fails to match a
      # tree tip.
      matched_name <- as.character(pr_normalize_names(matched_name))

      # Capture TNRS's structured match-quality columns so pr_get_tree()
      # can surface them per name in `result$mapping`. Realigned to
      # `normalised` by the same `row` index used for `matched_name`.
      tnrs_audit <- .pr_tnrs_audit_table(tnrs_res, row)

      replaced_idx <- !is.na(matched_name) &
        nzchar(matched_name) &
        matched_name != normalised
      resolved[replaced_idx] <- matched_name[replaced_idx]
      if (any(replaced_idx)) {
        tnrs_replacements <- stats::setNames(
          resolved[replaced_idx],
          original[replaced_idx]
        )
        n_repl <- length(tnrs_replacements)
        examples_n <- min(3L, n_repl)
        examples <- paste(
          sprintf(
            "'%s' -> '%s'",
            names(tnrs_replacements)[seq_len(examples_n)],
            unname(tnrs_replacements)[seq_len(examples_n)]
          ),
          collapse = "; "
        )
        cli::cli_warn(c(
          "TNRS replaced {n_repl} input name{?s} with OTL-resolved form{?s}.",
          "i" = "See {.code result$backend_meta$tnrs_replacements} for the full mapping.",
          ">" = "e.g. {examples}"
        ))
      }

      # Flag homonyms: TNRS reporting more than one match means the
      # resolved name is only one of several candidate taxa.
      .pr_warn_tnrs_homonyms(original, tnrs_audit$tnrs_number_matches)
    }
  } else if (do_tnrs && !requireNamespace("rotl", quietly = TRUE)) {
    if (!isTRUE(getOption("prepR4pcm.tnrs_warning_shown"))) {
      cli::cli_warn(c(
        "TNRS preflight requires {.pkg rotl}; skipping.",
        "i" = 'Install with {.code install.packages("rotl")} for higher match rates with backends like {.val {source}}.',
        ">" = "(This warning appears once per session.)"
      ))
      options(prepR4pcm.tnrs_warning_shown = TRUE)
    }
  }

  list(
    original = original,
    normalised = normalised,
    resolved = resolved,
    query = resolved,
    tnrs_replacements = tnrs_replacements,
    tnrs_audit = tnrs_audit
  )
}


# Internal: source = "auto" fall-through dispatcher --------------------
#
# Try backends in a priority order, return the first one that resolves
# >= min_match * length(species). If none meets the threshold, return
# the best of the lot with a warning.

.pr_get_tree_auto <- function(
  species,
  taxon = NULL,
  n_tree = 1L,
  cache = FALSE,
  tnrs = "auto",
  min_match = 0.8,
  ...
) {
  # Priority order: try the broadest-coverage CRAN backends first,
  # then taxon-specific, then GitHub-only.
  candidates <- c("rotl", "fishtree", "clootl", "datelife")
  # rtrees needs taxon; only include if the user provided one.
  if (!is.null(taxon) && nzchar(taxon)) {
    candidates <- c("rtrees", candidates)
  }
  # Only try installed backends.
  status <- pr_get_tree_status(check_network = FALSE)
  candidates <- candidates[
    candidates %in% status$source[status$installed]
  ]
  if (length(candidates) == 0L) {
    cli::cli_abort(c(
      "No tree-retrieval backends are installed.",
      "i" = "Run {.code pr_get_tree_status()} to see install instructions."
    ))
  }

  best <- NULL
  attempts <- list()
  for (b in candidates) {
    res <- tryCatch(
      pr_get_tree(
        species,
        source = b,
        taxon = taxon,
        n_tree = n_tree,
        cache = cache,
        tnrs = tnrs,
        ...
      ),
      error = function(e) NULL
    )
    if (is.null(res)) {
      attempts[[b]] <- list(success = FALSE, n_matched = 0L)
      next
    }
    n_matched <- length(res$matched)
    attempts[[b]] <- list(success = TRUE, n_matched = n_matched)
    if (is.null(best) || n_matched > length(best$matched)) {
      best <- res
    }
    if (n_matched >= min_match * length(species)) {
      best$backend_meta$auto_attempts <- attempts
      best$backend_meta$auto_chose <- b
      return(best)
    }
  }

  if (is.null(best)) {
    cli::cli_abort("No backend produced a tree for this species list.")
  }
  cli::cli_warn(c(
    "No backend met the {.arg min_match} threshold of {min_match}.",
    "i" = "Returning the best available result ({.val {best$source}}, {length(best$matched)}/{length(species)} matched)."
  ))
  best$backend_meta$auto_attempts <- attempts
  best$backend_meta$auto_chose <- best$source
  best
}


# Internal helpers --------------------------------------------------------

# Pull a clean character vector of species names from `x`. Accepts a
# reconciliation object, a character vector, or a data frame.
.pr_extract_species_for_tree <- function(x, species_col = NULL) {
  if (inherits(x, "reconciliation")) {
    if (!is.null(species_col)) {
      cli::cli_alert_info(
        "{.arg species_col} ignored for {.cls reconciliation} input; using {.code mapping$name_y}."
      )
    }
    nm <- x$mapping$name_y
    return(unique(stats::na.omit(as.character(nm))))
  }
  if (is.character(x)) {
    return(unique(stats::na.omit(x)))
  }
  if (is.data.frame(x)) {
    if (is.null(species_col)) {
      species_col <- pr_detect_species_column(x)
    }
    if (!species_col %in% names(x)) {
      cli::cli_abort(c(
        "Column {.val {species_col}} not found in {.arg x}.",
        "i" = "Available columns: {.val {names(x)}}."
      ))
    }
    return(unique(stats::na.omit(as.character(x[[species_col]]))))
  }
  cli::cli_abort(c(
    "{.arg x} must be a {.cls reconciliation}, character vector, or data frame.",
    "i" = "Got: {.cls {class(x)[1]}}."
  ))
}


# rotl backend: resolve names via TNRS, then induced subtree -------------

.pr_get_tree_rotl <- function(species, n_tree = 1L, ...) {
  if (!requireNamespace("rotl", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The {.val rotl} backend requires the {.pkg rotl} package.",
        "i" = 'Install with: {.code install.packages("rotl")}.'
      )
    )
  }

  if (n_tree > 1L) {
    cli::cli_warn(c(
      "{.pkg rotl} returns the Open Tree of Life {.emph synthesis} tree (single).",
      "i" = "{.arg n_tree} = {n_tree} ignored; returning n = 1.",
      ">" = "For posterior samples, try {.code source = \"datelife\"} or {.code source = \"rtrees\"}."
    ))
  }

  # Step 1: TNRS name match -> OTT ids. (rotl backend has its own TNRS
  # internally and always uses it -- the dispatcher's TNRS preflight is
  # gated off for rotl.)
  tnrs <- rotl::tnrs_match_names(species)
  matched_idx <- !is.na(tnrs$ott_id)
  ott_ids <- tnrs$ott_id[matched_idx]

  if (length(ott_ids) == 0) {
    cli::cli_abort(
      "{.pkg rotl} returned 0 matches for {length(species)} species; cannot build a tree."
    )
  }

  # Step 2: induced subtree.
  tree <- rotl::tol_induced_subtree(ott_ids = ott_ids, ...)

  # in_query: which entries of the input species vector got an OTT id?
  in_query <- matched_idx

  list(
    tree = tree,
    in_query = in_query,
    backend_meta = list(tnrs_table = tnrs)
  )
}


# clootl backend: bird-only, Clements taxonomy --------------------------

.pr_get_tree_clootl <- function(species, n_tree = 1L, ...) {
  if (!requireNamespace("clootl", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The {.val clootl} backend requires the {.pkg clootl} package.",
        "i" = 'Install with: {.code install.packages("clootl")}.',
        ">" = "See {.url https://github.com/eliotmiller/clootl} for details."
      )
    )
  }

  # Workaround for clootl 0.1.4: extractTree() / sampleTrees() call
  # `utils::data("clootl_data")` internally without `package =`. When
  # clootl is not on the search path, that lookup fails (the function
  # errors with "object 'clootl_data' not found", or — once we work
  # around it — emits a harmless "data set 'clootl_data' not found"
  # warning). Temporarily attach clootl for the duration of this
  # call so the lookup resolves cleanly, and detach on exit so the
  # user's search path is unchanged. Tracked upstream at
  # https://github.com/eliotmiller/clootl.
  if (!"package:clootl" %in% search()) {
    suppressPackageStartupMessages(
      attachNamespace("clootl")
    )
    on.exit(
      try(detach("package:clootl", character.only = TRUE), silent = TRUE),
      add = TRUE
    )
  }

  # The dispatcher (.pr_resolve_query) has already pr_normalize_names()-ed
  # `species`. We pass it through verbatim. This wrapper now returns
  # only list(tree, in_query, citations); the dispatcher computes
  # matched / unmatched against the user's original input.
  #
  # n_tree = 1: clootl::extractTree() returns a single phylo.
  # n_tree > 1: clootl::sampleTrees(count = n_tree) returns a
  #             multiPhylo, but requires the AvesData repo (set up via
  #             `clootl::get_avesdata_repo()`). Without it the call
  #             errors with "AvesData repo not found". As of clootl
  #             0.1.4 `count` is documented as "work in progress, can
  #             only sample 100 for now", so upstream caps at 100.
  call_args <- list(...)
  call_args$species <- species
  if (n_tree > 1L) {
    # `clootl::sampleTrees()` does not accept a `force` argument
    # (only `extractTree()` does), so don't inject one here.
    if (is.null(call_args$count)) {
      call_args$count <- n_tree
    }
    tree <- do.call(clootl::sampleTrees, call_args)
  } else {
    # Default `force = TRUE` so a species that isn't in the eBird /
    # Clements taxonomy is dropped from the returned tree rather
    # than erroring out the whole call. The dispatcher records the
    # missing species in $unmatched (via the in_query vector this
    # wrapper returns). Users can opt out by passing `force = FALSE`.
    if (is.null(call_args$force)) {
      call_args$force <- TRUE
    }
    tree <- do.call(clootl::extractTree, call_args)
  }

  if (
    is.null(tree) || !(inherits(tree, "phylo") || inherits(tree, "multiPhylo"))
  ) {
    cli::cli_abort(c(
      "{.pkg clootl} returned no tree for this species list.",
      "i" = "No queried species matched the requested eBird / Clements taxonomy.",
      ">" = "Check the names in {.arg x} or call {.fn reconcile_suggest} against a Clements-compatible reference."
    ))
  }

  # Compute in_query: which queries does the returned tree contain?
  # Normalise both sides for the intersection (species is already
  # normalised by the dispatcher; tip labels may need light cleanup).
  ref_tips <- if (inherits(tree, "multiPhylo")) {
    tree[[1]]$tip.label
  } else {
    tree$tip.label
  }
  norm_query <- pr_normalize_names(species)
  norm_tip <- pr_normalize_names(ref_tips)
  in_query <- norm_query %in% norm_tip

  # Gather citation block via clootl::getCitations() if present.
  citations <- tryCatch(
    if (
      exists("getCitations", envir = asNamespace("clootl"), inherits = FALSE)
    ) {
      get("getCitations", envir = asNamespace("clootl"))(
        if (inherits(tree, "multiPhylo")) tree[[1]] else tree
      )
    } else {
      NULL
    },
    error = function(e) NULL
  )

  list(tree = tree, in_query = in_query, citations = citations)
}


# rtrees backend: taxon-specific mega-trees ------------------------------

.pr_get_tree_rtrees <- function(species, taxon = NULL, n_tree = 1L, ...) {
  if (!requireNamespace("rtrees", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The {.val rtrees} backend requires the {.pkg rtrees} package.",
        "i" = 'Install with: {.code install.packages("rtrees")}.',
        ">" = "See {.url https://daijiang.github.io/rtrees/} for details."
      )
    )
  }

  if (is.null(taxon) || !nzchar(taxon)) {
    cli::cli_abort(
      c(
        "The {.val rtrees} backend requires a {.arg taxon} argument.",
        "i" = "Choose one of: {.val bird}, {.val mammal}, {.val fish}, {.val amphibian}, {.val reptile}, {.val plant}, {.val shark_ray}, {.val bee}, {.val butterfly}.",
        ">" = "Example: {.code pr_get_tree(x, source = \"rtrees\", taxon = \"bird\")}."
      )
    )
  }

  # rtrees::get_tree() does NOT have an n_tree argument. Multi-tree
  # output is determined by which mega-tree set is selected (e.g.
  # mammal_tree = "vertlife" / "phylacine" returns 100 posterior
  # trees by default; bee_tree = "bootstrap" returns 100; fish_tree
  # = "all-taxon" returns 100). For those taxa, n_tree is informational
  # only -- the count comes from the underlying mega-tree.
  # Single-tree taxa (bird default, etc.) ignore n_tree entirely.
  call_args <- list(...)
  call_args$sp_list <- species
  call_args$taxon <- taxon
  call_args$show_grafted <- TRUE

  tree <- do.call(.pr_rtrees_get_tree, call_args)

  # rtrees returns a phylo when only one source tree was used, and a
  # multiPhylo when many were sampled (e.g. 100 trees from the bird /
  # mammal posterior). Pull the tip-label set from whichever shape we
  # got -- for a multiPhylo all trees share the same tip set so the
  # first one is enough.
  ref_tips <- if (inherits(tree, "multiPhylo")) {
    tree[[1]]$tip.label
  } else {
    tree$tip.label
  }

  # rtrees flags higher-rank grafts with trailing stars. Parse these
  # markers without changing the labels preserved in the returned tree.
  tip_info <- .pr_parse_rtrees_tip_labels(ref_tips)
  norm_query <- pr_normalize_names(species)
  norm_tip <- pr_normalize_names(tip_info$markerless_label)
  in_query <- norm_query %in% norm_tip

  # Per-input placement table (Ayumi #74) ---------------------------
  # Columns:
  #   input_name        original input
  #   tree_name         actual tip label in the tree, or NA if dropped
  #   placement_status  one of "exact", "genus_added", "family_added",
  #                     "skipped" (rtrees decided not to graft it),
  #                     "unmatched" (didn't reach rtrees at all)
  # Map each normalised input to the tip (with its marker) it landed on.
  # Use a vector lookup keyed by the *cleaned* (markerless) tip label.
  tip_by_clean <- stats::setNames(ref_tips, norm_tip)
  status_by_clean <- stats::setNames(tip_info$placement_status, norm_tip)
  placement <- data.frame(
    input_name = species,
    tree_name = unname(tip_by_clean[norm_query]),
    placement_status = vapply(
      norm_query,
      function(nq) {
        if (!nq %in% norm_tip) {
          # rtrees took the name but
          "skipped"
        } else {
          # didn't put it in the tree
          status_by_clean[[nq]]
        }
      },
      character(1L)
    ),
    stringsAsFactors = FALSE
  )
  # rtrees keeps the per-input row even for skipped species; if any
  # of `species` somehow never made it past the dispatcher's
  # normalisation (e.g. all-empty after stripping), mark as "unmatched"
  # rather than "skipped" so the two failure modes stay distinct.
  na_input_idx <- !nzchar(norm_query) | is.na(norm_query)
  placement$placement_status[na_input_idx] <- "unmatched"

  # If show_grafted = TRUE rtrees flags grafted tips with `*` / `**`.
  # Surface the grafted set so users can see which species were placed
  # on higher-rank stand-ins.
  grafted_tips <- ref_tips[tip_info$placement_status != "exact"]

  list(
    tree = tree,
    in_query = in_query,
    backend_meta = list(
      taxon = taxon,
      n_grafted = length(grafted_tips),
      grafted_tips = grafted_tips,
      placement = tibble::as_tibble(placement)
    )
  )
}


# fishtree backend: fish-only, time-calibrated --------------------------

.pr_get_tree_fishtree <- function(species, n_tree = 1L, ...) {
  if (!requireNamespace("fishtree", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The {.val fishtree} backend requires the {.pkg fishtree} package.",
        "i" = 'Install with: {.code install.packages("fishtree")}.',
        ">" = "Reference: Rabosky et al. (2018) {.emph Nature} 559:392 ({.href [doi:10.1038/s41586-018-0273-1](https://doi.org/10.1038/s41586-018-0273-1)})."
      )
    )
  }

  # When n_tree > 1, switch to fishtree_complete_phylogeny() which
  # returns a multiPhylo of stochastically polytomy-resolved trees.
  # Otherwise fishtree_phylogeny() returns the single best-guess
  # chronogram.
  warns <- character()
  multi <- n_tree > 1L

  tree <- withCallingHandlers(
    if (multi) {
      fishtree::fishtree_complete_phylogeny(species = species, ...)
    } else {
      fishtree::fishtree_phylogeny(species = species, ...)
    },
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # If the user requested fewer than fishtree_complete_phylogeny()
  # produced, cap to n_tree.
  if (multi && inherits(tree, "multiPhylo") && length(tree) > n_tree) {
    tree <- tree[seq_len(n_tree)]
  }

  # fishtree uses underscore-form tip labels; for multiPhylo all trees
  # share the same tip set so the first one is enough for matching.
  ref_tips <- if (inherits(tree, "multiPhylo")) {
    tree[[1]]$tip.label
  } else {
    tree$tip.label
  }
  norm_query <- pr_normalize_names(species)
  norm_tip <- pr_normalize_names(ref_tips) # pr_normalize_names handles _ -> space
  in_query <- norm_query %in% norm_tip

  # Pull `type` from the call if user supplied it; default is chronogram.
  call_args <- list(...)
  type_used <- if (!is.null(call_args$type)) call_args$type else "chronogram"

  list(
    tree = tree,
    in_query = in_query,
    backend_meta = list(
      backend = "fishtree",
      type = type_used,
      warnings = warns,
      reference = "Rabosky et al. (2018) Nature 559:392 (doi:10.1038/s41586-018-0273-1)"
    )
  )
}


# datelife backend: chronograms from a published database --------------

.pr_get_tree_datelife <- function(
  species,
  n_tree = 1L,
  summary_format = NULL,
  use_tnrs = FALSE,
  ...
) {
  if (!pr_namespace_available("datelife")) {
    cli::cli_abort(
      c(
        "The {.val datelife} backend requires the {.pkg datelife} package.",
        "i" = 'Install with: {.code pak::pak("phylotastic/datelife")} (GitHub-only; archived from CRAN in 2024).',
        ">" = "See {.url https://github.com/phylotastic/datelife} for details."
      )
    )
  }

  # Default summary_format: single SDM tree when n_tree = 1; all per-source
  # candidates when n_tree > 1.
  if (is.null(summary_format)) {
    summary_format <- if (n_tree > 1L) "phylo_all" else "phylo_sdm"
  }

  # Build a make_datelife_query result so we know which input names
  # datelife can resolve. use_tnrs = FALSE keeps this offline; users
  # who want TNRS pass use_tnrs = TRUE (or set tnrs = "always" at the
  # dispatcher level, which has already run before we get here).
  make_datelife_query <- getExportedValue("datelife", "make_datelife_query")
  datelife_search <- getExportedValue("datelife", "datelife_search")

  query <- make_datelife_query(input = species, use_tnrs = use_tnrs)
  matched_names <- query$cleaned_names
  if (is.null(matched_names)) {
    matched_names <- character()
  }

  # in_query: which species (as the dispatcher passed them) does
  # datelife recognise? Use normalised comparison.
  norm_query <- pr_normalize_names(species)
  norm_keep <- pr_normalize_names(matched_names)
  in_query <- norm_query %in% norm_keep

  res <- datelife_search(
    input = query,
    summary_format = summary_format,
    use_tnrs = use_tnrs,
    ...
  )

  # Coerce return to phylo or multiPhylo per our contract.
  tree <- if (inherits(res, "phylo")) {
    res
  } else if (inherits(res, "multiPhylo")) {
    if (length(res) > n_tree) res[seq_len(n_tree)] else res
  } else if (
    is.list(res) &&
      all(vapply(res, inherits, logical(1), what = "phylo"))
  ) {
    # phylo_all returns a named list of phylo: coerce to multiPhylo
    out <- res
    if (length(out) > n_tree) {
      out <- out[seq_len(n_tree)]
    }
    class(out) <- "multiPhylo"
    out
  } else {
    cli::cli_abort(c(
      "Unexpected return type from {.code datelife::datelife_search}.",
      "i" = "Got class: {.cls {class(res)[1]}}.",
      ">" = "Expected: {.cls phylo}, {.cls multiPhylo}, or a list of {.cls phylo}."
    ))
  }

  # Per-source citations come from the names of the multiPhylo (datelife
  # uses the source citation as the name).
  source_citations <- if (
    inherits(tree, "multiPhylo") &&
      !is.null(names(tree))
  ) {
    names(tree)
  } else {
    NULL
  }

  list(
    tree = tree,
    in_query = in_query,
    backend_meta = list(
      backend = "datelife",
      version = as.character(utils::packageVersion("datelife")),
      summary_format = summary_format,
      source_citations = source_citations,
      reference = "Sanchez Reyes et al. (2024) Syst. Biol. 73:470 (doi:10.1093/sysbio/syae015)"
    )
  )
}


# Per-tree provenance helper --------------------------------------------
#
# Build a per-tree provenance list so downstream consumers (e.g. pigauto)
# can pair tree[[i]] with backend_meta$tree_provenance[[i]]. For a
# single phylo, the list has one element; for a multiPhylo, one per tree.

.pr_ensure_tree_provenance <- function(tree, backend_meta, source) {
  is_multi <- inherits(tree, "multiPhylo")
  n <- if (is_multi) length(tree) else 1L

  # Helper: pick first non-null. Local closure, not a global operator.
  null_or <- function(a, b) if (is.null(a)) b else a

  base_ref <- switch(
    source,
    rotl = "Open Tree of Life synthesis (OTT)",
    rtrees = "Daijiang Li, rtrees package (taxon-specific reference)",
    clootl = null_or(backend_meta$citations, "Clements taxonomy (clootl)"),
    fishtree = null_or(
      backend_meta$reference,
      "Rabosky et al. (2018) Nature 559:392 (doi:10.1038/s41586-018-0273-1)"
    ),
    datelife = null_or(
      backend_meta$reference,
      "Sanchez Reyes et al. (2024) Syst. Biol. 73:470"
    ),
    "(unknown)"
  )

  # For datelife multiPhylo, prefer the per-source citation (in tree names).
  per_tree_citations <- if (source == "datelife" && is_multi) {
    citations <- names(tree)
    if (is.null(citations) || length(citations) != n) {
      rep(base_ref, n)
    } else {
      citations
    }
  } else {
    rep(base_ref, n)
  }

  calibration_method <- switch(
    source,
    rotl = "topology only (no calibration)",
    rtrees = "graft / no recalibration",
    clootl = "Clements posterior sample",
    fishtree = if (n > 1) {
      "stochastic polytomy resolution"
    } else {
      "best-guess chronogram"
    },
    datelife = null_or(backend_meta$summary_format, "datelife summary"),
    NA_character_
  )

  prov <- vector("list", n)
  for (i in seq_len(n)) {
    prov[[i]] <- list(
      source_index = i,
      citation = per_tree_citations[[i]],
      calibration_method = calibration_method,
      n_tips = if (is_multi) {
        ape::Ntip(tree[[i]])
      } else {
        ape::Ntip(tree)
      }
    )
  }

  backend_meta$tree_provenance <- prov
  backend_meta
}


# Print method -----------------------------------------------------------

#' @export
print.pr_tree_result <- function(x, ...) {
  cli::cli_h1("Tree retrieval result")

  # Some backends (rtrees, clootl) can return a multiPhylo when more
  # than one source tree was used. Print a one-tree summary if it's a
  # single phylo, else summarise the list.
  is_multi <- inherits(x$tree, "multiPhylo")
  n_tips_str <- if (is_multi) {
    sprintf("%d tree%s", length(x$tree), if (length(x$tree) == 1) "" else "s")
  } else {
    n_tips <- ape::Ntip(x$tree)
    sprintf("%d tip%s", n_tips, if (n_tips == 1) "" else "s")
  }
  n_matched <- length(x$matched)
  n_unmatched <- length(x$unmatched)

  cli::cli_bullets(c(
    "*" = "Source:    {.val {x$source}}",
    "*" = "Tree:      {n_tips_str}",
    "*" = "Matched:   {.val {n_matched}} species",
    "!" = "Unmatched: {.val {n_unmatched}} species"
  ))

  if (n_unmatched > 0) {
    show <- utils::head(x$unmatched, 5)
    cli::cli_alert_info(
      "First {length(show)} unmatched: {.val {show}}"
    )
    cli::cli_alert_info(
      "Use {.code reconcile_suggest()} to find likely candidates."
    )
  }
  invisible(x)
}
