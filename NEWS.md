# prepR4pcm 1.0.2

* `reconcile_augment(source = "rtrees")` now correctly distinguishes exact placements from genus (`*`) and family (`**`) grafts, and reports the graft rank in its existing augmentation metadata (#112).

* `reconcile_crosswalk_supplement()` adds a safer post-baseline crosswalk workflow: run exact/normalised/synonym/fuzzy matching first, apply one-to-one crosswalk rows only to names that remain unresolved on both sides, and skip duplicate source or target candidates instead of accepting split/lump rows by row order.

# prepR4pcm 1.0.1

* `reconcile_crosswalk()` now warns more clearly when split/lump rows are kept as automatic overrides, and the bird workflow now demonstrates the safer pattern: run exact/normalised matching first, restrict automatic crosswalk use to one-to-one rows, and treat `1BL to many BT` / `Many BL to 1BT` cases as manual review candidates. The documentation now states explicitly that crosswalk overrides are applied before the matching cascade and can preempt otherwise exact or normalised matches.

# prepR4pcm 1.0.0

* **CRAN review response.** `DESCRIPTION` now includes the EcoEvoRxiv method reference, remaining `\dontrun{}` examples were replaced with CRAN-preferred executable structures, and `reconcile_export()` now defaults to a unique temporary directory rather than the current working directory.

* **Stable Software Note release.** Version 1.0.0 declares the package-ready release for Ecography Software Note submission, with release date 2026-06-16; no API-breaking changes were introduced relative to 0.5.1.

# prepR4pcm 0.5.1

* **CRAN preparation.** Release metadata, README installation guidance, and bundled-data citation links were updated for the first CRAN submission.

* **pkgdown publishing guard.** The pkgdown build wrapper now removes loose agent-instruction pages from the generated site and search index before GitHub Pages deployment.

* **pkgdown workflow fix.** `dependencies: '"most"'` in
  `.github/workflows/pkgdown.yaml` tells pak to skip `Enhances`
  (i.e. `datelife`) during lockfile resolution. The
  `workflow_dispatch` rebuild no longer fails on "Can't find
  package called datelife". The day-to-day path is unaffected:
  GitHub Pages still serves the docs straight from `main:/docs/`.

* **`comparing-tree-backends.Rmd` gains a "Branch lengths and
  time-calibration" section** with a per-backend comparison table
  and a practical decision tree for choosing between `fishtree` /
  `clootl` / `rtrees` / `datelife` / Grafen pseudo-time when you
  need real divergence-time branch lengths. Surfaces the
  alternative path when `datelife` isn't installable on a given
  system.

# prepR4pcm 0.5.0

This release adds two opt-in Global Names Architecture backends, a
phylogenetic-meta-analysis workflow, and a substantial round of
tree-handling audit metadata. All existing API contracts are preserved
unless explicitly called out under *Breaking changes*.

## New features

### Optional Global Names backends

* **`pr_normalize_names(parser = "gnparser")`** routes parsing through
  [rgnparser](https://CRAN.R-project.org/package=rgnparser), the R
  wrapper for the [gnparser](https://github.com/gnames/gnparser) Go
  binary. Set `parser = "gnparser"` for hardened parsing of hybrid
  signs, complex multi-author year strings, and Open Tree homonym /
  rank flag parentheticals. Returns the same shape and
  `normalisation_log` attribute as the internal cascade, so the two
  are drop-in interchangeable. Default stays `parser = "internal"`
  (zero-dependency).

* **`authority = "gnverifier"`** is accepted by every `reconcile_*`
  function (`reconcile_data()`, `reconcile_tree()`,
  `reconcile_multi()`, `reconcile_to_trees()`, `reconcile_trees()`).
  It routes the synonym stage through the
  [Global Names verifier](https://verifier.globalnames.org/) over
  HTTP (~100 sources in one round-trip) instead of a local taxadb
  database. No ~100 MB local cache; requires network access and
  `httr2`. Default stays `authority = "col"` (taxadb).

### Tree-handling audit metadata

* **`pr_get_tree()` / `pr_date_tree()` now return `result$mapping`** —
  one row per unique input species, with the user-facing name,
  normalised name, backend query name, returned tree tip, match type,
  and rtrees placement status when available. Replaces ad-hoc
  reconstruction from `$matched` + `$unmatched` + backend-specific
  metadata (#73).

* **TNRS match metadata in `result$mapping`** — four new columns
  (`tnrs_number_matches`, `tnrs_is_synonym`, `tnrs_approximate_match`,
  `tnrs_flags`) carry the structured output of
  `rotl::tnrs_match_names()`. Homonyms (`tnrs_number_matches > 1`)
  trigger a one-shot warning naming the affected species.

* **`result$backend_meta$placement` (rtrees only)** — per-input table
  with `input_name`, `tree_name`, `placement_status` (`exact` /
  `genus_added` / `family_added` / `skipped` / `unmatched`). Filter
  to `placement_status == "exact"` to drop grafted tips from a
  sensitivity analysis (#74).

* **TNRS substitutions are now auditable** —
  `result$backend_meta$tnrs_replacements` lists every name TNRS
  changed; a one-shot warning shows the first three. Silent name
  correction is no longer possible (#72).

* **Multi-tree reporting fields** on `pr_get_tree()` — `backend_meta`
  gains `n_requested`, `tip_set_consistent`, and `dropped_per_tree`
  (#76).

### Phylogenetic meta-analysis path

* **`pr_get_tree()` gains `resolve_polytomies` and `branch_lengths`
  arguments.** When the topology comes from `rotl`,
  `pr_get_tree(species, source = "rotl", resolve_polytomies = TRUE,
  branch_lengths = "grafen")` returns a tree ready for
  `metafor::rma.mv()`. Defaults preserve back-compat.

* **New: `pr_phylo_cor(tree)`** — thin wrapper around
  `ape::vcv(tree, corr = TRUE)` that turns the tree into the
  phylogenetic correlation matrix accepted by `metafor::rma.mv()`,
  `MCMCglmm::MCMCglmm()`, `glmmTMB::glmmTMB()`, and `brms::brm()` as
  a random-effect structure. Accepts `phylo`, `multiPhylo`, or
  `pr_tree_result` input.

* **New vignette: "Phylogenetic meta-analysis with rotl + prepR4pcm"** —
  end-to-end walk from species names through rotl topology,
  bifurcating + Grafen branches, correlation matrix, to
  `metafor::rma.mv()`. Uses a 13-species cross-taxon subset of
  Pottier et al. 2022's thermal-tolerance dataset.

## Bug fixes

* **`reconcile_augment()` keeps grafted trees ultrametric.** When the
  input tree is ultrametric and `branch_length != "zero"`, a post-graft
  correction ensures the augmented tips reach the present, so
  downstream PGLS / BM / OU models see a valid tree. Regression test
  pinned (PR #105, Losia Lagisz).

* **`pr_get_tree()` matched / unmatched accounting** now enforces
  three invariants: `matched` ⊆ `unique(input)`, `unmatched` ⊆
  `unique(input)`, and `|matched| + |unmatched| == |unique(input)|`.
  The `matched` slot preserves the user's original input format
  (underscores stay underscores). Previously, TNRS-resolved names
  could leak through (#73).

* **`pr_get_tree(source = "clootl")` is now ~250× faster** on large
  bird species lists (3.6 s vs > 15 min for 10,597 birds; #70). TNRS
  preflight no longer runs for clootl by default (clootl uses
  Clements taxonomy, not OTL); `force = TRUE` is passed to
  `clootl::extractTree()` so a single unmatched species doesn't
  error out the whole call.

* **`pr_get_tree(source = "clootl")` accepts underscore-separated
  names** by converting them to the space-separated form `clootl`
  expects, while preserving the user's originals in `$matched` /
  `$unmatched` (#75).

* **`pr_get_tree(source = "clootl", n_tree = 1)` no longer requires
  `library(clootl)`** — the wrapper temporarily attaches the namespace
  for the duration of the call.

* **`reconcile_apply()` validates `species_col`** before filtering
  data, so a typo errors clearly instead of silently returning zero
  data rows.

* **`reconcile_multi()` no longer undercounts** dataset-specific
  matches when the same species appears in different formats across
  datasets (e.g. `Homo_sapiens` vs `Homo sapiens`). The cascade gains
  a `multi_x = TRUE` mode; the mapping gains the documented
  `in_<dataset>` logical columns (#10, Ayumi Mizuno).

* **`reconcile_summary()` no longer auto-prints when assigned.** The
  formatted report lives on the returned object's `formatted_text`
  slot and renders via `print.reconciliation_summary()` (#12, Ayumi
  Mizuno).

* **Trailing parenthetical leak fixed in name normalisation.** The
  internal cascade now strips any trailing parenthetical, catching
  Open Tree's `(species in domain Eukaryota)` and similar TNRS
  leak-through.

* **`pr_lookup_authority()` and `pr_ensure_db()`** no longer print
  raw `{.pkg ...}` / `{.code ...}` cli template strings; errors route
  through `cli::cli_abort()` (#4, Eduardo Santos).

* **Removed `V.PhyloMaker3`** from `Suggests` and `Remotes` — the
  repo doesn't exist. `vphylomaker` augment backend now prefers
  `V.PhyloMaker2` with a fallback to `V.PhyloMaker`.

## Documentation

* **New cover prose on the README and landing page** — names both
  halves of the prerequisite that `prepR4pcm` solves: reconcile names
  *and* retrieve / date trees. New "Quick example — fetching a tree"
  snippet.

* **Three new vignettes** (in addition to the meta-analysis one
  above):
  * **"Posterior-tree pipeline (prepR4pcm + pigauto)"** — chaining
    `reconcile_data()` → `pr_get_tree()` →
    `pigauto::multi_impute_trees()`.
  * **"Comparing tree backends"** — when do they agree?
  * **"Assembling mammal trait databases for PCMs"**
    (`db-assembly-workflow_mammals`, Santiago Ortega; closes #11) —
    combining Amniote / PanTHERIA / TetrapodTraits, reconciling,
    applying manual corrections, producing a tree-aligned
    species-level data frame.

* **`?pr_get_tree` and `?pr_tree_compare` `@references`** are now
  spelled out with DOIs (#79, #80, Jimuel Celeste, Jr.). Every
  author-year citation in the help text has a full reference (Jetz,
  Rabosky, Upham, Sanchez Reyes, Chang, Michonneau, Kuhner &
  Felsenstein, Robinson & Foulds).

* **`?mammal_tree_example` cites Upham et al. 2019** (#11). The
  bundled 5,987-tip mammal phylogeny is documented as a subset of
  the VertLife mammal phylogeny, with the 76 `X_`-prefixed Mesozoic
  stem-mammal fossils described as products of the Upham et al.
  "backbone-and-patch" framework.

* **`?pr_date_tree` clarifies `n_dated > 1` semantics** — one
  topology with `n_dated = 50` returns 50 chronograms sharing the
  topology but differing in branch lengths (one per DateLife source),
  not 50 different topologies.

* **`getting-started.Rmd`** makes explicit that `col`, `itis`, `gbif`,
  `ncbi`, `ott`, `itis_test` are taxadb authority names, not R
  packages, and that `ott` (Open Tree Taxonomy) differs from `rotl`
  (the R package that retrieves trees from Open Tree of Life).
  Expanded mismatch-types section with worked examples for
  formatting / synonymy / typos.

* **Several rounds of README and vignette feedback** from Mal Lagisz
  (#53, #61, #64) — clearer Features definitions, typical-workflow
  diagram, Quick example, sentence-level revisions throughout.
  Detailed feedback from Ayumi Mizuno (#1) seeded the multi-row
  species and asymmetric datasets vignette subsections.

* **Logo redesign** — tree tips align with matrix rows. Favicons
  regenerated.

* **New author**: Bhavya Jain added to `Authors@R` and the citation
  block.

## Build, CI, and dependencies

* **`httr2` and `rgnparser` added to `Suggests`** — for the new
  `authority = "gnverifier"` and `parser = "gnparser"` backends
  respectively.

* **CI workflow trimmed** to `pull_request` + `workflow_dispatch`
  only, Linux-only by default. The `workflow_dispatch` trigger has
  an `os` input set to `full` before a release / CRAN submission for
  the full macOS / Windows / Linux × 3 R-version matrix. Saves ~85%
  of GitHub Actions minutes.

* **`datelife` moved from `Suggests` + `Remotes` to `Enhances`** —
  datelife was archived from CRAN in 2024 and has a heavy transitive
  dep tree pak's resolver can't install on clean CI. Users opt in
  with `pak::pak("phylotastic/datelife")`.

* **`piggyback` allowlisted** in the Suggests-vs-Remotes consistency
  test.

* **`dplyr`, `readr`, and `stringr`** moved from `Suggests` to
  `Imports` (used routinely in the package).

## Tests

* `tests/testthat/test-pr_lookup_gnverifier.R` — gnverifier helper
  (happy-path mix, network-failure degradation, `db_version` warning,
  mismatched-response shape, session-cache hit, missing-httr2 abort,
  live integration against `verifier.globalnames.org`).
* `tests/testthat/test-pr_normalize.R` — gnparser backend (mocked +
  live).
* Regression tests for the rtrees placement table, the TNRS metadata
  columns, the multi-tree reporting fields, the clootl performance
  fix, the ultrametric-preserving graft, and the matched/unmatched
  invariants.
* Existing combinatorial test layer (252 `test_that()` blocks,
  ~2,737 expectations) carried forward unchanged.

# prepR4pcm 0.4.0

This release closes out the multi-round tree-handling overhaul started
at issue #42 (Ayumi Mizuno) and tracked at issue #48. Across Rounds
4 - 8 the package gained:

- Five tree-retrieval backends (rotl, rtrees, clootl, fishtree, datelife)
  unified under `pr_get_tree(..., n_tree, source = "auto")`.
- Three augmentation backends (rtrees, V.PhyloMaker3/V.PhyloMaker2, U.PhyloMaker)
  on `reconcile_augment()`.
- A standalone dating function (`pr_date_tree()`) for adding chronogram
  calibrations to existing topologies.
- Per-tree provenance metadata that pairs every tree in a multiPhylo
  with its citation, calibration method, and tip count -- designed
  for downstream consumption by
  [pigauto](https://itchyshin.github.io/pigauto/)`::multi_impute_trees()`.
- `pr_cite_tree()` for formatting citations (text / markdown / bibtex).
- `pr_tree_compare()` with bipartition-matched branch-length correlation.
- `pr_get_tree_status()` for backend health probing.
- An on-disk cache (`pr_tree_cache_*`) for repeat retrievals.
- Three vignettes covering the cross-package pipeline, backend
  comparison, and the original workflows.

## Round 8: V.PhyloMaker / U.PhyloMaker + bipartition correlation + ultrametric check

* **`reconcile_augment()` gains two new sources:**
  - `source = "vphylomaker"` -- plant-only alternative to `rtrees`,
    via the GitHub packages `V.PhyloMaker3` (preferred) or
    `V.PhyloMaker2` (fallback). Both share the same
    `phylo.maker(sp.list, tree, scenarios)` API. Use this when you
    want explicit V.PhyloMaker scenario control (S1 / S2 / S3, see
    Jin & Qian 2019/2022).
  - `source = "uphylomaker"` -- universal (plants + animals)
    alternative via `U.PhyloMaker` (Jin & Qian 2023, *Plant
    Diversity*). Same `phylo.maker` convention plus a `gen.list`
    argument; the helper auto-loads `U.PhyloMaker::nodes.info.1`
    when not supplied.
* **Bipartition-matched branch-length correlation in
  `pr_tree_compare()`.** The previous Pearson-on-sorted-edges
  approximation is replaced with a proper bipartition match: for
  each edge in tree A, the corresponding edge in tree B is the one
  splitting the same set of tips. Edges whose bipartition isn't
  shared are dropped from the correlation.
* **`check_ultrametric = TRUE` argument on `pr_get_tree()`,
  `pr_date_tree()`, and `reconcile_augment()`.** After producing the
  tree, runs `ape::is.ultrametric()` and warns if the backend was
  expected to produce an ultrametric tree but didn't. Skipped for
  `rotl` (synthesis topology, no real branch lengths) and for the
  internal augment with `branch_length = "zero"` (which breaks
  ultrametricity by design). Does not modify the tree -- to force
  ultrametricity, call `phytools::force.ultrametric()` or
  `ape::chronos()` on the result yourself.
* **TimeTree.org REST client deferred** as a non-goal. TimeTree's
  TOS restricts bulk querying for non-commercial use; a REST wrapper
  isn't a clean fit for prepR4pcm's "install-and-go" backend model.
  Users who need TimeTree data can fetch it manually and feed the
  resulting topology into `pr_date_tree()` (or skip dating entirely).

## Round 7: documentation polish and CI cleanup

* **New vignette: "Comparing tree backends -- when do they agree?"**
  Walks through `pr_tree_compare()` end-to-end, including how to
  read the pairwise Jaccard / Robinson-Foulds / branch-length
  matrices and what to do for each disagreement pattern. Closes
  the docs gap left by Round 6.
* **`Remotes:` simplification.** Daijiang Li merged a
  `Remotes: daijiang/megatrees` entry upstream in `rtrees`
  ([daijiang/rtrees#10](https://github.com/daijiang/rtrees/issues/10)),
  so we no longer need to carry the megatrees Remotes entry
  ourselves. Dropped from `DESCRIPTION` and the
  `.transitive_remotes_allowlist` test fixture.
* **One-shot rotl-missing TNRS warning.** Previously the warning
  fired once per backend call; now it fires once per session via
  `options(prepR4pcm.tnrs_warning_shown = TRUE)`. Mainly a fix for
  `source = "auto"`, which calls each candidate backend in turn.
* **Round 7 edge-case + integration tests.** New tests cover
  corrupt cache files, missing-key reads, cache invalidation by
  `taxon` and `n_tree`, tree comparisons across exotic shapes
  (no edge lengths, 3+ trees, named arguments), `min_match`
  boundary values, end-to-end pipelines including
  `pr_get_tree -> pr_cite_tree`, multi-tree provenance preservation,
  cache save/load round-trip, and the `auto` dispatcher's
  `auto_attempts` / `auto_chose` metadata.

## Round 6: tree-handling UX polish

* **Local on-disk cache for `pr_get_tree()`.** Pass `cache = TRUE`
  to memoise the request on disk; subsequent identical calls are
  instant. New companion functions:
  - `pr_tree_cache_dir(path = NULL)` -- get/set the cache directory.
    Defaults to `tools::R_user_dir()`; can be set to a project-local
    path so the cache travels with the analysis.
  - `pr_tree_cache_status()` -- list cache entries with timestamps
    and sizes.
  - `pr_tree_cache_clear(confirm, source)` -- wipe the cache;
    optionally restrict to a single backend.
* **`pr_get_tree_status(check_network)` health probe.** One-call
  report listing every backend with installed / version /
  needs_network / reachable / install hint. Useful for first-time
  users figuring out which backends are available.
* **`pr_tree_compare(...)` for comparing trees.** Tip-set Jaccard,
  Robinson-Foulds distance on the shared subtree, branch-length
  agreement. Accepts `phylo`, `multiPhylo`, or `pr_tree_result`
  inputs (positional or named).
* **TNRS preflight for non-TNRS backends.** New `tnrs` argument on
  `pr_get_tree()`: `"auto"` (default; runs TNRS for clootl + fishtree
  to lift their match rates), `"always"` (run regardless), `"never"`
  (skip). Silently skipped with a one-shot warning when `rotl` is
  not installed.
* **`source = "auto"` fall-through dispatcher.** Tries installed
  backends in priority order; returns the first that resolves at
  least `min_match` of the species (default 0.8) or the best of the
  lot if none meets the threshold. New `min_match` argument
  controls the threshold.
* `digest` added to `Suggests` (used by the new cache-key hashing).

## Round 5: posterior-tree support and the prepR4pcm -> pigauto pipeline

* **`pr_get_tree()` gains a unified `n_tree` parameter** so users can
  request a posterior sample (`multiPhylo`) when a backend supports
  one. Default `n_tree = 1L` (back-compat). When `n_tree > 1`:
  - `"rotl"` -- still returns 1 (the synthesis tree); a one-shot
    warning explains that no posterior is available.
  - `"rtrees"` -- `n_tree` is informational only; the number of
    returned trees is fixed by the selected mega-tree and any
    backend-specific arguments forwarded through `...`.
  - `"clootl"` -- `n_tree = 1` calls `clootl::extractTree()`;
    `n_tree > 1` calls `clootl::sampleTrees(count = n_tree)` and
    requires the AvesData repo.
  - `"fishtree"` -- switches to `fishtree_complete_phylogeny()` for
    a multi-tree stochastic polytomy resolution.
  - `"datelife"` -- (new backend; see below) returns one chronogram
    per database source, capped at `n_tree`.

* **New backend: `pr_get_tree(source = "datelife")`.** Universal
  database of pre-computed chronograms (Sanchez Reyes et al. 2024,
  *Syst. Biol.* 73:470). Returns a single SDM-summary chronogram
  by default; `n_tree > 1` returns a multiPhylo of per-source
  candidate chronograms. Backend lives in `Suggests` + `Remotes`
  (datelife was archived from CRAN in 2024 -- install with
  `pak::pak("phylotastic/datelife")`).

* **New function: `pr_date_tree(tree, n_dated, ...)`.** Wraps
  `datelife::datelife_use()` to time-calibrate an existing topology.
  Returns the same `pr_tree_result` shape as `pr_get_tree()` so
  downstream consumers (notably
  [pigauto](https://itchyshin.github.io/pigauto/)) treat retrieved
  and dated trees interchangeably.

* **New function: `pr_cite_tree(result, format = ...)`.** Formats a
  citation block (text / markdown / bibtex) for a `pr_tree_result`,
  including per-tree source citations when the result is a
  multi-tree posterior. Helpful for paper methods sections, figure
  legends, and PR descriptions.

* **Per-tree provenance metadata.** Every `pr_tree_result` now
  carries `backend_meta$tree_provenance`: a list with one entry per
  returned tree (citation, calibration method, tip count). For
  `multiPhylo` results, `tree[[i]]` pairs naturally with
  `backend_meta$tree_provenance[[i]]`. Designed to feed
  `pigauto::multi_impute_trees()`.

* **Cross-package vignette: "From species names to a phylogenetic
  posterior -- prepR4pcm + pigauto".** End-to-end walkthrough showing
  how to chain `reconcile_data()` -> `pr_get_tree()` (or
  `pr_date_tree()`) -> `pigauto::multi_impute_trees()` -> pooled
  inference via Rubin's rules. The vignette uses mock data and
  `eval = FALSE` chunks for the parts that need pigauto / datelife
  installed.

* **Cross-link with `pigauto`.** `?pr_get_tree`, `?pr_date_tree`,
  and `?reconcile_augment` mention pigauto in `@seealso`. The
  pkgdown reference index gains a "Sister package: pigauto" callout.

## New features

* `reconcile_augment()` gains `source = c("internal", "rtrees")`
  (default `"internal"`, the existing genus-level grafting behaviour)
  and `taxon` arguments. With `source = "rtrees"`, grafting is
  delegated to `rtrees::get_tree(tree_by_user = TRUE)`, which uses
  your tree as the backbone and lets `rtrees`' taxon-specific
  reference tree place each missing species via genus / family
  information. Helpful when the genus is absent from your tree but
  present in `rtrees`' reference -- the internal mode would skip
  these. Returns the same result shape (with `meta$source` recording
  which backend was used). Refs #42 (Ayumi Mizuno).

* `pr_get_tree()` gains a fourth backend, `source = "fishtree"`,
  exposing the time-calibrated fish-only phylogeny of Rabosky et al.
  (2018, *Nature* 559:392) via the CRAN package \pkg{fishtree}.
  Returns a chronogram by default; pass `type = "phylogram"` for the
  uncalibrated version. Captures `fishtree`'s own warning text into
  `backend_meta$warnings` so the matched/unmatched report is honest.

* `pr_get_tree()` connects a reconciled species list to an external
  phylogenetic resource and returns a pruned candidate tree plus a
  matching report (matched / unmatched / source / backend metadata).
  Four backends ship:
  - `"rotl"` -- Open Tree of Life synthesis tree (universal coverage,
    via the CRAN package \pkg{rotl}).
  - `"rtrees"` -- taxon-specific mega-trees (bird, mammal, fish,
    amphibian, reptile, plant, shark/ray, bee, butterfly), via the
    GitHub package \pkg{rtrees} (`pak::pak("daijiang/rtrees")`).
  - `"clootl"` -- bird-only phylogenies in current Clements
    taxonomy, via the GitHub package \pkg{clootl}
    (`pak::pak("eliotmiller/clootl")`).
  - `"fishtree"` -- fish-only time-calibrated phylogeny, via the
    CRAN package \pkg{fishtree}.

  Accepts a reconciliation object, a character vector, or a data
  frame as input. Each backend is loaded only on demand --
  asking for a backend you don't have installed produces a helpful
  migration error with the install command. Refs #42 (Ayumi
  Mizuno).

## What's NOT in this round (deferred work)

For honesty / handoff so future contributors don't lose track:

* **#10** -- `reconcile_multi()` may undercount dataset-specific
  matches when names differ only by formatting (Round 4 candidate;
  needs root-cause debug).
* **#12** -- `reconcile_summary()` prints even when assigned to a
  variable (Round 4 candidate; small UX fix, needs `quiet` /
  `print_report` argument).
* **#14** -- Ayumi's documentation-feedback summary (Round 3).
* **#15-#21, #26** -- Per-function documentation feedback wave from
  pooherna and Sergio (Round 3; will be batched into a single
  doc-quality pass).
* **#16** -- Suggested Delhey citation correction. Requires
  verification before changing; current citation may already be
  correct.
* The remaining 22 of 37 Tier-4 defence-in-depth claim-parity tests
  outlined in the round-2 plan; they're documented but not yet
  written.
* **Round 5 work** (separate tracking issue): a unified `n_tree`
  parameter on `pr_get_tree()` so each backend can return a posterior
  sample (multiPhylo) when one is available; a new `pr_date_tree()`
  function wrapping `datelife::datelife_use()` for time-calibrating
  user topologies; a `datelife` retrieval backend on `pr_get_tree()`;
  per-tree provenance metadata; and a cross-package vignette
  documenting the prepR4pcm -> pigauto pipeline for posterior-tree
  PCMs.

## New vignette and example data

* New vignette **"Assembling mammal trait databases for phylogenetic
  comparative models"** (`db-assembly-workflow_mammals`), contributed
  by Santiago Ortega. Walks through combining three mammal trait
  sources (Amniote, PanTHERIA, TetrapodTraits), reconciling the
  unique species names against a phylogenetic tree, applying
  manual corrections, and collapsing the matched records into a
  model-ready species-level data frame aligned with a pruned tree.
  Closes #11.

* Four new bundled example datasets used by the new vignette:
  `mammal_amniote_example` (Myhrvold et al. 2015),
  `mammal_pantheria_example` (Jones et al. 2009),
  `mammal_tetrapodtraits_example` (Moura et al. 2024), and
  `mammal_tree_example` (a 5,987-tip mammal phylogeny, source to
  be confirmed with the contributor).

* `dplyr`, `readr`, and `stringr` moved from `Suggests` to `Imports`,
  reflecting that they are used routinely in the package's vignettes
  and R code rather than being optional.

## Breaking changes

* `pr_valid_authorities()` no longer lists `iucn`, `tpl`, `fb`, `slb`,
  or `wd`. Empirical testing against `taxadb` v22.12 (the database the
  package depends on) showed that `iucn` errors with a schema mismatch
  and the other four are not `taxadb` providers at all. Anyone passing
  one of these values was getting a hard error from inside `taxadb`;
  the call sites now produce a targeted migration message pointing
  users at the working authorities. (Identified in follow-up to #5,
  Ayumi Mizuno.)

  If you were passing one of the removed authorities, switch to one of
  `"col"`, `"itis"`, `"gbif"`, `"ncbi"`, or `"ott"`, or pass
  `authority = NULL` to skip synonym resolution.

## New features

* `authority = "ott"` (Open Tree of Life) is supported again, after
  Round 1 dropped it on incomplete diagnosis. The original failure
  was at `taxadb::td_create()` with the default schema set
  `c("dwc", "common")` --- `taxadb` v22.12 does not ship a `common`
  schema for OTT. We now restrict the schema to `"dwc"` (the only
  schema the cascade actually consumes), unblocking OTT and
  potentially other providers that lack a `common` schema. Re-closes
  #5 properly.

* `authority = "itis_test"` exposes `taxadb`'s small bundled testing
  dataset. Useful for examples and unit tests without a network
  round-trip.

## Bug fixes

* `pr_lookup_authority()` and `pr_ensure_db()` no longer print raw
  `{.pkg ...}` / `{.code ...}` cli template strings in their error
  messages. The functions now route errors through `cli::cli_abort()`,
  which interprets the markup. Closes #4 (Eduardo Santos).

* The matching cascade no longer prints `Stage 3/4: Synonym
  resolution (...)` when `authority = NULL`, or
  `Stage 4/4: Fuzzy matching (...)` when `fuzzy = FALSE`. Stage
  numbering is computed from the active stages only, so a call with
  `authority = NULL, fuzzy = FALSE` reports `Stage 1/2: Exact ...`
  and `Stage 2/2: Normalised ...`. Previously, the fixed `Stage X/4`
  labels suggested matches were being made at synonym/fuzzy stages
  even when they were skipped. Closes #13 (Ayumi Mizuno).

* `reconcile_tree()` and `reconcile_data()` previously dropped manual
  overrides silently when the override `name_x` was not in the data
  or `name_y` was not in the target. The reconciliation object now
  carries an `unused_overrides` slot listing every rejected override
  with a `reason` (`name_x_not_in_data`, `name_y_not_in_target`, or
  `already_matched`), and the functions emit a `cli_alert_warning()`
  pointing the user at it. `reconcile_summary()` includes a count
  and a per-row listing in the verbose section. Closes #8a
  (Ayumi Mizuno).

## New features

* `reconcile_crosswalk()` now accepts `.csv`, `.tsv`, or `.txt`
  (tab-delimited) file paths in addition to data frames. The format
  is inferred from the file extension. Closes #8b (Ayumi Mizuno).

## Documentation

* Installation instructions are standardised on `pak::pak(...)` across
  the README and the *Getting started* vignette. Closes #6
  (Ayumi Mizuno).

* The `@param authority` block in `reconcile_tree()` /
  `reconcile_data()` now reflects which authorities are actually
  supported. `tpl`, `slb`, `wd`, `iucn`, `fb` are flagged as
  experimental ("coverage and current availability vary"); `ott` is
  documented as not supported in the current default `taxadb` release.

* The `bird-workflow` vignette now guards its `caper` and `MCMCglmm`
  chunks with `eval = requireNamespace(..., quietly = TRUE)`, so the
  vignette knits cleanly for users (and CRAN check environments)
  without those Suggests packages installed.

* Added a hex sticker logo (`man/figures/logo.svg` / `logo.png`) to
  the README and the pkgdown site.

* pkgdown site rebuilt to fix stale search-index links that pointed
  to 404 pages for `reconcile_override_batch` and `reconcile_suggest`.
  Closes #7 (Ayumi Mizuno).

## Breaking changes

* The first argument of 13 exported functions has been renamed from
  `x` to `reconciliation`: `reconcile_apply()`, `reconcile_augment()`,
  `reconcile_export()`, `reconcile_mapping()`, `reconcile_merge()`,
  `reconcile_override()`, `reconcile_override_batch()`,
  `reconcile_plot()`, `reconcile_report()`, `reconcile_review()`,
  `reconcile_splits_lumps()`, `reconcile_suggest()`, and
  `reconcile_summary()`. This fixes #3 (Santiago Ortega), where
  `reconcile_apply(result = res_tree, ...)` raised an "unused
  argument" error because the parameter was named `x`. Positional
  calls (`reconcile_apply(res_tree, ...)`) continue to work
  unchanged; only code that passed the reconciliation as `x = ...`
  needs updating to `reconciliation = ...`. `reconcile_diff(x, y)`
  is intentionally unchanged — both arguments are reconciliation
  objects in a symmetric comparison, so neither is the
  "reconciliation".

## Documentation

* New vignette subsections on **multi-row species** and
  **asymmetric datasets** in the bird workflow vignette, addressing
  #1 (Ayumi Mizuno). Shows how to aggregate to species level before
  merging, how to join the mapping back to a full multi-row dataset,
  and when to pick `how = "inner"` vs `how = "left"` for focal
  study × reference database merges.
* `reconcile_merge()` help page now carries the same guidance in a
  `@details` block, covering pairwise row expansion warnings and
  the four join types.
* Rewrote every exported function's help page for an ecologist /
  evolutionary biologist audience (the primary users running PCM,
  PGLS, and PGLMM analyses). The previous pages read like API
  reference; the new pages explain the reconciliation workflow,
  taxadb authority choices, fuzzy matching semantics, and tree
  augmentation trade-offs in terms the audience actually uses.
* `?prepR4pcm` is now a proper package landing page with a canonical
  workflow code block, a "Key concepts" section (reconciliation
  object, four-stage cascade, provenance, splits/lumps, augmentation),
  and function-family pointers.
* Each taxadb `authority` option in `reconcile_data()` and
  `reconcile_tree()` is now glossed in one line (`"col"` = Catalogue
  of Life, `"gbif"` = GBIF Backbone, etc.) to help users choose
  without consulting the taxadb manual.
* `reconcile_augment()` gained a "When to use this" section with
  explicit cautions about reporting augmented tips, running
  sensitivity analyses, and preferring PhyloMaker / TACT for
  publication-grade augmentation.
* `reconcile_suggest()` now explains Levenshtein similarity and the
  60/40 genus/epithet weighting in plain language.
* `reconcile_mapping()` documents every column of the returned
  tibble, including when `name_resolved` is `NA` and the full
  `match_type` vocabulary.
* New help page for the `reconciliation` S3 class documenting its
  four list components (`mapping`, `meta`, `counts`, `overrides`)
  and S3 methods. This also clears previous R CMD check
  "Missing link(s)" warnings from cross-references.
* Reorganised the pkgdown reference index into seven task-oriented
  groups (Match species names / Inspect and audit / Corrections and
  crosswalks / Apply, merge, export / Augment phylogenies /
  Name utilities / Bundled example data), each with a short
  descriptive line.

## Tests

* Added a combinatorial test layer that stresses parameter
  combinations rather than single cases. Three new test files
  (`test-authority-mocked.R`, `test-workflows.R`,
  `test-robustness.R`) and parametric grid extensions to nine
  existing files take the suite from ~311 expectations to
  **1,868 expectations** across 252 `test_that()` blocks (0 failures).
* Every historical bug that has shipped — #495 cartesian merge
  explosion, the `drop_unresolved` no-op, the diacritics regex
  failure, factor coercion, silent multi-phylo handling — lived in
  parameter combinations that single-axis tests never exercised.
  The new layer tests combinations and asserts invariants
  (row counts, NA counts, tree tip counts, set membership, S3 class,
  idempotence).
* `test-authority-mocked.R` stubs `pr_lookup_authority()` via
  `local_mocked_bindings()` so the synonym-resolution branch is
  exercised without hitting taxadb or the network, covering
  accepted→synonym, synonym→accepted, neither-found, and
  network-error scenarios for `col` / `itis` / `gbif` / `ncbi`.
* `test-workflows.R` chains functions end-to-end the way real users
  do, including the #495 asymmetric pattern (750 shared /
  96 only_x / 10,400 only_y).
* `test-robustness.R` covers adversarial inputs: empty data,
  all-`NA` species columns, factor columns, Unicode (diacritics
  and Japanese kana), minimal 1-row/1-tip cases, large-input
  smoke tests, invalid types, and malformed arguments.

# prepR4pcm 0.3.0

## New features

* `reconcile_report()` generates a self-contained HTML report documenting
  all name-matching decisions — suitable for sharing or archiving.
* `reconcile_merge()` joins two reconciled datasets into a single
  analysis-ready data frame using the mapping table from `reconcile_data()`.
* `reconcile_augment()` grafts unresolved species onto a tree using
  genus-level placement (sister to congener or MRCA of congeners).
* `reconcile_splits_lumps()` detects taxonomic splits and lumps from
  synonym resolution results.
* Fuzzy matching via `fuzzy = TRUE` catches likely typos using
  component-based Levenshtein similarity.
* `resolve = "flag"` marks low-confidence matches for manual review.
* `reconcile_plot()` visualises match composition as a bar chart or pie
  chart using base R graphics.
* `reconcile_suggest()` shows the closest fuzzy candidates for each
  unresolved species — useful for finding near-misses.
* `reconcile_diff()` compares two reconciliation objects and reports
  gained/lost matches, type changes, and target changes.
* `reconcile_override_batch()` applies multiple overrides at once from a
  data frame or CSV file.
* `reconcile_review()` provides an interactive console interface for
  accepting or rejecting flagged and fuzzy matches one at a time.
* `print.reconciliation()` now shows a coverage bar:
  `[████████████████████░░░░░░░░░░] 71% (657/919)`.
* Stage-level progress messages for large datasets (> 500 species) in
  the matching cascade.

## Data

* Example datasets expanded from ~200 to ~920 species (Corvoidea +
  allied passerine families) for realistic demonstrations.
* All `\dontrun{}` examples replaced with runnable examples using
  bundled data.

## Performance

* Fuzzy matching (`pr_fuzzy_match()`) now uses genus pre-filtering: only
  species whose genus is within 2 edits are compared. This reduces
  computation from O(n×m) to roughly O(n×k) where k is the number of
  congeners+near-genera, giving ~100× speedup on large datasets
  (e.g., 1360×6504 from >10 min to ~3 sec).
* `reconcile_suggest()` uses the same genus pre-filter and vectorised
  `adist()`, making it usable for 1000+ unresolved species.

## Bug fixes

* **Fixed crosswalk overrides having no effect.** The cascade's override
  pre-stage required exact string matches between override names and
  input names. When override names used spaces but tree tips used
  underscores (or vice versa), no overrides were applied. Overrides
  now normalise both sides before comparison.
* Fixed `reconcile_plot()` error when passing `main` argument: the
  internal `pr_plot_bar()` hardcoded `main` and also passed `...`,
  causing a duplicate argument error.
* Fixed `.Rbuildignore` patterns that excluded `.rda` data files on
  case-insensitive filesystems.

# prepR4pcm 0.2.0

* Core reconciliation engine: exact → normalised → synonym → fuzzy cascade.
* `reconcile_tree()`, `reconcile_data()`, `reconcile_trees()`.
* `reconcile_apply()`, `reconcile_export()`, `reconcile_override()`.
* `reconcile_to_trees()`, `reconcile_multi()`, `reconcile_crosswalk()`.
* `reconcile_summary()`, `reconcile_mapping()`.
* Bundled datasets: avonet_subset, nesttrait_subset, delhey_subset,
  crosswalk_birdlife_birdtree, tree_jetz, tree_clements25.
* Bird workflow vignette.
