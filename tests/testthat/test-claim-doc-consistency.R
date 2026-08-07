# Round 11 preventive checks: doc-vs-code consistency.
#
# These tests catch "the docs claim X but the code does Y" drift,
# which has bitten us repeatedly:
#   - pr_get_tree(source = ...) backend list claimed n_tree=50 worked
#     for backends that didn't accept n_tree at all.
#   - getting-started.Rmd documented `ott` as if it were an R
#     package when it's actually a taxadb authority name.
#   - pr_phylo_cor() docs reference packages not in DESCRIPTION.
#
# We can't catch every subtle wording bug, but we CAN catch:
#   1. Authority names: pr_valid_authorities() <-> getting-started.Rmd.
#   2. Backend names: pr_get_tree's source enum <-> pkgdown <-> vignettes.
#   3. Every `pkg::` reference in source resolves: package is on
#      CRAN, in DESCRIPTION's Suggests/Enhances, or in Remotes.

skip_if_not_root_accessible <- function() {
  root <- .claim_root()
  if (is.na(root)) {
    skip("source tree not accessible")
  }
  root
}


test_that("every authority in pr_valid_authorities() is named in getting-started.Rmd", {
  skip_on_cran()
  root <- skip_if_not_root_accessible()
  vig <- file.path(root, "vignettes", "getting-started.Rmd")
  if (!file.exists(vig)) {
    skip("vignette not found")
  }
  txt <- paste(readLines(vig, warn = FALSE), collapse = "\n")
  for (auth in pr_valid_authorities()) {
    # Match e.g. **`col`** or `col` in the authorities section
    pat <- sprintf("\\*?\\*?`%s`\\*?\\*?", auth)
    expect_true(
      grepl(pat, txt, perl = TRUE),
      info = sprintf(
        "authority %s not mentioned as `%s` in getting-started.Rmd",
        auth,
        auth
      )
    )
  }
})


test_that("every pr_get_tree(source = ...) value is documented in the Rd help page", {
  skip_on_cran()
  root <- skip_if_not_root_accessible()
  rd_path <- file.path(root, "man", "pr_get_tree.Rd")
  if (!file.exists(rd_path)) {
    skip("Rd file not found")
  }
  rd_txt <- paste(readLines(rd_path, warn = FALSE), collapse = "\n")

  sources <- eval(formals(pr_get_tree)$source)
  for (src in sources) {
    pat <- sprintf('"%s"', src)
    expect_true(
      grepl(pat, rd_txt, fixed = TRUE),
      info = sprintf("source = '%s' not mentioned in pr_get_tree.Rd", src)
    )
  }
})


test_that("reconcile_augment backend arguments stay complete and version-neutral", {
  skip_on_cran()
  root <- skip_if_not_root_accessible()
  rd_path <- file.path(root, "man", "reconcile_augment.Rd")
  tree_rd_path <- file.path(root, "man", "pr_get_tree.Rd")
  if (!all(file.exists(c(rd_path, tree_rd_path)))) {
    skip("Rd files not found")
  }

  rd_txt <- paste(readLines(rd_path, warn = FALSE), collapse = "\n")
  tree_rd_txt <- paste(readLines(tree_rd_path, warn = FALSE), collapse = "\n")
  sources <- eval(formals(reconcile_augment)$source)

  source_item <- sub(
    ".*\\\\item\\{source\\}\\{(.*?)\\n\\n\\\\item\\{taxon\\}.*",
    "\\1",
    rd_txt,
    perl = TRUE
  )
  taxon_item <- sub(
    ".*\\\\item\\{taxon\\}\\{(.*?)\\n\\n\\\\item\\{check_ultrametric\\}.*",
    "\\1",
    rd_txt,
    perl = TRUE
  )

  for (src in sources) {
    expect_true(
      grepl(sprintf('"%s"', src), source_item, fixed = TRUE),
      info = sprintf("source = '%s' missing from reconcile_augment source docs", src)
    )
  }
  for (src in setdiff(sources, "rtrees")) {
    expect_true(
      grepl(sprintf('"%s"', src), taxon_item, fixed = TRUE),
      info = sprintf("taxon docs do not say source = '%s' ignores taxon", src)
    )
  }

  expect_false(
    grepl("rtrees 1.0.4", tree_rd_txt, fixed = TRUE),
    info = "pr_get_tree docs must not pin grafting semantics to old rtrees 1.0.4"
  )
  expect_true(
    grepl("no exact-only switch", tree_rd_txt, fixed = TRUE),
    info = "pr_get_tree docs should state the version-neutral exact-only limitation"
  )
})


test_that("every pr_get_tree(source = ...) backend is in the pkgdown reference index", {
  skip_on_cran()
  root <- skip_if_not_root_accessible()
  yml <- file.path(root, "_pkgdown.yml")
  if (!file.exists(yml)) {
    skip("_pkgdown.yml not found")
  }
  yml_txt <- paste(readLines(yml, warn = FALSE), collapse = "\n")
  expect_true(
    grepl("- pr_get_tree\\b", yml_txt),
    info = "pr_get_tree should be in the pkgdown reference index"
  )
})


test_that("tree backend n_tree docs avoid stale backend API claims", {
  skip_on_cran()
  root <- skip_if_not_root_accessible()
  doc_paths <- file.path(
    root,
    c(
      "NEWS.md",
      "_pkgdown.yml",
      "README.md",
      file.path("R", "pr_get_tree.R"),
      file.path("man", "pr_get_tree.Rd"),
      file.path("vignettes", "comparing-tree-backends.Rmd")
    )
  )
  doc_paths <- doc_paths[file.exists(doc_paths)]
  doc_txt <- paste(
    vapply(
      doc_paths,
      function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
      character(1)
    ),
    collapse = "\n"
  )

  stale_claims <- c(
    "rtrees::get_tree(n_tree",
    "extractTree(sample.size",
    "sample.size = n_tree"
  )
  for (claim in stale_claims) {
    expect_false(
      grepl(claim, doc_txt, fixed = TRUE),
      info = paste("stale n_tree backend claim still appears:", claim)
    )
  }

  expect_true(
    grepl("informational only", doc_txt, fixed = TRUE),
    info = "rtrees docs should clarify that n_tree is informational"
  )
  expect_true(
    grepl("sampleTrees(count", doc_txt, fixed = TRUE),
    info = "clootl docs should name sampleTrees() for n_tree > 1"
  )
})


test_that("`pkg::` references in vignettes/man point to real packages", {
  skip_on_cran()
  root <- skip_if_not_root_accessible()

  # Collect every "pkg::" reference from R source, vignettes, and Rd.
  files <- c(
    list.files(file.path(root, "vignettes"), "\\.Rmd$", full.names = TRUE),
    list.files(file.path(root, "R"), "\\.R$", full.names = TRUE),
    list.files(file.path(root, "man"), "\\.Rd$", full.names = TRUE)
  )
  pkgs <- character()
  for (f in files) {
    txt <- readLines(f, warn = FALSE)
    m <- regmatches(txt, regexpr("[a-zA-Z][a-zA-Z0-9.]+::", txt))
    pkgs <- c(pkgs, sub("::$", "", m))
  }
  pkgs <- sort(unique(pkgs[nzchar(pkgs)]))

  # Skip non-package tokens that look like pkg::fn:
  #   - "github" (a Remotes-prefix string)
  #   - "prepR4pcm" (the package itself)
  #   - "stats" / "utils" / "tools" etc (base packages)
  base <- c(
    "base",
    "stats",
    "utils",
    "tools",
    "graphics",
    "grDevices",
    "methods"
  )
  internal <- c("github", "prepR4pcm")
  pkgs <- setdiff(pkgs, c(base, internal))

  # Read DESCRIPTION's full dep set
  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  parse_field <- function(name) {
    if (!name %in% colnames(desc)) {
      return(character())
    }
    v <- desc[1, name]
    if (is.na(v)) {
      return(character())
    }
    parts <- trimws(strsplit(v, ",")[[1]])
    parts <- gsub("\\s*\\([^)]*\\)\\s*$", "", parts)
    parts <- gsub(",\\s*$", "", parts)
    parts[nzchar(parts)]
  }
  imports <- parse_field("Imports")
  suggests <- parse_field("Suggests")
  enhances <- parse_field("Enhances")
  depends <- setdiff(parse_field("Depends"), "R")
  declared <- c(imports, suggests, enhances, depends)

  # External packages that we reference in docs/vignettes but
  # don't actually need ourselves. These appear in eval=FALSE
  # chunks or in prose like "feed the result to metafor::rma.mv()".
  # The user installs these themselves when they want to use them.
  # Add to this allowlist when you add a new external-pkg ref;
  # the point of this test is to catch *bugs* like a misspelled
  # package name, not to require everything we ever mention to be
  # in our DESCRIPTION.
  external_ok <- c(
    "pigauto", # sister package; pigauto::multi_impute_trees() in vignettes
    "pak", # `pak::pak(...)` install instructions
    "metafor", # downstream consumer in meta-analysis vignette
    "brms", # `?pr_phylo_cor` mentions as downstream consumer
    "glmmTMB", # `?pr_phylo_cor` mentions as downstream consumer
    "memoise", # NEWS.md prose about caching strategy
    "phangorn", # comparing-tree-backends.Rmd: alternative consensus tool
    "datelife", # optional GitHub-only backend, guarded at runtime
    "U.PhyloMaker", # optional GitHub-only augmentation backend
    "V.PhyloMaker", # optional GitHub-only augmentation backend
    "V.PhyloMaker2" # optional GitHub-only augmentation backend
  )

  unaccounted <- setdiff(pkgs, c(declared, external_ok))
  expect_equal(
    length(unaccounted),
    0,
    info = paste0(
      "These `pkg::` references appear in vignettes/R/man but the ",
      "package is not in DESCRIPTION's Imports/Suggests/Enhances/",
      "Depends and is not on the external-OK allowlist:\n  ",
      paste(unaccounted, collapse = "\n  ")
    )
  )
})


test_that("ott is documented as a taxadb authority, not as a separate R package", {
  # Round 11 regression test: rotl (the package) and ott (the
  # taxonomy / taxadb provider) were confused in earlier doc rounds.
  skip_on_cran()
  root <- skip_if_not_root_accessible()
  vig <- file.path(root, "vignettes", "getting-started.Rmd")
  if (!file.exists(vig)) {
    skip("vignette not found")
  }
  txt <- paste(readLines(vig, warn = FALSE), collapse = "\n")

  # The vignette must explicitly say ott is NOT an R package and is
  # NOT the same as rotl.
  expect_true(
    grepl("not an R package|not the same as.*rotl", txt, perl = TRUE),
    info = "getting-started.Rmd should explicitly distinguish ott (taxadb authority) from rotl (package)"
  )
})


test_that("pr_date_tree help page explains n_dated semantics (one topology -> N branch-length variants)", {
  skip_on_cran()
  root <- skip_if_not_root_accessible()
  rd <- file.path(root, "man", "pr_date_tree.Rd")
  if (!file.exists(rd)) {
    skip("Rd not found")
  }
  txt <- paste(readLines(rd, warn = FALSE), collapse = "\n")
  expect_true(
    grepl("share.*topology|do(es)? NOT change", txt, perl = TRUE),
    info = "pr_date_tree.Rd should clarify that n_dated > 1 keeps topology fixed"
  )
})
