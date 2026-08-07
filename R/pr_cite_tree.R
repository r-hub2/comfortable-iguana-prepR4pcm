# pr_cite_tree() — format citations for a pr_tree_result --------------
#
# Reviewers and journals routinely ask "where did this tree come from?".
# `pr_cite_tree()` walks the `backend_meta$tree_provenance` set in
# `pr_get_tree()` / `pr_date_tree()` and emits a formatted citation
# block, including the backend package and the underlying paper.

#' Format the citations for a tree result
#'
#' Given a `pr_tree_result` produced by [pr_get_tree()] or
#' [pr_date_tree()], emit a formatted citation block listing the
#' backend used, the underlying paper(s), and per-tree source
#' citations when the result is a multi-tree posterior. Useful when
#' writing the methods section of a paper or when adding a tree
#' provenance footnote to a figure.
#'
#' @param result A `pr_tree_result` from [pr_get_tree()] or
#'   [pr_date_tree()].
#' @param format A length-1 character vector. One of:
#'   \describe{
#'     \item{`"text"` (default)}{Plain-text citation block, suitable
#'       for printing or copy-pasting.}
#'     \item{`"markdown"`}{GitHub-flavoured markdown with bullets and
#'       headings; suitable for issue threads, PR descriptions, and
#'       README sections.}
#'     \item{`"bibtex"`}{One BibTeX entry per source. Use this to
#'       paste into a manuscript bibliography. Note: the entries are
#'       hand-rolled from package metadata, not pulled from a
#'       canonical bibliographic source --- always sanity-check
#'       before submission.}
#'   }
#'
#' @return A length-1 character vector containing the formatted
#'   citation block. The result is also printed (invisibly returned)
#'   so calling `pr_cite_tree(res)` on its own at the console shows
#'   the block.
#'
#' @seealso [pr_get_tree()] / [pr_date_tree()] for producing the
#'   `pr_tree_result` that this function formats.
#'
#' @examples
#' # Build a minimal `pr_tree_result` by hand so the three citation
#' # formats are visible without a network call. In real use this
#' # object is returned by `pr_get_tree()` or `pr_date_tree()`.
#' fake_res <- structure(
#'   list(
#'     source       = "fishtree",
#'     tree         = ape::read.tree(text = "(Salmo_salar,Esox_lucius);"),
#'     backend_meta = list(tree_provenance = list())
#'   ),
#'   class = "pr_tree_result"
#' )
#'
#' cat(pr_cite_tree(fake_res, format = "text"))      # human-readable
#' cat(pr_cite_tree(fake_res, format = "markdown"))  # for a README
#' cat(pr_cite_tree(fake_res, format = "bibtex"))    # for a .bib file
#'
#' \donttest{
#'   # Realistic use after actually retrieving a tree from a backend:
#'   if (requireNamespace("fishtree", quietly = TRUE)) {
#'     res <- pr_get_tree(c("Salmo salar", "Esox lucius"),
#'                        source = "fishtree")
#'     cat(pr_cite_tree(res, format = "markdown"))
#'   }
#' }
#'
#' @export
pr_cite_tree <- function(result,
                          format = c("text", "markdown", "bibtex")) {
  if (!inherits(result, "pr_tree_result")) {
    cli::cli_abort(c(
      "{.arg result} must be a {.cls pr_tree_result} object.",
      "i" = "Got: {.cls {class(result)[1]}}.",
      ">" = "Use {.fn pr_get_tree} or {.fn pr_date_tree} to produce one."
    ))
  }
  format <- match.arg(format)

  source <- result$source
  prov   <- result$backend_meta$tree_provenance
  if (is.null(prov)) prov <- list()

  # Backend-package and underlying-paper citations.
  pkg_meta <- .pr_cite_backend_meta(source, result$backend_meta)

  switch(
    format,
    text     = .pr_cite_text(source, pkg_meta, prov),
    markdown = .pr_cite_markdown(source, pkg_meta, prov),
    bibtex   = .pr_cite_bibtex(source, pkg_meta, prov)
  )
}


# Internal: per-backend metadata ---------------------------------------

.pr_cite_backend_meta <- function(source, backend_meta) {
  switch(
    source,
    rotl = list(
      pkg_name  = "rotl",
      pkg_cite  = "Michonneau, F., Brown, J. W., & Winter, D. J. (2016). rotl: an R package to interact with the Open Tree of Life data. Methods in Ecology and Evolution, 7(12), 1476-1481. doi:10.1111/2041-210X.12593",
      paper_cite = "OpenTreeOfLife / Hinchliff et al. (2015) Synthesis of phylogeny and taxonomy into a comprehensive tree of life. PNAS 112:12764-12769. doi:10.1073/pnas.1423041112",
      bibkey    = "michonneau2016rotl"
    ),
    rtrees = list(
      pkg_name  = "rtrees",
      pkg_cite  = "Li, D. (2023). rtrees: an R package to assemble phylogenetic trees from megatrees. Ecography, 2023, e06643. doi:10.1111/ecog.06643",
      paper_cite = "Li, D. (2023). rtrees: an R package to assemble phylogenetic trees from megatrees. Ecography, 2023, e06643. doi:10.1111/ecog.06643",
      bibkey    = "li2023rtrees"
    ),
    clootl = list(
      pkg_name  = "clootl",
      pkg_cite  = "Miller, E. T. (2023). clootl: Clements taxonomy phylogenies. R package, https://github.com/eliotmiller/clootl",
      paper_cite = "See clootl::getCitations() for the per-tree source citations from the Clements / eBird taxonomy.",
      bibkey    = "miller2023clootl"
    ),
    fishtree = list(
      pkg_name  = "fishtree",
      pkg_cite  = "Chang, J., Rabosky, D. L., Smith, S. A., & Alfaro, M. E. (2019). An R package and online resource for macroevolutionary studies using the ray-finned fish tree of life. Methods in Ecology and Evolution, 10(7), 1118-1124. doi:10.1111/2041-210X.13182",
      paper_cite = "Rabosky, D. L., Chang, J., Title, P. O., Cowman, P. F., Sallan, L., Friedman, M., Kashner, K., Garilao, C., Near, T. J., Coll, M., & Alfaro, M. E. (2018). An inverse latitudinal gradient in speciation rate for marine fishes. Nature, 559(7714), 392-395. doi:10.1038/s41586-018-0273-1",
      bibkey    = "rabosky2018fishtree"
    ),
    datelife = list(
      pkg_name  = "datelife",
      pkg_cite  = "Sanchez Reyes, L. L., O'Meara, B., et al. (2024). datelife R package. doi:10.5281/zenodo.593938",
      paper_cite = "Sanchez Reyes, L. L., McTavish, E. J., & O'Meara, B. (2024). DateLife: Leveraging databases and analytical tools to reveal the dated Tree of Life. Systematic Biology, 73(2), 470-485. doi:10.1093/sysbio/syae015",
      bibkey    = "sanchezreyes2024datelife"
    ),
    list(
      pkg_name  = source,
      pkg_cite  = "(no citation available)",
      paper_cite = "(no citation available)",
      bibkey    = "unknown"
    )
  )
}


# Internal: text format -------------------------------------------------

.pr_cite_text <- function(source, pkg_meta, prov) {
  lines <- c(
    paste0("Tree retrieved via prepR4pcm::pr_get_tree(source = \"",
           source, "\")."),
    "",
    "Backend package:",
    paste0("  ", pkg_meta$pkg_cite),
    "",
    "Underlying reference:",
    paste0("  ", pkg_meta$paper_cite)
  )
  if (length(prov) > 1L) {
    lines <- c(
      lines, "",
      paste0("Per-tree source citations (n = ", length(prov), "):")
    )
    for (i in seq_along(prov)) {
      cm <- prov[[i]]$calibration_method
      if (is.null(cm)) cm <- "n/a"
      lines <- c(
        lines,
        sprintf("  [%d] %s (calibration: %s)",
                i, prov[[i]]$citation, cm)
      )
    }
  }
  out <- paste(lines, collapse = "\n")
  cat(out, "\n", sep = "")
  invisible(out)
}


# Internal: markdown format --------------------------------------------

.pr_cite_markdown <- function(source, pkg_meta, prov) {
  lines <- c(
    paste0("### Tree retrieved via `prepR4pcm::pr_get_tree(source = \"",
           source, "\")`"),
    "",
    "**Backend package:**",
    "",
    paste0("- ", pkg_meta$pkg_cite),
    "",
    "**Underlying reference:**",
    "",
    paste0("- ", pkg_meta$paper_cite)
  )
  if (length(prov) > 1L) {
    lines <- c(
      lines, "",
      paste0("**Per-tree source citations** (n = ", length(prov), "):"),
      ""
    )
    for (i in seq_along(prov)) {
      cm <- prov[[i]]$calibration_method
      if (is.null(cm)) cm <- "n/a"
      lines <- c(
        lines,
        sprintf("- [%d] %s _(calibration: %s)_",
                i, prov[[i]]$citation, cm)
      )
    }
  }
  out <- paste(lines, collapse = "\n")
  cat(out, "\n", sep = "")
  invisible(out)
}


# Internal: bibtex format ----------------------------------------------

.pr_cite_bibtex <- function(source, pkg_meta, prov) {
  # Hand-rolled BibTeX: not pulled from a canonical source, so flag it.
  pkg_entry <- sprintf(
    "@misc{%s_pkg,\n  note = {%s}\n}",
    pkg_meta$bibkey, .pr_bibtex_escape(pkg_meta$pkg_cite)
  )
  paper_entry <- sprintf(
    "@misc{%s_paper,\n  note = {%s}\n}",
    pkg_meta$bibkey, .pr_bibtex_escape(pkg_meta$paper_cite)
  )

  entries <- c(
    "% Auto-generated by prepR4pcm::pr_cite_tree(format = \"bibtex\").",
    "% Sanity-check before submitting to a journal.",
    pkg_entry,
    "",
    paper_entry
  )
  if (length(prov) > 1L) {
    for (i in seq_along(prov)) {
      entries <- c(
        entries, "",
        sprintf("@misc{%s_tree%d,\n  note = {%s}\n}",
                pkg_meta$bibkey, i,
                .pr_bibtex_escape(prov[[i]]$citation))
      )
    }
  }
  out <- paste(entries, collapse = "\n")
  cat(out, "\n", sep = "")
  invisible(out)
}


# Internal: minimal escape for BibTeX special chars --------------------

.pr_bibtex_escape <- function(x) {
  if (is.null(x)) return("")
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("{",  "\\{",  x, fixed = TRUE)
  x <- gsub("}",  "\\}",  x, fixed = TRUE)
  x
}


