# Backend health probe -------------------------------------------------
#
# New users routinely hit "do I have everything installed to use this?"
# pr_get_tree_status() answers that for every backend in one call:
# package installed, version available, network reachable (when the
# backend needs network).

#' Report the install status of every `pr_get_tree()` backend
#'
#' Walks every backend supported by [pr_get_tree()] and reports
#' whether the underlying package is installed (and at what version),
#' whether it requires network, and what to do if it's missing. Useful
#' for first-time users figuring out which backends are available, and
#' for CI sanity checks.
#'
#' @param check_network Logical. Should the probe attempt a tiny
#'   network call to test that backends needing the network are
#'   actually reachable? Default `FALSE` (purely local check, no
#'   side effects). Set `TRUE` to also test reachability --- adds
#'   1-3 seconds and requires internet.
#'
#' @return A `data.frame` with one row per backend and columns:
#'   \describe{
#'     \item{`source`}{Backend name, as passed to `pr_get_tree()`.}
#'     \item{`installed`}{Logical --- is the package available?}
#'     \item{`version`}{Character --- installed version, or `NA`.}
#'     \item{`needs_network`}{Logical --- does the backend hit a
#'       remote server at runtime?}
#'     \item{`reachable`}{Logical or `NA` --- result of the network
#'       check (only populated when `check_network = TRUE`).}
#'     \item{`install_hint`}{Character --- the install command to run
#'       when `installed = FALSE`.}
#'     \item{`source_repo`}{Character --- "CRAN" or a GitHub
#'       repo for non-CRAN backends.}
#'   }
#'
#' @seealso [pr_get_tree()] / [pr_date_tree()] for the consumers.
#'
#' @examples
#' # Local-only probe (fast, no network)
#' pr_get_tree_status()
#'
#' \donttest{
#'   # Also test reachability of remote backends
#'   pr_get_tree_status(check_network = TRUE)
#' }
#'
#' @export
pr_get_tree_status <- function(check_network = FALSE) {
  backends <- list(
    list(source = "rotl",     pkg = "rotl",
         needs_network = TRUE,
         install_hint  = 'install.packages("rotl")',
         source_repo   = "CRAN"),
    list(source = "rtrees",   pkg = "rtrees",
         needs_network = FALSE,
         install_hint  = 'install.packages("rtrees")',
         source_repo   = "CRAN"),
    list(source = "clootl",   pkg = "clootl",
         needs_network = FALSE,
         install_hint  = 'install.packages("clootl")',
         source_repo   = "CRAN"),
    list(source = "fishtree", pkg = "fishtree",
         needs_network = TRUE,
         install_hint  = 'install.packages("fishtree")',
         source_repo   = "CRAN"),
    list(source = "datelife", pkg = "datelife",
         needs_network = FALSE,    # local cache; TNRS is opt-in
         install_hint  = 'pak::pak("phylotastic/datelife")',
         source_repo   = "github::phylotastic/datelife")
  )

  rows <- lapply(backends, function(b) {
    installed <- requireNamespace(b$pkg, quietly = TRUE)
    version <- if (installed) {
      tryCatch(
        as.character(utils::packageVersion(b$pkg)),
        error = function(e) NA_character_
      )
    } else {
      NA_character_
    }
    reachable <- if (check_network && b$needs_network) {
      .pr_check_backend_reachable(b$source)
    } else {
      NA
    }
    data.frame(
      source        = b$source,
      installed     = installed,
      version       = version,
      needs_network = b$needs_network,
      reachable     = reachable,
      install_hint  = b$install_hint,
      source_repo   = b$source_repo,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


# Internal: backend reachability probe ----------------------------------
#
# rotl: ping api.opentreeoflife.org via a tiny tnrs_match_names call.
# fishtree: ping fishtreeoflife.org via a tiny fishtree_taxonomy call.
# datelife: local cache, no network needed (return TRUE if installed).
# rtrees / clootl: local data, return TRUE if installed.

.pr_check_backend_reachable <- function(source) {
  if (!requireNamespace(source, quietly = TRUE)) return(NA)
  out <- tryCatch({
    switch(source,
      rotl     = {
        rotl::tnrs_match_names("Homo sapiens")
        TRUE
      },
      fishtree = {
        fishtree::fishtree_taxonomy(ranks = "order")
        TRUE
      },
      TRUE
    )
  }, error = function(e) FALSE)
  out
}
