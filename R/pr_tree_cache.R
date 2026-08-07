# Local on-disk cache for pr_get_tree() / pr_date_tree() ---------------
#
# External tree retrieval is the slowest part of a typical workflow:
# rotl + datelife hit a remote server; rtrees + clootl + fishtree pull
# from disk-bundled mega-tree data that's still slow to subset. Repeat
# calls with the same species list happen all the time during analysis
# iteration. A simple disk-backed cache keyed on the request hash gets
# you a 100x speedup for those cases without changing user code.
#
# Cache shape:
#   <cache_dir>/<source>/<hash>.rds   <-- saved pr_tree_result
#
# Hash is over (species, source, n_tree, taxon, tnrs, ...args) with a
# fixed serialisation so equivalent calls produce the same key.

#' Get or set the local tree-retrieval cache directory
#'
#' Returns the path to the cache directory used by [pr_get_tree()] and
#' [pr_date_tree()] when called with `cache = TRUE`. Pass a path to
#' override the default.
#'
#' @param path A length-1 character vector or `NULL`. If non-`NULL`,
#'   sets the cache directory to `path` (creating it if it doesn't
#'   exist). If `NULL` (default), returns the currently configured
#'   directory.
#'
#' @return A length-1 character vector --- the absolute path of the
#'   cache directory.
#'
#' @details
#' The default cache directory is [tools::R_user_dir()] with type
#' `"cache"` and the package name `"prepR4pcm"`, which on Linux is
#' typically `~/.cache/R/prepR4pcm/`, on macOS
#' `~/Library/Caches/org.R-project.R/R/prepR4pcm/`, and on Windows
#' something under `%LOCALAPPDATA%\R\cache\R\prepR4pcm\`.
#'
#' To use a cache directory you control, pass its path explicitly with
#' `pr_tree_cache_dir(path)`.
#'
#' @seealso [pr_tree_cache_status()] / [pr_tree_cache_clear()];
#'   [pr_get_tree()] for the consumer.
#'
#' @examples
#' # Default location
#' pr_tree_cache_dir()
#'
#' old_cache <- getOption("prepR4pcm.cache_dir", NULL)
#' tmp_cache <- tempfile("prepR4pcm-cache-")
#' pr_tree_cache_dir(tmp_cache)
#' options(prepR4pcm.cache_dir = old_cache)
#' unlink(tmp_cache, recursive = TRUE)
#'
#' @export
pr_tree_cache_dir <- function(path = NULL) {
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1L) {
      cli::cli_abort("{.arg path} must be a length-1 character vector.")
    }
    path <- normalizePath(path, mustWork = FALSE)
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
    options(prepR4pcm.cache_dir = path)
    return(invisible(path))
  }
  configured <- getOption("prepR4pcm.cache_dir", default = NULL)
  if (!is.null(configured)) {
    return(configured)
  }
  default <- tools::R_user_dir("prepR4pcm", which = "cache")
  default
}


#' Show the contents of the local tree-retrieval cache
#'
#' Lists every cache entry by `source`, with file size and modification
#' timestamp. Useful for figuring out where the disk space went or
#' for confirming a fresh run hit the cache.
#'
#' @return A `data.frame` (sorted by most recent first) with columns
#'   `source`, `hash`, `size_kb`, `modified`. Returns an empty data
#'   frame with the same columns when the cache is empty.
#'
#' @seealso [pr_tree_cache_dir()] / [pr_tree_cache_clear()].
#'
#' @examples
#' pr_tree_cache_status()
#'
#' @export
pr_tree_cache_status <- function() {
  dir <- pr_tree_cache_dir()
  if (!dir.exists(dir)) {
    return(data.frame(
      source = character(),
      hash = character(),
      size_kb = numeric(),
      modified = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }
  files <- list.files(
    dir,
    pattern = "\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(files) == 0L) {
    return(data.frame(
      source = character(),
      hash = character(),
      size_kb = numeric(),
      modified = as.POSIXct(character()),
      stringsAsFactors = FALSE
    ))
  }
  rel <- substring(files, nchar(dir) + 2L)
  parts <- strsplit(rel, "/", fixed = TRUE)
  source <- vapply(parts, `[`, character(1), 1)
  hash <- vapply(
    parts,
    function(p) sub("\\.rds$", "", p[length(p)]),
    character(1)
  )
  info <- file.info(files)
  out <- data.frame(
    source = source,
    hash = hash,
    size_kb = round(info$size / 1024, 1),
    modified = info$mtime,
    stringsAsFactors = FALSE
  )
  out[order(out$modified, decreasing = TRUE), , drop = FALSE]
}


#' Clear the local tree-retrieval cache
#'
#' Removes all cached `pr_get_tree()` / `pr_date_tree()` results. By
#' default asks for confirmation before deleting; pass `confirm = FALSE`
#' to skip the prompt (useful in scripts).
#'
#' @param confirm Logical. Ask interactively before deleting? Default
#'   `TRUE`. Ignored in non-interactive sessions (deletion proceeds).
#' @param source A length-1 character vector or `NULL`. If non-`NULL`,
#'   only entries from that backend are cleared (e.g. `"datelife"` to
#'   wipe only datelife cache after a database refresh). If `NULL`
#'   (default), all entries are cleared.
#'
#' @return Invisibly, the number of files removed.
#'
#' @seealso [pr_tree_cache_dir()] / [pr_tree_cache_status()].
#'
#' @examples
#' # Demo against a throwaway cache so the user's real cache is untouched
#' old_opt   <- getOption("prepR4pcm.cache_dir")
#' tmp_cache <- file.path(tempdir(), "prepR4pcm-cache-demo")
#' pr_tree_cache_dir(tmp_cache)
#'
#' # Drop two dummy entries so there is something to clear:
#' dir.create(file.path(tmp_cache, "fishtree"), showWarnings = FALSE)
#' dir.create(file.path(tmp_cache, "rotl"),     showWarnings = FALSE)
#' saveRDS(NULL, file.path(tmp_cache, "fishtree", "abc.rds"))
#' saveRDS(NULL, file.path(tmp_cache, "rotl",     "def.rds"))
#'
#' pr_tree_cache_status()                # 2 entries
#' pr_tree_cache_clear(confirm = FALSE)  # removes both
#' pr_tree_cache_status()                # empty
#'
#' # Restore the previous cache directory
#' options(prepR4pcm.cache_dir = old_opt)
#'
#' @export
pr_tree_cache_clear <- function(confirm = TRUE, source = NULL) {
  dir <- pr_tree_cache_dir()
  if (!dir.exists(dir)) {
    cli::cli_alert_info("Cache directory does not exist; nothing to clear.")
    return(invisible(0L))
  }
  target <- if (is.null(source)) dir else file.path(dir, source)
  files <- list.files(
    target,
    pattern = "\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(files) == 0L) {
    cli::cli_alert_info("Cache is empty.")
    return(invisible(0L))
  }
  if (confirm && interactive()) {
    msg <- if (is.null(source)) {
      sprintf("Delete %d cache file(s) from '%s'? [y/N]: ", length(files), dir)
    } else {
      sprintf(
        "Delete %d cache file(s) for source = '%s'? [y/N]: ",
        length(files),
        source
      )
    }
    ans <- readline(msg)
    if (!tolower(ans) %in% c("y", "yes")) {
      cli::cli_alert_info("Aborted.")
      return(invisible(0L))
    }
  }
  removed <- file.remove(files)
  cli::cli_alert_success("Removed {.val {sum(removed)}} cache file{?s}.")
  invisible(sum(removed))
}


# Internal: build a cache key from a request -------------------------

.pr_tree_cache_key <- function(
  species,
  source,
  n_tree = 1L,
  taxon = NULL,
  tnrs = NULL,
  query = NULL,
  ...
) {
  # Keep species/query order because the cached pr_tree_result stores
  # input-order-sensitive reporting vectors (`matched`, `unmatched`,
  # `mapping`, placement tables). Backend options are serialised in a
  # stable form.
  extras <- .pr_tree_cache_extras(list(...))
  payload <- list(
    species = unique(as.character(species)),
    query = if (is.null(query)) NULL else unique(as.character(query)),
    source = source,
    n_tree = as.integer(n_tree),
    taxon = taxon,
    tnrs = tnrs,
    extras = extras
  )
  # Use a serialisation that's stable across R sessions
  raw <- serialize(payload, connection = NULL, ascii = TRUE)
  # SHA-1 via openssl if available; fall back to digest::digest if not.
  hash <- if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(raw, algo = "sha1", serialize = FALSE)
  } else {
    # Minimal fallback: paste-and-hash with R's md5sum on a tempfile.
    tf <- tempfile()
    on.exit(unlink(tf), add = TRUE)
    writeBin(raw, tf)
    as.character(tools::md5sum(tf))
  }
  hash
}


# Internal: stable representation of backend-specific cache arguments -----

.pr_tree_cache_extras <- function(extras) {
  if (length(extras) == 0L) {
    return(list())
  }
  nm <- names(extras)
  if (is.null(nm)) {
    nm <- rep("", length(extras))
  }
  nm[!nzchar(nm)] <- paste0("..", which(!nzchar(nm)))
  names(extras) <- nm
  extras[order(names(extras))]
}


# Internal: read / write a cache entry --------------------------------

.pr_tree_cache_get <- function(key, source) {
  dir <- pr_tree_cache_dir()
  path <- file.path(dir, source, paste0(key, ".rds"))
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(e) NULL)
}


.pr_tree_cache_put <- function(key, source, value) {
  dir <- pr_tree_cache_dir()
  sub <- file.path(dir, source)
  if (!dir.exists(sub)) {
    dir.create(sub, recursive = TRUE, showWarnings = FALSE)
  }
  path <- file.path(sub, paste0(key, ".rds"))
  saveRDS(value, path)
  invisible(path)
}
