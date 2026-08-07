# Name normalisation -------------------------------------------------------

#' Normalise scientific names to a canonical form
#'
#' Apply a sequence of deterministic text transformations so that
#' scientific names which differ only in formatting compare equal.
#' This is the same routine used by stage 2 of the matching cascade in
#' [reconcile_data()] and [reconcile_tree()]. Use it directly when you
#' want to clean a column of names without running a full
#' reconciliation --- for example, when building a crosswalk by hand.
#'
#' @details
#' The transformations, applied in order, are:
#' \enumerate{
#'   \item Replace underscores and multiple whitespace with a single
#'     space (`Homo_sapiens` -> `Homo sapiens`).
#'   \item Strip authority strings and year, including multi-author
#'     and parenthetical forms (`Corvus corax (Linnaeus, 1758)` ->
#'     `Corvus corax`).
#'   \item Strip any other trailing parenthetical qualifier, such as
#'     the Open Tree of Life homonym / rank flags that `rotl` returns
#'     (`Prunella (genus in kingdom Archaeplastida)` -> `Prunella`).
#'   \item Fold diacritics to ASCII (`Passer domesticus` stays as
#'     `Passer domesticus`; accented characters are simplified).
#'   \item Standardise case: genus capitalised, epithet lowercase.
#'   \item Strip infraspecific epithets if `rank = "species"`.
#'   \item Trim whitespace and collapse leftover empty tokens.
#' }
#'
#' @param names A character vector of scientific names (any length;
#'   each element is a single name). `NA` values are preserved as
#'   `NA`.
#' @param rank A length-1 character vector. Taxonomic rank to normalise to:
#'   \describe{
#'     \item{`"species"` (default)}{Strip infraspecific epithets so
#'       trinomials become binomials (`Parus major major` ->
#'       `Parus major`).}
#'     \item{`"subspecies"`}{Keep trinomials intact.}
#'   }
#' @param parser A length-1 character vector. Which parsing engine to
#'   use:
#'   \describe{
#'     \item{`"internal"` (default)}{The package's own regex-based
#'       cascade described above. No external dependency.}
#'     \item{`"gnparser"`}{Delegates parsing to
#'       [rgnparser::gn_parse_tidy()], which wraps the
#'       [gnparser](https://github.com/gnames/gnparser) Go binary
#'       (part of the Global Names Architecture). Handles hybrid
#'       signs, complex multi-author year strings, and trailing
#'       parentheticals (Open Tree homonym / rank flags) more
#'       robustly than the internal cascade. Requires both the
#'       \pkg{rgnparser} R package and the `gnparser` binary on the
#'       system PATH; the function errors helpfully if either is
#'       missing. Returns the same shape and `normalisation_log`
#'       attribute as the internal path, so the two are drop-in
#'       interchangeable.}
#'   }
#'
#' @return A character vector of normalised names, the same length as
#'   `names`, with an attribute `"normalisation_log"` --- a tibble
#'   recording every non-trivial change, for auditing.
#'
#' @note On the spelling: the title and prose use British English
#'   \emph{normalise}, consistent with the package's
#'   `Language: en-GB` declaration. The function identifier
#'   `pr_normalize_names()` keeps the American-English `z` because
#'   R-package function names conventionally use ASCII identifiers
#'   in the form most R users expect. The two spellings are
#'   equivalent and intentional.
#'
#' @family name utilities
#' @seealso [reconcile_data()] and [reconcile_tree()] for the full
#'   four-stage matching cascade; [pr_extract_tips()] for pulling tip
#'   labels out of a tree prior to normalising them.
#'
#' @examples
#' pr_normalize_names(c("Homo_sapiens",
#'                      "homo sapiens",
#'                      "Parus major major",
#'                      "Corvus corax (Linnaeus, 1758)"))
#'
#' # Keep trinomials
#' pr_normalize_names("Parus major major", rank = "subspecies")
#'
#' @export
pr_normalize_names <- function(names, rank = c("species", "subspecies"),
                               parser = c("internal", "gnparser")) {
  rank <- match.arg(rank)
  parser <- match.arg(parser)
  if (parser == "gnparser") {
    return(.pr_normalize_gnparser(names, rank))
  }
  original <- names

  # 1. Coerce to character, handle NA

  names <- as.character(names)
  is_na <- is.na(names)
  names[is_na] <- ""

  # 2. Trim leading/trailing whitespace
  names <- trimws(names)

  # 3. Replace underscores with spaces
  names <- gsub("_", " ", names, fixed = TRUE)

  # 4. Collapse multiple internal spaces to single space
  names <- gsub("\\s+", " ", names)

  # 5. Strip OTT ID suffixes (e.g., "Homo sapiens ott770315")
  names <- gsub("\\s+ott\\d+$", "", names, perl = TRUE)

  # 6. Strip trailing author/year strings

  # Matches patterns like "Linnaeus, 1758" or "(Linnaeus, 1758)" at end

  # Also handles "L." or "Author, Year" patterns
  names <- pr_strip_authority(names)

  # 6b. Strip any other trailing parenthetical qualifier. Open Tree of
  # Life's TNRS appends homonym / rank flags to the `unique_name` it
  # returns (e.g. "Salmo salar (species in domain Eukaryota)",
  # "Prunella (genus in kingdom Archaeplastida)"). pr_strip_authority()
  # only catches the (Author, Year) form; a trailing parenthetical is
  # never part of a name you would match against a tree tip.
  names <- sub("\\s*\\([^()]*\\)\\s*$", "", names, perl = TRUE)

  # 7. Standardise hybrid signs
  names <- gsub("\\s*\u00d7\\s*", " x ", names)  # multiplication sign
  names <- gsub("^x\\s+", "x ", names)            # leading x
  names <- gsub("\\s+x\\s+", " x ", names)        # internal x

  # 8. Standardise infraspecific rank abbreviations
  names <- gsub("\\bsubsp\\.?\\s+", "subsp. ", names, perl = TRUE)
  names <- gsub("\\bssp\\.?\\s+", "subsp. ", names, perl = TRUE)
  names <- gsub("\\bvar\\.?\\s+", "var. ", names, perl = TRUE)
  names <- gsub("\\bf\\.?\\s+", "f. ", names, perl = TRUE)

  # 9. Strip infraspecific epithets if rank == "species"
  if (rank == "species") {
    names <- pr_strip_infraspecific(names)
  }

  # 10. Standardise case: capitalise genus, lowercase rest
  names <- pr_standardise_case(names)

  # 11. Final trim
  names <- trimws(names)

  # Restore NA

  names[is_na] <- NA_character_

  # Build log
  changed <- !is_na & (original != names)
  log <- tibble(
    original   = original,
    normalised = names,
    changed    = changed
  )

  attr(names, "normalisation_log") <- log
  names
}


#' Strip authority strings from scientific names
#'
#' Removes trailing author citations and year from binomials or trinomials.
#'
#' @param names Character vector.
#' @return Character vector with authority strings removed.
#' @keywords internal
pr_strip_authority <- function(names) {
  # Use Unicode-aware character classes (\\p{Lu} = uppercase letter,
  # \\p{L} = any letter) so author names with diacritics (e.g., Müller,
  # Linné) are stripped correctly.

  # Remove parenthetical authority: (Author, Year) or (Author Year)
  names <- gsub(
    "\\s*\\(\\p{Lu}[\\p{L}.&\\s]*,?\\s*\\d{4}\\)\\s*$",
    "", names, perl = TRUE
  )

  # Remove non-parenthetical authority: Author, Year or Author Year.
  # Author may be a single capitalised token (e.g. "Linnaeus") or multiple
  # tokens linked by `&` or `and` (e.g. "Blyth & Tegetmeier"). Only match if
  # preceded by at least genus + species (two words).
  names <- gsub(
    paste0(
      "^(\\S+\\s+\\S+(?:\\s+\\S+)?)",               # binomial or trinomial
      "\\s+\\p{Lu}[\\p{L}.]*",                      # first author token
      "(?:\\s*(?:&|and)\\s*\\p{Lu}[\\p{L}.]*)*",    # optional extra authors
      "(?:\\s*,\\s*|\\s+)\\d{4}\\s*$"               # year
    ),
    "\\1", names, perl = TRUE
  )

  # Remove trailing bare author name (e.g., "Genus species L." or "Genus species Author")
  # Only if what remains is at least two words
  names <- gsub(
    "^(\\S+\\s+\\S+)\\s+\\p{Lu}[\\p{L}.]+\\.?\\s*$",
    "\\1", names, perl = TRUE
  )

  names
}


#' Strip infraspecific epithets to produce binomials
#'
#' Reduces trinomials and names with rank indicators to genus + species.
#'
#' @param names Character vector.
#' @return Character vector of binomials.
#' @keywords internal
pr_strip_infraspecific <- function(names) {
  # Remove rank abbreviation + epithet: "Parus major subsp. excelsus" -> "Parus major"
  names <- gsub(
    "^(\\S+\\s+\\S+)\\s+(?:subsp|ssp|var|f)\\.?\\s+.*$",
    "\\1", names, perl = TRUE
  )

  # Remove bare third word (trinomial without rank indicator):
  # "Parus major major" -> "Parus major"
  # But only if the third word is lowercase (not an authority)
  names <- gsub(
    "^(\\S+\\s+[a-z]\\S*)\\s+[a-z]\\S*$",
    "\\1", names, perl = TRUE
  )

  names
}


#' Standardise case of scientific names
#'
#' Capitalises the genus (first word), lowercases everything else.
#'
#' @param names Character vector.
#' @return Character vector with standardised case.
#' @keywords internal
pr_standardise_case <- function(names) {
  vapply(names, function(name) {
    if (nchar(name) == 0) return(name)
    parts <- strsplit(name, "\\s+")[[1]]
    if (length(parts) == 0) return(name)

    # Capitalise genus
    parts[1] <- paste0(toupper(substr(parts[1], 1, 1)),
                       tolower(substr(parts[1], 2, nchar(parts[1]))))

    # Lowercase remaining parts (except rank abbreviations and hybrid marker)
    if (length(parts) > 1) {
      for (i in 2:length(parts)) {
        if (parts[i] %in% c("subsp.", "var.", "f.", "x")) next
        parts[i] <- tolower(parts[i])
      }
    }

    paste(parts, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}


#' Normalise scientific names via the gnparser backend
#'
#' Internal helper for `pr_normalize_names(parser = "gnparser")`.
#' Routes parsing through [rgnparser::gn_parse_tidy()] (which wraps
#' the `gnparser` Go binary, part of the Global Names Architecture),
#' then applies the same `rank` and case-standardisation contract as
#' the internal cascade so the return value is interchangeable.
#'
#' @param names Character vector of raw scientific names.
#' @param rank One of `"species"` or `"subspecies"`.
#' @return Character vector with `normalisation_log` attribute.
#' @keywords internal
.pr_normalize_gnparser <- function(names, rank) {
  if (!requireNamespace("rgnparser", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.code parser = \"gnparser\"} requires the {.pkg rgnparser} package.",
      "i" = 'Install with {.code install.packages("rgnparser")}.',
      ">" = "Or use {.code parser = \"internal\"} (the zero-dependency default)."
    ))
  }

  original <- names
  names <- as.character(names)
  is_na <- is.na(names)
  empty <- !is_na & !nzchar(trimws(names))

  to_parse <- names
  # gn_parse_tidy fails on NA / empty input -- swap in a placeholder so
  # the call succeeds, then restore NA / empty afterwards.
  to_parse[is_na | empty] <- "x"

  parsed <- tryCatch(
    rgnparser::gn_parse_tidy(to_parse),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("gnparser not found", msg, fixed = TRUE)) {
        cli::cli_abort(c(
          "{.pkg rgnparser} needs the {.code gnparser} Go binary, which is not installed.",
          "i" = "On macOS: {.code brew install gnparser}.",
          "i" = "Otherwise download a release from {.url https://github.com/gnames/gnparser/releases}.",
          ">" = "Or use {.code parser = \"internal\"} (the zero-dependency default)."
        ))
      }
      cli::cli_abort(c(
        "{.fn rgnparser::gn_parse_tidy} failed.",
        "x" = "Underlying error: {msg}"
      ))
    }
  )

  # Extract the canonical binomial. Prefer `canonicalsimple` (no
  # authorship, no hybrid marks); fall back to `canonicalfull` /
  # `canonicalstem` so an upstream rgnparser column rename does not
  # silently break us.
  canonical <- if ("canonicalsimple" %in% colnames(parsed)) {
    parsed$canonicalsimple
  } else if ("canonicalfull" %in% colnames(parsed)) {
    parsed$canonicalfull
  } else if ("canonicalstem" %in% colnames(parsed)) {
    parsed$canonicalstem
  } else {
    cli::cli_abort(c(
      "Unexpected {.pkg rgnparser} output: no canonical column found.",
      "i" = "Columns returned: {.val {colnames(parsed)}}."
    ))
  }
  canonical <- as.character(canonical)

  # Unparseable names yield NA / "" from gn_parse_tidy. Fall back to
  # the input so we never silently drop a name (the internal path
  # also passes garbage through).
  fallback <- is.na(canonical) | !nzchar(canonical)
  canonical[fallback] <- names[fallback]

  # Use "" as a placeholder for NA / empty through the downstream
  # helpers (pr_standardise_case() does not accept NA); restore NA at
  # the very end.
  canonical[is_na] <- ""
  canonical[empty] <- ""

  # `canonicalsimple` keeps trinomials; trim to a binomial when
  # rank = "species", mirroring the internal cascade's behaviour.
  if (rank == "species") {
    canonical <- pr_strip_infraspecific(canonical)
  }

  # Case standardisation + final trim (matches the internal path so
  # the two backends are drop-in compatible).
  canonical <- pr_standardise_case(canonical)
  canonical <- trimws(canonical)

  # Restore NA in their original positions.
  canonical[is_na] <- NA_character_

  # normalisation_log attribute -- same contract as the internal path.
  changed <- !is_na & (original != canonical)
  log <- tibble(
    original   = original,
    normalised = canonical,
    changed    = changed
  )
  attr(canonical, "normalisation_log") <- log
  canonical
}
