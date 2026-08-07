# Taxonomic authority lookup -----------------------------------------------

# Session-level cache for synonym lookups
.pr_cache <- new.env(parent = emptyenv())

#' Valid taxonomic authorities
#'
#' Returns the set of authority codes that the package accepts when
#' resolving species-name synonyms. Most are served by \pkg{taxadb}
#' (a local database mirroring the providers documented in
#' `?taxadb::td_create`); `"gnverifier"` is the one HTTP-backed
#' authority, calling the Global Names Architecture
#' [verifier service](https://verifier.globalnames.org/) instead of a
#' local database.
#'
#' \describe{
#'   \item{`"col"`}{Catalogue of Life. The default and a sensible
#'     starting point for most taxa.}
#'   \item{`"itis"`}{Integrated Taxonomic Information System. Strong
#'     coverage for North American vertebrates and plants.}
#'   \item{`"gbif"`}{GBIF backbone. Wider coverage; captures more
#'     recent synonymy.}
#'   \item{`"ncbi"`}{NCBI Taxonomy. Best when you are working with
#'     sequence data.}
#'   \item{`"ott"`}{Open Tree of Life synthetic taxonomy. Useful when
#'     your downstream phylogeny is from the Open Tree synthesis. We
#'     restrict the schema to `"dwc"` (Darwin Core) when calling
#'     `taxadb::td_create()` because the `"common"` schema does not
#'     ship for OTT under \pkg{taxadb} v22.12.}
#'   \item{`"itis_test"`}{A small bundled subset of ITIS, cached
#'     locally with \pkg{taxadb} for testing. Intended for examples
#'     and unit tests; not for analysis.}
#'   \item{`"gnverifier"`}{Global Names verifier --- HTTP-backed
#'     verification against ~100 authoritative sources (Catalogue of
#'     Life, ITIS, GBIF, NCBI, Open Tree, ...). No local database is
#'     downloaded; requires network access and the \pkg{httr2} package.
#'     Useful when you want broader source coverage than any single
#'     taxadb provider, or want to avoid the ~100 MB taxadb download.}
#' }
#'
#' Five authority codes that previous versions of the package
#' advertised --- `iucn`, `tpl`, `fb`, `slb`, `wd` --- are not on this
#' list. Empirical testing against \pkg{taxadb} v22.12 showed that
#' `iucn` errors with a schema mismatch and the other four are not
#' \pkg{taxadb} providers at all. Anyone who was passing one of those
#' values was getting a hard error; passing them now produces a
#' helpful migration message instead.
#'
#' @keywords internal
pr_valid_authorities <- function() {
  c("col", "itis", "gbif", "ncbi", "ott", "itis_test", "gnverifier")
}

# Authorities that earlier versions of the package incorrectly listed
# as supported. We keep the set so that we can produce a targeted
# migration error if a user passes one of these (rather than the
# generic "not a valid authority" message).
.pr_removed_authorities <- function() {
  c("iucn", "tpl", "fb", "slb", "wd")
}


#' Validate a user-supplied authority string
#'
#' Used by every entry-point function that accepts `authority`.
#' Lower-cases the input, returns it unchanged if `NULL` (synonym
#' resolution skipped), errors with a helpful message if the value
#' was previously listed but is no longer supported, or with a
#' standard "unknown authority" message otherwise.
#'
#' @param authority A length-1 character vector or NULL. The user-supplied value.
#' @param call Calling environment, for `cli_abort(call = ...)`.
#' @return The lower-cased, validated authority (or NULL).
#' @keywords internal
pr_validate_authority <- function(authority, call = caller_env()) {
  if (is.null(authority)) return(NULL)
  authority <- tolower(authority)

  if (authority %in% pr_valid_authorities()) {
    return(authority)
  }

  if (authority %in% .pr_removed_authorities()) {
    removed <- .pr_removed_authorities()
    valid <- pr_valid_authorities()
    cli::cli_abort(
      c(
        "{.val {authority}} is not a supported authority.",
        "x" = paste0(
          "{.val {authority}} was listed in earlier versions of the ",
          "package but is not actually supported by {.pkg taxadb} ",
          "v22.12 (the database we test against)."
        ),
        "i" = "Removed authorities: {.val {removed}}.",
        ">" = "Switch to one of: {.val {valid}}.",
        ">" = "Or pass {.code authority = NULL} to skip synonym resolution."
      ),
      call = call
    )
  }

  cli::cli_abort(
    c(
      "Unknown authority: {.val {authority}}.",
      "i" = "Valid options: {.val {pr_valid_authorities()}}."
    ),
    call = call
  )
}

#' Ensure the taxadb local database is available
#'
#' Downloads the database for the specified authority if not already cached.
#'
#' @param authority A length-1 character vector. Taxonomic authority code.
#' @param db_version A length-1 character vector or NULL. Database version.
#'
#' @return Invisibly returns the authority string.
#' @keywords internal
pr_ensure_db <- function(authority, db_version = NULL) {
  if (!requireNamespace("taxadb", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Synonym resolution requires the {.pkg taxadb} package.",
        "i" = 'Install with: {.code install.packages("taxadb")}'
      ),
      call = caller_env()
    )
  }

  # We restrict to the Darwin Core ("dwc") schema rather than letting
  # taxadb default to schema = c("dwc", "common"). The cascade only
  # consumes scientific names (dwc); the `common` schema (vernacular
  # names) is unused. More importantly, the "common" schema does not
  # ship for OTT under taxadb v22.12, so the default would error on
  # otherwise-valid OTT calls. Restricting to "dwc" keeps every
  # authority on pr_valid_authorities() callable.
  args <- list(provider = authority, schema = "dwc")
  if (!is.null(db_version)) args$version <- db_version

  tryCatch(
    {
      cli_alert_info(
        "Ensuring local {.val {toupper(authority)}} database is available..."
      )
      do.call(taxadb::td_create, args)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to create/access taxadb database.",
          "x" = conditionMessage(e),
          "i" = 'Try running {.code taxadb::td_create("{authority}", schema = "dwc")} manually.'
        ),
        call = caller_env()
      )
    }
  )

  invisible(authority)
}


#' Look up names in a taxonomic authority
#'
#' For each name, queries the configured authority and returns the accepted
#' name, taxonomic status, and taxon ID. Most authorities are backed by a
#' local \pkg{taxadb} database; `authority = "gnverifier"` calls the
#' Global Names HTTP verifier instead.
#'
#' @param names Character vector of scientific names.
#' @param authority A length-1 character vector. Authority code (e.g.,
#'   `"col"`). Pass `"gnverifier"` for HTTP-backed verification against
#'   ~100 sources; see `vignette("getting-started")` for the trade-off.
#' @param db_version A length-1 character vector or NULL. Ignored when
#'   `authority = "gnverifier"` (the GNverifier service does not expose
#'   per-snapshot versions); a non-NULL value emits a single warning.
#'
#' @return A tibble with columns: `input`, `accepted_name`, `status`,
#'   `taxon_id`, `authority`.
#' @keywords internal
pr_lookup_authority <- function(names, authority = "col", db_version = NULL) {
  if (authority != "gnverifier") {
    pr_ensure_db(authority, db_version)
  }

  unique_names <- unique(names[!is.na(names)])

  if (length(unique_names) == 0) {
    return(tibble(
      input         = character(),
      accepted_name = character(),
      status        = character(),
      taxon_id      = character(),
      authority     = character()
    ))
  }

  # Check session cache
  cache_key <- paste0("lookup_", authority, "_",
                       db_version %||% "latest")
  cached <- if (exists(cache_key, envir = .pr_cache)) {
    get(cache_key, envir = .pr_cache)
  } else {
    NULL
  }

  # Identify which names need lookup
  if (!is.null(cached)) {
    already_done <- intersect(unique_names, cached$input)
    to_lookup <- setdiff(unique_names, cached$input)
  } else {
    already_done <- character()
    to_lookup <- unique_names
  }

  if (length(to_lookup) > 0) {
    if (authority == "gnverifier") {
      new_results_df <- .pr_lookup_gnverifier(to_lookup, db_version)
    } else {
      new_results_df <- .pr_lookup_taxadb(to_lookup, authority, db_version)
    }

    # Update cache
    if (!is.null(cached)) {
      assign(cache_key, rbind(cached, new_results_df), envir = .pr_cache)
    } else {
      assign(cache_key, new_results_df, envir = .pr_cache)
    }
  }

  # Return results for all requested names
  full_cache <- get(cache_key, envir = .pr_cache)
  full_cache[full_cache$input %in% unique_names, ]
}


#' Look up names in a taxadb-backed authority
#'
#' Internal helper extracted from `pr_lookup_authority()` so the
#' taxadb path can sit alongside the gnverifier path without
#' duplicating the cache machinery in `pr_lookup_authority()`.
#'
#' @param to_lookup Character vector of names to look up.
#' @param authority A length-1 character vector. Authority code.
#' @param db_version A length-1 character vector or NULL.
#' @return A tibble with the same 5 columns as `pr_lookup_authority()`.
#' @keywords internal
.pr_lookup_taxadb <- function(to_lookup, authority, db_version = NULL) {
    # Batch query: taxadb::filter_name accepts a character vector.
    # Only forward `version` when the caller supplied one — taxadb 0.2.1
    # errors on `version = NULL` because parse_schema() runs
    # `if (version == "latest")` and gets logical(0). Omitting the
    # argument lets taxadb fall back to latest_version().
    filter_args <- list(to_lookup, provider = authority)
    if (!is.null(db_version)) filter_args$version <- db_version
    all_hits <- tryCatch(
      do.call(taxadb::filter_name, filter_args),
      error = function(e) {
        cli_alert_warning(
          "taxadb lookup failed for {.val {authority}}: {conditionMessage(e)}. Names will be recorded as not found."
        )
        NULL
      }
    )

    # Process results per name
    new_results <- lapply(to_lookup, function(name) {
      if (is.null(all_hits) || nrow(all_hits) == 0) {
        return(tibble(
          input         = name,
          accepted_name = NA_character_,
          status        = "not_found",
          taxon_id      = NA_character_,
          authority     = authority
        ))
      }

      # Match on scientificName, and on `input` only if taxadb provides
      # that column. taxadb 0.2.1 returns scientificName but not input,
      # so unconditionally referencing all_hits$input returns NULL and
      # `NULL == name` yields logical(0), which then collides with the
      # length-N scientificName mask under tibble's strict subscripting.
      match_mask <- all_hits$scientificName == name
      if ("input" %in% names(all_hits)) {
        match_mask <- match_mask | all_hits$input == name
      }
      hits <- all_hits[match_mask, , drop = FALSE]

      if (nrow(hits) == 0) {
        return(tibble(
          input         = name,
          accepted_name = NA_character_,
          status        = "not_found",
          taxon_id      = NA_character_,
          authority     = authority
        ))
      }

      # Prefer accepted name
      accepted <- hits[hits$taxonomicStatus == "accepted", ]

      if (nrow(accepted) > 0) {
        return(tibble(
          input         = name,
          accepted_name = accepted$scientificName[1],
          status        = "accepted",
          taxon_id      = accepted$taxonID[1],
          authority     = authority
        ))
      }

      # Name is a synonym — follow acceptedNameUsageID
      syn <- hits[1, ]
      accepted_id <- syn$acceptedNameUsageID

      if (!is.na(accepted_id) && nchar(accepted_id) > 0) {
        tryCatch({
          id_args <- list(accepted_id, provider = authority)
          if (!is.null(db_version)) id_args$version <- db_version
          accepted_hit <- do.call(taxadb::filter_id, id_args)
          if (!is.null(accepted_hit) && nrow(accepted_hit) > 0) {
            return(tibble(
              input         = name,
              accepted_name = accepted_hit$scientificName[1],
              status        = syn$taxonomicStatus[1] %||% "synonym",
              taxon_id      = accepted_id,
              authority     = authority
            ))
          }
        }, error = function(e) NULL)
      }

      # Could not resolve
      tibble(
        input         = name,
        accepted_name = NA_character_,
        status        = syn$taxonomicStatus[1] %||% "unknown",
        taxon_id      = syn$taxonID[1],
        authority     = authority
      )
    })

    do.call(rbind, new_results)
}


#' Look up names via the Global Names verifier (HTTP)
#'
#' Internal helper for `pr_lookup_authority(authority = "gnverifier")`.
#' POSTs the input vector to the
#' [Global Names verifier](https://verifier.globalnames.org/) and maps
#' each `bestResult` back to the 5-column tibble contract used by the
#' taxadb path. Returns all-rows-`not_found` and emits a single
#' warning on network failure, mirroring the taxadb branch's
#' degradation behaviour so the cascade above keeps running.
#'
#' @param names Character vector of names to verify.
#' @param db_version Ignored; emits a single warning if non-NULL.
#' @return A tibble with the same 5 columns as `pr_lookup_authority()`.
#' @keywords internal
.pr_lookup_gnverifier <- function(names, db_version = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.code authority = \"gnverifier\"} requires the {.pkg httr2} package.",
      "i" = 'Install with {.code install.packages("httr2")}.',
      ">" = "Or pick a taxadb-backed authority (e.g. {.val col})."
    ))
  }

  if (!is.null(db_version)) {
    cli::cli_warn(c(
      "{.arg db_version} is ignored when {.code authority = \"gnverifier\"}.",
      "i" = "The Global Names verifier does not expose per-snapshot versions."
    ))
  }

  not_found <- function(n) {
    tibble(
      input         = n,
      accepted_name = NA_character_,
      status        = rep("not_found", length(n)),
      taxon_id      = NA_character_,
      authority     = rep("gnverifier", length(n))
    )
  }

  body <- list(nameStrings = as.list(names))

  resp <- tryCatch(
    {
      req <- httr2::request("https://verifier.globalnames.org/api/v1/verifications")
      req <- httr2::req_body_json(req, body)
      req <- httr2::req_user_agent(
        req,
        "prepR4pcm (https://github.com/itchyshin/prepR4pcm)"
      )
      httr2::req_perform(req)
    },
    error = function(e) e
  )

  if (inherits(resp, "error")) {
    cli::cli_warn(c(
      "GNverifier lookup failed: {conditionMessage(resp)}.",
      "i" = "Names will be recorded as not found; the rest of the cascade still runs."
    ))
    return(not_found(names))
  }

  parsed <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) e
  )

  if (inherits(parsed, "error")) {
    cli::cli_warn(c(
      "GNverifier returned an unparseable response: {conditionMessage(parsed)}.",
      "i" = "Names will be recorded as not found."
    ))
    return(not_found(names))
  }

  # The verifier returns `{"names": [...]}` (since v1). Older deployments
  # used `namesList`. Accept either so a transparent server update does
  # not break us.
  results <- parsed[["names"]] %||% parsed[["namesList"]]
  if (is.null(results) || length(results) != length(names)) {
    cli::cli_warn(c(
      "GNverifier response shape did not match the request.",
      "i" = "Names will be recorded as not found."
    ))
    return(not_found(names))
  }

  rows <- lapply(seq_along(names), function(i) {
    entry <- results[[i]]
    best  <- entry[["bestResult"]]
    if (is.null(best)) {
      return(tibble(
        input         = names[[i]],
        accepted_name = NA_character_,
        status        = "not_found",
        taxon_id      = NA_character_,
        authority     = "gnverifier"
      ))
    }

    accepted <- best[["currentCanonicalSimple"]] %||%
                best[["currentCanonicalFull"]]   %||%
                best[["matchedCanonicalSimple"]] %||%
                NA_character_
    accepted <- if (length(accepted) && nzchar(accepted)) accepted else NA_character_

    match_type <- best[["matchType"]] %||% ""
    current    <- best[["currentName"]] %||% best[["currentCanonicalSimple"]] %||% ""
    matched    <- best[["matchedName"]] %||% best[["matchedCanonicalSimple"]] %||% ""

    status <- if (identical(match_type, "Exact") &&
                  nzchar(current) && nzchar(matched) &&
                  identical(current, matched)) {
      "accepted"
    } else {
      "synonym"
    }

    tibble(
      input         = names[[i]],
      accepted_name = as.character(accepted),
      status        = status,
      taxon_id      = as.character(best[["recordId"]] %||%
                                   best[["taxonId"]]  %||%
                                   NA_character_),
      authority     = "gnverifier"
    )
  })

  do.call(rbind, rows)
}
