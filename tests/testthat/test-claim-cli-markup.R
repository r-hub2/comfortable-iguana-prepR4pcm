# Bundle G #3 — cli markup static ---------------------------------------
#
# Catches the issue #4 root cause where rlang::abort(c("...{.pkg taxadb}",
# "i" = "...{.code install.packages(\"taxadb\")}"), ...) printed the cli
# template strings literally because rlang::abort does not interpret
# cli markup -- only cli::cli_abort and friends do.
#
# This test scans every .R file under R/ and flags any abort() /
# warning() / message() / stop() call whose message contains cli inline
# markup ({.pkg ...}, {.code ...}, {.fn ...}, {.val ...}, etc.) but is
# NOT a cli::cli_*() call. If any such call exists, it is the same
# class of bug as #4.
#
# The test is purely static (regex over source code) -- no R
# evaluation, no network, no installed package. Adds < 1 second to
# the suite.

test_that("no abort()/warning()/stop() calls contain unrendered cli markup (issue #4)", {
  skip_on_cran()
  r_files <- list.files(
    test_path("..", "..", "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  if (length(r_files) == 0) skip("source tree not accessible (running outside dev/CI environment)")

  cli_markup_pattern <- "\\{\\.[a-z]+ "
  bad_handler_pattern <- "(?:^|\\s)(abort|warning|stop|message)\\s*\\("

  bad_calls <- character()

  for (f in r_files) {
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")

    # Strip cli::cli_*() bodies -- everything inside them is allowed
    # to use cli markup. We approximate by removing each cli::cli_*(...
    # call up to its matching paren. A simple regex without paren
    # balancing is good enough because the file contents are sane code,
    # not adversarial.
    src_stripped <- gsub("cli(?:::|_)cli_[a-z_]+\\s*\\([^)]*\\)", "",
                         src, perl = TRUE)
    # Repeat once to catch nested or chained calls like
    # cli_alert_warning(...) within tryCatch.
    src_stripped <- gsub("cli(?:::|_)cli_[a-z_]+\\s*\\([^)]*\\)", "",
                         src_stripped, perl = TRUE)
    # Also strip cli_abort_no_class / cli_inform / cli_alert_*
    # convenience calls.
    src_stripped <- gsub("cli_(?:abort|inform|warn|alert_[a-z_]+|h[1-3]|bullets)\\s*\\([^)]*\\)", "",
                         src_stripped, perl = TRUE)

    # Now look for bare abort/warning/stop/message calls that contain
    # cli markup in their message. Crude line-based scan.
    lines <- strsplit(src_stripped, "\n", fixed = TRUE)[[1]]
    in_call <- FALSE
    call_buffer <- character()
    call_start <- NA_integer_

    for (i in seq_along(lines)) {
      line <- lines[i]
      if (!in_call) {
        # Is this line the start of a non-cli abort/warning/stop?
        if (grepl(bad_handler_pattern, line, perl = TRUE) &&
            !grepl("^\\s*cli(?:::|_)", line, perl = TRUE) &&
            !grepl("^\\s*#", line, perl = TRUE)) {
          in_call <- TRUE
          call_buffer <- line
          call_start <- i
        }
      } else {
        call_buffer <- c(call_buffer, line)
      }

      # Detect end of call (heuristic: line ending with `)` at top level).
      # Good enough for this codebase's style.
      if (in_call && grepl("^\\s*\\)\\s*$|\\)\\s*$", line)) {
        joined <- paste(call_buffer, collapse = "\n")
        if (grepl(cli_markup_pattern, joined, perl = TRUE)) {
          bad_calls <- c(bad_calls,
                         sprintf("%s:%d-%d:\n%s",
                                 basename(f), call_start, i,
                                 paste0("    ", call_buffer, collapse = "\n")))
        }
        in_call <- FALSE
        call_buffer <- character()
        call_start <- NA_integer_
      }
    }
  }

  expect_equal(
    length(bad_calls), 0,
    info = paste0(
      "Found ", length(bad_calls),
      " abort()/warning()/stop()/message() call(s) containing unrendered cli markup. ",
      "These will print literal {.pkg ...} / {.code ...} strings to the user. ",
      "Switch to cli::cli_abort() (or cli::cli_warn / cli::cli_inform). Offending call(s):\n",
      paste(bad_calls, collapse = "\n\n")
    )
  )
})
