# A health report bundles several check verdicts and renders them as a compact
# "health strip": one overall line, then one row per check. This is the UI block
# from the design sketch -- in the terminal (colour via cli when available) and
# as an HTML fragment for Quarto / R Markdown.
#
# Glyphs are built with intToUtf8() rather than written literally, so the source
# stays pure ASCII (portable for R CMD check) while still rendering the marks.

#' Bundle check verdicts into a health report
#'
#' @param ... `tw_check` objects (from the `diagnose_*()` functions), or a single
#'   list of them.
#' @return A `tw_report` object with a print method that draws the health strip.
#' @export
#' @examples
#' x <- data.frame(id = 1:10, a = 1:10)
#' y <- data.frame(id = 1:8, b = 1:8)
#' joined <- merge(x, y, by = "id", all.x = TRUE)
#' tw_report(
#'   diagnose_join(x, y, by = "id", type = "left"),
#'   diagnose_missing(joined, threshold = 0.10)
#' )
tw_report <- function(...) {
  checks <- list(...)
  if (length(checks) == 1L && is.list(checks[[1]]) && !inherits(checks[[1]], "tw_check")) {
    checks <- checks[[1]]
  }
  if (!all(vapply(checks, inherits, logical(1), "tw_check"))) {
    stop("All inputs must be `tw_check` objects (from the diagnose_*() functions).", call. = FALSE)
  }
  structure(list(checks = checks), class = "tw_report")
}

#' @export
print.tw_report <- function(x, details = FALSE, ...) {
  checks <- x$checks
  if (!length(checks)) {
    cat("(no checks)\n")
    return(invisible(x))
  }

  st <- vapply(checks, function(c) c$status, character(1))
  overall <- report_status(checks)
  dot <- paste0("  ", intToUtf8(183), "  ") # middle dot separator

  counts <- sprintf("%d passed", sum(st == "pass"))
  if (any(st == "review")) counts <- c(counts, sprintf("%d to review", sum(st == "review")))
  if (any(st == "problem")) {
    np <- sum(st == "problem")
    counts <- c(counts, sprintf("%d problem%s", np, if (np == 1L) "" else "s"))
  }
  header <- tw_style(overall, paste(tw_glyph(overall), tw_label(overall)))
  cat(paste0(header, dot, paste(counts, collapse = dot), "\n"))

  labels <- gsub("_", " ", vapply(checks, function(c) c$check, character(1)))
  w <- max(nchar(labels))
  for (i in seq_along(checks)) {
    cat(sprintf("  %s %-*s  %s\n", tw_style(st[i], tw_glyph(st[i])), w, labels[i], checks[[i]]$summary))
    if (isTRUE(details)) render_details(checks[[i]])
  }
  invisible(x)
}

# Worst-of status across a set of checks.
report_status <- function(checks) {
  st <- vapply(checks, function(c) c$status, character(1))
  if ("problem" %in% st) "problem" else if ("review" %in% st) "review" else "pass"
}

# Status glyphs: tick (U+2713), warning (U+26A0), cross (U+2717).
tw_glyph <- function(status) {
  intToUtf8(switch(status, pass = 10003L, review = 9888L, problem = 10007L))
}

tw_label <- function(status) switch(status, pass = "Checked", review = "Review", problem = "Problem")

# Colour text by status when the cli package is available; otherwise plain text.
tw_style <- function(status, text) {
  if (!requireNamespace("cli", quietly = TRUE)) return(text)
  switch(status,
    pass = cli::col_green(text),
    review = cli::col_yellow(text),
    problem = cli::col_red(text),
    text
  )
}

# The "expanded" view: the scalar numbers behind a verdict (the audit trail).
render_details <- function(chk) {
  d <- chk$details
  keep <- vapply(d, function(v) {
    length(v) == 1L && (is.numeric(v) || is.logical(v) || is.character(v))
  }, logical(1))
  for (k in names(d)[keep]) {
    cat(sprintf("       - %s: %s\n", k, format(d[[k]])))
  }
  invisible()
}

#' Render a health report as an HTML fragment
#'
#' Returns a self-contained HTML string (inline styles, no dependencies) suitable
#' for a Quarto / R Markdown chunk with `results = "asis"`.
#'
#' @param report A `tw_report`.
#' @return A length-one character string of HTML.
#' @export
tw_html <- function(report) {
  stopifnot(inherits(report, "tw_report"))

  fg <- c(pass = "#1a7f37", review = "#9a6700", problem = "#cf222e")
  bg <- c(pass = "#dafbe1", review = "#fff8c5", problem = "#ffebe9")
  glyph <- c(pass = "&#10003;", review = "&#9888;", problem = "&#10007;")

  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    gsub("`([^`]+)`", "<code>\\1</code>", s) # render `code` spans
  }

  rows <- vapply(report$checks, function(c) {
    sprintf(
      "<div style=\"padding:5px 12px;border-left:4px solid %s\"><span style=\"color:%s\">%s</span> <strong>%s</strong> &mdash; %s</div>",
      fg[[c$status]], fg[[c$status]], glyph[[c$status]],
      esc(gsub("_", " ", c$check)), esc(c$summary)
    )
  }, character(1))

  overall <- report_status(report$checks)
  header <- sprintf(
    "<div style=\"background:%s;color:%s;padding:7px 12px;font-weight:600\">%s %s</div>",
    bg[[overall]], fg[[overall]], glyph[[overall]], tw_label(overall)
  )

  sprintf(
    "<div style=\"font-family:system-ui,sans-serif;border:1px solid #d0d7de;border-radius:6px;overflow:hidden;max-width:680px\">%s%s</div>",
    header, paste(rows, collapse = "")
  )
}
