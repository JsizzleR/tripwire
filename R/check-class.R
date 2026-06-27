# The verdict object every check returns. This small, boring structure is the
# contract the whole product rests on: a status, a plain-English summary, and
# the numbers behind it. A UI "health strip" renders this; a calibrated judge
# layer can later emit the same shape.

#' Construct a check verdict
#'
#' @param check Short machine name of the check, e.g. `"join_integrity"`.
#' @param status One of `"pass"`, `"review"`, `"problem"`.
#' @param summary One plain-English sentence a non-coder can act on.
#' @param details Named list of the underlying numbers (the audit trail).
#' @return An object of class `tw_check`.
#' @noRd
new_tw_check <- function(check, status, summary, details = list()) {
  status <- match.arg(status, c("pass", "review", "problem"))
  structure(
    list(check = check, status = status, summary = summary, details = details),
    class = "tw_check"
  )
}

#' @export
print.tw_check <- function(x, ...) {
  cat(sprintf("%s %s - %s\n", tw_glyph(x$status), gsub("_", " ", x$check), x$summary))
  invisible(x)
}

# Internal helpers -----------------------------------------------------------

# Capitalize the first letter (for human-readable summaries).
cap <- function(s) {
  substr(s, 1, 1) <- toupper(substr(s, 1, 1))
  s
}

# Render a verdict as a one-line warning string.
format_check_warning <- function(chk) {
  sprintf("[%s] %s", gsub("_", " ", chk$check), chk$summary)
}
