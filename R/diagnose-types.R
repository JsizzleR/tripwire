# Did a column silently change type? The classic: a numeric column becomes text
# after a bind or a bad parse, so every downstream sum and mean is quietly wrong
# (or errors much later, far from the cause). This compares column classes before
# and after a step.

#' Diagnose silent column type changes
#'
#' Flags columns whose class changed across a step. A numeric column that became
#' text or a factor is treated as a problem (it breaks arithmetic); other changes
#' are flagged for review.
#'
#' @param before,after Data frames for the state before and after the step.
#' @param cols Optional columns to check; defaults to those shared by both.
#' @return A `tw_check` verdict.
#' @export
#' @examples
#' before <- data.frame(id = 1:3, amount = c(1.5, 2.0, 3.5))
#' after <- transform(before, amount = as.character(amount))
#' diagnose_types(before, after)
diagnose_types <- function(before, after, cols = NULL) {
  stopifnot(is.data.frame(before), is.data.frame(after))
  shared <- intersect(names(before), names(after))
  cols <- if (is.null(cols)) shared else intersect(cols, shared)

  cls <- function(d, k) paste(class(d[[k]]), collapse = "/")
  before_class <- vapply(cols, function(k) cls(before, k), character(1))
  after_class <- vapply(cols, function(k) cls(after, k), character(1))

  changed <- cols[before_class != after_class]
  numericish <- c("numeric", "integer", "double")
  dangerous <- changed[
    before_class[changed] %in% numericish &
      after_class[changed] %in% c("character", "factor")
  ]

  details <- list(cols = cols, before_class = before_class, after_class = after_class,
                  changed = changed, dangerous = dangerous, n_changed = length(changed))

  status <-
    if (length(dangerous)) "problem"
    else if (length(changed)) "review"
    else "pass"

  new_tw_check("type_stability", status, types_summary(details), details)
}

types_summary <- function(d) {
  if (!length(d$changed)) {
    return("No column changed type.")
  }
  top <- d$changed[seq_len(min(3L, length(d$changed)))]
  render_one <- function(k) sprintf("`%s` %s -> %s", k, d$before_class[[k]], d$after_class[[k]])
  body <- paste(vapply(top, render_one, character(1)), collapse = ", ")
  extra <- if (length(d$changed) > length(top)) {
    sprintf(" (and %d more)", length(d$changed) - length(top))
  } else {
    ""
  }
  n <- length(d$changed)
  noun <- if (n == 1L) "column" else "columns"
  sprintf("%d %s changed type: %s%s.", n, noun, body, extra)
}
