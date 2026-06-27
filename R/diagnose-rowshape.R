# Did an operation change the row count in a way you didn't intend? A filter that
# was meant to trim a few outliers but removed most of the data; a step that
# quietly grew the table (an accidental fan-out). This compares before/after row
# counts and flags the surprises.

#' Diagnose a surprising change in row count
#'
#' @param before,after Data frames (or single row-count numbers) for the state
#'   before and after the step.
#' @param max_drop Fraction (0-1); dropping at least this share triggers a review.
#' @return A `tw_check` verdict.
#' @export
#' @examples
#' before <- data.frame(x = 1:100)
#' after <- subset(before, x > 80) # keeps only 20
#' diagnose_rowshape(before, after)
diagnose_rowshape <- function(before, after, max_drop = 0.5) {
  n0 <- if (is.data.frame(before)) nrow(before) else as.integer(before)
  n1 <- if (is.data.frame(after)) nrow(after) else as.integer(after)
  stopifnot(length(n0) == 1L, length(n1) == 1L, !is.na(n0), !is.na(n1))

  frac_drop <- if (n0 > 0) (n0 - n1) / n0 else 0
  details <- list(n_before = n0, n_after = n1, delta = n1 - n0,
                  frac_drop = frac_drop, max_drop = max_drop)

  status <-
    if (n0 > 0 && n1 == 0) "problem"
    else if (n1 > n0 || frac_drop >= max_drop) "review"
    else "pass"

  new_tw_check("row_shape", status, rowshape_summary(details), details)
}

rowshape_summary <- function(d) {
  fmt <- function(n) formatC(n, format = "d", big.mark = ",")

  if (d$n_before > 0 && d$n_after == 0) {
    return(sprintf("The step removed every row (0 of %s remain).", fmt(d$n_before)))
  }
  if (d$n_after > d$n_before) {
    if (d$n_before == 0) {
      return(sprintf("Row count grew from 0 to %s rows - did the step add rows unexpectedly?",
                     fmt(d$n_after)))
    }
    return(sprintf("Row count grew %s -> %s (%.1fx) - did the step add or duplicate rows?",
                   fmt(d$n_before), fmt(d$n_after), d$n_after / d$n_before))
  }
  if (d$frac_drop >= d$max_drop) {
    return(sprintf("The step dropped %s of %s rows (%.0f%%).",
                   fmt(d$n_before - d$n_after), fmt(d$n_before), 100 * d$frac_drop))
  }
  sprintf("Row count %s -> %s, within the expected range.", fmt(d$n_before), fmt(d$n_after))
}
