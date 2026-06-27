# The second check. After a step runs, did missingness quietly appear or grow?
# A left join NA-fills unmatched rows; a bad parse turns a whole column to NA;
# a reshape spreads holes. None of it errors. This check profiles missingness
# and (optionally) compares against a "before" snapshot to catch what a step
# introduced.

#' Diagnose surprising missingness
#'
#' Flags columns whose share of missing values is high, or -- when a `before`
#' snapshot is supplied -- whose missingness jumped after an operation.
#'
#' @param data Data frame to inspect (the "after" state).
#' @param cols Optional character vector of columns to check; defaults to all.
#' @param before Optional data frame: the same table before the operation. When
#'   given, the check scores the *increase* in missingness rather than its level.
#' @param threshold Fraction (0-1) at or above which a column is flagged.
#' @return A `tw_check` verdict (status, summary, and the underlying numbers).
#' @export
#' @examples
#' after <- data.frame(id = 1:5, region = c("E", NA, NA, "S", "W"))
#' diagnose_missing(after, threshold = 0.10)
diagnose_missing <- function(data, cols = NULL, before = NULL, threshold = 0.10) {
  stopifnot(is.data.frame(data), is.numeric(threshold), length(threshold) == 1L)
  if (is.null(cols)) cols <- names(data)
  cols <- intersect(cols, names(data))
  n <- nrow(data)

  na_frac <- vapply(cols, function(k) if (n == 0L) 0 else mean(is.na(data[[k]])), numeric(1))

  mode <- if (is.null(before)) "level" else "delta"
  if (mode == "delta") {
    stopifnot(is.data.frame(before))
    before_frac <- vapply(cols, function(k) {
      if (k %in% names(before) && nrow(before) > 0L) mean(is.na(before[[k]])) else 0
    }, numeric(1))
    score <- na_frac - before_frac
  } else {
    before_frac <- NULL
    score <- na_frac
  }

  flagged <- cols[score >= threshold]
  empty_cols <- cols[na_frac >= 0.999]

  details <- list(
    mode = mode, threshold = threshold, cols = cols,
    na_frac = na_frac, before_frac = before_frac,
    flagged = flagged, n_flagged = length(flagged)
  )

  status <-
    if (length(intersect(flagged, empty_cols))) "problem"
    else if (length(flagged)) "review"
    else "pass"

  new_tw_check("missingness", status, missing_summary(details), details)
}

# Build the one-sentence, human-readable summary.
missing_summary <- function(d) {
  pct <- function(x) sprintf("%.0f%%", 100 * x)

  if (!length(d$flagged)) {
    return(sprintf("No column is at or above the %s missing-data threshold.", pct(d$threshold)))
  }

  score <- if (d$mode == "delta") {
    d$na_frac[d$flagged] - d$before_frac[d$flagged]
  } else {
    d$na_frac[d$flagged]
  }
  fl <- d$flagged[order(score, decreasing = TRUE)]
  top <- fl[seq_len(min(3L, length(fl)))]

  render_one <- function(k) {
    if (d$mode == "delta") {
      sprintf("`%s` %s -> %s", k, pct(d$before_frac[[k]]), pct(d$na_frac[[k]]))
    } else {
      sprintf("`%s` %s", k, pct(d$na_frac[[k]]))
    }
  }
  body <- paste(vapply(top, render_one, character(1)), collapse = ", ")
  extra <- if (length(fl) > length(top)) sprintf(" (and %d more)", length(fl) - length(top)) else ""

  nft <- length(fl)
  noun <- if (nft == 1L) "column" else "columns"
  if (d$mode == "delta") {
    verb <- if (nft == 1L) "rose" else "rose"
    sprintf("Missingness %s past %s in %d %s after this step: %s%s.",
            verb, pct(d$threshold), nft, noun, body, extra)
  } else {
    verb <- if (nft == 1L) "is" else "are"
    sprintf("%d %s %s at or above %s missing: %s%s.",
            nft, noun, verb, pct(d$threshold), body, extra)
  }
}
