# Are there duplicate rows you didn't expect, or a key that's supposed to be
# unique but isn't? Duplicate keys are the seed of the next silent join fan-out,
# so catching them early is worth a lot.

#' Diagnose unexpected duplicate rows or keys
#'
#' @param data Data frame to inspect.
#' @param by Optional key columns expected to be unique. If omitted, whole-row
#'   duplicates are checked.
#' @return A `tw_check` verdict.
#' @export
#' @examples
#' d <- data.frame(id = c(1, 2, 2, 3), v = 1:4)
#' diagnose_duplicates(d, by = "id")
diagnose_duplicates <- function(data, by = NULL) {
  stopifnot(is.data.frame(data))
  n <- nrow(data)

  if (is.null(by)) {
    n_dup <- sum(duplicated(data))
    details <- list(by = NULL, n = n, n_dup = n_dup,
                    dup_frac = if (n > 0) n_dup / n else 0)
  } else {
    miss <- setdiff(by, names(data))
    if (length(miss)) {
      stop(sprintf("Key column(s) not found: %s", paste(miss, collapse = ", ")), call. = FALSE)
    }
    keys <- encode_keys(by, data, data)$x # reuse the collision-free key encoder
    tab <- table(keys)
    repeated <- tab[tab > 1L]
    n_dup <- sum(repeated) - length(repeated) # rows beyond the first per repeated key
    details <- list(by = by, n = n, n_dup = n_dup,
                    dup_frac = if (n > 0) n_dup / n else 0,
                    n_repeated_keys = length(repeated))
  }

  status <- if (n_dup > 0) "review" else "pass"
  new_tw_check("duplicates", status, duplicates_summary(details), details)
}

duplicates_summary <- function(d) {
  fmt <- function(x) formatC(x, format = "d", big.mark = ",")

  if (d$n_dup == 0) {
    if (is.null(d$by)) return("No duplicate rows.")
    return(sprintf("`%s` is unique - no repeated keys.", paste(d$by, collapse = "`, `")))
  }
  if (is.null(d$by)) {
    return(sprintf("Found %s duplicate row%s (of %s).",
                   fmt(d$n_dup), if (d$n_dup == 1L) "" else "s", fmt(d$n)))
  }
  sprintf("`%s` is not unique: %s key%s %s, %s extra row%s.",
          paste(d$by, collapse = "`, `"),
          fmt(d$n_repeated_keys), if (d$n_repeated_keys == 1L) "" else "s",
          if (d$n_repeated_keys == 1L) "repeats" else "repeat",
          fmt(d$n_dup), if (d$n_dup == 1L) "" else "s")
}
