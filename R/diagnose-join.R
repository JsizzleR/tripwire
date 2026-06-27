# The first check. A join is the single most common place a data analysis
# silently goes wrong: rows quietly disappear (keys with no match) or quietly
# multiply (duplicate keys fan out). No error is ever raised. This function
# notices, and explains what happened in one sentence.

#' Diagnose a join for silent row loss or fan-out
#'
#' Analyses what a join between `x` and `y` on `by` *would* do, without you
#' having to eyeball row counts. It reports rows that have no match (and would
#' be dropped or NA-filled) and rows that would multiply because a key is
#' duplicated on the other side.
#'
#' @param x,y Data frames to be joined.
#' @param by Character vector of one or more shared key column names.
#' @param type Join type: `"inner"`, `"left"`, `"right"`, or `"full"`.
#' @return A `tw_check` verdict (status, summary, and the underlying numbers).
#' @export
#' @examples
#' x <- data.frame(id = 1:10, a = 1:10)
#' y <- data.frame(id = 1:8, b = 1:8) # ids 9 and 10 have no match
#' diagnose_join(x, y, by = "id", type = "inner")
diagnose_join <- function(x, y, by, type = c("inner", "left", "right", "full")) {
  type <- match.arg(type)
  stopifnot(is.data.frame(x), is.data.frame(y))
  if (!is.character(by) || length(by) < 1L) {
    stop("`by` must be a character vector of one or more key column names.", call. = FALSE)
  }
  miss <- unique(c(setdiff(by, names(x)), setdiff(by, names(y))))
  if (length(miss)) {
    stop(sprintf("Key column(s) not found in both tables: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }

  keys <- encode_keys(by, x, y)
  kx <- keys$x
  ky <- keys$y

  n_x <- nrow(x)
  n_y <- nrow(y)
  ty <- table(ky) # occurrences of each key in y
  tx <- table(kx) # occurrences of each key in x

  mult_x <- as.integer(ty[kx]); mult_x[is.na(mult_x)] <- 0L # y-matches per x row
  mult_y <- as.integer(tx[ky]); mult_y[is.na(mult_y)] <- 0L # x-matches per y row

  n_x_unmatched <- sum(mult_x == 0L)
  n_y_unmatched <- sum(mult_y == 0L)
  n_matched_x <- n_x - n_x_unmatched

  # Realized output-row count, by join type.
  n_out <- switch(type,
    inner = sum(mult_x),
    left  = sum(pmax(mult_x, 1L)),
    right = sum(pmax(mult_y, 1L)),
    full  = sum(pmax(mult_x, 1L)) + n_y_unmatched
  )

  # The row count if every match were 1:1 (no fan-out). n_out above this means
  # rows multiplied.
  baseline <- switch(type,
    inner = n_matched_x,
    left  = n_x,
    right = n_y,
    full  = n_x + n_y_unmatched
  )

  details <- list(
    type = type, by = by,
    n_x = n_x, n_y = n_y, n_out = n_out, baseline = baseline,
    n_x_unmatched = n_x_unmatched, n_y_unmatched = n_y_unmatched,
    fanout = n_out > baseline,
    dup_keys_x = any(tx > 1L), dup_keys_y = any(ty > 1L)
  )

  new_tw_check(
    "join_integrity",
    join_status(type, details),
    join_summary(type, details),
    details
  )
}

# Encode (possibly multi-column) keys as collision-free strings. Each key column
# is mapped to integer codes against the union of its values across both tables,
# then columns are combined in mixed radix. There is no in-band separator, so
# distinct keys can never collide -- which matters for a tool whose whole job is
# catching silent failures. NA is treated as its own matchable level.
encode_keys <- function(by, x, y) {
  code_x <- numeric(nrow(x))
  code_y <- numeric(nrow(y))
  radix <- 1
  for (k in by) {
    levels_k <- unique(c(as.character(x[[k]]), as.character(y[[k]])))
    ix <- match(as.character(x[[k]]), levels_k) - 1L
    iy <- match(as.character(y[[k]]), levels_k) - 1L
    code_x <- code_x + ix * radix
    code_y <- code_y + iy * radix
    radix <- radix * length(levels_k)
  }
  list(x = as.character(code_x), y = as.character(code_y))
}

# Decide pass / review / problem from the numbers.
join_status <- function(type, d) {
  # Both sides had rows but nothing matched: almost always a real bug.
  if (d$n_x > 0 && d$n_y > 0 && d$n_out == 0) return("problem")

  drops <- switch(type,
    inner = d$n_x_unmatched > 0 || d$n_y_unmatched > 0,
    left  = d$n_x_unmatched > 0,
    right = d$n_y_unmatched > 0,
    full  = FALSE
  )
  if (isTRUE(d$fanout) || isTRUE(drops)) return("review")
  "pass"
}

# Build the one-sentence, human-readable summary.
join_summary <- function(type, d) {
  by_lab <- paste0("`", paste(d$by, collapse = "`, `"), "`")

  if (d$n_x > 0 && d$n_y > 0 && d$n_out == 0) {
    return(sprintf("%s join on %s produced 0 rows - no keys matched between the two tables.",
                   cap(type), by_lab))
  }

  parts <- character(0)

  if (isTRUE(d$fanout)) {
    parts <- c(parts, sprintf("multiplied rows %d -> %d (%.1fx) from duplicate keys",
                              d$baseline, d$n_out, d$n_out / max(d$baseline, 1L)))
  }

  dropped <- switch(type,
    inner = d$n_x_unmatched, left = d$n_x_unmatched,
    right = d$n_y_unmatched, full = 0L
  )
  if (dropped > 0) {
    base_n <- if (type == "right") d$n_y else d$n_x
    pct <- 100 * dropped / base_n
    if (type == "left") {
      parts <- c(parts, sprintf("%d of %d left rows had no match and were NA-filled (%.0f%%)",
                                dropped, base_n, pct))
    } else {
      side <- if (type == "right") "right" else "left"
      parts <- c(parts, sprintf("dropped %d of %d %s rows with no match (%.0f%%)",
                                dropped, base_n, side, pct))
    }
  }

  if (!length(parts)) {
    return(sprintf("%s join on %s is clean: %d rows in, %d out, every key matched 1:1.",
                   cap(type), by_lab, d$n_x, d$n_out))
  }
  sprintf("%s join on %s %s.", cap(type), by_lab, paste(parts, collapse = "; and "))
}
