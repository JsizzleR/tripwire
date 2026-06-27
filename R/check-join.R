# Human-facing convenience: do the join AND watch it in one call. The diagnostic
# verdict is attached to the result as an attribute, and a non-pass verdict is
# surfaced as a warning so it can't pass by unnoticed.

#' Join two tables and check the join in one step
#'
#' A drop-in for a base `merge()` that also runs [diagnose_join()] and warns you
#' when the join silently dropped or multiplied rows. The verdict is attached to
#' the result as the `"tw_check"` attribute.
#'
#' @inheritParams diagnose_join
#' @param warn If `TRUE`, emit a warning when the verdict is not `"pass"`;
#'   otherwise print the verdict.
#' @return The joined data frame, with a `tw_check` verdict in its attributes.
#' @export
#' @examples
#' x <- data.frame(id = 1:10, a = 1:10)
#' y <- data.frame(id = 1:8, b = 1:8)
#' out <- check_join(x, y, by = "id", type = "inner")
#' attr(out, "tw_check")
check_join <- function(x, y, by, type = c("inner", "left", "right", "full"), warn = TRUE) {
  type <- match.arg(type)
  chk <- diagnose_join(x, y, by, type)

  if (isTRUE(warn) && chk$status != "pass") {
    warning(format_check_warning(chk), call. = FALSE)
  } else {
    print(chk)
  }

  joined <- merge(
    x, y, by = by,
    all.x = type %in% c("left", "full"),
    all.y = type %in% c("right", "full")
  )
  attr(joined, "tw_check") <- chk
  joined
}
