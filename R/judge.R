# Tier-2: calibration-gated LLM-judge checks.
#
# Deterministic checks (Tier-1) answer questions arithmetic can answer. Others --
# "does the stated claim match the numbers?", "is this the right method?" -- need
# an LLM judge. But an LLM judge is itself fallible, so before its verdict carries
# weight it must be CALIBRATED: measured against human labels with yourhonor. This
# module turns (a judge + its yourhonor calibration) into a tw_check whose status
# is GATED by how trustworthy the judge is. A poorly-calibrated judge can inform,
# but it cannot assert a problem.
#
# yourhonor + ellmer are optional (Suggests); Tier-1 stays dependency-free.

#' Trust level of a calibrated judge
#'
#' Reads the agreement from a yourhonor calibration and turns it into a gate:
#' `"use"` (AC1 >= 0.8), `"caution"` (>= 0.6), or `"not_calibrated"` -- the same
#' thresholds `yourhonor::report()` uses for its USE / USE WITH CAUTION / NOT
#' CALIBRATED verdict.
#'
#' @param calibration A `yourhonor_calibration` from `yourhonor::calibrate_judge()`.
#' @param coefficient Agreement coefficient to gate on (default Gwet's AC1).
#' @return A list: `level`, `coefficient`, `estimate`, `conf_low`, `conf_high`.
#' @export
tw_judge_trust <- function(calibration, coefficient = "gwet_ac1") {
  if (!inherits(calibration, "yourhonor_calibration")) {
    stop("`calibration` must be a yourhonor_calibration object.", call. = FALSE)
  }
  ag <- calibration$agreement
  i <- which(ag$coefficient == coefficient)
  if (length(i) == 0L) i <- 1L
  i <- i[1]
  est <- ag$estimate[i]
  level <- if (is.na(est)) {
    "not_calibrated"
  } else if (est >= 0.8) {
    "use"
  } else if (est >= 0.6) {
    "caution"
  } else {
    "not_calibrated"
  }
  list(
    level = level, coefficient = ag$coefficient[i], estimate = est,
    conf_low = ag$conf_low[i], conf_high = ag$conf_high[i]
  )
}

#' A calibration-gated judge check
#'
#' Runs `judge` on a single analysis `item`, maps its label to a tripwire status,
#' and GATES that status by the judge's calibration: a `"caution"` judge cannot
#' return `problem` (it is capped to `review`); a `"not_calibrated"` judge is
#' advisory (never worse than `review`). Returns a `tw_check` that renders in a
#' health strip beside the deterministic checks.
#'
#' @param item A one-row data frame describing the analysis to judge (the columns
#'   the judge reads, e.g. `input`, `target`).
#' @param judge A function `(data) -> character` returning one label per row --
#'   the SAME judge calibrated with `yourhonor::calibrate_judge()`.
#' @param calibration That judge's `yourhonor_calibration`.
#' @param dimension Short name of what is being judged (e.g. `"interpretation"`).
#' @param label_map Named map from judge labels to tripwire statuses.
#' @return A `tw_check` verdict.
#' @export
#' @examplesIf requireNamespace("yourhonor", quietly = TRUE)
#' gold <- data.frame(
#'   human_label = c("C", "C", "P", "I", "C", "P", "I", "C"),
#'   judge_label = c("C", "C", "C", "I", "C", "P", "P", "C")
#' )
#' cal <- yourhonor::calibrate_judge(gold, n_boot = 200, seed = 1)
#' judge <- function(data) rep("I", nrow(data)) # a judge that flags a problem
#' item <- data.frame(input = "claim: doubled; data: +12%", target = "claim matches numbers?")
#' tw_judge_check(item, judge, cal, dimension = "interpretation")
tw_judge_check <- function(item, judge, calibration, dimension = "interpretation",
                           label_map = c(C = "pass", P = "review", I = "problem")) {
  stopifnot(is.function(judge))
  if (!inherits(calibration, "yourhonor_calibration")) {
    stop("`calibration` must be a yourhonor_calibration object.", call. = FALSE)
  }
  raw <- as.character(judge(item))[1]
  if (is.na(raw) || !raw %in% names(label_map)) {
    stop(sprintf("Judge returned an unmapped label: '%s'.", raw), call. = FALSE)
  }
  raw_status <- label_map[[raw]]
  trust <- tw_judge_trust(calibration)

  gated <- switch(trust$level,
    use = raw_status,
    caution = if (identical(raw_status, "problem")) "review" else raw_status,
    # not_calibrated: advisory only -- never worse than review
    if (identical(raw_status, "pass")) "pass" else "review"
  )

  new_tw_check(
    paste0("judge_", dimension),
    gated,
    judge_summary(dimension, raw_status, gated, trust),
    details = list(
      dimension = dimension,
      judge_label = raw,
      raw_status = raw_status,
      gated_status = gated,
      gated = !identical(gated, raw_status),
      trust = trust$level,
      agreement = trust$estimate,
      agreement_coefficient = trust$coefficient,
      n_calibration_items = calibration$n_items
    )
  )
}

# Build the one-sentence summary for a judge verdict.
judge_summary <- function(dimension, raw_status, gated, trust) {
  est <- if (is.na(trust$estimate)) "NA" else formatC(trust$estimate, format = "f", digits = 2)
  verdict <- switch(raw_status,
    pass = "found no issue",
    review = "flagged a possible issue",
    problem = "flagged a serious issue",
    "returned a verdict"
  )
  trust_note <- switch(trust$level,
    use = sprintf("calibrated, %s = %s", trust$coefficient, est),
    caution = sprintf("judge only moderately calibrated, %s = %s", trust$coefficient, est),
    sprintf("judge NOT calibrated, %s = %s - advisory only", trust$coefficient, est)
  )
  gated_note <- if (!identical(gated, raw_status)) sprintf("; capped to %s", gated) else ""
  sprintf("Judge %s on '%s' (%s%s).", verdict, dimension, trust_note, gated_note)
}
