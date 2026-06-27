# The judge tier needs yourhonor (Suggests). Tests build a tiny gold set with
# pre-recorded judge labels (no API call) and check the gating logic.

make_cal <- function() {
  gold <- data.frame(
    human_label = c("C", "C", "P", "I", "C", "P", "I", "C", "C", "I"),
    judge_label = c("C", "C", "C", "I", "C", "P", "P", "C", "C", "I"),
    stringsAsFactors = FALSE
  )
  yourhonor::calibrate_judge(gold, n_boot = 200, seed = 1)
}

test_that("tw_judge_trust turns agreement into a gate level", {
  skip_if_not_installed("yourhonor")
  trust <- tw_judge_trust(make_cal())
  expect_true(trust$level %in% c("use", "caution", "not_calibrated"))
  expect_true(is.numeric(trust$estimate))
  expect_equal(trust$coefficient, "gwet_ac1")
})

test_that("tw_judge_check returns a gated tw_check", {
  skip_if_not_installed("yourhonor")
  cal <- make_cal()
  judge <- function(data) rep("I", nrow(data)) # always flags a problem
  item <- data.frame(input = "claim: doubled; data: +12%", target = "claim matches numbers?")
  chk <- tw_judge_check(item, judge, cal, dimension = "interpretation")

  expect_s3_class(chk, "tw_check")
  expect_equal(chk$check, "judge_interpretation")
  expect_equal(chk$details$judge_label, "I")
  expect_equal(chk$details$raw_status, "problem")
  # unless the judge is fully calibrated (use), a "problem" is capped to "review"
  if (chk$details$trust != "use") {
    expect_equal(chk$status, "review")
    expect_true(chk$details$gated)
  }
})

test_that("a not-calibrated judge can never assert a problem", {
  skip_if_not_installed("yourhonor")
  # judge that disagrees with humans badly -> low agreement
  gold <- data.frame(
    human_label = c("C", "C", "C", "C", "C", "I", "I", "I", "I", "I"),
    judge_label = c("I", "I", "I", "C", "C", "C", "C", "C", "I", "I"),
    stringsAsFactors = FALSE
  )
  cal <- yourhonor::calibrate_judge(gold, n_boot = 200, seed = 1)
  judge <- function(data) rep("I", nrow(data))
  item <- data.frame(input = "x", target = "y")
  chk <- tw_judge_check(item, judge, cal)

  expect_false(chk$status == "problem")
  expect_true(chk$status %in% c("review", "pass"))
})

test_that("an unmapped judge label errors clearly", {
  skip_if_not_installed("yourhonor")
  cal <- make_cal()
  judge <- function(data) rep("WAT", nrow(data))
  item <- data.frame(input = "x", target = "y")
  expect_error(tw_judge_check(item, judge, cal), "unmapped label")
})
