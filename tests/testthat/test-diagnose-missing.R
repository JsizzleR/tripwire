test_that("no missing data passes", {
  d <- data.frame(a = 1:5, b = letters[1:5])
  chk <- diagnose_missing(d)
  expect_s3_class(chk, "tw_check")
  expect_equal(chk$status, "pass")
})

test_that("a column at or above the threshold is flagged for review", {
  d <- data.frame(a = c(1, NA, NA, 4, 5), b = 1:5) # a is 40% missing
  chk <- diagnose_missing(d, threshold = 0.10)
  expect_equal(chk$status, "review")
  expect_true("a" %in% chk$details$flagged)
  expect_match(chk$summary, "`a` 40%")
})

test_that("an entirely missing column is a problem", {
  d <- data.frame(a = rep(NA, 5), b = 1:5)
  chk <- diagnose_missing(d)
  expect_equal(chk$status, "problem")
})

test_that("missingness below the threshold passes", {
  d <- data.frame(a = c(NA, 2:20), b = 1:20) # 5% missing
  chk <- diagnose_missing(d, threshold = 0.10)
  expect_equal(chk$status, "pass")
})

test_that("before/after mode catches missingness a step introduced", {
  before <- data.frame(region = letters[1:5])
  after <- data.frame(region = c("a", NA, NA, "d", "e")) # 40% now, 0% before
  chk <- diagnose_missing(after, before = before, threshold = 0.10)
  expect_equal(chk$status, "review")
  expect_match(chk$summary, "0% -> 40%")
})
