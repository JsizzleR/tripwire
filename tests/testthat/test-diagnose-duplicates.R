test_that("a unique key passes", {
  d <- data.frame(id = 1:4, v = 1:4)
  chk <- diagnose_duplicates(d, by = "id")
  expect_equal(chk$status, "pass")
})

test_that("repeated keys are flagged", {
  d <- data.frame(id = c(1, 2, 2, 3), v = 1:4)
  chk <- diagnose_duplicates(d, by = "id")
  expect_equal(chk$status, "review")
  expect_match(chk$summary, "not unique")
})

test_that("whole-row duplicates are flagged", {
  d <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
  chk <- diagnose_duplicates(d)
  expect_equal(chk$status, "review")
  expect_match(chk$summary, "duplicate row")
})

test_that("no duplicates passes", {
  d <- data.frame(a = 1:3, b = letters[1:3])
  chk <- diagnose_duplicates(d)
  expect_equal(chk$status, "pass")
})
