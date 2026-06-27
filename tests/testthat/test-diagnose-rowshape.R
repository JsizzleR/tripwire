test_that("a modest change passes", {
  before <- data.frame(x = 1:100)
  after <- data.frame(x = 1:90)
  chk <- diagnose_rowshape(before, after)
  expect_equal(chk$status, "pass")
})

test_that("dropping most rows triggers review", {
  before <- data.frame(x = 1:100)
  after <- data.frame(x = 1:20)
  chk <- diagnose_rowshape(before, after, max_drop = 0.5)
  expect_equal(chk$status, "review")
  expect_match(chk$summary, "dropped 80")
})

test_that("removing every row is a problem", {
  before <- data.frame(x = 1:100)
  after <- before[0, , drop = FALSE]
  chk <- diagnose_rowshape(before, after)
  expect_equal(chk$status, "problem")
})

test_that("unexpected growth is flagged, and counts work too", {
  chk <- diagnose_rowshape(100, 250)
  expect_equal(chk$status, "review")
  expect_match(chk$summary, "grew")
})
