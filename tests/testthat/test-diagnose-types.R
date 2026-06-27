test_that("stable types pass", {
  before <- data.frame(id = 1:3, amount = c(1.5, 2, 3))
  after <- before
  chk <- diagnose_types(before, after)
  expect_equal(chk$status, "pass")
})

test_that("numeric becoming text is a problem", {
  before <- data.frame(id = 1:3, amount = c(1.5, 2, 3))
  after <- transform(before, amount = as.character(amount))
  chk <- diagnose_types(before, after)
  expect_equal(chk$status, "problem")
  expect_match(chk$summary, "amount")
})

test_that("a non-arithmetic-breaking type change is a review", {
  before <- data.frame(g = c("a", "b", "c"), stringsAsFactors = FALSE)
  after <- data.frame(g = factor(c("a", "b", "c")))
  chk <- diagnose_types(before, after)
  expect_equal(chk$status, "review")
})
