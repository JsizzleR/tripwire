test_that("a clean 1:1 inner join passes", {
  x <- data.frame(id = 1:5, a = letters[1:5])
  y <- data.frame(id = 1:5, b = LETTERS[1:5])
  chk <- diagnose_join(x, y, "id", "inner")
  expect_s3_class(chk, "tw_check")
  expect_equal(chk$status, "pass")
  expect_equal(chk$details$n_out, 5)
  expect_false(chk$details$fanout)
})

test_that("an inner join flags silently dropped rows", {
  x <- data.frame(id = 1:10, a = 1:10)
  y <- data.frame(id = 1:8, b = 1:8) # ids 9, 10 have no match
  chk <- diagnose_join(x, y, "id", "inner")
  expect_equal(chk$status, "review")
  expect_equal(chk$details$n_x_unmatched, 2)
  expect_equal(chk$details$n_out, 8)
  expect_match(chk$summary, "dropped 2 of 10")
})

test_that("an inner join flags fan-out from duplicate keys", {
  x <- data.frame(id = c(1, 2, 3), a = 1:3)
  y <- data.frame(id = c(1, 1, 2), b = 1:3) # id 1 duplicated in y
  chk <- diagnose_join(x, y, "id", "inner")
  expect_true(chk$details$fanout)
  expect_equal(chk$status, "review")
  expect_equal(chk$details$n_out, 3) # id1 -> 2 rows, id2 -> 1, id3 dropped
  expect_match(chk$summary, "multiplied rows")
})

test_that("a total key mismatch is a problem, not a quiet empty table", {
  x <- data.frame(id = 1:3, a = 1:3)
  y <- data.frame(id = 4:6, b = 1:3)
  chk <- diagnose_join(x, y, "id", "inner")
  expect_equal(chk$status, "problem")
  expect_equal(chk$details$n_out, 0)
  expect_match(chk$summary, "0 rows")
})

test_that("a left join preserves rows but still flags unmatched keys", {
  x <- data.frame(id = 1:10, a = 1:10)
  y <- data.frame(id = 1:8, b = 1:8)
  chk <- diagnose_join(x, y, "id", "left")
  expect_equal(chk$status, "review")
  expect_equal(chk$details$n_out, 10) # all left rows kept
  expect_equal(chk$details$n_x_unmatched, 2)
  expect_match(chk$summary, "NA-filled")
})

test_that("multi-column keys are handled", {
  x <- data.frame(g = c("a", "a", "b"), id = c(1, 2, 1), v = 1:3)
  y <- data.frame(g = c("a", "b"), id = c(1, 1), w = 1:2)
  chk <- diagnose_join(x, y, c("g", "id"), "inner")
  expect_equal(chk$status, "review") # (a, 2) has no match
  expect_equal(chk$details$n_out, 2)
})

test_that("missing key columns raise a clear error", {
  x <- data.frame(id = 1:3)
  y <- data.frame(other = 1:3)
  expect_error(diagnose_join(x, y, "id", "inner"), "not found")
})
