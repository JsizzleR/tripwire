test_that("report overall status is the worst of its checks", {
  good_x <- data.frame(id = 1:5)
  good_y <- data.frame(id = 1:5)
  pass_chk <- diagnose_join(good_x, good_y, "id", "inner")

  bad_x <- data.frame(id = 1:10)
  bad_y <- data.frame(id = 1:8)
  rev_chk <- diagnose_join(bad_x, bad_y, "id", "inner")

  rep <- tw_report(pass_chk, rev_chk)
  expect_s3_class(rep, "tw_report")
  expect_equal(report_status(rep$checks), "review")
})

test_that("tw_report accepts a bare list of checks", {
  x <- data.frame(id = 1:5)
  y <- data.frame(id = 1:5)
  chk <- diagnose_join(x, y, "id", "inner")
  rep <- tw_report(list(chk, chk))
  expect_equal(length(rep$checks), 2)
})

test_that("non-check inputs are rejected", {
  expect_error(tw_report(1, 2), "tw_check")
})

test_that("a report prints a header and one row per check", {
  x <- data.frame(id = 1:5)
  y <- data.frame(id = 1:5)
  rep <- tw_report(diagnose_join(x, y, "id", "inner"))
  expect_output(print(rep), "Checked")
  expect_output(print(rep), "join integrity")
})

test_that("tw_html produces a self-contained HTML fragment", {
  x <- data.frame(id = 1:10)
  y <- data.frame(id = 1:8)
  rep <- tw_report(diagnose_join(x, y, "id", "inner"))
  html <- tw_html(rep)
  expect_type(html, "character")
  expect_length(html, 1)
  expect_match(html, "<div")
  expect_match(html, "join integrity")
})
