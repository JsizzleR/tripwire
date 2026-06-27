# Live-session checks operate on object names in the global environment -- the
# same place mcptools runs them (execute_tool_call -> do.call(tool, args), where
# globalenv() is the user's live session).

test_that("tw_check_join runs on live data frames by name", {
  assign("orders", data.frame(id = 1:10, a = 1:10), envir = globalenv())
  assign("lookup", data.frame(id = 1:8, b = 1:8), envir = globalenv())
  withr::defer(rm(list = c("orders", "lookup"), envir = globalenv()))

  res <- tw_check_join("orders", "lookup", by = "id", type = "inner")
  expect_equal(res$overall_status, "review")
  expect_equal(res$checks[[1]]$check, "join_integrity")
})

test_that("tw_check_data flags missingness on a live data frame", {
  assign("tbl", data.frame(a = c(1, NA, NA, 4, 5), b = 1:5), envir = globalenv())
  withr::defer(rm(list = "tbl", envir = globalenv()))

  res <- tw_check_data("tbl", missing_threshold = 0.10)
  expect_equal(res$overall_status, "review")
})

test_that("tw_check_transform catches an in-memory numeric->character change", {
  assign("raw", data.frame(id = 1:5, amount = c(1.5, 2, 3, 4, 5)), envir = globalenv())
  assign("clean", data.frame(id = 1:5, amount = as.character(c(1.5, 2, 3, 4, 5))), envir = globalenv())
  withr::defer(rm(list = c("raw", "clean"), envir = globalenv()))

  res <- tw_check_transform("raw", "clean")
  expect_equal(res$overall_status, "problem") # numeric -> character breaks arithmetic
})

test_that("tw_get gives a clear error for a missing or non-data-frame object", {
  expect_error(tw_get("does_not_exist_xyz"), "No object named")
  assign("not_a_df", 1:10, envir = globalenv())
  withr::defer(rm(list = "not_a_df", envir = globalenv()))
  expect_error(tw_get("not_a_df"), "not a data frame")
})

test_that("the on-disk variant works for data not yet loaded", {
  l <- tempfile(fileext = ".csv")
  r <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(id = 1:10, a = 1:10), l, row.names = FALSE)
  utils::write.csv(data.frame(id = 1:8, b = 1:8), r, row.names = FALSE)
  res <- tw_check_join_file(l, r, by = "id", type = "inner")
  expect_equal(res$overall_status, "review")
})

test_that("tw_mcp_tools builds the three live-session tools", {
  skip_if_not_installed("ellmer")
  tools <- tw_mcp_tools()
  expect_length(tools, 3)
})

test_that("invoking a tool the way the session does resolves the live object", {
  skip_if_not_installed("ellmer")
  assign("orders", data.frame(id = 1:10, a = 1:10), envir = globalenv())
  assign("lookup", data.frame(id = 1:8, b = 1:8), envir = globalenv())
  withr::defer(rm(list = c("orders", "lookup"), envir = globalenv()))

  tool <- tw_mcp_tools()[[1]] # check_join
  out <- do.call(tool, list(x = "orders", y = "lookup", by = "id", type = "inner"))
  verdict <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_equal(verdict$overall_status, "review")
  expect_equal(verdict$checks[[1]]$check, "join_integrity")
})
