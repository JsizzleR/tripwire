# Wiring tripwire into AI assistants via the Model Context Protocol (MCP).
#
# These tools run IN THE USER'S LIVE R SESSION via mcptools session routing: the
# assistant passes the NAME of a data frame, mcptools ships the tool to the
# session where that object lives, the check runs against the in-memory data, and
# only the small verdict comes back. The data is never copied or serialised --
# so this works on arbitrarily large data frames.
#
# To use it, the user registers their interactive R session once with
# `mcptools::mcp_session()` (e.g. in .Rprofile). On-disk CSV variants
# (`*_file`) are also provided for data that hasn't been loaded yet.
#
# The ellmer/mcptools wiring is optional (Suggests); tripwire's core checks stay
# dependency-free.

# Resolve a named data frame from the live session's global environment.
tw_get <- function(name, envir = globalenv()) {
  if (!is.character(name) || length(name) != 1L || !exists(name, envir = envir, inherits = TRUE)) {
    stop(sprintf("No object named '%s' found in the R session.", name), call. = FALSE)
  }
  obj <- get(name, envir = envir, inherits = TRUE)
  if (!is.data.frame(obj)) {
    stop(sprintf("'%s' is not a data frame (it is %s).", name, class(obj)[1]), call. = FALSE)
  }
  obj
}

# Read a CSV into a data frame (base utils).
tw_read_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("File not found: %s", path), call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# Collapse a list of tw_check verdicts into a plain, JSON-friendly list.
tw_as_list <- function(checks) {
  if (inherits(checks, "tw_report")) checks <- checks$checks
  list(
    overall_status = report_status(checks),
    checks = lapply(checks, function(c) {
      list(check = c$check, status = c$status, summary = c$summary)
    })
  )
}

# Encode a result as a compact JSON string for an MCP text content block.
tw_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", pretty = FALSE)
}

# ---- Live-session checks (operate on object NAMES in the live R session) -----

#' Check a join between two live data frames
#'
#' Runs in the caller's R session against the in-memory data frames named by `x`
#' and `y` -- nothing is copied or written to disk.
#'
#' @param x,y Names of the left and right data frames in the R session.
#' @param by Key column name to join on (comma-separated for multiple columns).
#' @param type Join type: `"inner"`, `"left"`, `"right"`, or `"full"`.
#' @return A list with `overall_status` and the per-check verdicts.
#' @export
#' @examples
#' orders <- data.frame(id = 1:10, a = 1:10)
#' lookup <- data.frame(id = 1:8, b = 1:8)
#' tw_check_join("orders", "lookup", by = "id", type = "inner")
tw_check_join <- function(x, y, by, type = "inner") {
  keys <- trimws(strsplit(by, ",")[[1]])
  tw_as_list(list(diagnose_join(tw_get(x), tw_get(y), by = keys, type = type)))
}

#' Check a live data frame for missingness and duplicate rows
#'
#' @param data Name of the data frame in the R session.
#' @param missing_threshold Fraction (0-1) at which a column is flagged.
#' @return A list with `overall_status` and the per-check verdicts.
#' @export
tw_check_data <- function(data, missing_threshold = 0.10) {
  d <- tw_get(data)
  tw_as_list(list(
    diagnose_missing(d, threshold = missing_threshold),
    diagnose_duplicates(d)
  ))
}

#' Check what a transformation did to a live data frame
#'
#' Compares two in-memory data frames (the state before and after a step) and
#' reports row loss/growth, column type changes, new missingness, and -- if `by`
#' is given -- duplicate keys.
#'
#' @param before,after Names of the data frames before and after the step.
#' @param by Optional key column(s) (comma-separated) expected to stay unique.
#' @return A list with `overall_status` and the per-check verdicts.
#' @export
tw_check_transform <- function(before, after, by = NULL) {
  b <- tw_get(before)
  a <- tw_get(after)
  checks <- list(
    diagnose_rowshape(b, a),
    diagnose_types(b, a),
    diagnose_missing(a, before = b)
  )
  if (!is.null(by) && nzchar(by)) {
    checks <- c(checks, list(diagnose_duplicates(a, by = trimws(strsplit(by, ",")[[1]]))))
  }
  tw_as_list(checks)
}

# ---- On-disk CSV variants (for data not yet loaded) --------------------------

#' Check a join between two CSV files
#' @inheritParams tw_check_join
#' @param left,right Paths to the left and right CSV files.
#' @return A list with `overall_status` and the per-check verdicts.
#' @export
tw_check_join_file <- function(left, right, by, type = "inner") {
  keys <- trimws(strsplit(by, ",")[[1]])
  tw_as_list(list(diagnose_join(tw_read_csv(left), tw_read_csv(right), by = keys, type = type)))
}

#' Check a CSV file for missingness and duplicate rows
#' @param path Path to the CSV file.
#' @param missing_threshold Fraction (0-1) at which a column is flagged.
#' @return A list with `overall_status` and the per-check verdicts.
#' @export
tw_check_data_file <- function(path, missing_threshold = 0.10) {
  d <- tw_read_csv(path)
  tw_as_list(list(
    diagnose_missing(d, threshold = missing_threshold),
    diagnose_duplicates(d)
  ))
}

# ---- MCP tool wrappers (named so they serialise cleanly into a session) ------

tw_tool_join <- function(x, y, by, type = "inner") tw_json(tw_check_join(x, y, by, type))
tw_tool_data <- function(data, missing_threshold = 0.10) tw_json(tw_check_data(data, missing_threshold))
tw_tool_transform <- function(before, after, by = NULL) tw_json(tw_check_transform(before, after, by))

#' tripwire's live-session checks as a list of ellmer tool definitions
#'
#' Each tool takes the NAME of a data frame in the user's R session and runs the
#' check there (via mcptools session routing), returning the verdict as JSON.
#'
#' @return A list of [ellmer::tool()] objects.
#' @export
tw_mcp_tools <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("tw_mcp_tools() needs the 'ellmer' package: install.packages('ellmer').", call. = FALSE)
  }
  in_session <- "The check runs in your live R session against the in-memory data; do NOT export to CSV."
  list(
    ellmer::tool(
      fun = tw_tool_join,
      description = paste(
        "Before trusting a join, check it for silently dropped rows (keys with no",
        "match) or multiplied rows (fan-out from duplicate keys).", in_session
      ),
      name = "check_join",
      arguments = list(
        x = ellmer::type_string("Name of the left data frame in the R session (e.g. 'orders')."),
        y = ellmer::type_string("Name of the right data frame in the R session (e.g. 'customers')."),
        by = ellmer::type_string("Key column to join on (comma-separated for multiple columns)."),
        type = ellmer::type_enum(
          c("inner", "left", "right", "full"),
          "The join type. Defaults to inner.",
          required = FALSE
        )
      )
    ),
    ellmer::tool(
      fun = tw_tool_data,
      description = paste(
        "Check a data frame for surprising missingness (columns with many NAs) and",
        "duplicate rows.", in_session
      ),
      name = "check_data",
      arguments = list(
        data = ellmer::type_string("Name of the data frame in the R session."),
        missing_threshold = ellmer::type_number(
          "Fraction (0-1) of missing values at which a column is flagged. Defaults to 0.1.",
          required = FALSE
        )
      )
    ),
    ellmer::tool(
      fun = tw_tool_transform,
      description = paste(
        "Compare a data frame before and after a transformation step and report",
        "what silently changed: row loss or growth, column type changes, newly",
        "introduced missingness, and optionally duplicate keys.", in_session
      ),
      name = "check_transform",
      arguments = list(
        before = ellmer::type_string("Name of the data frame BEFORE the step."),
        after = ellmer::type_string("Name of the data frame AFTER the step."),
        by = ellmer::type_string(
          "Optional key column(s), comma-separated, expected to stay unique.",
          required = FALSE
        )
      )
    )
  )
}

#' Serve tripwire's checks as an MCP server (stdio)
#'
#' Launches a Model Context Protocol server exposing tripwire's checks as tools,
#' for any MCP client (Claude Code, Claude Desktop, Positron Assistant). Run
#' non-interactively, e.g. `Rscript -e "tripwire::tw_mcp_server()"`.
#'
#' With `session_tools = TRUE` (default), tool calls are routed into the live R
#' session the user has registered with [mcptools::mcp_session()], so checks run
#' against in-memory data frames by name. Without a registered session, the tools
#' run in the server process (where the named objects do not exist) and return a
#' clear error -- use the `*_file` checks for on-disk data in that case.
#'
#' @param session_tools Route tool calls into a live R session (default `TRUE`).
#' @param ... Passed on to [mcptools::mcp_server()].
#' @return Invisibly; runs the server loop until the client disconnects.
#' @export
tw_mcp_server <- function(session_tools = TRUE, ...) {
  if (!requireNamespace("mcptools", quietly = TRUE)) {
    stop("tw_mcp_server() needs the 'mcptools' package: install.packages('mcptools').", call. = FALSE)
  }
  mcptools::mcp_server(tools = tw_mcp_tools(), session_tools = session_tools, ...)
}
