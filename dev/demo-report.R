# Health-strip demo. Run from anywhere:
#   Rscript /Users/jsizl/Projects/tripwire/dev/demo-report.R
# Loads R/ directly (no install) and renders the health strip in colour.

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg)) {
  normalizePath(file.path(dirname(file_arg), ".."))
} else {
  getwd()
}
for (f in list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f)
}

set.seed(1)
orders <- data.frame(
  order_id = 1:6000,
  customer_id = sample(1:1600, 6000, replace = TRUE)
)
customers <- data.frame(
  customer_id = 1:1200,
  region = sample(c("East", "West", "North", "South"), 1200, replace = TRUE)
)

cat("Scenario A — a clean join, every key matched:\n\n")
tiers <- data.frame(customer_id = 1:1200, tier = sample(c("free", "pro"), 1200, replace = TRUE))
joined_a <- merge(customers, tiers, by = "customer_id")
print(tw_report(
  diagnose_join(customers, tiers, by = "customer_id", type = "inner"),
  diagnose_missing(joined_a, threshold = 0.10)
))

cat("\n\nScenario B — the order/customer left join, checked as a set:\n\n")
joined_b <- merge(orders, customers, by = "customer_id", all.x = TRUE)
print(tw_report(
  diagnose_join(orders, customers, by = "customer_id", type = "left"),
  diagnose_missing(joined_b, cols = c("order_id", "region"), threshold = 0.10)
))

cat("\n\nScenario C — checking a short pipeline end to end (all five checks):\n\n")
raw <- data.frame(
  id = 1:500,
  price = round(runif(500, 5, 100), 2),
  category = sample(c("a", "b", "c"), 500, replace = TRUE)
)
# A pipeline with three silent slips:
step <- raw[raw$price > 90, ]          # over-aggressive filter: keeps ~10%
step$price <- as.character(step$price) # price silently becomes text
step <- rbind(step, step[1, ])         # an accidental duplicate row
print(tw_report(
  diagnose_rowshape(raw, step),
  diagnose_types(raw, step),
  diagnose_duplicates(step, by = "id"),
  diagnose_missing(step)
))

cat("\n(Run print(report, details = TRUE) for the audit trail; tw_html(report) for Quarto.)\n")
