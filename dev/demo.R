# A zero-dependency demo. Run from anywhere:
#   Rscript /Users/jsizl/Projects/tripwire/dev/demo.R
# It loads the package's R/ files directly (no install needed) and shows the
# check catching a real, silent bug.

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

# 6,000 orders, each tagged with a customer id in 1..1600.
orders <- data.frame(
  order_id = 1:6000,
  customer_id = sample(1:1600, 6000, replace = TRUE)
)

# ...but the customers lookup only knows customers 1..1200.
# Every order whose customer is 1201..1600 will silently vanish on an inner join.
customers <- data.frame(
  customer_id = 1:1200,
  region = sample(c("East", "West", "North", "South"), 1200, replace = TRUE)
)

cat("Goal: attach each order's region by joining orders to the customers lookup.\n\n")

chk <- diagnose_join(orders, customers, by = "customer_id", type = "inner")
print(chk)

cat("\nNo error was raised. Without the check, those orders are just gone\n")
cat("from your analysis and you'd never know.\n")
