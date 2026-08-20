# Tier-2 demo: a calibrated LLM judge as a gated tw_check, beside the Tier-1
# deterministic checks. Run:
#   Rscript dev/demo-judge.R
#
# The point: a join can be mechanically perfect (Tier-1 all green) while the
# CONCLUSION drawn from it is wrong. A deterministic check can't catch that; a
# judge can -- but only a judge whose reliability has been measured gets to say so.

args <- commandArgs(FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(file_arg)) normalizePath(file.path(dirname(file_arg), "..")) else getwd()
for (f in list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE)) source(f)
suppressMessages(library(yourhonor))

# 1) A tiny canonical gold set for a "does the claim match the numbers?" judge.
#    human_label is the trusted verdict; judge_label is what the judge said
#    (pre-recorded here so we calibrate with no API call). C=matches, P=partly,
#    I=contradicted. The judge is off on a couple of items, so agreement < 1.
gold <- data.frame(
  input = c(
    "result: mean +2%; claim: 'roughly flat'",
    "result: median -30%; claim: 'a big drop'",
    "result: p = 0.20; claim: 'no significant effect'",
    "result: +3%; claim: 'sales doubled'",
    "result: r = 0.9; claim: 'strong correlation'",
    "result: +12%; claim: 'sales doubled'",
    "result: n = 5; claim: 'strong evidence'",
    "result: -55%; claim: 'collapsed'",
    "result: +0.5%; claim: 'flat'",
    "result: +140%; claim: 'more than doubled'"
  ),
  target = "Does the stated claim match the numbers? C=matches, P=partly, I=contradicted.",
  human_label = c("C", "C", "C", "I", "C", "I", "P", "C", "C", "C"),
  judge_label = c("C", "C", "C", "I", "C", "I", "I", "C", "P", "C"),
  stringsAsFactors = FALSE
)

cal <- yourhonor::calibrate_judge(gold, n_boot = 500, seed = 1)
cat("== Judge calibration (yourhonor) ==\n")
print(cal)
cat("\n")
yourhonor::report(cal, output = "console")

# 2) A stand-in judge (deterministic). In production this is one ellmer call:
#      RUBRIC <- "Grade whether the claim matches the numbers as C, P or I."
#      judge <- function(data) {
#        ch <- ellmer::chat_anthropic(system_prompt = RUBRIC)
#        vapply(data$input, \(x) ch$clone()$chat(x), character(1))
#      }
demo_judge <- function(data) {
  vapply(data$input, function(s) {
    # toy rule: a "doubled" claim backed by a small % is contradicted
    if (grepl("doubled", s) && grepl("\\+(?:[0-9]|[1-4][0-9])%", s, perl = TRUE)) "I" else "C"
  }, character(1), USE.NAMES = FALSE)
}

# 3) The scenario: a clean join, but a wrong conclusion.
set.seed(1)
orders <- data.frame(customer_id = 1:1000, spend = round(rnorm(1000, 50), 2))
plans <- data.frame(customer_id = 1:1000, plan = sample(c("free", "pro"), 1000, TRUE))
item <- data.frame(
  input = "result: pro vs free mean spend +12%; claim: 'pro users spend doubled'",
  target = "Does the stated claim match the numbers?"
)

cat("\n\n== Health strip: Tier-1 (deterministic) + Tier-2 (calibrated judge) ==\n\n")
report <- tw_report(
  diagnose_join(orders, plans, by = "customer_id", type = "inner"), # Tier-1
  tw_judge_check(item, demo_judge, cal, dimension = "interpretation") # Tier-2
)
print(report, details = TRUE)

cat("\nThe join is clean, but the claim ('doubled' on a +12% result) is wrong.\n")
cat("The judge catches it -- and because the judge is only moderately calibrated,\n")
cat("the verdict is surfaced for review, not asserted as fact.\n")
