---
name: tripwire-checks
description: Use when validating an R data pipeline for silent failures - a join that drops or multiplies rows, missingness that appears after a step, a column that changed type, unexpected duplicates, or surprising row-count changes. Triggers on data-quality and join-integrity questions ("did this step quietly break my data?").
license: MIT
---

# Tripwire checks

Tripwire runs deterministic checks that each return a verdict (pass / review /
problem), a one-sentence summary, and the numbers behind it.

The checks run in the user's **live R session against the in-memory data
frames** - pass the data frame's NAME, never write it to CSV or copy it. (This
requires the session to be registered with `mcptools::mcp_session()`.)

When the user is about to trust a join or a transformation, call:

- `check_join(x, y, by, type)` - names of two data frames; before trusting a join.
- `check_transform(before, after, by)` - names of the data frames before and
  after a step; reports what silently changed (row loss/growth, column type
  changes, new missingness, duplicate keys).
- `check_data(data, missing_threshold)` - one data frame's missingness and
  duplicate rows.

Report the `overall_status` and each check's `summary`. Surface the underlying
numbers only if the user asks. Treat `problem` as stop-and-look and `review` as
worth a glance.
