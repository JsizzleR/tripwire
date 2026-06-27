# tripwire

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/JsizzleR/tripwire/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JsizzleR/tripwire/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Catch silent failures in data-analysis steps.**

A data operation can go wrong without raising a single error: a join quietly
drops or multiplies rows, a column slips from numeric to text, missingness creeps
in, a conclusion overstates what the numbers actually show. `tripwire` turns
those *silent* failures into a clear, structured verdict — a "health strip" you,
or an AI assistant, can read at a glance.

## One example

```r
# 6,000 orders; the customers lookup is missing some customers
diagnose_join(orders, customers, by = "customer_id", type = "inner")
#> ⚠ join integrity — Inner join on `customer_id` dropped 1461 of 6000 left rows with no match (24%).
```

No error was raised. A quarter of the orders just vanished from the analysis.
`tripwire` is the thing that notices.

## Two tiers of checking

Every check returns the same verdict object — `tw_check`: a **status**
(`pass` / `review` / `problem`), a **plain-English summary**, and the **numbers**
behind it. `tw_report()` rolls a set of verdicts into a health strip.

### Tier 1 — deterministic checks

No LLM, instant, free — the things arithmetic can settle:

| check | catches |
|---|---|
| `diagnose_join` | rows silently dropped (unmatched keys) or multiplied (fan-out) |
| `diagnose_missing` | columns going — or growing — `NA`, including before/after a step |
| `diagnose_rowshape` | a step that dropped too much, or grew unexpectedly |
| `diagnose_types` | a column that silently changed type (numeric → text) |
| `diagnose_duplicates` | duplicate rows, or a key that should be unique but isn't |

```
✗ Problem  ·  1 passed  ·  2 to review  ·  1 problem
  ⚠ row shape       The step dropped 445 of 500 rows (89%).
  ✗ type stability  1 column changed type: `price` numeric -> character.
  ⚠ duplicates      `id` is not unique: 1 key repeats, 1 extra row.
  ✓ missingness     No column is at or above the 10% missing-data threshold.
```

The core is **base R, dependency-free**, and runs in your session against your
real objects — no data leaves the box.

### Tier 2 — a calibrated LLM judge

For the questions arithmetic *can't* answer — *is this the right method? does the
claim match the numbers?* — an LLM judge weighs in. But an LLM judge is itself
fallible, so its verdict is **gated by its own measured reliability**: calibrated
against human labels with [yourhonor](https://github.com/JsizzleR/yourhonor), so
a judge that doesn't agree with people can't assert a `problem` — only flag a
`review`. Verify the verifier.

```
⚠ Review  ·  1 passed  ·  1 to review
  ✓ join integrity        Inner join on `customer_id` is clean: 1000 rows in, 1000 out, every key matched 1:1.
  ⚠ judge interpretation  Judge flagged a serious issue on 'interpretation' (judge only moderately calibrated, gwet_ac1 = 0.73; capped to review).
```

The join is mechanically perfect; the *conclusion* drawn from it is wrong — and
the judge catches it without overstating its own confidence.

## Use it from an AI assistant (MCP)

tripwire ships an MCP server (built on Posit's `mcptools`), so an assistant can
run the checks against the data frames already in your **live R session** — you
pass a data frame's *name*, not a copy or a file, so it works on data of any
size. The tools are `check_join`, `check_transform`, and `check_data`.

Register with **Claude Code** via a `.mcp.json` at the repo root:

```json
{
  "mcpServers": {
    "tripwire": {
      "command": "Rscript",
      "args": ["-e", "tripwire::tw_mcp_server()"]
    }
  }
}
```

or `claude mcp add -s user tripwire -- Rscript -e "tripwire::tw_mcp_server()"`.
The same server registers with **Positron Assistant** (`mcptools` lists it as a
supported client; see `.posit/assistant/`). Register the R session whose data you
want reachable by running `mcptools::mcp_session()` in it (e.g. from your
`.Rprofile`).

Verify the whole round-trip with no client:

```sh
bash dev/probe-mcp-live.sh   # routes a check into a background in-memory session
bash dev/probe-mcp.sh        # tests the bare MCP protocol
```

## Quick start

```r
# from a local clone for now (private repo)
#   R CMD INSTALL .          # or devtools::load_all()
# pak::pak("JsizzleR/tripwire")   # once public

library(tripwire)
diagnose_join(orders, customers, by = "customer_id")
```

Or try the demos with no install:

```sh
Rscript dev/demo.R          # the single join check
Rscript dev/demo-report.R   # the five deterministic checks, as a health strip
Rscript dev/demo-judge.R    # Tier 1 + Tier 2 (calibrated judge) together
```

## Design rules

- **Specific and actionable** — "dropped 1,461 of 6,000 rows (24%)", never "warning".
- **Auditable** — every verdict shows what it checked and the numbers; the judge
  shows its calibration. You can verify the verifier.
- **Dependency-free core** — the Tier-1 checks are base R. The render (`cli`),
  MCP (`ellmer`/`mcptools`), and judge (`yourhonor`) layers are all optional
  (`Suggests`).
