# tripwire

> Working name — trivial to rename.

**Catch silent failures in data-analysis steps.**

A data operation can go wrong without raising a single error: a join quietly
drops or multiplies rows, keys don't line up, missingness creeps in. `tripwire`
runs small, dependency-free checks that turn those silent failures into a clear,
structured verdict — the kind of "health strip" you can show next to a result.

## The idea, in one example

```r
# 6,000 orders; the customers lookup is missing some customers
diagnose_join(orders, customers, by = "customer_id", type = "inner")
#> ⚠ join integrity — Inner join on `customer_id` dropped 1461 of 6000 left rows with no match (24%).
```

No error was raised. The rows just vanished. `tripwire` is the thing that notices.

## Try it without installing

```sh
Rscript dev/demo.R
```

## What's here

This is an early build: two deterministic checks (`diagnose_join`,
`diagnose_missing`) and a `tw_report` that renders their verdicts as a health strip. The verdict object
it returns — `tw_check`, carrying a **status** (`pass` / `review` / `problem`),
a **plain-English summary**, and the **numbers** behind it — is the contract
everything else builds on:

- more deterministic checks (row-shape, type coercion, duplicate
  keys, degenerate results, range sanity);
- a UI that renders the verdict as a health strip;
- later, a calibrated-judge tier that emits the same `tw_check` shape for the
  questions arithmetic can't answer ("is this the right method?").

## Use it from an AI assistant (MCP)

tripwire ships an MCP server (built on Posit's `mcptools`), so an AI assistant
can run the checks against the data frames already in your **live R session** —
you pass a data frame's *name*, not a copy or a file, so it works on data of any
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
supported client; see `.posit/assistant/`).

Register the R session whose data you want to check by running
`mcptools::mcp_session()` in it (e.g. from your `.Rprofile`). Verify the whole
round-trip with no client: `bash dev/probe-mcp-live.sh` runs a check against a
background in-memory session; `bash dev/probe-mcp.sh` tests the bare protocol.
Requires the `ellmer` and `mcptools` packages (Suggests).

## Design rules

- **Specific and actionable** — "dropped 1,503 of 6,000 rows (25%)", never "warning".
- **Auditable** — every verdict shows what it checked and the numbers. You can
  verify the verifier.
- **Dependency-free core** — the checks run in your session, against your real
  objects; no data leaves the box.
