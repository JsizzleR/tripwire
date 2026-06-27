#!/usr/bin/env bash
# END-TO-END LIVE-SESSION probe. Proves the check runs IN a live R session
# against in-memory data frames (by name), with no CSV / no data copy.
#
#   bash dev/probe-mcp-live.sh
#
# Process 1 (background): a "user" R session that holds data in memory and
#   registers itself with mcptools::mcp_session(), then pumps its event loop.
# Process 2 (foreground): the MCP server, fed a check_join call by object name;
#   mcptools routes it into the live session.
set -uo pipefail

READY=/tmp/tw_live_ready
OUT=/tmp/tw_live_out.txt
rm -f "$READY" "$OUT" /tmp/mcptools-socket*

Rscript -e '
suppressMessages(library(tripwire))
set.seed(1)
orders <- data.frame(order_id = 1:6000, customer_id = sample(1:1600, 6000, TRUE))
customers <- data.frame(customer_id = 1:1200, region = sample(c("E","W","N","S"), 1200, TRUE))
mcptools::mcp_session()
writeLines("ready", "/tmp/tw_live_ready")
repeat { later::run_now(timeout = 0.3) }
' &
SESSION_PID=$!
trap 'kill $SESSION_PID 2>/dev/null' EXIT

# Wait (bounded, no `sleep`) for the session to register.
for _ in $(seq 1 60); do
  [ -f "$READY" ] && break
  perl -e 'select(undef,undef,undef,0.2)'
done

# Drive the server: check_join on the IN-MEMORY data frames, by NAME.
{
  printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"probe","version":"0"}}}'
  printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/initialized"}'
  printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"check_join","arguments":{"x":"orders","y":"customers","by":"customer_id","type":"inner"}}}'
  perl -e 'select(undef,undef,undef,5)'
} | Rscript -e 'tripwire::tw_mcp_server()' >"$OUT" 2>/dev/null

python3 - "$OUT" <<'PY'
import sys, json
resps = {}
for line in open(sys.argv[1]):
    line = line.strip()
    if not line:
        continue
    try:
        o = json.loads(line)
    except Exception:
        continue
    if isinstance(o, dict) and "id" in o:
        resps[o["id"]] = o

call = resps.get(2, {}).get("result", {})
text = (call.get("content") or [{}])[0].get("text")
print("live check_join verdict:", text)
try:
    v = json.loads(text)
    assert v["overall_status"] == "review"
    assert v["checks"][0]["check"] == "join_integrity"
    print("\nRESULT: PASS - check ran in the live session on in-memory data (no CSV, no copy).")
except Exception as e:
    print("\nRESULT: FAIL ->", repr(e))
    sys.exit(1)
PY
