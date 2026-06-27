#!/usr/bin/env bash
# Drive the tripwire MCP server over stdio (no Claude Code needed) and verify the
# initialize -> tools/list -> tools/call round-trip.
#
#   bash dev/probe-mcp.sh
#
# Notes: MCP stdio is newline-delimited JSON on stdout; the server answers
# tools/* only after notifications/initialized, and exits the instant stdin hits
# EOF, so we hold stdin open briefly (macOS has no `timeout`).
set -uo pipefail

X=/tmp/tw_probe_x.csv
Y=/tmp/tw_probe_y.csv
OUT=/tmp/tw_probe_out.txt
printf 'id,a\n1,1\n2,2\n3,3\n' > "$X"
printf 'id,b\n1,9\n2,8\n'      > "$Y"   # id 3 has no match -> "review"

{
  printf '%s\n' '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"probe","version":"0"}}}'
  printf '%s\n' '{"jsonrpc":"2.0","method":"notifications/initialized"}'
  printf '%s\n' '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
  printf '%s\n' "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"check_join\",\"arguments\":{\"left\":\"$X\",\"right\":\"$Y\",\"by\":\"id\",\"type\":\"inner\"}}}"
  perl -e 'select(undef,undef,undef,3)'   # hold stdin ~3s so the server drains
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

init = resps.get(1, {}).get("result", {})
print("initialize -> protocolVersion:", init.get("protocolVersion"),
      "| server:", init.get("serverInfo", {}).get("name"))

names = [t.get("name") for t in resps.get(2, {}).get("result", {}).get("tools", [])]
print("tools/list ->", names)

call = resps.get(3, {}).get("result", {})
text = (call.get("content") or [{}])[0].get("text")
print("tools/call -> isError:", call.get("isError"))
print("tools/call -> verdict:", text)

try:
    verdict = json.loads(text)
    assert {"check_join", "check_file", "check_change"} <= set(names)
    assert "list_r_sessions" not in names
    assert verdict.get("overall_status") == "review"
    assert verdict["checks"][0]["check"] == "join_integrity"
    print("\nRESULT: PASS - initialize + tools/list + tools/call verified end to end.")
except Exception as e:
    print("\nRESULT: FAIL ->", repr(e))
    sys.exit(1)
PY
