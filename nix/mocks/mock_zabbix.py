# Mock Zabbix JSON-RPC subset for the alert metric chart: trigger.get with
# selectItems + paged history.get, deterministic series. Auth: Bearer
# ZABBIX_TOKEN (or MOCK_ZABBIX_TOKEN). Pure stdlib, mirrors mock_grafana.py
# conventions. /debug/stats exposes request counters for cache assertions.
import json
import math
import os
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

DEFAULT_PORT = 18087
TOKEN = os.environ.get("MOCK_ZABBIX_TOKEN") or os.environ.get("ZABBIX_TOKEN", "")

TRIGGERS = {
    "42": {
        "triggerid": "42",
        "items": [
            {
                "itemid": "1001",
                "key_": "system.cpu.load",
                "name": "CPU load",
                "value_type": "0",
                "units": "",
            },
            {
                "itemid": "1002",
                "key_": "vm.memory.util",
                "name": "Memory utilization",
                "value_type": "3",
                "units": "%",
            },
        ],
    },
    # a trigger whose only item is text-valued: no numeric metric
    "99": {
        "triggerid": "99",
        "items": [
            {
                "itemid": "2001",
                "key_": "agent.variant",
                "name": "Agent variant",
                "value_type": "4",
                "units": "",
            }
        ],
    },
}

STEP_SECONDS = 60


def seeded_state():
    return {"history_requests": 0, "trigger_requests": 0}


def error(message):
    return {"message": message}


def history_rows(itemid, time_from, time_till, limit):
    offset = int(itemid) % 97
    rows = []
    clock = max(int(time_from), 0)
    clock -= clock % STEP_SECONDS
    while clock <= int(time_till) and len(rows) < limit:
        value = round(50 + 40 * math.sin((clock % 7200) / 7200.0 * math.pi) + offset, 3)
        rows.append({"itemid": itemid, "clock": str(clock), "value": str(value)})
        clock += STEP_SECONDS
    return rows


class Handler(BaseHTTPRequestHandler):
    server_version = "MockZabbix/1.0"

    def _send(self, code, payload):
        data = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _rpc(self, method, params):
        if method == "trigger.get":
            self.server.state["trigger_requests"] += 1
            triggerids = params.get("triggerids") or []
            return 200, {"jsonrpc": "2.0", "result": [TRIGGERS[t] for t in triggerids if t in TRIGGERS]}
        if method == "history.get":
            self.server.state["history_requests"] += 1
            itemids = params.get("itemids") or []
            if not itemids:
                return 200, {"jsonrpc": "2.0", "result": []}
            limit = int(params.get("limit", 10000))
            rows = history_rows(itemids[0], params.get("time_from", 0), params.get("time_till", 2**31), limit)
            return 200, {"jsonrpc": "2.0", "result": rows}
        return 400, {"jsonrpc": "2.0", "error": {"code": -32601, "message": f"method {method} not supported"}}

    def do_GET(self):
        if self.path.rstrip("/") == "/health":
            self._send(200, {"status": "ok"})
            return
        if self.path.rstrip("/") == "/debug/stats":
            self._send(200, self.server.state)
            return
        self._send(404, error("not found"))

    def do_POST(self):
        if self.path.rstrip("/") == "/debug/reset":
            self.server.state = seeded_state()
            self._send(200, {"status": "reset"})
            return
        if self.path.rstrip("/") != "/api_jsonrpc.php":
            self._send(404, error("not found"))
            return
        if self.headers.get("Authorization") != "Bearer " + TOKEN:
            self._send(401, error("unauthorized"))
            return
        length = int(self.headers.get("Content-Length", 0))
        try:
            body = json.loads(self.rfile.read(length) or b"{}")
            method, params = body["method"], body.get("params", {})
        except (json.JSONDecodeError, KeyError):
            self._send(400, {"jsonrpc": "2.0", "error": {"code": -32700, "message": "parse error"}})
            return
        code, payload = self._rpc(method, params)
        self._send(code, payload)


def main():
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    else:
        port = int(os.environ.get("MOCK_ZABBIX_PORT", DEFAULT_PORT))
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    server.state = seeded_state()
    print(f"mock-zabbix listening on 127.0.0.1:{port}", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()
