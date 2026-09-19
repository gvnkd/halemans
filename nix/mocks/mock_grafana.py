# Mock Grafana subset for the alert metric chart: provisioning alert-rule
# GET + /api/ds/query range-query POST, returning a deterministic two-series
# victoriametrics-style frame response. Auth: Bearer GRAFANA_TOKEN (or
# MOCK_GAFANA_TOKEN). Pure stdlib, mirrors mock_assets.py conventions.
import json
import math
import os
import re
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse

DEFAULT_PORT = 18086
TOKEN = os.environ.get("MOCK_GRAFANA_TOKEN") or os.environ.get("GRAFANA_TOKEN", "")

RULE_UID = "mock-rule-cpu"
RULE_EXPR = 'avg(rate(node_cpu_seconds_total{mode!="idle"}[5m])) by (instance)'

RULE = {
    "uid": RULE_UID,
    "title": "Mock CPU saturation",
    "data": [
        {
            "refId": "A",
            "queryType": "",
            "datasourceUid": "mock-victoriametrics",
            "model": {
                "expr": RULE_EXPR,
                "instant": False,
                "range": True,
                "datasource": {"type": "prometheus", "uid": "mock-victoriametrics"},
            },
        }
    ],
}

STEP_MS = 60 * 1000


def seeded_state():
    return {"fail500": 0}


def error(message):
    return {"message": message}


def series_frame(name, times, values):
    return {
        "schema": {
            "fields": [
                {"name": "Time", "type": "time", "typeInfo": {"frame": "time.Time"}},
                {"name": name, "type": "number", "typeInfo": {"frame": "float64"}},
            ]
        },
        "data": {"values": [times, values]},
    }


def query_response(from_ms, to_ms):
    times = list(range(int(from_ms), int(to_ms) + 1, STEP_MS)) or [int(from_ms)]
    hours = [(t - times[0]) / 3600000.0 for t in times]
    values_a = [round(50 + 40 * math.sin(h * math.pi) + h * 5, 3) for h in hours]
    values_b = [round(20 + 10 * math.cos(h * math.pi), 3) for h in hours]
    return {
        "results": {
            "A": {
                "status": 200,
                "frames": [
                    series_frame("instance-1", times, values_a),
                    series_frame("instance-2", times, values_b),
                ],
            }
        }
    }


class Handler(BaseHTTPRequestHandler):
    server_version = "MockGrafana/1.0"

    def _send(self, code, payload):
        data = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _authorized(self):
        if self.headers.get("Authorization") != "Bearer " + TOKEN:
            self._send(401, error("unauthorized"))
            return False
        return True

    def _maybe_fail(self):
        if self.server.state["fail500"] > 0:
            self.server.state["fail500"] -= 1
            self._send(500, error("forced failure"))
            return True
        return False

    def do_GET(self):
        path = urlparse(self.path).path.rstrip("/")
        if path == "/health":
            self._send(200, {"status": "ok"})
            return
        if not self._authorized():
            return
        if self._maybe_fail():
            return
        m = re.fullmatch(r"/api/v1/provisioning/alert-rules/([\w-]+)", path)
        if m:
            if m.group(1) != RULE_UID:
                self._send(404, error("alert rule not found"))
                return
            self._send(200, RULE)
            return
        self._send(404, error("not found"))

    def do_POST(self):
        path = urlparse(self.path).path.rstrip("/")
        if path == "/debug/reset":
            self.server.state = seeded_state()
            self._send(200, {"status": "reset"})
            return
        m = re.fullmatch(r"/debug/fail/(\d+)", path)
        if m:
            length = int(self.headers.get("Content-Length", 0))
            try:
                body = json.loads(self.rfile.read(length) or b"{}")
            except json.JSONDecodeError:
                body = {}
            self.server.state["fail500"] = int(body.get("times", 1))
            self._send(200, {"status": "armed", "code": int(m.group(1)),
                             "times": self.server.state["fail500"]})
            return
        if path == "/api/ds/query":
            if not self._authorized():
                return
            if self._maybe_fail():
                return
            length = int(self.headers.get("Content-Length", 0))
            try:
                body = json.loads(self.rfile.read(length) or b"{}")
                from_ms = float(body["from"])
                to_ms = float(body["to"])
            except (json.JSONDecodeError, KeyError, ValueError):
                self._send(400, error("bad query request"))
                return
            self._send(200, query_response(from_ms, to_ms))
            return
        self._send(404, error("not found"))


def main():
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    else:
        port = int(os.environ.get("MOCK_GRAFANA_PORT", DEFAULT_PORT))
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    server.state = seeded_state()
    print(f"mock-grafana listening on 127.0.0.1:{port}", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()
