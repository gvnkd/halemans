import json
import os
import re
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse, parse_qs

DEFAULT_PORT = 18082
SPACE = "DEV"

SEEDED_PAGES = {
    "1001": {
        "title": "dev-host-01",
        "body": "<p>owner: team-sre</p>"
                "<p>Dev zabbix host seeded by seed-zabbix.sh; runs the halemans.test.trigger item.</p>"
                "<p>runbook: https://wiki.example/runbooks/dev-host-01</p>",
    },
    "1002": {
        "title": "dev-cpu-sim",
        "body": "<p>owner: team-sre</p>"
                "<p>Grafana check simulating CPU load on dev-host-01.</p>"
                "<p>runbook: https://wiki.example/runbooks/dev-cpu-sim</p>",
    },
    "1003": {
        "title": "payments-api",
        "body": "<p>owner: team-payments</p>"
                "<p>Payments service, dev instance.</p>"
                "<p>runbook: https://wiki.example/runbooks/payments-api</p>",
    },
}


def page_view(page_id, page):
    return {
        "id": page_id,
        "title": page["title"],
        "type": "page",
        "body": {"view": {"value": page["body"]}},
        "_links": {"webui": f"/spaces/{SPACE}/pages/{page_id}", "base": ""},
    }


class Handler(BaseHTTPRequestHandler):
    server_version = "MockConfluence/1.0"

    def _send(self, code, payload):
        data = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _authorized(self):
        expected = "Bearer " + os.environ.get("CONFLUENCE_TOKEN", "")
        if self.headers.get("Authorization") != expected:
            self._send(401, {"message": "unauthorized"})
            return False
        return True

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")
        if path == "/health":
            self._send(200, {"status": "ok"})
            return
        if not self._authorized():
            return
        if path == "/rest/api/content/search":
            self._search(parse_qs(parsed.query))
            return
        m = re.fullmatch(r"/rest/api/content/(\w+)", path)
        if m:
            self._content(m.group(1))
            return
        self._send(404, {"message": "not found"})

    def _search(self, query):
        cql = query.get("cql", [""])[0]
        m = re.search(r'text\s*~\s*"([^"]+)"', cql, re.IGNORECASE)
        term = m.group(1).lower() if m else ""
        results = []
        m_type = re.search(r"type\s*=\s*(\w+)", cql, re.IGNORECASE)
        if m_type is None or m_type.group(1).lower() == "page":
            for page_id, page in SEEDED_PAGES.items():
                haystack = (page["title"] + " " + page["body"]).lower()
                if not term or term in haystack:
                    results.append(page_view(page_id, page))
        self._send(200, {
            "results": results,
            "size": len(results),
            "totalSize": len(results),
        })

    def _content(self, page_id):
        page = SEEDED_PAGES.get(page_id)
        if page is None:
            self._send(404, {"message": f"No content found with id {page_id}"})
            return
        self._send(200, page_view(page_id, page))


def main():
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    else:
        port = int(os.environ.get("MOCK_CONFLUENCE_PORT", DEFAULT_PORT))
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    print(f"mock-confluence listening on 127.0.0.1:{port}", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()
