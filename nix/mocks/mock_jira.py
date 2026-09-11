import json
import os
import re
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse, parse_qs

DEFAULT_PORT = 18083

SEEDED_ISSUES = {
    "DEV-100": {
        "id": "10000",
        "project": "DEV",
        "summary": "Closed old ticket",
        "description": "",
        "labels": [],
        "status": {"name": "Done", "statusCategory": {"key": "done", "name": "Done"}},
    },
    "DEV-101": {
        "id": "10001",
        "project": "DEV",
        "summary": "Investigate halemans test trigger on dev-host-01",
        "description": "Investigate the halemans test trigger firing on the dev zabbix host.",
        "labels": ["dev-host-01"],
        "status": {"name": "Open", "statusCategory": {"key": "indeterminate", "name": "In Progress"}},
    },
}


def clause_match(issue, clause):
    clause = clause.strip()
    # NB: jql_match strips parens before splitting, so `project in (A, B)`
    # arrives here as `project in A, B`.
    m = re.fullmatch(r"project\s+in\s+(.+)", clause, re.IGNORECASE)
    if m:
        projects = [p.strip().strip('"').lower() for p in m.group(1).split(",") if p.strip()]
        return issue["project"].lower() in projects
    m = re.fullmatch(r"project\s*=\s*(.+)", clause, re.IGNORECASE)
    if m:
        return issue["project"].lower() == m.group(1).strip().strip('"').lower()
    m = re.fullmatch(r"statusCategory\s*!=\s*(.+)", clause, re.IGNORECASE)
    if m:
        return issue["status"]["statusCategory"]["key"].lower() != m.group(1).strip().strip('"').lower()
    m = re.fullmatch(r"labels\s*~\s*(.+)", clause, re.IGNORECASE)
    if m:
        term = m.group(1).strip().strip('"').lower()
        return any(term in label.lower() for label in issue["labels"])
    m = re.fullmatch(r"text\s*~\s*(.+)", clause, re.IGNORECASE)
    if m:
        term = m.group(1).strip().strip('"').lower()
        return term in (issue["summary"] + " " + issue["description"]).lower()
    return False


def jql_match(issue, jql):
    normalized = jql.replace("(", " ").replace(")", " ")
    groups = re.split(r"\s+AND\s+", normalized, flags=re.IGNORECASE)
    for group in groups:
        alts = [a for a in re.split(r"\s+OR\s+", group, flags=re.IGNORECASE) if a.strip()]
        if alts and not any(clause_match(issue, alt) for alt in alts):
            return False
    return True


def search_view(issue):
    return {
        "key": issue["key"],
        "fields": {
            "summary": issue["summary"],
            "description": issue["description"],
            "labels": issue["labels"],
            "status": issue["status"],
        },
    }


def issue_view(issue):
    view = search_view(issue)
    view["id"] = issue["id"]
    view["self"] = f"/rest/api/3/issue/{issue['key']}"
    return view


class Handler(BaseHTTPRequestHandler):
    server_version = "MockJira/1.0"

    def _send(self, code, payload):
        data = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _authorized(self):
        expected = "Bearer " + os.environ.get("JIRA_TOKEN", "")
        if self.headers.get("Authorization") != expected:
            self._send(401, {"errorMessages": ["unauthorized"]})
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
        if path == "/rest/api/3/search":
            self._search(parse_qs(parsed.query))
            return
        if path == "/rest/api/3/myself":
            self._send(200, {"accountId": "mock-jira", "displayName": "Mock Jira"})
            return
        m = re.fullmatch(r"/rest/api/3/issue/([\w-]+)", path)
        if m:
            self._issue(m.group(1))
            return
        self._send(404, {"errorMessages": ["not found"]})

    def do_POST(self):
        parsed = urlparse(self.path)
        m = re.fullmatch(r"/debug/issue/([\w-]+)/status", parsed.path.rstrip("/"))
        if m:
            self._debug_set_status(m.group(1))
            return
        if not self._authorized():
            return
        if parsed.path.rstrip("/") == "/rest/api/3/issue":
            self._create()
            return
        self._send(404, {"errorMessages": ["not found"]})

    def _debug_set_status(self, key):
        length = int(self.headers.get("Content-Length", 0))
        try:
            body = json.loads(self.rfile.read(length) or b"{}")
        except json.JSONDecodeError:
            self._send(400, {"errorMessages": ["invalid json"]})
            return
        issue = self.server.issues.get(key)
        if issue is None:
            self._send(404, {"errorMessages": [f"Issue {key} does not exist"]})
            return
        name = body.get("status", "Open")
        category = body.get("category", "done" if name.lower() == "done" else "indeterminate")
        issue["status"] = {"name": name, "statusCategory": {"key": category, "name": name}}
        self._send(200, issue_view(issue))

    def _search(self, query):
        jql = query.get("jql", [""])[0]
        max_results = int(query.get("maxResults", ["50"])[0])
        matches = [i for i in self.server.issues.values() if jql_match(i, jql)]
        matches.sort(key=lambda i: i["key"])
        self._send(200, {
            "issues": [search_view(i) for i in matches[:max_results]],
            "total": len(matches),
        })

    def _issue(self, key):
        issue = self.server.issues.get(key)
        if issue is None:
            self._send(404, {"errorMessages": [f"Issue {key} does not exist"]})
            return
        self._send(200, issue_view(issue))

    def _create(self):
        length = int(self.headers.get("Content-Length", 0))
        try:
            body = json.loads(self.rfile.read(length) or b"{}")
        except json.JSONDecodeError:
            self._send(400, {"errorMessages": ["invalid json"]})
            return
        fields = body.get("fields", {})
        project = fields.get("project", {}).get("key", "DEV")
        summary = fields.get("summary", "")
        if project == "FAIL" or "FORCE500" in summary:
            self._send(500, {"errorMessages": ["forced failure"]})
            return
        num = self.server.next_nums.get(project, 1)
        self.server.next_nums[project] = num + 1
        issue_id = str(self.server.next_id)
        self.server.next_id += 1
        key = f"{project}-{num}"
        self.server.issues[key] = {
            "id": issue_id,
            "key": key,
            "project": project,
            "summary": summary,
            "description": fields.get("description", ""),
            "labels": fields.get("labels", []),
            "status": {"name": "Open", "statusCategory": {"key": "indeterminate", "name": "In Progress"}},
        }
        self._send(201, {
            "id": issue_id,
            "key": key,
            "self": f"/rest/api/3/issue/{key}",
        })


def main():
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    else:
        port = int(os.environ.get("MOCK_JIRA_PORT", DEFAULT_PORT))
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    server.issues = {}
    for key, issue in SEEDED_ISSUES.items():
        server.issues[key] = dict(issue, key=key)
    server.next_nums = {"DEV": 102}
    server.next_id = 10002
    print(f"mock-jira listening on 127.0.0.1:{port}", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()
