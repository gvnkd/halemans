import json
import os
import re
import sys
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse

DEFAULT_PORT = 18084
MODEL = "mock-llm-1"

DISK_ANALYSIS = """\
## Analysis

The alert pattern indicates **disk pressure**: the fired check tracks
filesystem usage and the recent events show steady growth rather than a spike.
The CMDB runbook for this host lists accumulating application logs as the
usual offender.

## Recommended actions

1. Inspect filesystem usage (`df -h`) and the largest consumers.
2. Rotate or prune application logs per the runbook.
3. If the growth is legitimate, expand the volume and raise the threshold.

```json
{
  "probable_cause": "Filesystem usage above threshold, most likely from accumulating application logs",
  "confidence": 0.8,
  "suggested_actions": [
    "Check filesystem usage with df -h on the affected host",
    "Rotate or prune logs per the CMDB runbook",
    "Expand the volume if the growth is legitimate"
  ],
  "references": [
    "https://wiki.example/runbooks/dev-host-01"
  ]
}
```
"""

GENERIC_ANALYSIS = """\
## Analysis

The alert does not match a known specific failure pattern. Based on the alert
metadata and recent events this looks like a generic service degradation; no
strong host-level signal is present in the provided context.

## Recommended actions

1. Confirm the scope: single host or service-wide.
2. Check the service's own metrics and logs around the alert start time.
3. Escalate to the owning team if the alert persists.

```json
{
  "probable_cause": "Insufficient context for a specific cause; likely an application-level degradation",
  "confidence": 0.4,
  "suggested_actions": [
    "Verify whether the alert is host-scoped or service-scoped",
    "Inspect service logs around the alert start time",
    "Escalate to the owning team if it persists"
  ],
  "references": []
}
```
"""

MALFORMED_ANALYSIS = """\
## Analysis

Disk pressure indicators are present, but the structured block below is
intentionally malformed for failure-path testing.

```json
{ "probable_cause": "broken on purpose", "confidence": 0.5, ]
```
"""


def last_user_content(body):
    for message in reversed(body.get("messages", [])):
        if message.get("role") != "user":
            continue
        content = message.get("content", "")
        if isinstance(content, list):
            return " ".join(
                part.get("text", "") for part in content if isinstance(part, dict)
            )
        return content
    return ""


class Handler(BaseHTTPRequestHandler):
    server_version = "MockLlm/1.0"

    def _send(self, code, payload, headers=None):
        data = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        for key, value in (headers or {}).items():
            self.send_header(key, value)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _read_body(self):
        length = int(self.headers.get("Content-Length", 0))
        try:
            return json.loads(self.rfile.read(length) or b"{}")
        except json.JSONDecodeError:
            return None

    def do_GET(self):
        path = urlparse(self.path).path.rstrip("/")
        if path == "/health":
            self._send(200, {"status": "ok"})
            return
        if path == "/v1/models":
            self._send(200, {"data": [{"id": MODEL}]})
            return
        self._send(404, {"error": {"message": "not found"}})

    def do_POST(self):
        path = urlparse(self.path).path.rstrip("/")
        if path == "/debug/reset":
            self.server.forced_failures = []
            self._send(200, {"ok": True})
            return
        m = re.fullmatch(r"/debug/fail/(429|500|malformed)", path)
        if m:
            self._debug_fail(m.group(1))
            return
        if path == "/v1/chat/completions":
            self._completion()
            return
        self._send(404, {"error": {"message": "not found"}})

    def _debug_fail(self, kind):
        body = self._read_body()
        if body is None:
            self._send(400, {"error": {"message": "invalid json"}})
            return
        times = max(1, int(body.get("times", 1)))
        self.server.forced_failures.extend([kind] * times)
        self._send(200, {"ok": True, "pending": len(self.server.forced_failures)})

    def _completion(self):
        body = self._read_body()
        if body is None:
            self._send(400, {"error": {"message": "invalid json"}})
            return
        if self.server.forced_failures:
            kind = self.server.forced_failures.pop(0)
            if kind == "429":
                self._send(429, {
                    "error": {"message": "rate limit exceeded", "type": "rate_limit_error"},
                }, headers={"Retry-After": "1"})
                return
            if kind == "500":
                self._send(500, {
                    "error": {"message": "internal error", "type": "server_error"},
                })
                return
            self._respond(MALFORMED_ANALYSIS, body)
            return
        content = last_user_content(body)
        analysis = DISK_ANALYSIS if "disk" in content.lower() else GENERIC_ANALYSIS
        self._respond(analysis, body)

    def _respond(self, analysis, body):
        self.server.completion_count += 1
        prompt_tokens = max(1, len(last_user_content(body)) // 4)
        completion_tokens = max(1, len(analysis) // 4)
        self._send(200, {
            "id": f"chatcmpl-mock-{self.server.completion_count}",
            "object": "chat.completion",
            "created": int(time.time()),
            "model": body.get("model", MODEL),
            "choices": [{
                "index": 0,
                "message": {"role": "assistant", "content": analysis},
                "finish_reason": "stop",
            }],
            "usage": {
                "prompt_tokens": prompt_tokens,
                "completion_tokens": completion_tokens,
                "total_tokens": prompt_tokens + completion_tokens,
            },
        })


def main():
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    else:
        port = int(os.environ.get("MOCK_LLM_PORT", DEFAULT_PORT))
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    server.forced_failures = []
    server.completion_count = 0
    print(f"mock-llm listening on 127.0.0.1:{port}", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()
