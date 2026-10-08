# Mock Mattermost v4 API subset for the notification channel integration.
# Covers exactly what Application.Service.Mattermost.Api needs:
#   POST /api/v4/posts                      create post (root or thread reply)
#   GET  /api/v4/posts/{id}                 fetch post
#   PUT  /api/v4/posts/{id}/patch           edit message/props (status sync)
#   PUT  /api/v4/posts/{id}                 edit message/props (legacy alias)
#   DELETE /api/v4/posts/{id}               delete post (deleteOnClose path)
#   GET  /api/v4/channels/{id}/posts         channel posts (per_page + before cursor)
#   PUT  /api/v4/channels/{id}/banner        set channel banner {text, color}
#   GET  /api/v4/channels/{id}/banner        current channel banner
#   DELETE /api/v4/channels/{id}/banner      clear channel banner
#   GET  /api/v4/users/me/teams             bot team memberships
#   GET  /api/v4/teams/{id}/channels/name/{c}  channel by team id + name
#   GET  /api/v4/teams/name/{team}          resolve team name -> id
#   GET  /api/v4/channels/name/{t}/{c}      resolve channel name -> id
#   GET  /api/v4/users/me                   bot identity check
# Auth: Authorization: Bearer $MATTERMOST_TOKEN (default mock-mattermost-token).
#
# Debug surface (no auth, mirrors mock_jira.py conventions):
#   GET  /debug/posts                       all posts, creation order
#   GET  /debug/posts/{id}                  one post
#   GET  /debug/action-calls                recorded integration callbacks
#   GET  /debug/banners                     all channel banners, by channel id
#   POST /debug/click {post_id, action, user_name, ...}
#       simulates a user clicking an interactive message button: finds the
#       action in the post's props.attachments and POSTs to its
#       integration.url exactly like a real Mattermost server does
#   POST /debug/reset                       drop all state
# Pure stdlib, mirrors mock_jira.py conventions.

import json
import os
import re
import sys
import time
import urllib.request
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlparse

DEFAULT_PORT = 18088
DEFAULT_TOKEN = "mock-mattermost-token"


def now_ms():
    return int(time.time() * 1000)


class Handler(BaseHTTPRequestHandler):
    server_version = "MockMattermost/1.0"

    def log_message(self, fmt, *args):
        pass

    def _send(self, code, payload):
        data = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _body(self):
        length = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(length) if length else b"{}"
        try:
            return json.loads(raw or b"{}")
        except json.JSONDecodeError:
            return None

    def _authorized(self):
        expected = "Bearer " + os.environ.get("MATTERMOST_TOKEN", DEFAULT_TOKEN)
        if self.headers.get("Authorization") != expected:
            self._send(401, {"id": "api.context.session_expired.app_error", "message": "unauthorized"})
            return False
        return True

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")
        if path == "/health":
            self._send(200, {"status": "ok"})
            return
        if path.startswith("/debug/"):
            self._debug_get(path)
            return
        if not self._authorized():
            return
        m = re.fullmatch(r"/api/v4/posts/([\w-]+)", path)
        if m:
            self._get_post(m.group(1))
            return
        m = re.fullmatch(r"/api/v4/channels/([\w-]+)/posts", path)
        if m:
            self._channel_posts(m.group(1), parsed.query)
            return
        m = re.fullmatch(r"/api/v4/channels/([\w-]+)/banner", path)
        if m:
            banner = self.server.banners.get(m.group(1))
            if banner is None:
                self._send(404, {"id": "api.context.404.app_error", "message": "banner not found"})
            else:
                self._send(200, dict(banner))
            return
        m = re.fullmatch(r"/api/v4/teams/name/([\w-]+)", path)
        if m:
            self._send(200, self.server.team(m.group(1)))
            return
        m = re.fullmatch(r"/api/v4/channels/name/([\w-]+)/([\w-]+)", path)
        if m:
            self._send(200, self.server.channel(m.group(1), m.group(2)))
            return
        if path == "/api/v4/users/me":
            self._send(200, {"id": "bot-mock-mattermost", "username": "halemans", "email": "halemans@localhost"})
            return
        # Bot membership listing + team-id channel lookup: the client's
        # resolveChannel uses ONLY these two (the channels/name/{t}/{c}
        # shortcut 404s on some real servers).
        if path == "/api/v4/users/me/teams":
            self._send(200, list(self.server.teams.values()))
            return
        m = re.fullmatch(r"/api/v4/teams/([\w-]+)/channels/name/([\w-]+)", path)
        if m:
            for channel in self.server.channels.values():
                if channel["team_id"] == m.group(1) and channel["name"] == m.group(2):
                    self._send(200, channel)
                    return
            for team in self.server.teams.values():
                if team["id"] == m.group(1):
                    self._send(200, self.server.channel(team["name"], m.group(2)))
                    return
            self._send(404, {"id": "api.context.404.app_error", "message": "channel not found"})
            return
        self._send(404, {"id": "api.context.404.app_error", "message": "not found"})

    def do_POST(self):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")
        if path.startswith("/debug/"):
            self._debug_post(path)
            return
        if not self._authorized():
            return
        if path == "/api/v4/posts":
            self._create_post()
            return
        self._send(404, {"id": "api.context.404.app_error", "message": "not found"})

    def do_PUT(self):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")
        if not self._authorized():
            return
        m = re.fullmatch(r"/api/v4/posts/([\w-]+)/patch", path)
        if m:
            self._patch_post(m.group(1))
            return
        m = re.fullmatch(r"/api/v4/posts/([\w-]+)", path)
        if m:
            self._patch_post(m.group(1))
            return
        m = re.fullmatch(r"/api/v4/channels/([\w-]+)/banner", path)
        if m:
            body = self._body()
            if body is None:
                self._send(400, {"id": "api.context.invalid_body_param.app_error", "message": "invalid json"})
                return
            self.server.banners[m.group(1)] = {
                "channel_id": m.group(1),
                "text": body.get("text", ""),
                "color": body.get("color", ""),
            }
            self._send(200, dict(self.server.banners[m.group(1)]))
            return
        self._send(404, {"id": "api.context.404.app_error", "message": "not found"})

    def do_DELETE(self):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")
        if not self._authorized():
            return
        m = re.fullmatch(r"/api/v4/posts/([\w-]+)", path)
        if m:
            post = self.server.posts.pop(m.group(1), None)
            if post is None:
                self._send(404, {"id": "api.context.404.app_error", "message": "post not found"})
            else:
                self._send(200, {"status": "OK"})
            return
        m = re.fullmatch(r"/api/v4/channels/([\w-]+)/banner", path)
        if m:
            self.server.banners.pop(m.group(1), None)
            self._send(200, {"status": "OK"})
            return
        self._send(404, {"id": "api.context.404.app_error", "message": "not found"})

    def _create_post(self):
        body = self._body()
        if body is None:
            self._send(400, {"id": "api.context.invalid_body_param.app_error", "message": "invalid json"})
            return
        channel_id = body.get("channel_id", "")
        if not channel_id:
            self._send(400, {"id": "api.post.create_post.channel_id.app_error", "message": "channel_id required"})
            return
        if channel_id not in self.server.channels:
            self._send(404, {"id": "api.context.404.app_error", "message": f"channel {channel_id} not found"})
            return
        root_id = body.get("root_id") or ""
        if root_id and root_id not in self.server.posts:
            self._send(404, {"id": "api.context.404.app_error", "message": f"root post {root_id} not found"})
            return
        post = {
            "id": f"post-{self.server.next_post_id}",
            "channel_id": channel_id,
            "root_id": root_id,
            "message": body.get("message", ""),
            "props": body.get("props", {}),
            "type": body.get("type", ""),
            "user_id": "bot-mock-mattermost",
            "create_at": now_ms(),
            "update_at": now_ms(),
        }
        self.server.next_post_id += 1
        self.server.posts[post["id"]] = post
        self._send(201, dict(post))

    def _get_post(self, post_id):
        post = self.server.posts.get(post_id)
        if post is None:
            self._send(404, {"id": "api.context.404.app_error", "message": f"post {post_id} not found"})
            return
        self._send(200, dict(post))

    def _channel_posts(self, channel_id, query):
        # Mirrors MM: newest-first page, per_page cap, "before" taking a POST
        # ID as the anchor (a create_at timestamp is rejected with 400 — real
        # MM semantics, hit on mm.officesvc.bz). Channel order across the
        # whole listing must be stable for the before-cursor walk, so ties
        # break by post id.
        params = parse_qs(query)
        per_page = min(int(params.get("per_page", ["60"])[0]), 200)
        before_id = params.get("before", [""])[0]
        posts = [p for p in self.server.posts.values() if p["channel_id"] == channel_id]
        if before_id:
            anchor = self.server.posts.get(before_id)
            if anchor is None:
                self._send(400, {"id": "api.context.400.app_error", "message": f"post {before_id} not found"})
                return
            posts = [p for p in posts if p["create_at"] < anchor["create_at"]]
        posts.sort(key=lambda p: (p["create_at"], p["id"]), reverse=True)
        page = posts[:per_page]
        self._send(200, {"order": [p["id"] for p in page], "posts": {p["id"]: dict(p) for p in page}})

    def _patch_post(self, post_id):
        post = self.server.posts.get(post_id)
        if post is None:
            self._send(404, {"id": "api.context.404.app_error", "message": f"post {post_id} not found"})
            return
        body = self._body()
        if body is None:
            self._send(400, {"id": "api.context.invalid_body_param.app_error", "message": "invalid json"})
            return
        for key in ("message", "props", "type", "file_ids"):
            if key in body:
                post[key] = body[key]
        post["update_at"] = now_ms()
        self._send(200, dict(post))

    def _debug_get(self, path):
        if path == "/debug/posts":
            self._send(200, {"posts": list(self.server.posts.values())})
            return
        m = re.fullmatch(r"/debug/posts/([\w-]+)", path)
        if m:
            post = self.server.posts.get(m.group(1))
            if post is None:
                self._send(404, {"message": "not found"})
                return
            self._send(200, dict(post))
            return
        if path == "/debug/action-calls":
            self._send(200, {"calls": self.server.action_calls})
            return
        if path == "/debug/banners":
            self._send(200, {"banners": list(self.server.banners.values())})
            return
        self._send(404, {"message": "not found"})

    def _debug_post(self, path):
        if path == "/debug/reset":
            self.server.reset()
            self._send(200, {"status": "reset"})
            return
        if path == "/debug/click":
            self._click()
            return
        if path == "/debug/action-echo":
            # Built-in integration sink: point a post action's integration.url
            # here to exercise /debug/click without an external HTTP listener.
            self.server.action_calls.append(
                {"action": "echo", "post_id": None, "url": path, "payload": self._body() or {}}
            )
            self._send(200, {"ok": True})
            return
        self._send(404, {"message": "not found"})

    def _click(self):
        body = self._body()
        if body is None:
            self._send(400, {"message": "invalid json"})
            return
        post_id = body.get("post_id", "")
        post = self.server.posts.get(post_id)
        if post is None:
            self._send(404, {"message": f"post {post_id} not found"})
            return
        action_name = body.get("action", "")
        action = None
        for attachment in post.get("props", {}).get("attachments", []):
            for candidate in attachment.get("actions", []):
                if candidate.get("name") == action_name or candidate.get("id") == action_name:
                    action = candidate
                    break
            if action:
                break
        if action is None:
            self._send(404, {"message": f"action {action_name!r} not found on post {post_id}"})
            return
        integration = action.get("integration", {})
        url = integration.get("url", "")
        if not url:
            self._send(400, {"message": "action has no integration url"})
            return
        context = dict(integration.get("context", {}))
        for key, value in body.get("context", {}).items():
            context[key] = value
        context.setdefault("post_id", post_id)
        context.setdefault("channel_id", post["channel_id"])
        payload = {
            "context": context,
            "user_id": body.get("user_id", "user-mock-clicker"),
            "user_name": body.get("user_name", "mock-clicker"),
        }
        call = {"action": action_name, "post_id": post_id, "url": url, "payload": payload}
        try:
            req = urllib.request.Request(
                url,
                data=json.dumps(payload).encode(),
                headers={"Content-Type": "application/json"},
                method="POST",
            )
            with urllib.request.urlopen(req, timeout=10) as resp:
                call["upstream_status"] = resp.status
                call["upstream_body"] = resp.read().decode(errors="replace")[:2000]
        except Exception as exc:  # record and report; a dead endpoint is a test failure, not a mock crash
            call["upstream_status"] = None
            call["upstream_body"] = f"{type(exc).__name__}: {exc}"
        self.server.action_calls.append(call)
        if call["upstream_status"] is None:
            self._send(502, call)
        else:
            self._send(200, call)
        return


class Server(ThreadingHTTPServer):
    def __init__(self, addr, handler):
        super().__init__(addr, handler)
        # Post ids are NOT reset by /debug/reset: they stay unique across
        # epochs like a real MM server, so mattermost_posts.root_post_id
        # lookups never collide between test examples.
        self.next_post_id = 1
        self.reset()

    def reset(self):
        self.posts = {}
        self.channels = {}
        self.channel_names = {}
        self.action_calls = []
        self.banners = {}
        # next_post_id deliberately survives: see __init__.
        # The client's resolveChannel lists bot team memberships BEFORE any
        # post exists, so the default team must survive /debug/reset.
        self.teams = {}
        self.team("mock")

    def team(self, name):
        if name not in self.teams:
            self.teams[name] = {"id": f"team-{len(self.teams) + 1}", "name": name, "display_name": name}
        return self.teams[name]

    def channel(self, team_name, channel_name):
        key = (team_name, channel_name)
        if key not in self.channel_names:
            team = self.team(team_name)
            channel = {
                "id": f"chan-{len(self.channels) + 1}",
                "team_id": team["id"],
                "name": channel_name,
                "display_name": channel_name,
            }
            self.channels[channel["id"]] = channel
            self.channel_names[key] = channel
        return self.channel_names[key]


def main():
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    else:
        port = int(os.environ.get("MOCK_MATTERMOST_PORT", DEFAULT_PORT))
    server = Server(("127.0.0.1", port), Handler)
    server.team("mock")
    print(f"mock-mattermost listening on 127.0.0.1:{port}", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()
