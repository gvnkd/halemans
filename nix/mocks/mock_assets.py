import json
import os
import re
import struct
import sys
import time
import zlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse, parse_qs

DEFAULT_PORT = 18085
BASE = "/rest/assets/latest"

# Distinct visible 16x16 icon PNGs per icon id (1x1 transparent squares made
# every card look icon-less).
def solid_png(rgb, size=16):
    def chunk(typ, data):
        return (struct.pack(">I", len(data)) + typ + data
                + struct.pack(">I", zlib.crc32(typ + data) & 0xffffffff))
    ihdr = struct.pack(">IIBBBBB", size, size, 8, 2, 0, 0, 0)
    raw = b"".join(b"\x00" + bytes(rgb) * size for _ in range(size))
    return (b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", ihdr)
            + chunk(b"IDAT", zlib.compress(raw)) + chunk(b"IEND", b""))

ICON_COLORS = {
    25: (70, 130, 180),   # server: steel blue
    26: (60, 179, 113),   # database: medium sea green
    27: (218, 165, 32),   # cluster: goldenrod
}

def icon_png(icon_id):
    return solid_png(ICON_COLORS.get(icon_id, (128, 128, 128)))

SCHEMA = {
    "id": 110,
    "name": "Capacity CMDB",
    "objectSchemaKey": "CHCMDB",
    "status": "Ok",
    "description": "Mock capacity CMDB",
    "created": "2026-09-01T09:00:00.000+0300",
    "updated": "2026-09-08T09:00:00.000+0300",
    "objectCount": 7,
    "objectTypeCount": 3,
}

OBJECT_TYPES = [
    {"id": 1001, "name": "Host", "type": 0, "objectSchemaId": 110, "position": 0,
     "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-01T09:00:00.000+0300",
     "objectCount": 3, "inherited": False, "abstractObjectType": False,
     "icon": {"id": 25, "name": "server", "url16": "/rest/insight/1.0/icon/25/icon.png", "url48": "/rest/insight/1.0/icon/25/icon.png"}},
    {"id": 1002, "name": "Database", "type": 0, "objectSchemaId": 110, "position": 1,
     "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-01T09:00:00.000+0300",
     "objectCount": 3, "inherited": False, "abstractObjectType": False,
     "icon": {"id": 26, "name": "database", "url16": "/rest/insight/1.0/icon/26/icon.png", "url48": "/rest/insight/1.0/icon/26/icon.png"}},
    {"id": 1003, "name": "Cluster", "type": 0, "objectSchemaId": 110, "position": 2,
     "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-01T09:00:00.000+0300",
     "objectCount": 1, "inherited": False, "abstractObjectType": False,
     "icon": {"id": 27, "name": "cluster", "url16": "/rest/insight/1.0/icon/27/icon.png", "url48": "/rest/insight/1.0/icon/27/icon.png"}},
]

STATUS_TYPES = {
    1: {"id": 1, "name": "Active", "category": 1},
    2: {"id": 2, "name": "Running", "category": 1},
    6: {"id": 6, "name": "Stopped", "category": 0},
}

ICONS = {
    25: {"id": 25, "name": "server", "url16": "/rest/insight/1.0/icon/25/icon.png", "url48": "/rest/insight/1.0/icon/25/icon.png"},
    26: {"id": 26, "name": "database", "url16": "/rest/insight/1.0/icon/26/icon.png", "url48": "/rest/insight/1.0/icon/26/icon.png"},
    27: {"id": 27, "name": "cluster", "url16": "/rest/insight/1.0/icon/27/icon.png", "url48": "/rest/insight/1.0/icon/27/icon.png"},
}


def attr(attr_id, name, value, type_id=None):
    type_attr = {"id": attr_id, "name": name, "label": name, "type": 0,
                 "defaultType": {"id": 0, "name": "Text"}, "editable": True,
                 "system": False, "indexed": True}
    if type_id is not None:
        type_attr["id"] = type_id
    return {"id": attr_id, "objectTypeAttribute": type_attr,
            "objectAttributeValues": [{"displayValue": value, "searchValue": value}]}


def status_attr(attr_id, status_id):
    status = STATUS_TYPES[status_id]
    return {"id": attr_id,
            "objectTypeAttribute": {"id": attr_id, "name": "Status", "label": "Status", "type": 7,
                                    "defaultType": {"id": 7, "name": "Status"}, "editable": True,
                                    "system": False, "indexed": True},
            "objectAttributeValues": [{"status": {"id": status["id"], "name": status["name"]},
                                       "displayValue": status["name"], "searchValue": status["name"]}]}


SEEDED_OBJECTS = [
    {
        "id": 10001, "label": "dev-host-01", "objectKey": "CHCMDB-10001",
        "avatar": {"url16": "/rest/insight/1.0/icon/25/icon.png", "url48": "/rest/insight/1.0/icon/25/icon.png"},
        "objectType": OBJECT_TYPES[0],
        "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-08T09:00:00.000+0300",
        "attributes": [
            attr(1, "Name", "dev-host-01"),
            attr(2, "Owner", "team-sre"),
            attr(3, "Cluster", "prod-eu-1"),
            attr(4, "IP", "10.0.0.11"),
            attr(5, "Datacenter", "dc-eu-1"),
            status_attr(6, 1),
            # Milestone 9: facet-source attributes (the "Sergey case" —
            # zabbix env is the zabbix service's env, the real one is here).
            attr(7, "Service", "PostgreSQL"),
            attr(8, "DB Cluster", "ibstaffcopdb01"),
            attr(9, "Environments", "PROD"),
            attr(10, "Team", "IT:RnD:DBA"),
            attr(11, "Location", "LV"),
        ],
    },
    {
        "id": 10002, "label": "dev-db-01", "objectKey": "CHCMDB-10002",
        "avatar": {"url16": "/rest/insight/1.0/icon/26/icon.png", "url48": "/rest/insight/1.0/icon/26/icon.png"},
        "objectType": OBJECT_TYPES[1],
        "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-08T09:00:00.000+0300",
        "attributes": [
            attr(1, "Name", "dev-db-01"),
            attr(2, "Owner", "team-dba"),
            attr(3, "Cluster", "prod-eu-1"),
            attr(4, "IP", "10.0.0.21"),
            attr(5, "Datacenter", "dc-eu-1"),
            status_attr(6, 2),
            attr(7, "Service", "PostgreSQL"),
            attr(8, "DB Cluster", "ibstaffcopdb02"),
            attr(9, "Environments", "PROD"),
            attr(10, "Team", "IT:RnD:DBA"),
            attr(11, "Location", "LV"),
        ],
    },
    {
        "id": 10003, "label": "prod-eu-1", "objectKey": "CHCMDB-10003",
        "avatar": {"url16": "/rest/insight/1.0/icon/27/icon.png", "url48": "/rest/insight/1.0/icon/27/icon.png"},
        "objectType": OBJECT_TYPES[2],
        "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-08T09:00:00.000+0300",
        "attributes": [
            attr(1, "Name", "prod-eu-1"),
            attr(2, "Owner", "team-sre"),
            attr(5, "Datacenter", "dc-eu-1"),
            status_attr(6, 1),
        ],
    },
    # Field-mapping demo hosts: Service/Location/Environments matrix. The
    # Environments attribute is a comma-separated list on two of them to
    # exercise the first-element mapping semantics.
    {
        "id": 10004, "label": "etcd-eu-01", "objectKey": "CHCMDB-10004",
        "avatar": {"url16": "/rest/insight/1.0/icon/25/icon.png", "url48": "/rest/insight/1.0/icon/25/icon.png"},
        "objectType": OBJECT_TYPES[0],
        "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-08T09:00:00.000+0300",
        "attributes": [
            attr(1, "Name", "etcd-eu-01"),
            attr(2, "Owner", "team-sre"),
            attr(4, "IP", "10.0.1.11"),
            status_attr(6, 1),
            attr(7, "Service", "ETCD"),
            attr(9, "Environments", "PROD,TEST"),
            attr(11, "Location", "EU"),
        ],
    },
    {
        "id": 10005, "label": "etcd-us-01", "objectKey": "CHCMDB-10005",
        "avatar": {"url16": "/rest/insight/1.0/icon/25/icon.png", "url48": "/rest/insight/1.0/icon/25/icon.png"},
        "objectType": OBJECT_TYPES[0],
        "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-08T09:00:00.000+0300",
        "attributes": [
            attr(1, "Name", "etcd-us-01"),
            attr(2, "Owner", "team-sre"),
            attr(4, "IP", "10.0.2.11"),
            status_attr(6, 1),
            attr(7, "Service", "ETCD"),
            attr(9, "Environments", "TEST"),
            attr(11, "Location", "US"),
        ],
    },
    {
        "id": 10006, "label": "pg-eu-01", "objectKey": "CHCMDB-10006",
        "avatar": {"url16": "/rest/insight/1.0/icon/26/icon.png", "url48": "/rest/insight/1.0/icon/26/icon.png"},
        "objectType": OBJECT_TYPES[1],
        "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-08T09:00:00.000+0300",
        "attributes": [
            attr(1, "Name", "pg-eu-01"),
            attr(2, "Owner", "team-dba"),
            attr(4, "IP", "10.0.1.21"),
            status_attr(6, 2),
            attr(7, "Service", "POSTGRES"),
            attr(9, "Environments", "PROD"),
            attr(11, "Location", "EU"),
        ],
    },
    {
        "id": 10007, "label": "pg-us-01", "objectKey": "CHCMDB-10007",
        "avatar": {"url16": "/rest/insight/1.0/icon/26/icon.png", "url48": "/rest/insight/1.0/icon/26/icon.png"},
        "objectType": OBJECT_TYPES[1],
        "created": "2026-09-01T09:00:00.000+0300", "updated": "2026-09-08T09:00:00.000+0300",
        "attributes": [
            attr(1, "Name", "pg-us-01"),
            attr(2, "Owner", "team-dba"),
            attr(4, "IP", "10.0.2.21"),
            status_attr(6, 2),
            attr(7, "Service", "POSTGRES"),
            attr(9, "Environments", "TEST, PROD"),
            attr(11, "Location", "US"),
        ],
    },
]


def seeded_state():
    return {"objects": {obj["id"]: json.loads(json.dumps(obj)) for obj in SEEDED_OBJECTS},
            "fail500": 0}


def flat_attributes(obj):
    result = {}
    for a in obj["attributes"]:
        name = a["objectTypeAttribute"]["name"]
        values = []
        for v in a["objectAttributeValues"]:
            if "displayValue" in v:
                values.append(str(v["displayValue"]))
            elif "value" in v:
                values.append(str(v["value"]))
            elif "status" in v:
                values.append(v["status"]["name"])
        result[name.lower()] = " ".join(values).lower()
    result["label"] = obj["label"].lower()
    result["key"] = obj["objectKey"].lower()
    return result


def aql_error(message):
    return {"errorMessages": [message], "errors": {}}


def clause_match(obj, clause):
    flat = flat_attributes(obj)
    m = re.fullmatch(r'objectSchema\s*=\s*"(?P<val>(?:\\.|[^"])*)"', clause, re.IGNORECASE)
    if m:
        return m.group("val").replace('\\"', '"').replace("\\\\", "\\").lower() == SCHEMA["name"].lower()
    m = re.fullmatch(r'objectType\s+in\s+objectTypeAndChildren\("(?P<val>(?:\\.|[^"])*)"\)', clause, re.IGNORECASE)
    if m:
        wanted = m.group("val").replace('\\"', '"').replace("\\\\", "\\").lower()
        type_names = [obj["objectType"]["name"].lower()]
        parent = obj["objectType"].get("parentObjectTypeId")
        if parent:
            for t in OBJECT_TYPES:
                if t["id"] == parent:
                    type_names.append(t["name"].lower())
        return wanted in type_names
    m = re.fullmatch(r'"(?P<attr>(?:\\.|[^"])*)"\s*(?P<op>like|=)\s*"(?P<val>(?:\\.|[^"])*)"', clause, re.IGNORECASE)
    if not m:
        m = re.fullmatch(r'(?P<attr>[\w-]+)\s*(?P<op>like|=)\s*"(?P<val>(?:\\.|[^"])*)"', clause, re.IGNORECASE)
    if m:
        name = m.group("attr").replace('\\"', '"').replace("\\\\", "\\").lower()
        value = m.group("val").replace('\\"', '"').replace("\\\\", "\\").lower()
        haystack = flat.get(name, "")
        if m.group("op").lower() == "like":
            return value in haystack
        return value == haystack
    raise ValueError("unsupported AQL clause: " + clause)


def aql_match(obj, query):
    clauses = [c.strip() for c in re.split(r"\s+AND\s+", query.strip(), flags=re.IGNORECASE) if c.strip()]
    if not clauses:
        raise ValueError("empty AQL")
    for clause in clauses:
        if re.match(r"^order\s+by\b", clause, re.IGNORECASE):
            continue
        if not clause_match(obj, clause):
            return False
    return True


class Handler(BaseHTTPRequestHandler):
    server_version = "MockAssets/1.0"

    def _send(self, code, payload):
        data = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _redirect_login(self):
        data = b"<html><body>login</body></html>"
        self.send_response(302)
        self.send_header("Location", "/login.jsp")
        self.send_header("Content-Type", "text/html")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def _authorized(self):
        expected = "Bearer " + os.environ.get("ASSETS_TOKEN", "")
        if self.headers.get("Authorization") != expected:
            self._send(401, aql_error("unauthorized"))
            return False
        return True

    def _maybe_fail(self):
        if self.server.state["fail500"] > 0:
            self.server.state["fail500"] -= 1
            self._send(500, aql_error("forced failure"))
            return True
        return False

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")
        if path == "/health":
            self._send(200, {"status": "ok"})
            return
        # Publicly fetchable without auth on a real Jira host: the card's
        # <img> tags hit these (icon URLs are stored verbatim, §8.4).
        if path == "/login.jsp":
            data = b"<html><body>login</body></html>"
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
            return
        m_icon = re.fullmatch(r"/rest/(insight|assets)/[^/]+/icon/(\d+)/icon.png", path)
        if m_icon:
            data = icon_png(int(m_icon.group(2)))
            self.send_response(200)
            self.send_header("Content-Type", "image/png")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
            return
        if not path.startswith(BASE):
            self._redirect_login()
            return
        if not self._authorized():
            return
        if self._maybe_fail():
            return
        rest = path[len(BASE):]
        if rest == "/objectschema/list":
            self._send(200, {"objectschemas": [SCHEMA]})
            return
        m = re.fullmatch(r"/objectschema/(\d+)/objecttypes/flat", rest)
        if m:
            if int(m.group(1)) != SCHEMA["id"]:
                self._send(404, aql_error(f"NotFoundInsightException: Не удалось найти элемент «Схема объектов» с идентификатором «{m.group(1)}»"))
                return
            self._send(200, OBJECT_TYPES)
            return
        if rest == "/aql/objects":
            self._aql_objects(parse_qs(parsed.query))
            return
        m = re.fullmatch(r"/object/(\d+)/history", rest)
        if m:
            self._history(int(m.group(1)))
            return
        m = re.fullmatch(r"/object/(\d+)", rest)
        if m:
            self._object(int(m.group(1)))
            return
        m = re.fullmatch(r"/objectconnectedtickets/(\d+)/tickets", rest)
        if m:
            self._tickets(int(m.group(1)))
            return
        m = re.fullmatch(r"/config/statustype/(\d+)", rest)
        if m:
            status = STATUS_TYPES.get(int(m.group(1)))
            if status is None:
                self._send(404, aql_error(f"NotFoundInsightException: Не удалось найти элемент «Статус» с идентификатором «{m.group(1)}»"))
                return
            self._send(200, status)
            return
        m = re.fullmatch(r"/icon/(\d+)", rest)
        if m:
            icon = ICONS.get(int(m.group(1)))
            if icon is None:
                self._send(404, aql_error(f"NotFoundInsightException: Не удалось найти элемент «Иконка» с идентификатором «{m.group(1)}»"))
                return
            self._send(200, icon)
            return
        self._send(404, '<?xml version="1.0" encoding="UTF-8"?><status><status-code>404</status-code><message>HTTP 404 Not Found</message></status>')

    def do_POST(self):
        parsed = urlparse(self.path)
        path = parsed.path.rstrip("/")
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
            self._send(200, {"status": "armed", "code": int(m.group(1)), "times": self.server.state["fail500"]})
            return
        self._redirect_login()

    def _object(self, object_id):
        obj = self.server.state["objects"].get(object_id)
        if obj is None:
            self._send(404, aql_error(f"NotFoundInsightException: Не удалось найти элемент «Объект» с идентификатором «{object_id}»"))
            return
        self._send(200, obj)

    def _history(self, object_id):
        if object_id not in self.server.state["objects"]:
            self._send(404, aql_error(f"NotFoundInsightException: Не удалось найти элемент «Объект» с идентификатором «{object_id}»"))
            return
        self._send(200, [
            {"actor": {"name": "admin", "displayName": "admin", "avatarUrl": ""},
             "id": 1, "created": "2026-09-01T09:00:00.000+0300", "type": "OBJECT_CREATED",
             "objectId": object_id, "affectedAttribute": None, "oldValue": None, "newValue": None},
        ])

    def _tickets(self, object_id):
        if object_id not in self.server.state["objects"]:
            self._send(404, aql_error(f"NotFoundInsightException: Не удалось найти элемент «Объект» с идентификатором «{object_id}»"))
            return
        self._send(200, {
            "tickets": [{"key": "DEV-101", "summary": "Investigate halemans test trigger on dev-host-01",
                          "status": {"name": "Open"}}] if object_id == 10001 else [],
            "allTicketsQuery": "object%20in%20objects%20%3D%20" + str(object_id),
        })

    def _aql_objects(self, query):
        ql = query.get("qlQuery", [""])[0]
        page = max(1, int(query.get("page", ["1"])[0]))
        per_page = max(1, int(query.get("resultPerPage", ["25"])[0]))
        try:
            matches = [obj for obj in self.server.state["objects"].values() if aql_match(obj, ql)]
        except ValueError as err:
            self._send(400, aql_error(str(err)))
            return
        matches.sort(key=lambda o: o["id"])
        total = len(matches)
        start = (page - 1) * per_page
        entries = matches[start:start + per_page]
        start_index = start + 1 if entries else 0
        to_index = start + len(entries)
        self._send(200, {
            "objectEntries": entries,
            "objectTypeAttributes": [],
            "objectTypeId": 0,
            "objectTypeIsInherited": False,
            "abstractObjectType": False,
            "totalFilterCount": total,
            "startIndex": start_index,
            "toIndex": to_index,
            "qlQuery": ql,
        })


def main():
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    else:
        port = int(os.environ.get("MOCK_ASSETS_PORT", DEFAULT_PORT))
    server = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    server.state = seeded_state()
    print(f"mock-assets listening on 127.0.0.1:{port}", file=sys.stderr)
    server.serve_forever()


if __name__ == "__main__":
    main()
