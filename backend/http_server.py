"""Static file serving + the BaseHTTPRequestHandler subclass wiring HTTP
verbs to dispatch.py/batch.py, plus the process entry point (main()).
"""
import os
import sys
import json
import signal
import mimetypes
from urllib.parse import urlparse, parse_qs, quote
from http.server import HTTPServer, BaseHTTPRequestHandler

from . import config
from . import state
from . import query
from . import dispatch
from . import batch
from . import seed

USAGE = """
Pure-Python OData v2 Mock Server + Static File Server for SAP Fiori Elements demo.

ZERO external dependencies - only Python 3.6+ standard library.

Usage:
    python serve.py                # http://localhost:8000
    python serve.py --port 9090    # custom port

Opens browser-friendly demo at the given port.
All OData requests are handled in-process (no Node.js needed).
"""

# SAP Gateway compatibility headers - sent with every OData response
SAP_GATEWAY_HEADERS = {
    "DataServiceVersion": "2.0",
    "MaxDataServiceVersion": "2.0",
    "sap-server": "true",
    "sap-platform": "ABAP",
}


def read_static_file(rel_path):
    # [SECURITY FIX] startswith() на нормализованном пути пропускал соседние
    # каталоги с совпадающим префиксом имени (WEBAPP_DIR="/app/web" впускал
    # "/app/web-evil/secret.txt"). commonpath() даёт точную проверку границы
    # каталога — эквивалент экранирования динамического параметра пути.
    base_dir = os.path.normpath(config.WEBAPP_DIR)
    full_path = os.path.normpath(os.path.join(base_dir, rel_path))
    try:
        if os.path.commonpath([base_dir, full_path]) != base_dir:
            return None
    except ValueError:
        return None  # разные диски на Windows и т.п.
    if not os.path.isfile(full_path):
        return None
    ct, _ = mimetypes.guess_type(full_path)
    if ct is None:
        ct = "application/octet-stream"
    with open(full_path, "rb") as f:
        return (f.read(), ct)


def _requote_query(parsed_query):
    return "?" + "&".join("%s=%s" % (k, quote(v, safe="")) for k, vals in parsed_query.items() for v in vals) if parsed_query else ""


class FioriHandler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_request(self, code='-', size='-'):
        if self.path.startswith(config.ODATA_PREFIX) or self.path.startswith("/annotation/"):
            super().log_request(code, size)

    def _send_response_base(self, status, content_type, body_bytes, extra_headers=None):
        """Base response sender - DRY helper for all response types."""
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body_bytes)))
        
        # SAP Gateway compatibility headers
        if content_type.startswith("application/json") or content_type.startswith("application/xml"):
            for header, value in SAP_GATEWAY_HEADERS.items():
                self.send_header(header, value)
        
        # SAP message header (if pending)
        sap_message = state._pop_sap_message()
        if sap_message:
            self.send_header("sap-message", sap_message)
        
        # ETag header (if pending)
        etag = state._pop_pending_etag()
        if etag:
            self.send_header("ETag", etag)
        
        # CORS headers
        self._send_cors()
        
        # Extra headers (caller-specific)
        if extra_headers:
            for header, value in extra_headers.items():
                self.send_header(header, value)
        
        self.end_headers()
        self.wfile.write(body_bytes)

    def _send_json(self, status, obj, extra_headers=None):
        body = json.dumps(obj, ensure_ascii=False).encode("utf-8")
        self._send_response_base(status, "application/json; charset=utf-8", body, extra_headers)

    def _send_xml(self, status, xml_bytes, extra_headers=None):
        self._send_response_base(status, "application/xml; charset=utf-8", xml_bytes, extra_headers)

    def _send_text(self, status, text, extra_headers=None):
        body = text.encode("utf-8")
        self._send_response_base(status, "text/plain; charset=utf-8", body, extra_headers)

    def _send_no_content(self, status=204, extra_headers=None):
        """Send 204 No Content - used for successful mutations without response body."""
        self._send_response_base(status, "application/json", b"", extra_headers)

    def _send_cors(self):
        # [Аудит: OWASP A05:2021] Access-Control-Allow-Origin:"*" вместе с
        # Access-Control-Expose-Headers: X-CSRF-Token позволял ЛЮБОМУ чужому
        # origin кросс-доменно прочитать CSRF-токен и обойти csrf_check_failed().
        # Вместо wildcard — отражаем Origin, только если он совпадает с
        # локальной разработкой (localhost/127.0.0.1, любой порт) или с
        # публичным preview-доменом sandbox (см. start_server.sh: Caddy
        # проксирует preview-<bot-id>.space-z.ai на этот же процесс — там
        # запросы фактически same-origin, но браузер всё равно шлёт Origin
        # для небезопасных методов, и мы обязаны на него ответить).
        origin = self.headers.get("Origin", "")
        if query._is_allowed_cors_origin(origin):
            self.send_header("Access-Control-Allow-Origin", origin)
            self.send_header("Vary", "Origin")
            self.send_header("Access-Control-Expose-Headers", "X-CSRF-Token")

    def _read_body(self):
        length = int(self.headers.get("Content-Length", 0))
        if length > 0:
            return self.rfile.read(length)
        return b""

    def _parse_path(self):
        parsed = urlparse(self.path)
        return parsed.path, parse_qs(parsed.query)

    def do_HEAD(self):
        path, query_params = self._parse_path()
        if path == config.ODATA_PREFIX or path == config.ODATA_PREFIX + "/":
            self.send_response(200)
            self.send_header("X-CSRF-Token", config.CSRF_TOKEN)
            self._send_cors()
            self.send_header("Content-Length", "0")
            self.end_headers()
            return
        self._serve_static_head(path)

    def _serve_static_head(self, path):
        rel = path.lstrip("/")
        if not rel:
            rel = "index.html"
        result = read_static_file(rel)
        if result:
            data, ct = result
            self.send_response(200)
            self.send_header("Content-Type", ct)
            self.send_header("Content-Length", str(len(data)))
            self._send_cors()
            self.end_headers()
        else:
            self.send_error(404)

    def do_GET(self):
        path, query_params = self._parse_path()

        if path == config.ODATA_PREFIX + "/$metadata":
            result = read_static_file("localService/metadata.xml")
            if result:
                self._send_xml(200, result[0])
            else:
                self.send_error(404, "metadata.xml not found")
            return

        if path == "/annotation/annotations.xml":
            result = read_static_file("annotation/annotations.xml")
            if result:
                self._send_xml(200, result[0])
            else:
                self.send_error(404, "annotations.xml not found")
            return

        if path == config.ODATA_PREFIX or path == config.ODATA_PREFIX + "/":
            entity_sets = list(config.REFERENCE_DATA.keys()) + list(state.store.keys())
            self._send_json(200, {"d": {"EntitySets": entity_sets}})
            return

        # [Fix] Dev-only visibility endpoint - "display locks", per request.
        # Not part of the OData service tree on purpose (no metadata/
        # annotation baggage) - a plain diagnostic JSON list of every
        # currently outstanding draft (= every currently-held "lock" in the
        # Fiori/BOPF sense: a draft's mere existence IS the lock, there's no
        # separate lock-table like ../backend/mock_gateway's LockEntry/
        # LockService) with its owning user, for manual testing of the
        # foreign-user-lock scenario without needing to open a debugger.
        if path == "/dev/locks":
            locks = []
            for draft_uuid, draft_row in state.draft_store["CheckRoots"].items():
                owner = draft_row.get("InProcessByUser", config.MOCK_USER)
                locks.append({
                    "RootId": draft_row.get("RootId"),
                    "DocId": draft_row.get("DocId"),
                    "ActiveUUID": draft_row.get("ActiveUUID"),
                    "DraftUUID": draft_uuid,
                    "IsCreateDraft": draft_row.get("ActiveUUID") == config.ZERO_GUID,
                    "InProcessByUser": owner,
                    "InProcessByUserDescription": state._mock_user_display_name(owner),
                    "CreatedByUser": draft_row.get("CreatedByUser", config.MOCK_USER),
                    "CreatedAt": draft_row.get("_draft_created_at") or draft_row.get("CreatedAt"),
                    "LastChangedAt": draft_row.get("LastChangedAt"),
                })
            self._send_json(200, {"locks": locks, "knownMockUsers": config.MOCK_USER_REGISTRY})
            return

        if path.startswith("/sap/bc/lrep/"):
            self._send_json(200, {"changes": []})
            return

        if path.startswith("/sap/") and not path.startswith(config.ODATA_PREFIX):
            self.send_error(404)
            return

        if path.startswith(config.ODATA_PREFIX + "/"):
            rel_url = path[len(config.ODATA_PREFIX) + 1:]
            full_rel = rel_url + _requote_query(query_params)
            hdrs = {k.lower(): v for k, v in self.headers.items()}
            status, body, ct = dispatch.dispatch_request("GET", full_rel, headers=hdrs)
            if ct == "text/plain":
                self._send_text(status, body)
            else:
                self._send_json(status, body)
            return

        self._serve_static(path)

    def _serve_static(self, path):
        rel = path.lstrip("/")
        if not rel:
            rel = "index.html"
        result = read_static_file(rel)
        if result:
            data, ct = result
            self.send_response(200)
            self.send_header("Content-Type", ct)
            self.send_header("Content-Length", str(len(data)))
            self._send_cors()
            self.send_header("Cache-Control", "no-cache")
            self.end_headers()
            self.wfile.write(data)
        else:
            last_segment = rel.rsplit("/", 1)[-1]
            is_extensionless_route = "." not in last_segment
            result = read_static_file("index.html") if is_extensionless_route else None
            if result:
                data, ct = result
                self.send_response(200)
                self.send_header("Content-Type", "text/html; charset=utf-8")
                self.send_header("Content-Length", str(len(data)))
                self._send_cors()
                self.end_headers()
                self.wfile.write(data)
            else:
                self.send_error(404)

    def do_POST(self):
        path, query_params = self._parse_path()

        if path == config.ODATA_PREFIX + "/$batch":
            raw_body = self._read_body().decode("utf-8", errors="replace")
            hdrs = {k.lower(): v for k, v in self.headers.items()}
            if query.csrf_check_failed(hdrs):
                self._send_json(403, {"error": {"message": {"value": "CSRF token validation failed"}}})
                return
            ct_header = self.headers.get("Content-Type", "")
            status, body, ct = batch.handle_batch(raw_body, ct_header)
            body_bytes = body.encode("utf-8") if isinstance(body, str) else body
            self.send_response(status)
            self.send_header("Content-Type", ct)
            self.send_header("Content-Length", str(len(body_bytes)))
            self._send_cors()
            self.end_headers()
            self.wfile.write(body_bytes)
            return

        if path.startswith(config.ODATA_PREFIX + "/"):
            rel_url = path[len(config.ODATA_PREFIX) + 1:]
            full_rel = rel_url + _requote_query(query_params)
            hdrs = {k.lower(): v for k, v in self.headers.items()}
            if query.csrf_check_failed(hdrs):
                self._send_json(403, {"error": {"message": {"value": "CSRF token validation failed"}}})
                return
            raw = self._read_body()
            body = None
            if raw:
                try:
                    body = json.loads(raw.decode("utf-8"))
                except:
                    body = raw.decode("utf-8", errors="replace")
            status, resp_body, ct = dispatch.dispatch_request("POST", full_rel, body=body, headers=hdrs)
            if resp_body is None:
                self._send_no_content(status)
            elif ct == "text/plain":
                self._send_text(status, resp_body)
            else:
                self._send_json(status, resp_body)
            return

        self.send_error(404)

    def do_MERGE(self):
        self._handle_mutation("MERGE")

    def do_PATCH(self):
        self._handle_mutation("PATCH")

    def do_PUT(self):
        self._handle_mutation("PUT")

    def do_DELETE(self):
        self._handle_mutation("DELETE")

    def _handle_mutation(self, method):
        path, query_params = self._parse_path()
        if not path.startswith(config.ODATA_PREFIX + "/"):
            self.send_error(405, "Method Not Allowed")
            return
        hdrs = {k.lower(): v for k, v in self.headers.items()}
        if query.csrf_check_failed(hdrs):
            self._send_json(403, {"error": {"message": {"value": "CSRF token validation failed"}}})
            return
        rel_url = path[len(config.ODATA_PREFIX) + 1:]
        full_rel = rel_url + _requote_query(query_params)
        raw = self._read_body()
        body = None
        if raw:
            try:
                body = json.loads(raw.decode("utf-8"))
            except:
                body = None
        status, resp_body, ct = dispatch.dispatch_request(method, full_rel, body=body, headers=hdrs)
        if resp_body is None:
            self._send_no_content(status)
        elif ct == "text/plain":
            self._send_text(status, resp_body)
        else:
            self._send_json(status, resp_body)

    def do_OPTIONS(self):
        self.send_response(204)
        origin = self.headers.get("Origin", "")
        if query._is_allowed_cors_origin(origin):
            self.send_header("Access-Control-Allow-Origin", origin)
            self.send_header("Vary", "Origin")
            self.send_header("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, MERGE, HEAD, OPTIONS")
            self.send_header("Access-Control-Allow-Headers", "Content-Type, X-CSRF-Token, If-Match, Accept, X-HTTP-Method")
            self.send_header("Access-Control-Expose-Headers", "X-CSRF-Token")
        
        # SAP Gateway headers for OPTIONS preflight
        for header, value in SAP_GATEWAY_HEADERS.items():
            self.send_header(header, value)
        
        self.end_headers()


def print_banner(port):
    print("""
=====================================================
   Fiori Elements Demo Server (Pure Python)
=====================================================
   App:    http://localhost:%d
   OData:  http://localhost:%d%s/
   Ctrl+C to stop
=====================================================
""" % (port, port, config.ODATA_PREFIX))


def main():
    port = config.DEFAULT_PORT
    i = 1
    while i < len(sys.argv):
        if sys.argv[i] == "--port" and i + 1 < len(sys.argv):
            port = int(sys.argv[i + 1])
            i += 2
        elif sys.argv[i] in ("--help", "-h"):
            print(USAGE)
            sys.exit(0)
        else:
            i += 1

    server = HTTPServer(("0.0.0.0", port), FioriHandler)

    # Создаём начальные тестовые данные
    seed.seed_test_data(port=port)

    print_banner(port)

    def handle_sigint(sig, frame):
        print("\nShutting down...")
        server.server_close()
        sys.exit(0)

    signal.signal(signal.SIGINT, handle_sigint)
    server.serve_forever()
