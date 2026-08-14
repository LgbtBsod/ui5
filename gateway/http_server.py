"""Generic, multi-service-aware HTTP handler: the new top-level entry point
that replaces backend/http_server.py's FioriHandler/main() as the live
process, while backend/ itself is completely unchanged - it becomes the
ZCHECK_SRV plugin, reached only via the registry.

Route ownership - what stays generic here vs what's a per-plugin concern -
mirrors backend/http_server.py's FioriHandler as closely as possible, since
that's the exact set of behaviors that must keep working identically for
this app (see .claude/plans's design rationale for why each route landed
where it did):
  - CORS, SAP Gateway compat headers, /sap/bc/lrep/* stub, static file
    serving with path-traversal protection and SPA extensionless-route
    fallback: generic, gateway-owned, evaluated once for whichever request
    comes in - not duplicated per plugin.
  - $metadata, $batch: NOT part of the dispatch_request contract (backend's
    own http_server.py never routed these through dispatch_request either -
    they're separate concerns) - handled here per-plugin via the registry's
    metadataRelPath and the plugin's optional handle_batch.
  - Everything else under a registered prefix: delegated wholesale to that
    plugin's dispatch_request - the gateway has zero opinion on what's
    inside.
  - No registered prefix matches a /sap/... path: 404 - the "unregistered
    service" behavior a real Gateway gives, made possible by the registry
    (backend/http_server.py's old catch-all only ever had one prefix to
    compare against).
"""
from __future__ import annotations

import json
import mimetypes
import os
import signal
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import parse_qs, quote, urlparse

from .registry import ServiceRegistry, ServicePlugin

REGISTRY_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "service_registry.json")
DEFAULT_PORT = 8000

# SAP Gateway compatibility headers - sent with every OData response,
# identical to backend/http_server.py's SAP_GATEWAY_HEADERS (infrastructure-
# level, not tied to any one plugin's business logic).
SAP_GATEWAY_HEADERS = {
    "DataServiceVersion": "2.0",
    "MaxDataServiceVersion": "2.0",
    "sap-server": "true",
    "sap-platform": "ABAP",
}

# [Fix, gateway pass] CORS origin allowlist reuses backend._is_allowed_cors_origin
# as-is (see backend/query.py) rather than duplicating the localhost/preview-
# domain regex logic here - it's the one live plugin's already-correct,
# already-tested implementation, and the plan deliberately keeps backend/
# untouched. A future gateway with multiple browser-facing plugins might
# want this promoted to a gateway-owned, plugin-overridable concern; not
# needed while there's exactly one plugin serving browser traffic.
from backend import _is_allowed_cors_origin  # noqa: E402


def _requote_query(parsed_query):
    return "?" + "&".join(
        "%s=%s" % (k, quote(v, safe="")) for k, vals in parsed_query.items() for v in vals
    ) if parsed_query else ""


def _read_file_under(base_dir, rel_path):
    """[Fix, gateway pass] Generic, webRoot-parameterized static file
    reader - the fallback for any plugin that doesn't supply its own
    read_static_file (backend/ does, so ZCHECK_SRV never actually reaches
    this; a future minimal plugin without one still gets safe static
    serving for free). Same path-traversal guard as backend/http_server.py's
    read_static_file (commonpath() boundary check, not a startswith()
    prefix check - a startswith() check on a normalized path would let a
    sibling directory with a matching name prefix through)."""
    base = os.path.normpath(base_dir)
    full_path = os.path.normpath(os.path.join(base, rel_path))
    try:
        if os.path.commonpath([base, full_path]) != base:
            return None
    except ValueError:
        return None  # different drives on Windows, etc.
    if not os.path.isfile(full_path):
        return None
    ct, _ = mimetypes.guess_type(full_path)
    if ct is None:
        ct = "application/octet-stream"
    with open(full_path, "rb") as f:
        return (f.read(), ct)


def _static_reader_for(plugin: ServicePlugin):
    """Prefer the plugin's own read_static_file (already knows its own web
    root internally, e.g. backend.config.WEBAPP_DIR) - fall back to the
    generic webRoot-parameterized reader above for a plugin that doesn't
    provide one."""
    if plugin.read_static_file:
        return plugin.read_static_file
    return lambda rel_path: _read_file_under(plugin.web_root, rel_path)


def make_handler(registry: ServiceRegistry) -> type:
    """Returns a BaseHTTPRequestHandler subclass bound to this registry -
    a factory (not a module-level class) so tests can spin up a handler
    against a throwaway registry without touching the real one."""

    class GatewayHandler(BaseHTTPRequestHandler):
        # [Fix, gateway pass] Same single-threaded requirement as backend/
        # http_server.py's FioriHandler, for the same reason - see that
        # class's own comment. This constraint now protects N independent
        # plugins' mailbox patterns, not just one; it must not be relaxed
        # (no ThreadingMixIn) without redesigning every plugin's
        # pop_pending_headers-style state, not just this file.
        protocol_version = "HTTP/1.0"

        def log_request(self, code='-', size='-'):
            path = urlparse(self.path).path
            if registry.resolve(path) or path.startswith("/annotation/"):
                super().log_request(code, size)

        def _send_response_base(self, status, content_type, body_bytes, plugin=None, extra_headers=None):
            self.send_response(status)
            self.send_header("Content-Type", content_type)
            self.send_header("Content-Length", str(len(body_bytes)))
            if content_type.startswith("application/json") or content_type.startswith("application/xml"):
                for header, value in SAP_GATEWAY_HEADERS.items():
                    self.send_header(header, value)
            if plugin:
                for header, value in plugin.pop_pending_headers().items():
                    self.send_header(header, value)
            self._send_cors()
            if extra_headers:
                for header, value in extra_headers.items():
                    self.send_header(header, value)
            self.end_headers()
            self.wfile.write(body_bytes)

        def _send_json(self, status, obj, plugin=None, extra_headers=None):
            body = json.dumps(obj, ensure_ascii=False).encode("utf-8")
            self._send_response_base(status, "application/json; charset=utf-8", body, plugin, extra_headers)

        def _send_xml(self, status, xml_bytes, plugin=None):
            self._send_response_base(status, "application/xml; charset=utf-8", xml_bytes, plugin)

        def _send_text(self, status, text, plugin=None):
            self._send_response_base(status, "text/plain; charset=utf-8", text.encode("utf-8"), plugin)

        def _send_no_content(self, status, plugin=None):
            self._send_response_base(status, "application/json", b"", plugin)

        def _send_cors(self):
            origin = self.headers.get("Origin", "")
            if _is_allowed_cors_origin(origin):
                self.send_header("Access-Control-Allow-Origin", origin)
                self.send_header("Vary", "Origin")
                self.send_header("Access-Control-Expose-Headers", "X-CSRF-Token")

        def _read_body(self):
            length = int(self.headers.get("Content-Length", 0))
            return self.rfile.read(length) if length > 0 else b""

        def _parse_path(self):
            parsed = urlparse(self.path)
            return parsed.path, parse_qs(parsed.query)

        def _serve_static(self, path, head_only=False):
            plugin = registry.default_static()
            if not plugin:
                self.send_error(404)
                return
            reader = _static_reader_for(plugin)
            rel = path.lstrip("/") or "index.html"
            result = reader(rel)
            if not result:
                last_segment = rel.rsplit("/", 1)[-1]
                is_extensionless_route = "." not in last_segment
                result = reader("index.html") if is_extensionless_route else None
            if not result:
                self.send_error(404)
                return
            data, ct = result
            self.send_response(200)
            self.send_header("Content-Type", ct)
            self.send_header("Content-Length", str(len(data)))
            self._send_cors()
            if not head_only:
                self.send_header("Cache-Control", "no-cache")
            self.end_headers()
            if not head_only:
                self.wfile.write(data)

        def do_HEAD(self):
            path, _q = self._parse_path()
            plugin = registry.resolve(path)
            if plugin and (path == plugin.url_prefix or path == plugin.url_prefix + "/"):
                self.send_response(200)
                if plugin.csrf_token:
                    self.send_header("X-CSRF-Token", plugin.csrf_token)
                self._send_cors()
                self.send_header("Content-Length", "0")
                self.end_headers()
                return
            self._serve_static(path, head_only=True)

        def do_GET(self):
            path, query_params = self._parse_path()
            plugin = registry.resolve(path)

            if plugin and path == plugin.url_prefix + "/$metadata":
                reader = _static_reader_for(plugin)
                result = reader(plugin.metadata_rel_path)
                if result:
                    self._send_xml(200, result[0], plugin)
                else:
                    self.send_error(404, "metadata.xml not found")
                return

            # UI5-manifest annotation convention - resolved from the SPA's
            # own base URL, not the OData service path (same as backend/
            # http_server.py's original route) - always the default-static
            # plugin's own webroot.
            if path.startswith("/annotation/") and path.endswith(".xml"):
                default_plugin = registry.default_static()
                if default_plugin:
                    reader = _static_reader_for(default_plugin)
                    result = reader(path.lstrip("/"))
                    if result:
                        self._send_xml(200, result[0])
                        return
                self.send_error(404, f"{path} not found")
                return

            if plugin and (path == plugin.url_prefix or path == plugin.url_prefix + "/"):
                hdrs = {k.lower(): v for k, v in self.headers.items()}
                status, body, ct = plugin.dispatch_request("GET", "", headers=hdrs)
                if ct == "text/plain":
                    self._send_text(status, body, plugin)
                else:
                    self._send_json(status, body, plugin)
                return

            # [Fix, gateway pass] Dev-only diagnostic, optionally supported -
            # see this module's own docstring on why this isn't a mandatory
            # plugin-contract member. Only the default-static plugin (the
            # one app actually being browsed) is asked for this.
            if path == "/dev/locks":
                default_plugin = registry.default_static()
                module = default_plugin.module if default_plugin else None
                if module and hasattr(module, "draft_store"):
                    locks = []
                    for draft_uuid, draft_row in module.draft_store.get("CheckRoots", {}).items():
                        owner = draft_row.get("InProcessByUser", module.MOCK_USER)
                        locks.append({
                            "RootId": draft_row.get("RootId"),
                            "DocId": draft_row.get("DocId"),
                            "ActiveUUID": draft_row.get("ActiveUUID"),
                            "DraftUUID": draft_uuid,
                            "IsCreateDraft": draft_row.get("ActiveUUID") == module.ZERO_GUID,
                            "InProcessByUser": owner,
                            "InProcessByUserDescription": module._mock_user_display_name(owner),
                            "CreatedByUser": draft_row.get("CreatedByUser", module.MOCK_USER),
                            "CreatedAt": draft_row.get("_draft_created_at") or draft_row.get("CreatedAt"),
                            "LastChangedAt": draft_row.get("LastChangedAt"),
                        })
                    self._send_json(200, {"locks": locks, "knownMockUsers": module.MOCK_USER_REGISTRY})
                else:
                    self._send_json(200, {"locks": [], "knownMockUsers": {}})
                return

            if path.startswith("/sap/bc/lrep/"):
                self._send_json(200, {"changes": []})
                return

            if path.startswith("/sap/") and not plugin:
                self.send_error(404)
                return

            if plugin:
                rel_url = path[len(plugin.url_prefix) + 1:]
                full_rel = rel_url + _requote_query(query_params)
                hdrs = {k.lower(): v for k, v in self.headers.items()}
                status, body, ct = plugin.dispatch_request("GET", full_rel, headers=hdrs)
                if ct == "text/plain":
                    self._send_text(status, body, plugin)
                else:
                    self._send_json(status, body, plugin)
                return

            self._serve_static(path)

        def do_POST(self):
            path, query_params = self._parse_path()
            plugin = registry.resolve(path)

            if plugin and path == plugin.url_prefix + "/$batch":
                raw_body = self._read_body().decode("utf-8", errors="replace")
                hdrs = {k.lower(): v for k, v in self.headers.items()}
                if plugin.csrf_check_failed and plugin.csrf_check_failed(hdrs):
                    self._send_json(403, {"error": {"message": {"value": "CSRF token validation failed"}}})
                    return
                if not plugin.handle_batch:
                    self.send_error(501, "This service does not support $batch")
                    return
                status, body, ct = plugin.handle_batch(raw_body, self.headers.get("Content-Type", ""))
                body_bytes = body.encode("utf-8") if isinstance(body, str) else body
                self._send_response_base(status, ct, body_bytes, plugin)
                return

            if plugin:
                rel_url = path[len(plugin.url_prefix) + 1:]
                full_rel = rel_url + _requote_query(query_params)
                hdrs = {k.lower(): v for k, v in self.headers.items()}
                if plugin.csrf_check_failed and plugin.csrf_check_failed(hdrs):
                    self._send_json(403, {"error": {"message": {"value": "CSRF token validation failed"}}})
                    return
                raw = self._read_body()
                body = None
                if raw:
                    try:
                        body = json.loads(raw.decode("utf-8"))
                    except Exception:
                        body = raw.decode("utf-8", errors="replace")
                status, resp_body, ct = plugin.dispatch_request("POST", full_rel, body=body, headers=hdrs)
                if resp_body is None:
                    self._send_no_content(status, plugin)
                elif ct == "text/plain":
                    self._send_text(status, resp_body, plugin)
                else:
                    self._send_json(status, resp_body, plugin)
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
            plugin = registry.resolve(path)
            if not plugin:
                self.send_error(405, "Method Not Allowed")
                return
            hdrs = {k.lower(): v for k, v in self.headers.items()}
            if plugin.csrf_check_failed and plugin.csrf_check_failed(hdrs):
                self._send_json(403, {"error": {"message": {"value": "CSRF token validation failed"}}})
                return
            rel_url = path[len(plugin.url_prefix) + 1:]
            full_rel = rel_url + _requote_query(query_params)
            raw = self._read_body()
            body = None
            if raw:
                try:
                    body = json.loads(raw.decode("utf-8"))
                except Exception:
                    body = None
            status, resp_body, ct = plugin.dispatch_request(method, full_rel, body=body, headers=hdrs)
            if resp_body is None:
                self._send_no_content(status, plugin)
            elif ct == "text/plain":
                self._send_text(status, resp_body, plugin)
            else:
                self._send_json(status, resp_body, plugin)

        def do_OPTIONS(self):
            self.send_response(204)
            origin = self.headers.get("Origin", "")
            if _is_allowed_cors_origin(origin):
                self.send_header("Access-Control-Allow-Origin", origin)
                self.send_header("Vary", "Origin")
                self.send_header("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, MERGE, HEAD, OPTIONS")
                self.send_header("Access-Control-Allow-Headers", "Content-Type, X-CSRF-Token, If-Match, Accept, X-HTTP-Method")
                self.send_header("Access-Control-Expose-Headers", "X-CSRF-Token")
            for header, value in SAP_GATEWAY_HEADERS.items():
                self.send_header(header, value)
            self.end_headers()

    return GatewayHandler


def print_banner(port, registry: ServiceRegistry):
    lines = ["", "=" * 55, "   Multi-Service Gateway (Pure Python)", "=" * 55,
              "   App:    http://localhost:%d" % port]
    for plugin in registry.all_plugins():
        lines.append("   OData:  http://localhost:%d%s/  (%s)" % (port, plugin.url_prefix, plugin.name))
    lines.append("   Ctrl+C to stop")
    lines.append("=" * 55)
    print("\n".join(lines))


def main():
    port = DEFAULT_PORT
    i = 1
    while i < len(sys.argv):
        if sys.argv[i] == "--port" and i + 1 < len(sys.argv):
            port = int(sys.argv[i + 1])
            i += 2
        else:
            i += 1

    registry = ServiceRegistry(REGISTRY_PATH)
    handler_cls = make_handler(registry)

    for plugin in registry.all_plugins():
        if plugin.seed_test_data:
            plugin.seed_test_data(port=port)

    server = HTTPServer(("0.0.0.0", port), handler_cls)
    print_banner(port, registry)

    def handle_sigint(sig, frame):
        print("\nShutting down...")
        server.server_close()
        sys.exit(0)

    signal.signal(signal.SIGINT, handle_sigint)
    server.serve_forever()
