#!/usr/bin/env python3
"""Static dev server with OData metadata MIME and backend proxy for /sap/* requests."""

import http.server
import os
import shutil
import socketserver
import sys
import urllib.error
import urllib.request
from pathlib import Path
from typing import Optional


def is_client_disconnect_error(exc: BaseException) -> bool:
    """Return True for common socket errors raised on client disconnect."""
    current = exc
    while current is not None:
        if isinstance(current, OSError) and (
            getattr(current, "winerror", None) in {10053, 10054}
            or getattr(current, "errno", None) in {32, 104}
        ):
            return True
        current = current.__cause__ or current.__context__
    return False

BACKEND_BASE = os.environ.get("UI5_BACKEND_BASE", "http://127.0.0.1:8000")
PROXY_PREFIXES = ("/sap/",)


class ODataStaticRequestHandler(http.server.SimpleHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def __init__(self, *args, directory: Optional[str] = None, **kwargs):
        super().__init__(*args, directory=directory, **kwargs)

    @staticmethod
    def _is_client_disconnect(exc: OSError) -> bool:
        return is_client_disconnect_error(exc)

    def _write_payload(self, payload: bytes) -> None:
        try:
            self.wfile.write(payload)
        except OSError as exc:
            if self._is_client_disconnect(exc):
                return
            raise

    def handle(self) -> None:
        """Ignore client disconnects that happen before a full request line is read."""
        try:
            super().handle()
        except OSError as exc:
            if self._is_client_disconnect(exc):
                return
            raise

    def guess_type(self, path: str) -> str:
        normalized = (path or "").split("?", 1)[0]
        if normalized.endswith("/$metadata") or normalized.endswith("/$metadata.xml"):
            return "application/xml; charset=utf-8"
        return super().guess_type(path)

    def _should_proxy(self) -> bool:
        return any(self.path.startswith(prefix) for prefix in PROXY_PREFIXES)

    def _proxy(self) -> None:
        body = None
        if self.command not in {"GET", "HEAD"}:
            length = int(self.headers.get("Content-Length") or 0)
            body = self.rfile.read(length) if length > 0 else b""

        target = f"{BACKEND_BASE}{self.path}"
        req = urllib.request.Request(target, data=body, method=self.command)

        for key, value in self.headers.items():
            lk = key.lower()
            if lk in {"host", "content-length", "connection"}:
                continue
            req.add_header(key, value)

        try:
            with urllib.request.urlopen(req, timeout=30) as resp:
                payload = resp.read()
                self.send_response(resp.status)
                for k, v in resp.headers.items():
                    lk = k.lower()
                    if lk in {"transfer-encoding", "connection", "content-length"}:
                        continue
                    self.send_header(k, v)
                self.send_header("Content-Length", str(len(payload)))
                self.end_headers()
                if self.command != "HEAD":
                    self._write_payload(payload)
        except urllib.error.HTTPError as err:
            payload = err.read() if err.fp is not None else b""
            self.send_response(err.code)
            for k, v in err.headers.items():
                lk = k.lower()
                if lk in {"transfer-encoding", "connection", "content-length"}:
                    continue
                self.send_header(k, v)
            self.send_header("Content-Length", str(len(payload)))
            self.end_headers()
            if self.command != "HEAD":
                self._write_payload(payload)
        except Exception as exc:  # noqa: BLE001
            text = f"Proxy error: {exc}".encode("utf-8")
            self.send_response(502)
            self.send_header("Content-Type", "text/plain; charset=utf-8")
            self.send_header("Content-Length", str(len(text)))
            self.end_headers()
            if self.command != "HEAD":
                self._write_payload(text)

    def do_GET(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy()
        return super().do_GET()

    def do_HEAD(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy()
        return super().do_HEAD()

    def do_POST(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy()
        return super().do_POST()

    def do_PUT(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy()
        return super().do_PUT()

    def do_PATCH(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy()
        return super().do_PATCH()

    def do_DELETE(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy()
        return super().do_DELETE()

    def do_OPTIONS(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy()
        return super().do_OPTIONS()

    def copyfile(self, source, outputfile):
        """Ignore client disconnects while streaming static files."""
        try:
            shutil.copyfileobj(source, outputfile)
        except OSError as exc:
            if self._is_client_disconnect(exc):
                return
            raise


def main() -> None:
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8080
    repo_root = Path(__file__).resolve().parents[1]
    app_root = repo_root / "app"
    webapp_root = repo_root / "webapp"
    root = app_root if app_root.exists() else webapp_root if webapp_root.exists() else repo_root

    class ReusableTCPServer(socketserver.ThreadingTCPServer):
        allow_reuse_address = True
        daemon_threads = True

        def handle_error(self, request, client_address):
            exc = sys.exc_info()[1]
            if exc is not None and is_client_disconnect_error(exc):
                return
            return super().handle_error(request, client_address)

    with ReusableTCPServer(("", port), lambda *a, **kw: ODataStaticRequestHandler(*a, directory=str(root), **kw)) as httpd:
        print(f"Serving {root} on http://0.0.0.0:{port} (proxy backend: {BACKEND_BASE})")
        httpd.serve_forever()


if __name__ == "__main__":
    main()
