#!/usr/bin/env python3
"""Static dev server with OData metadata MIME and backend proxy for /sap/* requests."""

import http.server
import os
import shutil
import socketserver
import sys
import time
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
UI5_RESOURCES_BASE = os.environ.get("UI5_RESOURCES_BASE", "https://ui5.sap.com/1.71.70").rstrip("/")
PROXY_PREFIXES = ("/sap/",)
UI5_PROXY_PREFIXES = ("/resources/", "/test-resources/")
UI5_CACHE_ROOT = Path(os.environ.get("UI5_RESOURCE_CACHE_DIR", Path(__file__).resolve().parents[1] / ".ui5-resource-cache"))
PROXY_RETRY_COUNT = max(1, int(os.environ.get("UI5_PROXY_RETRY_COUNT", "2")))
PROXY_RETRY_DELAY_MS = max(50, int(os.environ.get("UI5_PROXY_RETRY_DELAY_MS", "200")))


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

    def end_headers(self) -> None:
        normalized = (self.path or "").split("?", 1)[0]
        is_local_app_asset = not self._should_proxy() and not self._should_proxy_ui5()
        if is_local_app_asset:
            self.send_header("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
            self.send_header("Pragma", "no-cache")
            self.send_header("Expires", "0")
        if normalized.endswith("/Component-preload.js") or normalized.endswith("Component-preload.js"):
            self.send_header("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
        super().end_headers()

    def _should_proxy(self) -> bool:
        return any(self.path.startswith(prefix) for prefix in PROXY_PREFIXES)

    def _should_proxy_ui5(self) -> bool:
        return bool(UI5_RESOURCES_BASE) and any(self.path.startswith(prefix) for prefix in UI5_PROXY_PREFIXES)

    def _ui5_cache_path(self) -> Path:
        normalized = (self.path or "/").split("?", 1)[0].lstrip("/")
        return UI5_CACHE_ROOT / normalized

    def _serve_cached_ui5_payload(self, cache_path: Path, head_only: bool = False) -> bool:
        if not cache_path.exists() or not cache_path.is_file():
            return False
        payload = cache_path.read_bytes()
        self.send_response(200)
        self.send_header("Content-Type", self.guess_type(str(cache_path)))
        self.send_header("Content-Length", str(len(payload)))
        self.send_header("X-Proxy-Cache", "HIT")
        self.end_headers()
        if not head_only:
            self._write_payload(payload)
        return True

    def _cache_ui5_payload(self, cache_path: Path, payload: bytes) -> None:
        cache_path.parent.mkdir(parents=True, exist_ok=True)
        cache_path.write_bytes(payload)

    def _proxy_with_retry(self, req: urllib.request.Request, target: str, is_ui5_proxy: bool, cache_path: Optional[Path]):
        last_exc = None
        for attempt in range(1, PROXY_RETRY_COUNT + 1):
            try:
                with urllib.request.urlopen(req, timeout=30) as resp:
                    payload = resp.read()
                    if is_ui5_proxy and self.command == "GET" and resp.status == 200 and cache_path is not None:
                        self._cache_ui5_payload(cache_path, payload)
                    return ("success", resp, payload)
            except urllib.error.HTTPError as err:
                payload = err.read() if err.fp is not None else b""
                if err.code >= 500:
                    last_exc = err
                    if attempt < PROXY_RETRY_COUNT:
                        time.sleep(PROXY_RETRY_DELAY_MS / 1000.0)
                        continue
                    if is_ui5_proxy and cache_path is not None and self._serve_cached_ui5_payload(cache_path, head_only=self.command == "HEAD"):
                        return ("cached", None, None)
                return ("http_error", err, payload)
            except Exception as exc:  # noqa: BLE001
                last_exc = exc
                if attempt < PROXY_RETRY_COUNT:
                    time.sleep(PROXY_RETRY_DELAY_MS / 1000.0)
                    continue
        if is_ui5_proxy and cache_path is not None and self._serve_cached_ui5_payload(cache_path, head_only=self.command == "HEAD"):
            return ("cached", None, None)
        raise last_exc or RuntimeError(f"Proxy failed for {target}")

    def _proxy(self, target_base: str) -> None:
        body = None
        is_ui5_proxy = target_base == UI5_RESOURCES_BASE
        cache_path = self._ui5_cache_path() if is_ui5_proxy else None
        if self.command not in {"GET", "HEAD"}:
            length = int(self.headers.get("Content-Length") or 0)
            body = self.rfile.read(length) if length > 0 else b""

        target = f"{target_base}{self.path}"
        req = urllib.request.Request(target, data=body, method=self.command)

        for key, value in self.headers.items():
            lk = key.lower()
            if lk in {"host", "content-length", "connection"}:
                continue
            req.add_header(key, value)

        try:
            status, resp, payload = self._proxy_with_retry(req, target, is_ui5_proxy, cache_path)
            if status == "cached":
                return
            if status == "success":
                self.send_response(resp.status)
                for k, v in resp.headers.items():
                    lk = k.lower()
                    if lk in {"transfer-encoding", "connection", "content-length"}:
                        continue
                    self.send_header(k, v)
                self.send_header("Content-Length", str(len(payload)))
                if is_ui5_proxy and cache_path is not None:
                    self.send_header("X-Proxy-Cache", "MISS")
                self.end_headers()
                if self.command != "HEAD":
                    self._write_payload(payload)
                return
            err = resp
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
            print(f"[proxy-error] {target} -> {exc}", file=sys.stderr)
            self.send_response(502)
            self.send_header("Content-Type", "text/plain; charset=utf-8")
            self.send_header("Content-Length", str(len(text)))
            self.end_headers()
            if self.command != "HEAD":
                self._write_payload(text)

    def do_GET(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy(BACKEND_BASE)
        if self._should_proxy_ui5():
            return self._proxy(UI5_RESOURCES_BASE)
        return super().do_GET()

    def do_HEAD(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy(BACKEND_BASE)
        if self._should_proxy_ui5():
            return self._proxy(UI5_RESOURCES_BASE)
        return super().do_HEAD()

    def do_POST(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy(BACKEND_BASE)
        if self._should_proxy_ui5():
            return self._proxy(UI5_RESOURCES_BASE)
        return super().do_POST()

    def do_PUT(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy(BACKEND_BASE)
        if self._should_proxy_ui5():
            return self._proxy(UI5_RESOURCES_BASE)
        return super().do_PUT()

    def do_PATCH(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy(BACKEND_BASE)
        if self._should_proxy_ui5():
            return self._proxy(UI5_RESOURCES_BASE)
        return super().do_PATCH()

    def do_DELETE(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy(BACKEND_BASE)
        if self._should_proxy_ui5():
            return self._proxy(UI5_RESOURCES_BASE)
        return super().do_DELETE()

    def do_OPTIONS(self):  # noqa: N802
        if self._should_proxy():
            return self._proxy(BACKEND_BASE)
        if self._should_proxy_ui5():
            return self._proxy(UI5_RESOURCES_BASE)
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
        ui5_part = f", proxy ui5 resources: {UI5_RESOURCES_BASE}" if UI5_RESOURCES_BASE else ""
        print(f"Serving {root} on http://0.0.0.0:{port} (proxy backend: {BACKEND_BASE}{ui5_part})")
        httpd.serve_forever()


if __name__ == "__main__":
    main()
