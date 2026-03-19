#!/usr/bin/env python3
"""Regression checks for dev_static_server UI5 resource cache behavior."""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SERVER_SCRIPT = ROOT / "scripts" / "dev_static_server.py"
PORT = int(os.environ.get("UI5_CACHE_TEST_PORT", "8095"))
BASE_URL = f"http://127.0.0.1:{PORT}"
RESOURCE_PATH = "/resources/sap/ui/core/routing/Router.js"
SECOND_RESOURCE_PATH = "/resources/sap/ui/core/library.js"
LOCAL_ASSET_PATH = "/index.html"


def wait_for_server(timeout_seconds: int = 30) -> None:
    deadline = time.time() + timeout_seconds
    while time.time() < deadline:
        try:
            with urllib.request.urlopen(f"{BASE_URL}{LOCAL_ASSET_PATH}", timeout=2) as response:
                if response.status == 200:
                    return
        except Exception:
            time.sleep(0.5)
    raise RuntimeError("dev_static_server did not start in time")


def fetch(path: str) -> tuple[int, bytes, dict]:
    request = urllib.request.Request(f"{BASE_URL}{path}", method="GET")
    with urllib.request.urlopen(request, timeout=30) as response:
        return response.status, response.read(), dict(response.headers.items())


def assert_true(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def main() -> int:
    cache_dir = Path(tempfile.mkdtemp(prefix="ui5-resource-cache-regression-"))
    env = os.environ.copy()
    env["UI5_RESOURCE_CACHE_DIR"] = str(cache_dir)
    env["PYTHONUNBUFFERED"] = "1"

    process = subprocess.Popen(
        [sys.executable, str(SERVER_SCRIPT), str(PORT)],
        cwd=str(ROOT),
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )

    try:
        wait_for_server()

        status_1, body_1, headers_1 = fetch(RESOURCE_PATH)
        assert_true(status_1 == 200, "First UI5 resource fetch must succeed")
        assert_true(headers_1.get("X-Proxy-Cache") == "MISS", "First UI5 resource fetch must be a cache MISS")
        body_1.decode("utf-8")

        cached_router = cache_dir / RESOURCE_PATH.lstrip("/")
        assert_true(cached_router.exists(), "Router.js must be cached after first fetch")

        status_2, body_2, headers_2 = fetch(RESOURCE_PATH)
        assert_true(status_2 == 200, "Second UI5 resource fetch must succeed")
        assert_true(headers_2.get("X-Proxy-Cache") == "HIT", "Second UI5 resource fetch must be a cache HIT")
        assert_true(body_1 == body_2, "Cache HIT payload must match original MISS payload")

        status_3, body_3, headers_3 = fetch(SECOND_RESOURCE_PATH)
        assert_true(status_3 == 200, "Second text UI5 resource fetch must succeed")
        assert_true(headers_3.get("X-Proxy-Cache") == "MISS", "Fresh second resource must start as MISS")
        body_3.decode("utf-8")

        cached_router.parent.mkdir(parents=True, exist_ok=True)
        cached_router.write_bytes(b"\xff\xfe\x00\x00broken-js")

        status_4, body_4, headers_4 = fetch(RESOURCE_PATH)
        assert_true(status_4 == 200, "Fetch after invalid cache rewrite must succeed")
        assert_true(headers_4.get("X-Proxy-Cache") == "MISS", "Invalid cached JS must be evicted and refetched")
        body_4.decode("utf-8")
        assert_true(body_4 == body_1, "Refetched Router.js payload must match original valid payload")

        _, _, local_headers = fetch(LOCAL_ASSET_PATH)
        cache_control = str(local_headers.get("Cache-Control", ""))
        assert_true("no-store" in cache_control.lower(), "Local app assets must remain no-store")
        assert_true("X-Proxy-Cache" not in local_headers, "Local app assets must not use UI5 proxy cache headers")

        print("ui5-resource-cache regression: PASS")
        print(f"cache dir: {cache_dir}")
        return 0
    finally:
        process.terminate()
        try:
            process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            process.kill()
        shutil.rmtree(cache_dir, ignore_errors=True)


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (AssertionError, RuntimeError, urllib.error.URLError) as exc:
        print(f"ui5-resource-cache regression: FAIL - {exc}", file=sys.stderr)
        raise SystemExit(1)
