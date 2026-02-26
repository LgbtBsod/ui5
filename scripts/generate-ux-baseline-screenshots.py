#!/usr/bin/env python3
"""Generate automatic Morning/Night baseline screenshots for key UI states.

Usage:
  python scripts/generate-ux-baseline-screenshots.py [url] [output_dir]

Defaults:
  url = http://127.0.0.1:8080/index.html
  output_dir = artifacts/ux/baseline
"""

from __future__ import annotations

import pathlib
import sys
import time
try:
    from playwright.sync_api import sync_playwright
except ModuleNotFoundError:
    sync_playwright = None

URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
OUT_DIR = pathlib.Path(sys.argv[2] if len(sys.argv) > 2 else "artifacts/ux/baseline")


def set_theme(page, mode: str) -> None:
    if mode == "morning":
        page.evaluate(
            """
            () => {
              document.documentElement.classList.add('light-mode');
              document.body.classList.add('appLight');
              document.body.classList.remove('appDark');
            }
            """
        )
    else:
        page.evaluate(
            """
            () => {
              document.documentElement.classList.remove('light-mode');
              document.body.classList.add('appDark');
              document.body.classList.remove('appLight');
            }
            """
        )


def capture_pair(page, mode: str, out_dir: pathlib.Path) -> None:
    set_theme(page, mode)
    page.wait_for_timeout(500)
    search_path = out_dir / f"{mode}-search.png"
    page.screenshot(path=str(search_path), full_page=True)

    # Try to capture detail route; keep deterministic fallback even if id is not found.
    page.goto(URL + "#/detail/CHK-001", wait_until="networkidle", timeout=90000)
    page.wait_for_timeout(700)
    set_theme(page, mode)
    page.wait_for_timeout(300)
    detail_path = out_dir / f"{mode}-detail.png"
    page.screenshot(path=str(detail_path), full_page=True)

    # Return to search for next mode.
    page.goto(URL, wait_until="networkidle", timeout=90000)
    page.wait_for_timeout(500)


def main() -> int:
    if sync_playwright is None:
        print("[error] playwright is not installed. Install dependency and retry: pip install playwright && playwright install chromium")
        return 2

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": 1440, "height": 900})
        page.goto(URL, wait_until="networkidle", timeout=90000)
        page.wait_for_timeout(700)

        capture_pair(page, "morning", OUT_DIR)
        capture_pair(page, "night", OUT_DIR)

        browser.close()

    now = time.strftime("%Y-%m-%d %H:%M:%S")
    print(f"[ok] baseline screenshots generated at {OUT_DIR} ({now})")
    print("[files] morning-search.png, morning-detail.png, night-search.png, night-detail.png")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
