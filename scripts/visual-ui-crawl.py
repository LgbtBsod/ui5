#!/usr/bin/env python3
"""Visual UI crawl for screenshot-based runtime diagnostics.

This script opens the app, clicks through key shell/search/detail areas,
captures screenshots for each step, and writes a JSON report with runtime notes.

Usage:
  python scripts/visual-ui-crawl.py [url] [output_dir]

Defaults:
  url = http://127.0.0.1:8080/index.html
  output_dir = docs/artifacts/visual-crawl
"""

from __future__ import annotations

import json
import re
import sys
import time
from pathlib import Path
from typing import Any

try:
    from playwright.sync_api import Error, Page, TimeoutError, sync_playwright
except ModuleNotFoundError:
    sync_playwright = None
    Error = Exception
    TimeoutError = Exception


URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
OUT_DIR = Path(sys.argv[2] if len(sys.argv) > 2 else "docs/artifacts/visual-crawl")
SEARCH_VIEW_ID = "checklist_app_comp---searchTargetPage"
DETAIL_VIEW_ID = "checklist_app_comp---detailTargetPage"


def slug(value: str) -> str:
    return re.sub(r"[^a-z0-9]+", "-", value.lower()).strip("-")


def wait_for_app_ready(page: Page, delay: int = 1200) -> None:
    page.wait_for_load_state("domcontentloaded")
    page.wait_for_load_state("networkidle")
    page.wait_for_function(
        """
        () => {
          if (typeof sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
            return false;
          }
          const core = sap.ui.getCore();
          return !!core.byId('checklist_app_comp---app--mainFcl')
            && !!core.byId('checklist_app_comp---app--appShellHeaderHost')
            && !!core.byId('checklist_app_comp---searchTargetPage--searchWorkbenchDock');
        }
        """,
        timeout=90000,
    )
    page.wait_for_timeout(delay)


def wait_for_detail_ready(page: Page, delay: int = 1500) -> None:
    wait_for_app_ready(page, delay)
    page.wait_for_function(
        """
        () => !!document.querySelector('#checklist_app_comp---detailTargetPage')
          && (
            !!document.querySelector('.detailControlPinnedDock')
            || !!document.querySelector('.detailControlInlineCard')
            || !!document.querySelector('.detailControlStickyBlock')
          )
        """,
        timeout=20000,
    )
    page.wait_for_timeout(300)


def dismiss_overlays(page: Page) -> None:
    for _ in range(3):
        page.keyboard.press("Escape")
        page.wait_for_timeout(120)
    page.evaluate(
        """
        () => {
          if (typeof sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
            return;
          }
          const core = sap.ui.getCore();
          Object.keys(core.mElements || {}).forEach((id) => {
            const control = core.byId(id);
            if (control && typeof control.isOpen === 'function' && control.isOpen() && typeof control.close === 'function') {
              control.close();
            }
          });
        }
        """
    )
    page.wait_for_timeout(250)


def safe_click(page: Page, selector: str, timeout: int = 6000) -> bool:
    try:
        locator = page.locator(selector).first
        locator.wait_for(state="visible", timeout=timeout)
        locator.click(timeout=timeout)
        page.wait_for_timeout(450)
        return True
    except TimeoutError:
        return False
    except Error:
        return False


def set_viewport(page: Page, width: int, height: int) -> None:
    page.set_viewport_size({"width": width, "height": height})
    page.wait_for_timeout(700)


def capture(page: Page, shots: list[dict[str, Any]], step: str, note: str, full_page: bool = False) -> str:
    index = len(shots) + 1
    name = f"{index:02d}-{slug(step)}.png"
    path = OUT_DIR / name
    path.parent.mkdir(parents=True, exist_ok=True)
    page.screenshot(path=str(path), full_page=full_page)
    shots.append(
        {
            "index": index,
            "step": step,
            "note": note,
            "path": str(path).replace("\\", "/"),
            "url": page.url,
            "viewport": page.viewport_size,
            "capturedAt": time.strftime("%Y-%m-%d %H:%M:%S"),
        }
    )
    return str(path)


def shell_state(page: Page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => ({
          shellButtons: Array.from(document.querySelectorAll('.appShellHeader .sapMBtn')).map((node) => (node.innerText || node.getAttribute('title') || '').trim()).filter(Boolean),
          themeSwitchPresent: !!document.querySelector('.themeDockSwitch'),
          stickySearchPresent: !!document.querySelector('.searchFilterExperienceShell'),
          stickyDetailPresent: !!document.querySelector('.detailControlStickyBlock')
        })
        """
    )


def get_first_root_id(page: Page) -> str:
    try:
        root_id = page.evaluate(
            """
            () => {
              if (typeof sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
                return '';
              }
              const core = sap.ui.getCore();
              const smartTable = core.byId('checklist_app_comp---searchTargetPage--searchSmartTable');
              const table = smartTable && smartTable.getTable && smartTable.getTable();
              if (!table) {
                return '';
              }
              const listItems = typeof table.getItems === 'function' ? table.getItems() : [];
              for (const item of listItems) {
                const ctx = item && item.getBindingContext && item.getBindingContext();
                const obj = ctx && ctx.getObject && ctx.getObject();
                if (obj && (obj.Key || obj.Id)) {
                  return String(obj.Key || obj.Id || '');
                }
              }
              return '';
            }
            """
        )
        return str(root_id or "").strip()
    except Error:
        return ""


def trigger_search_if_needed(page: Page) -> None:
    if get_first_root_id(page):
        return
    page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---searchTargetPage');
          const controller = view && view.getController && view.getController();
          if (controller && typeof controller.onSmartSearch === 'function') {
            controller.onSmartSearch();
          }
        }
        """
    )
    page.wait_for_timeout(2200)


def open_user_menu(page: Page) -> bool:
    return safe_click(page, ".shellUserBtn button, .shellUserBtn")


def refresh_user_menu(page: Page) -> bool:
    return safe_click(page, "[id$='shellUserRefreshButton']")


def toggle_theme(page: Page, times: int = 1, pause: int = 450) -> None:
    for _ in range(times):
        if not safe_click(page, ".themeDockSwitch", timeout=5000):
            break
        page.wait_for_timeout(pause)


def open_analytics(page: Page) -> bool:
    return safe_click(page, ".appShellHeader .shellActionBtn:has(.sapUiIcon[data-sap-ui-icon-content]), .appShellHeader .shellActionBtn")


def click_shell_button_by_text(page: Page, text: str) -> bool:
    try:
        button = page.locator(".appShellHeader .sapMBtn").filter(has_text=text).first
        button.wait_for(state="visible", timeout=5000)
        button.click(timeout=5000)
        page.wait_for_timeout(450)
        return True
    except TimeoutError:
        return False
    except Error:
        return False


def scroll_search(page: Page, distance: int) -> None:
    page.evaluate(
        """
        (value) => {
          const candidates = [
            document.querySelector('.sapMPageEnableScrolling'),
            document.querySelector('#checklist_app_comp---searchTargetPage'),
            document.scrollingElement
          ].filter(Boolean);
          const target = candidates.find((node) => (node.scrollHeight - node.clientHeight) > 120) || document.scrollingElement;
          target.scrollTop = Math.max(0, Math.min(target.scrollHeight - target.clientHeight, value));
        }
        """,
        distance,
    )
    page.wait_for_timeout(500)


def scroll_detail(page: Page, distance: int) -> None:
    page.evaluate(
        """
        (value) => {
          const candidates = [
            document.querySelector('#checklist_app_comp---detailTargetPage'),
            document.querySelector('#checklist_app_comp---detailTargetPage--detailObjectPage'),
            document.querySelector('.detailContent'),
            document.scrollingElement
          ].filter(Boolean);
          const target = candidates.find((node) => (node.scrollHeight - node.clientHeight) > 120) || document.scrollingElement;
          target.scrollTop = Math.max(0, Math.min(target.scrollHeight - target.clientHeight, value));
        }
        """,
        distance,
    )
    page.wait_for_timeout(600)


def reset_detail_scroll(page: Page) -> None:
    scroll_detail(page, 0)


def maybe_open_table_overflow(page: Page) -> bool:
    selectors = [
        "#checklist_app_comp---searchTargetPage--smartTableCustomToolbar-overflowButton",
        "#checklist_app_comp---searchTargetPage--searchResultsActionRail-overflowButton",
        ".searchSmartToolbarRail .sapMTBOverflowButton",
        ".searchResultsActionRail .sapMTBOverflowButton",
    ]
    for selector in selectors:
        if safe_click(page, selector, timeout=2500):
            return True
    return False


def open_first_detail(page: Page) -> str:
    trigger_search_if_needed(page)
    root_id = get_first_root_id(page)
    if not root_id:
        return ""
    page.goto(f"{URL}#/checklist/{root_id}", wait_until="networkidle", timeout=90000)
    wait_for_detail_ready(page, 1700)
    return root_id


def run() -> int:
    if sync_playwright is None:
        print("[error] playwright is not installed. Install dependency and retry: pip install playwright && playwright install chromium")
        return 2

    OUT_DIR.mkdir(parents=True, exist_ok=True)

    report: dict[str, Any] = {
        "generatedAt": time.strftime("%Y-%m-%d %H:%M:%S"),
        "url": URL,
        "outputDir": str(OUT_DIR).replace("\\", "/"),
        "screenshots": [],
        "console": [],
        "pageErrors": [],
        "requestFailures": [],
        "runtime": {},
    }

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        context = browser.new_context(viewport={"width": 1440, "height": 960}, device_scale_factor=1)
        page = context.new_page()

        page.on(
            "console",
            lambda msg: report["console"].append(
                {
                    "type": msg.type,
                    "text": msg.text,
                    "location": msg.location,
                }
            )
        )
        page.on("pageerror", lambda exc: report["pageErrors"].append(str(exc)))
        page.on(
            "requestfailed",
            lambda req: report["requestFailures"].append(
                {
                    "url": req.url,
                    "method": req.method,
                    "failure": req.failure,
                }
            ),
        )

        page.goto(URL, wait_until="networkidle", timeout=90000)
        wait_for_app_ready(page, 1600)
        report["runtime"]["shellStateInitial"] = shell_state(page)

        capture(page, report["screenshots"], "search-desktop-initial", "Desktop shell and search initial state", full_page=True)

        if open_user_menu(page):
            capture(page, report["screenshots"], "user-menu-open", "User popover opened from shell")
            if refresh_user_menu(page):
                page.wait_for_timeout(1100)
                capture(page, report["screenshots"], "user-menu-refreshed", "User popover after refresh without closing")
            dismiss_overlays(page)

        if click_shell_button_by_text(page, "Analytics"):
            capture(page, report["screenshots"], "analytics-open", "Analytics action opened from shell")
            dismiss_overlays(page)

        toggle_theme(page, times=1)
        capture(page, report["screenshots"], "theme-toggle-1", "After first theme toggle", full_page=True)
        toggle_theme(page, times=3, pause=250)
        capture(page, report["screenshots"], "theme-toggle-stress", "After rapid theme toggles", full_page=True)

        scroll_search(page, 640)
        capture(page, report["screenshots"], "search-scrolled", "Search page after scrolling into results", full_page=True)

        if safe_click(page, "#checklist_app_comp---searchTargetPage--searchScrollAnchorButton", timeout=2500):
            capture(page, report["screenshots"], "search-scroll-anchor", "Scroll anchor returned focus toward filters", full_page=True)

        set_viewport(page, 1024, 900)
        capture(page, report["screenshots"], "search-tablet", "Tablet viewport search layout")

        set_viewport(page, 390, 844)
        scroll_search(page, 900)
        capture(page, report["screenshots"], "search-phone", "Phone viewport search layout")
        if maybe_open_table_overflow(page):
            capture(page, report["screenshots"], "search-phone-overflow", "Phone viewport toolbar overflow menu")
            dismiss_overlays(page)

        set_viewport(page, 1440, 960)
        page.goto(URL, wait_until="networkidle", timeout=90000)
        wait_for_app_ready(page, 1600)
        root_id = open_first_detail(page)
        report["runtime"]["firstRootId"] = root_id

        if root_id:
            capture(page, report["screenshots"], "detail-desktop-top", "Detail page top section", full_page=True)
            scroll_detail(page, 780)
            capture(page, report["screenshots"], "detail-desktop-scrolled", "Detail page after scroll into content", full_page=True)

            reset_detail_scroll(page)
            set_viewport(page, 900, 900)
            page.wait_for_timeout(700)
            capture(page, report["screenshots"], "detail-narrow", "Narrow detail layout")

            reset_detail_scroll(page)
            set_viewport(page, 390, 844)
            page.wait_for_timeout(700)
            capture(page, report["screenshots"], "detail-phone", "Phone detail layout")
        else:
            report["runtime"]["detailSkipped"] = "No visible root id was found in search results"

        browser.close()

    report_path = OUT_DIR / "visual-ui-crawl-report.json"
    report_path.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    print(json.dumps({"ok": True, "reportPath": str(report_path).replace("\\", "/"), "screenshots": len(report["screenshots"])}, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(run())
