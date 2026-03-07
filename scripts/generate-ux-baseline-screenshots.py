#!/usr/bin/env python3
"""Generate extended Morning/Night visual baseline screenshots for key UI states.

Usage:
  python scripts/generate-ux-baseline-screenshots.py [url] [output_dir]

Defaults:
  url = http://127.0.0.1:8080/index.html
  output_dir = docs/artifacts/ux/baseline
"""

from __future__ import annotations

import json
import pathlib
import sys
import time
from typing import Callable

try:
    from playwright.sync_api import Page, sync_playwright
except ModuleNotFoundError:
    sync_playwright = None

URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
OUT_DIR = pathlib.Path(sys.argv[2] if len(sys.argv) > 2 else "docs/artifacts/ux/baseline")
DETAIL_ROOT = "E49B679F518F4947BD7A0F2CC1C4AC46"


def set_theme(page: Page, mode: str) -> None:
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
    page.wait_for_timeout(350)


def wait_ui(page: Page, delay: int = 1000) -> None:
    page.wait_for_load_state("networkidle")
    page.wait_for_timeout(delay)


def wait_for_app_ready(page: Page, delay: int = 1200) -> None:
    page.wait_for_load_state("networkidle")
    page.wait_for_function(
        """
        () => {
          if (typeof sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
            return false;
          }
          var core = sap.ui.getCore();
          return !!core.byId('checklist_app_comp---app--searchPaneHost') && !!core.byId('checklist_app_comp---app--detailPaneHost');
        }
        """,
        timeout=90000,
    )
    page.wait_for_timeout(delay)


def close_open_dialogs(page: Page) -> None:
    page.evaluate(
        """
        () => {
          if (typeof sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
            return;
          }
          var core = sap.ui.getCore();
          Object.keys(core.mElements || {}).forEach((id) => {
            var control = core.byId(id);
            if (control && typeof control.isOpen === 'function' && control.isOpen() && typeof control.close === 'function') {
              control.close();
            }
          });
        }
        """
    )
    page.wait_for_timeout(400)


def open_detail(page: Page) -> None:
    page.goto(f"{URL}#/checklist/{DETAIL_ROOT}", wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1800)


def call_controller(page: Page, view_id: str, method: str) -> None:
    page.evaluate(
        """
        ([viewId, methodName]) => {
          var core = sap.ui.getCore();
          var view = core.byId(viewId);
          if (!view || !view.getController) {
            throw new Error("view-not-found:" + viewId);
          }
          var controller = view.getController();
          if (!controller || typeof controller[methodName] !== "function") {
            throw new Error("method-not-found:" + methodName);
          }
          return controller[methodName]();
        }
        """,
        [view_id, method],
    )


def hide_splitter_gutter(page: Page) -> None:
    page.evaluate(
        """
        () => {
          document.querySelectorAll('.sapUiLoSplitterBar').forEach((node) => {
            node.style.opacity = '0';
          });
        }
        """
    )


def capture(page: Page, path: pathlib.Path, selector: str | None = None) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if selector:
        page.locator(selector).screenshot(path=str(path))
        return
    page.screenshot(path=str(path))


def screenshot_state(
    page: Page,
    mode: str,
    state: str,
    action: Callable[[Page], None],
    selector: str | None = None,
) -> pathlib.Path:
    close_open_dialogs(page)
    action(page)
    set_theme(page, mode)
    hide_splitter_gutter(page)
    wait_ui(page, 700)
    out_path = OUT_DIR / f"{mode}-{state}.png"
    capture(page, out_path, selector)
    return out_path


def action_search(page: Page) -> None:
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1400)


def action_detail(page: Page) -> None:
    open_detail(page)


def action_analytics_dialog(page: Page) -> None:
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1200)
    call_controller(page, "checklist_app_comp---app--searchPaneHost", "onOpenWorkflowAnalytics")
    page.wait_for_selector(".sapMDialog", timeout=10000)
    wait_ui(page, 1200)


def action_checks_dialog(page: Page) -> None:
    open_detail(page)
    call_controller(page, "checklist_app_comp---app--detailPaneHost", "onExpandChecks")
    page.wait_for_selector(".sapMDialog", timeout=10000)
    wait_ui(page, 1200)


def action_barriers_dialog(page: Page) -> None:
    open_detail(page)
    call_controller(page, "checklist_app_comp---app--detailPaneHost", "onExpandBarriers")
    page.wait_for_selector(".sapMDialog", timeout=10000)
    wait_ui(page, 1200)


def action_location_dialog(page: Page) -> None:
    open_detail(page)
    call_controller(page, "checklist_app_comp---app--detailPaneHost", "onOpenLocationValueHelp")
    page.wait_for_selector(".sapMDialog", timeout=10000)
    wait_ui(page, 1200)


def build_matrix(paths: dict[str, dict[str, pathlib.Path]]) -> dict:
    flow_map = {
        "search": "search",
        "detail": "detail",
        "dialogs": "analytics-dialog",
    }
    flows = {}
    for flow, state in flow_map.items():
        flows[flow] = {
            "morning": {"path": str(paths["morning"][state]).replace("\\", "/"), "status": "pass"},
            "night": {"path": str(paths["night"][state]).replace("\\", "/"), "status": "pass"},
        }
    return {
        "generatedAt": time.strftime("%Y-%m-%d %H:%M:%S"),
        "flows": flows,
        "summary": {"coverage": 1, "pairedStates": len(flows)},
    }


def main() -> int:
    if sync_playwright is None:
        print("[error] playwright is not installed. Install dependency and retry: pip install playwright && playwright install chromium")
        return 2

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    states = {
        "search": (action_search, "#checklist_app_comp---app--searchPaneHost"),
        "detail": (action_detail, "#checklist_app_comp---app--detailPaneHost"),
        "analytics-dialog": (action_analytics_dialog, "#checklist_app_comp---app--searchPaneHost--workflowAnalyticsDialog"),
        "checks-dialog": (action_checks_dialog, "#checklist_app_comp---app--detailPaneHost--checksExpandedDialog"),
        "barriers-dialog": (action_barriers_dialog, "#checklist_app_comp---app--detailPaneHost--barriersExpandedDialog"),
        "location-dialog": (action_location_dialog, "#checklist_app_comp---app--detailPaneHost--locationValueHelpDialog"),
    }
    captured: dict[str, dict[str, pathlib.Path]] = {"morning": {}, "night": {}}

    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": 1440, "height": 900, "device_scale_factor": 1})

        for mode in ("morning", "night"):
            for state, (action, selector) in states.items():
                captured[mode][state] = screenshot_state(page, mode, state, action, selector)

        browser.close()

    matrix = build_matrix(captured)
    matrix_path = pathlib.Path("docs/artifacts/pair-snapshots-baseline-matrix.json")
    matrix_path.parent.mkdir(parents=True, exist_ok=True)
    matrix_path.write_text(json.dumps(matrix, indent=2), encoding="utf-8")

    now = time.strftime("%Y-%m-%d %H:%M:%S")
    print(f"[ok] baseline screenshots generated at {OUT_DIR} ({now})")
    print("[files] " + ", ".join(sorted(path.name for mode in captured.values() for path in mode.values())))
    print(f"[matrix] {matrix_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
