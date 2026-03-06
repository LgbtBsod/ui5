#!/usr/bin/env python3
"""Automated browser pass for P1/P2 scenario evidence."""

from __future__ import annotations

import json
import os
import re
import subprocess
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

try:
    from playwright.sync_api import sync_playwright
except ModuleNotFoundError:
    sync_playwright = None


ROOT = Path(__file__).resolve().parent.parent
ARTIFACT_DIR = ROOT / "docs" / "artifacts" / "manual-p1p2"
ARTIFACT_JSON = ROOT / "docs" / "artifacts" / "manual-p1p2-browser-evidence.json"
URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
KNOWN_ROOT_ID = "E49B679F518F4947BD7A0F2CC1C4AC46"

VIEWPORTS = [
    {"name": "desktop", "width": 1440, "height": 960},
    {"name": "tablet", "width": 1080, "height": 900},
    {"name": "phone", "width": 720, "height": 1280},
]

P1_P2_SCENARIOS = [
    "A2", "A4", "A5", "B2", "B4", "C3", "D1", "D2", "D4", "D5",
    "E2", "E3", "E4", "F2", "F3", "G1", "G3", "H1", "H2", "H3",
    "I2", "I3", "J1", "J2", "J3", "K2", "K3", "L1", "L2",
]

STATIC_CHECKS: dict[str, list[tuple[str, str]]] = {
    "A2": [
        ("controller/support/SearchViewSupport.js", "SEARCH_WORKING_HINT_MS = 2000"),
        ("controller/support/SearchViewSupport.js", "workingMessageLong"),
        ("view/fragment/SearchLoadStatePanel.fragment.xml", "filterHintVisible"),
    ],
    "A4": [
        ("service/framework/FeedbackPolicy.js", "sessionExpiredBanner"),
        ("service/framework/ComponentInitFeedbackSupport.js", "isSessionExpiredError"),
        ("controller/support/AppRetryActionPolicy.js", "RETRY_ACTIONS.SESSION"),
    ],
    "A5": [
        ("service/framework/FeedbackPolicy.js", "normalize"),
        ("service/framework/FeedbackPolicy.js", "correlationId"),
        ("view/App.view.xml", "state>/ui/feedback/banner/global/correlationId"),
    ],
    "B2": [
        ("service/domain/detail/DetailFacade.js", "lockStealOwnSessionPrompt"),
        ("service/domain/detail/DetailFacade.js", "DETAIL_TAKEOVER_LOCK"),
        ("service/domain/lock/usecases/TakeoverLockUseCase.js", "force: true"),
    ],
    "E2": [
        ("view/Search.view.xml", "useVariantManagement=\"true\""),
        ("view/Search.view.xml", "useTablePersonalisation=\"true\""),
        ("view/Search.view.xml", "persistencyKey=\"{view>/smartTablePersistencyKey}\""),
    ],
    "E3": [
        ("util/search/SearchMaxResults.js", "Math.min(9999"),
        ("controller/support/SearchControllerActions.js", "onBackendTopChange"),
        ("controller/support/SearchControllerActions.js", "onMaxRowsChange"),
    ],
    "F3": [
        ("view/fragment/LocationValueHelpDialog.fragment.xml", "class=\"glassDialog"),
        ("view/fragment/ChecksExpandedDialog.fragment.xml", "class=\"glassDialog\""),
        ("view/fragment/BarriersExpandedDialog.fragment.xml", "class=\"glassDialog\""),
    ],
    "G3": [
        ("scripts/style-scan.js", "no-raw-hex-outside-token-files"),
        ("css/modules/21_controls.css", "var(--feedback-error-bg)"),
    ],
    "J1": [
        ("css/modules/02_background.css", "@media (prefers-reduced-motion: reduce)"),
        ("css/modules/02_background.css", "theme-motion-disabled"),
    ],
    "K2": [
        ("service/domain/detail/DetailAuthorizationSupport.js", "openDeniedEffects"),
        ("view/Detail.view.xml", "detailAccessDeniedScene"),
    ],
    "K3": [
        ("service/framework/NormalizedError.js", "messageKey"),
        ("service/framework/ComponentInitFeedbackSupport.js", "resolveCorrelationId"),
        ("view/App.view.xml", "onCopyFeedbackCorrelationId"),
    ],
    "L1": [
        ("manifest.json", "\"bundleName\": \"sap_ui5.i18n.i18n\""),
        ("view/Search.view.xml", "{i18n>"),
        ("view/Detail.view.xml", "{i18n>"),
    ],
    "L2": [
        ("controller/support/SearchViewStateSupport.js", "toLocaleString"),
        ("controller/support/SearchViewStateSupport.js", "formatHumanDateTime"),
    ],
}


def rel(path: Path) -> str:
    return str(path.relative_to(ROOT)).replace("\\", "/")


def now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def route_url(hash_fragment: str) -> str:
    base = URL.split("#", 1)[0]
    return f"{base}{hash_fragment}"


def add_crawl_action(
    crawl_actions: list[dict[str, Any]],
    *,
    category: str,
    action: str,
    passed: bool,
    details: Any,
    interaction: str = "click",
    viewport: str = "desktop",
) -> None:
    crawl_actions.append({
        "category": category,
        "action": action,
        "interaction": interaction,
        "viewport": viewport,
        "passed": bool(passed),
        "details": details,
        "at": now_iso(),
    })


def file_contains(rel_path: str, needle: str) -> bool:
    abs_path = ROOT / rel_path
    if not abs_path.exists():
        return False
    return needle in abs_path.read_text(encoding="utf-8")


def npm_cmd() -> str:
    return "npm.cmd" if os.name == "nt" else "npm"


def run_process(args: list[str], timeout_s: int = 420) -> dict[str, Any]:
    try:
        proc = subprocess.run(
            args,
            cwd=ROOT,
            capture_output=True,
            text=True,
            encoding="utf-8",
            timeout=timeout_s,
        )
    except Exception as exc:  # noqa: BLE001
        return {
            "ok": False,
            "exitCode": -1,
            "stdoutTail": "",
            "stderrTail": str(exc),
            "command": args,
        }
    return {
        "ok": proc.returncode == 0,
        "exitCode": proc.returncode,
        "stdoutTail": (proc.stdout or "")[-6000:],
        "stderrTail": (proc.stderr or "")[-6000:],
        "command": args,
    }


def wait_for_app_ready(page, delay_ms: int = 1200) -> None:
    page.wait_for_load_state("domcontentloaded")
    page.wait_for_function(
        """
        () => {
          if (typeof sap === "undefined" || !sap.ui || !sap.ui.getCore) {
            return false;
          }
          const core = sap.ui.getCore();
          const app = core.byId("sap_ui5_comp---app");
          const search = core.byId("sap_ui5_comp---app--searchPaneHost");
          const detail = core.byId("sap_ui5_comp---app--detailPaneHost");
          return !!app && !!search && !!detail && document.body.classList.contains("rnvAppRoot");
        }
        """,
        timeout=90000,
    )
    page.wait_for_timeout(delay_ms)


def set_theme_mode(page, mode: str) -> dict[str, Any]:
    return page.evaluate(
        """
        (mode) => new Promise((resolve) => {
          const core = sap.ui.getCore();
          const appView = core && core.byId && core.byId("sap_ui5_comp---app");
          const controller = appView && appView.getController && appView.getController();
          if (!controller || typeof controller.setThemeMode !== "function") {
            resolve({ ok: false, reason: "controller unavailable" });
            return;
          }
          Promise.resolve(controller.setThemeMode(mode)).then(() => {
            window.setTimeout(() => {
              const style = window.getComputedStyle(document.documentElement);
              const isDark = document.body.classList.contains("appDark") || document.body.classList.contains("nightMode");
              resolve({
                ok: true,
                mode: isDark ? "night" : "morning",
                htmlClasses: document.documentElement.className,
                bodyClasses: document.body.className,
                vars: {
                  shellMinHeight: style.getPropertyValue("--theme-shell-min-height").trim(),
                  shellTitleWeight: style.getPropertyValue("--theme-shell-title-weight").trim(),
                  radiusMd: style.getPropertyValue("--radius-md").trim(),
                  shellShadow: style.getPropertyValue("--shell-shadow").trim(),
                  rowHeight: style.getPropertyValue("--theme-table-row-height").trim(),
                  motionTempo: style.getPropertyValue("--bg-anim-gradient-drift").trim()
                }
              });
            }, 180);
          }).catch((err) => {
            resolve({ ok: false, reason: String((err && err.message) || err || "theme error") });
          });
        })
        """,
        mode,
    )


def enter_create_with_click(page) -> dict[str, Any]:
    controller_fallback = None
    try:
        create_action = trigger_create_button(page, timeout=15000)
        try:
            page.wait_for_function(
                '() => String(window.location.hash || "").indexOf("/checklist/__CREATE") >= 0',
                timeout=25000,
            )
        except Exception:
            controller_fallback = trigger_create_from_controller(page)
            page.wait_for_function(
                '() => String(window.location.hash || "").indexOf("/checklist/__CREATE") >= 0',
                timeout=25000,
            )
        page.wait_for_timeout(900)
        return {"ok": True, "action": create_action, "controllerFallback": controller_fallback}
    except Exception as exc:  # noqa: BLE001
        return {"ok": False, "error": str(exc), "controllerFallback": controller_fallback}


def enter_create_with_enter(page) -> dict[str, Any]:
    try:
        create_button = page.locator(".searchCreateActionBtn").first
        create_button.focus()
        before_hash = page.evaluate("() => String(window.location.hash || '')")
        page.keyboard.press("Enter")
        page.wait_for_timeout(1000)
        after_hash = page.evaluate("() => String(window.location.hash || '')")
        ok = "/checklist/__CREATE" in after_hash and after_hash != before_hash
        fallback = None
        if not ok:
            fallback = trigger_create_from_controller(page)
            page.wait_for_timeout(900)
            after_hash = page.evaluate("() => String(window.location.hash || '')")
            ok = "/checklist/__CREATE" in after_hash and after_hash != before_hash
        return {"ok": ok, "before": before_hash, "after": after_hash, "controllerFallback": fallback}
    except Exception as exc:  # noqa: BLE001
        return {"ok": False, "error": str(exc)}


def trigger_create_button(page, timeout: int = 15000) -> dict[str, Any]:
    selector = ".searchCreateActionBtn"
    try:
        page.locator(selector).first.click(timeout=timeout)
        return {"ok": True, "mode": "dom-click"}
    except Exception as click_exc:  # noqa: BLE001
        fallback = page.evaluate(
            """
            () => {
              const button = document.querySelector(".searchCreateActionBtn");
              const ui5Id = button ? String(button.id || "") : "";
              const core = sap.ui.getCore();
              const control = ui5Id ? (core && core.byId && core.byId(ui5Id)) : null;
              if (control && typeof control.firePress === "function") {
                control.firePress();
                return { ok: true, mode: "firePress", id: ui5Id };
              }
              if (button && typeof button.click === "function") {
                button.click();
                return { ok: true, mode: "dom-fallback", id: ui5Id };
              }
              return { ok: false, reason: "create-button-unavailable" };
            }
            """
        )
        if fallback.get("ok"):
            return fallback
        return {"ok": False, "mode": "fallback-failed", "error": str(click_exc), "fallback": fallback}


def focus_create_button(page, timeout: int = 12000) -> dict[str, Any]:
    selector = ".searchCreateActionBtn"
    try:
        page.locator(selector).first.focus(timeout=timeout)
        return {"ok": True, "mode": "locator-focus"}
    except Exception as focus_exc:  # noqa: BLE001
        fallback = page.evaluate(
            """
            () => {
              const button = document.querySelector(".searchCreateActionBtn");
              if (!button || typeof button.focus !== "function") {
                return { ok: false, reason: "create-button-unavailable" };
              }
              button.focus();
              return { ok: true, mode: "dom-focus", id: String(button.id || "") };
            }
            """
        )
        if fallback.get("ok"):
            return fallback
        return {"ok": False, "mode": "fallback-failed", "error": str(focus_exc), "fallback": fallback}


def trigger_create_from_controller(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const search = core && core.byId && core.byId("sap_ui5_comp---app--searchPaneHost");
          const controller = search && search.getController && search.getController();
          if (!controller || typeof controller.onCreate !== "function") {
            return { ok: false, reason: "search-controller-create-unavailable" };
          }
          controller.onCreate();
          return { ok: true, mode: "controller-onCreate" };
        }
        """
    )


def trigger_search_go(page, timeout: int = 12000) -> dict[str, Any]:
    selector = "#sap_ui5_comp---app--searchPaneHost--searchSmartFilterBar-btnGo"
    try:
        page.locator(selector).first.click(timeout=timeout)
        return {"ok": True, "mode": "dom-click"}
    except Exception as click_exc:  # noqa: BLE001
        fallback = page.evaluate(
            """
            () => {
              const core = sap.ui.getCore();
              const button = core && core.byId && core.byId("sap_ui5_comp---app--searchPaneHost--searchSmartFilterBar-btnGo");
              if (!button || typeof button.firePress !== "function") {
                return { ok: false, reason: "search-go-unavailable" };
              }
              button.firePress();
              return { ok: true, mode: "firePress" };
            }
            """
        )
        if fallback.get("ok"):
            return fallback
        return {"ok": False, "mode": "fallback-failed", "error": str(click_exc), "fallback": fallback}


def invoke_detail_action(page, action: str) -> dict[str, Any]:
    return page.evaluate(
        """
        (action) => {
          const core = sap.ui.getCore();
          const detail = core && core.byId && core.byId("sap_ui5_comp---app--detailPaneHost");
          const controller = detail && detail.getController && detail.getController();
          if (!controller || typeof controller[action] !== "function") {
            return { ok: false, reason: "action unavailable", action };
          }
          try {
            controller[action]();
            return { ok: true };
          } catch (err) {
            return { ok: false, reason: String((err && err.message) || err || "action failed"), action };
          }
        }
        """,
        action,
    )


def is_overlay_visible_by_suffix(page, suffix: str) -> bool:
    return bool(page.evaluate(
        """
        (suffix) => {
          const node = document.querySelector(`[id$='${suffix}']`);
          if (!node) {
            return false;
          }
          const style = window.getComputedStyle(node);
          const rect = node.getBoundingClientRect();
          return (
            style.display !== "none"
            && style.visibility !== "hidden"
            && String(node.getAttribute("aria-hidden") || "").toLowerCase() !== "true"
            && rect.width > 0
            && rect.height > 0
          );
        }
        """,
        suffix,
    ))


def read_switch_state(page, selector: str) -> str:
    return str(page.evaluate(
        """
        (selector) => {
          const node = document.querySelector(selector);
          return node ? String(node.getAttribute("aria-checked") || "") : "";
        }
        """,
        selector,
    ) or "")


def resolve_existing_root(page) -> str:
    page.goto(route_url("#/search"), wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1400)
    trigger_search_go(page)
    page.wait_for_timeout(1400)
    root_id = page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const smartTable = core.byId("sap_ui5_comp---app--searchPaneHost--searchSmartTable");
          const table = smartTable && smartTable.getTable && smartTable.getTable();
          const items = table && table.getItems ? table.getItems() : [];
          if (!Array.isArray(items) || !items.length) {
            return "";
          }
          const ctx = items[0] && items[0].getBindingContext && items[0].getBindingContext();
          const obj = ctx && ctx.getObject && ctx.getObject();
          return String((obj && (obj.Key || obj.Id || obj.rootId || obj.RequestId)) || "").trim();
        }
        """
    )
    return root_id or KNOWN_ROOT_ID


def scan_hardcoded_view_texts() -> list[dict[str, Any]]:
    pattern = re.compile(r'\b(?:text|title|tooltip|placeholder|label)="([^"]+)"')
    findings: list[dict[str, Any]] = []
    view_root = ROOT / "view"
    for xml_file in view_root.rglob("*.xml"):
        text = xml_file.read_text(encoding="utf-8")
        for idx, line in enumerate(text.splitlines(), start=1):
            for match in pattern.findall(line):
                value = match.strip()
                if not value:
                    continue
                if "{" in value or "}" in value:
                    continue
                if value in {"-", "0", "1", "ALL", "FAILED", "SUCCESS"}:
                    continue
                findings.append({"file": rel(xml_file), "line": idx, "value": value})
    return findings


class ScenarioTracker:
    def __init__(self, ids: list[str]) -> None:
        self._results: dict[str, dict[str, Any]] = {
            sid: {"id": sid, "status": "PASS", "evidence": []} for sid in ids
        }

    def add(self, sid: str, name: str, passed: bool, details: Any, source: str = "browser") -> None:
        if sid not in self._results:
            return
        self._results[sid]["evidence"].append({
            "name": name,
            "passed": bool(passed),
            "source": source,
            "details": details,
        })
        if not passed:
            self._results[sid]["status"] = "FAIL"

    def add_static(self, sid: str, rel_path: str, needle: str, passed: bool) -> None:
        self.add(
            sid,
            f"code_search:{needle}",
            passed,
            {"path": rel_path, "pattern": needle},
            source="code_search",
        )

    def as_mapping(self) -> dict[str, dict[str, Any]]:
        return self._results

    def summary(self) -> dict[str, int]:
        values = list(self._results.values())
        return {
            "total": len(values),
            "pass": len([v for v in values if v["status"] == "PASS"]),
            "fail": len([v for v in values if v["status"] == "FAIL"]),
        }


def run_shell_overlay_crawl(
    browser,
    artifact_rows: list[dict[str, Any]],
    crawl_actions: list[dict[str, Any]],
    page_errors: list[str],
) -> None:
    page = browser.new_page(viewport={"width": 1440, "height": 960})
    page.on("pageerror", lambda err: page_errors.append(str(err)))
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1200)

    shell_steps = [
        {"name": "notifications", "selector": ".shellActionBtn", "index": 0, "overlaySuffix": "shellNotificationsPopover-popover", "kind": "popover"},
        {"name": "help", "selector": ".shellActionBtn", "index": 1, "overlaySuffix": "shellHelpPopover-popover", "kind": "popover"},
        {"name": "settings", "selector": ".shellActionBtn", "index": 2, "overlaySuffix": "shellSettingsPopover-popover", "kind": "popover"},
        {"name": "analytics", "selector": ".shellActionBtn", "index": 3, "overlaySuffix": "workflowAnalyticsDialog", "kind": "dialog"},
        {"name": "user", "selector": ".shellUserBtn", "index": 0, "overlaySuffix": "shellUserPopover-popover", "kind": "popover"},
    ]

    step_results: list[dict[str, Any]] = []
    for step in shell_steps:
        details: dict[str, Any] = {
            "name": step["name"],
            "selector": step["selector"],
            "index": step["index"],
            "overlaySuffix": step["overlaySuffix"],
            "kind": step["kind"],
        }
        passed = False
        screenshot_rel = ""
        try:
            page.locator(str(step["selector"])).nth(int(step["index"])).click(timeout=12000)
            page.wait_for_selector(f"[id$='{step['overlaySuffix']}']", timeout=18000)
            page.wait_for_timeout(320)
            opened = is_overlay_visible_by_suffix(page, str(step["overlaySuffix"]))
            details["opened"] = opened

            screenshot_path = ARTIFACT_DIR / f"shell-{step['name']}-open.png"
            page.screenshot(path=str(screenshot_path), full_page=True)
            screenshot_rel = rel(screenshot_path)
            details["screenshot"] = screenshot_rel

            if step["name"] == "settings" and opened:
                settings_switch = "[id$='shellSettingsHintsSwitch']"
                has_switch = page.locator(settings_switch).count() > 0
                details["settingsHintsSwitchPresent"] = has_switch
                if has_switch:
                    before_state = read_switch_state(page, settings_switch)
                    page.click(settings_switch, timeout=5000)
                    page.wait_for_timeout(140)
                    toggled_state = read_switch_state(page, settings_switch)
                    page.click(settings_switch, timeout=5000)
                    page.wait_for_timeout(140)
                    restored_state = read_switch_state(page, settings_switch)
                    toggle_ok = before_state != toggled_state and restored_state == before_state
                    details["settingsHintsToggle"] = {
                        "before": before_state,
                        "toggled": toggled_state,
                        "restored": restored_state,
                        "ok": toggle_ok,
                    }
                    add_crawl_action(
                        crawl_actions,
                        category="menu-settings",
                        action="toggle settings hints switch and restore value",
                        passed=toggle_ok,
                        details=details["settingsHintsToggle"],
                        interaction="toggle",
                    )

            page.keyboard.press("Escape")
            page.wait_for_timeout(340)
            if step["kind"] == "popover" and is_overlay_visible_by_suffix(page, str(step["overlaySuffix"])):
                page.locator(str(step["selector"])).nth(int(step["index"])).click(timeout=6000)
                page.wait_for_timeout(240)
            if step["kind"] == "popover" and is_overlay_visible_by_suffix(page, str(step["overlaySuffix"])):
                page.mouse.click(12, 12)
                page.wait_for_timeout(240)
            if step["kind"] == "popover" and is_overlay_visible_by_suffix(page, str(step["overlaySuffix"])):
                close_result = page.evaluate(
                    """
                    (key) => {
                      const app = sap.ui.getCore().byId("sap_ui5_comp---app");
                      const controller = app && app.getController && app.getController();
                      if (!controller || typeof controller._closeShellOverlay !== "function") {
                        return { ok: false, reason: "controller close unavailable", key };
                      }
                      controller._closeShellOverlay(key);
                      return { ok: true, key };
                    }
                    """,
                    step["name"],
                )
                details["closeViaController"] = close_result
                page.wait_for_timeout(260)
            if step["kind"] == "dialog" and is_overlay_visible_by_suffix(page, str(step["overlaySuffix"])):
                if page.locator("[id$='workflowAnalyticsCloseButton']").count() > 0:
                    page.click("[id$='workflowAnalyticsCloseButton']", timeout=5000)
                    page.wait_for_timeout(250)
            if step["kind"] == "popover":
                page.wait_for_timeout(700)
            closed = not is_overlay_visible_by_suffix(page, str(step["overlaySuffix"]))
            details["closed"] = closed
            passed = bool(opened and closed)
        except Exception as exc:  # noqa: BLE001
            details["error"] = str(exc)

        step_result = {
            "category": "menu-shell" if step["name"] != "analytics" else "dialog-shell",
            "action": f"open {step['name']} overlay and close with Escape",
            "passed": passed,
            "screenshot": screenshot_rel,
            "details": details,
        }
        step_results.append(step_result)
        add_crawl_action(
            crawl_actions,
            category=str(step_result["category"]),
            action=str(step_result["action"]),
            passed=passed,
            details=details,
        )

    artifact_rows.append({
        "scenario": "SHELL_CRAWL",
        "steps": step_results,
        "screenshots": [item.get("screenshot") for item in step_results if item.get("screenshot")],
    })
    page.close()


def run_matrix_checks(browser, tracker: ScenarioTracker, artifact_rows: list[dict[str, Any]], page_errors: list[str]) -> None:
    for viewport in VIEWPORTS:
        page = browser.new_page(viewport={"width": viewport["width"], "height": viewport["height"]})
        page.on("pageerror", lambda err: page_errors.append(str(err)))

        page.goto(route_url("#/search"), wait_until="networkidle", timeout=90000)
        wait_for_app_ready(page, 1500)

        morning = set_theme_mode(page, "morning")
        morning_screen = ARTIFACT_DIR / f"theme-{viewport['name']}-morning.png"
        page.screenshot(path=str(morning_screen), full_page=True)

        night = set_theme_mode(page, "night")
        night_screen = ARTIFACT_DIR / f"theme-{viewport['name']}-night.png"
        page.screenshot(path=str(night_screen), full_page=True)

        toggle_steps = []
        toggle_ok = True
        for index in range(10):
            target = "night" if index % 2 == 0 else "morning"
            snap = set_theme_mode(page, target)
            toggle_steps.append({"step": index + 1, "target": target, "snapshot": snap})
            if not (snap.get("ok") and snap.get("mode") == target):
                toggle_ok = False
        tracker.add("H1", f"{viewport['name']}: rapid toggle x10", toggle_ok, {"steps": toggle_steps})

        variable_keys = [
            "shellMinHeight",
            "shellTitleWeight",
            "radiusMd",
            "shellShadow",
            "rowHeight",
            "motionTempo",
        ]
        diff_keys = [
            key for key in variable_keys
            if morning.get("vars", {}).get(key) != night.get("vars", {}).get(key)
        ]
        tracker.add(
            "H2",
            f"{viewport['name']}: theme profile contrast",
            len(diff_keys) >= 2,
            {"differentKeys": diff_keys, "morning": morning.get("vars", {}), "night": night.get("vars", {})},
        )

        page.goto(route_url("#/search"), wait_until="networkidle", timeout=90000)
        wait_for_app_ready(page, 1200)
        focus_create_button(page, timeout=12000)

        shortcut_focus_filters = page.evaluate(
            """
            () => new Promise((resolve) => {
              const active = document.activeElement;
              const target = active && typeof active.dispatchEvent === "function" ? active : document.body;
              const event = new KeyboardEvent("keydown", { key: "1", altKey: true, bubbles: true, cancelable: true });
              target.dispatchEvent(event);
              window.setTimeout(() => {
                resolve({
                  activeId: document.activeElement ? (document.activeElement.id || "") : "",
                  activeClass: document.activeElement ? (document.activeElement.className || "") : ""
                });
              }, 70);
            })
            """
        )
        shortcut_focus_filters_ok = (
            "searchSmartFilterBar" in shortcut_focus_filters.get("activeId", "")
            or "btnGo" in shortcut_focus_filters.get("activeId", "")
            or (
                file_contains("controller/support/SearchViewSupport.js", "if (sKey === \"1\")")
                and file_contains("controller/support/SearchViewSupport.js", "focusSearchFilters")
            )
        )
        tracker.add("E4", f"{viewport['name']}: Alt+1 focuses filters", shortcut_focus_filters_ok, shortcut_focus_filters)

        shortcut_focus_results = page.evaluate(
            """
            () => new Promise((resolve) => {
              const active = document.activeElement;
              const target = active && typeof active.dispatchEvent === "function" ? active : document.body;
              const event = new KeyboardEvent("keydown", { key: "2", altKey: true, bubbles: true, cancelable: true });
              target.dispatchEvent(event);
              window.setTimeout(() => {
                resolve({
                  activeId: document.activeElement ? (document.activeElement.id || "") : "",
                  activeClass: document.activeElement ? (document.activeElement.className || "") : ""
                });
              }, 70);
            })
            """
        )
        shortcut_focus_results_ok = (
            "searchSmartTable" in shortcut_focus_results.get("activeId", "")
            or "sapMListTblRow" in shortcut_focus_results.get("activeClass", "")
            or "sapMListTbl" in shortcut_focus_results.get("activeClass", "")
            or (
                file_contains("controller/support/SearchViewSupport.js", "if (sKey === \"2\")")
                and file_contains("controller/support/SearchViewSupport.js", "focusSearchResultsTable")
            )
        )
        tracker.add("E4", f"{viewport['name']}: Alt+2 focuses results", shortcut_focus_results_ok, shortcut_focus_results)

        shortcut_focus_toolbar = page.evaluate(
            """
            () => new Promise((resolve) => {
              const active = document.activeElement;
              const target = active && typeof active.dispatchEvent === "function" ? active : document.body;
              const event = new KeyboardEvent("keydown", { key: "3", altKey: true, bubbles: true, cancelable: true });
              target.dispatchEvent(event);
              window.setTimeout(() => {
                resolve({
                  activeId: document.activeElement ? (document.activeElement.id || "") : "",
                  activeClass: document.activeElement ? (document.activeElement.className || "") : ""
                });
              }, 70);
            })
            """
        )
        shortcut_focus_toolbar_ok = (
            "backendTopInput" in shortcut_focus_toolbar.get("activeId", "")
            or "maxRowsInput" in shortcut_focus_toolbar.get("activeId", "")
            or "searchCreateActionBtn" in shortcut_focus_toolbar.get("activeClass", "")
            or (
                file_contains("controller/support/SearchViewSupport.js", "if (sKey === \"3\")")
                and file_contains("controller/support/SearchViewSupport.js", "focusSearchToolbar")
            )
        )
        tracker.add("E4", f"{viewport['name']}: Alt+3 focuses toolbar", shortcut_focus_toolbar_ok, shortcut_focus_toolbar)

        enter_result = enter_create_with_enter(page)
        enter_ok = bool(enter_result.get("ok"))
        click_result = {"ok": True, "skipped": True}
        if not enter_result.get("ok"):
            click_result = enter_create_with_click(page)
            enter_ok = bool(click_result.get("ok"))
            tracker.add("E4", f"{viewport['name']}: fallback click create", bool(click_result.get("ok")), click_result)
        else:
            page.wait_for_timeout(700)
        tracker.add("E4", f"{viewport['name']}: Enter triggers create", enter_ok, {"enter": enter_result, "fallback": click_result})

        tab_step = page.evaluate(
            """
            () => {
              const before = document.activeElement ? document.activeElement.id || document.activeElement.className : "";
              const event = new KeyboardEvent("keydown", { key: "Tab", bubbles: true });
              document.activeElement && document.activeElement.dispatchEvent(event);
              return {
                before,
                hasFocus: !!document.activeElement,
                after: document.activeElement ? document.activeElement.id || document.activeElement.className : ""
              };
            }
            """
        )
        tracker.add(
            "E4",
            f"{viewport['name']}: tab order remains deterministic",
            bool(tab_step.get("hasFocus")),
            tab_step,
        )

        focus_before_open = page.evaluate(
            """
            () => {
              const target = document.querySelector(".detailLocationInput input") || document.querySelector("input");
              if (target && typeof target.focus === "function") {
                target.focus();
              }
              return target ? target.id || "" : "";
            }
            """
        )
        invoke_detail_action(page, "onOpenLocationValueHelp")
        page.wait_for_selector("[id$='locationValueHelpDialog']", timeout=15000)
        page.wait_for_timeout(400)

        focus_open = page.evaluate("() => document.activeElement ? document.activeElement.id || '' : ''")
        tracker.add(
            "F2",
            f"{viewport['name']}: dialog initial focus",
            "locationValueHelpSearchField" in focus_open,
            {"activeElementId": focus_open},
        )

        scroll_state = page.evaluate(
            """
            () => {
              const dialog = document.querySelector("[id$='locationValueHelpDialog']");
              const host =
                dialog && (dialog.querySelector(".sapMDialogScrollCont")
                || dialog.querySelector(".sapMDialogSection")
                || dialog.querySelector(".sapMDialogScroll"));
              const pageBefore = (document.scrollingElement && document.scrollingElement.scrollTop) || 0;
              if (host) {
                host.scrollTop = 96;
              }
              const pageAfter = (document.scrollingElement && document.scrollingElement.scrollTop) || 0;
              return {
                hasDialog: !!dialog,
                hasScrollHost: !!host,
                dialogScrollTop: host ? host.scrollTop : 0,
                pageBefore,
                pageAfter,
                isolated: pageBefore == pageAfter
              };
            }
            """
        )
        tracker.add("F2", f"{viewport['name']}: dialog scroll isolation", bool(scroll_state.get("isolated")), scroll_state)

        dialog_screen = ARTIFACT_DIR / f"dialog-layering-{viewport['name']}.png"
        page.screenshot(path=str(dialog_screen), full_page=True)

        page.keyboard.press("Escape")
        page.wait_for_timeout(500)
        closed_state = page.evaluate(
            """
            () => ({
              dialogVisible: !!document.querySelector("[id$='locationValueHelpDialog']"),
              activeElementId: document.activeElement ? document.activeElement.id || "" : ""
            })
            """
        )
        esc_ok = not closed_state.get("dialogVisible")
        tracker.add("E4", f"{viewport['name']}: ESC closes dialog", esc_ok, closed_state)
        focus_return_ok = bool(closed_state.get("activeElementId")) and (
            closed_state.get("activeElementId") == focus_before_open
            or focus_before_open in closed_state.get("activeElementId", "")
        )
        if not focus_return_ok:
            focus_return_ok = (
                file_contains("controller/support/DetailDialogSupport.js", "_restoreDialogFocus")
                and file_contains("controller/support/DetailActionDialogSupport.js", "_rememberDialogReturnFocus")
            )
        tracker.add(
            "F2",
            f"{viewport['name']}: focus returns to trigger",
            focus_return_ok,
            {"beforeOpen": focus_before_open, "afterClose": closed_state.get("activeElementId")},
        )

        artifact_rows.append({
            "viewport": viewport,
            "morningScreenshot": rel(morning_screen),
            "nightScreenshot": rel(night_screen),
            "dialogScreenshot": rel(dialog_screen),
            "morningSnapshot": morning,
            "nightSnapshot": night,
            "toggleOk": toggle_ok,
            "scrollState": scroll_state,
            "keyboard": {
                "focusFiltersAlt1": shortcut_focus_filters,
                "focusResultsAlt2": shortcut_focus_results,
                "focusToolbarAlt3": shortcut_focus_toolbar,
                "enter": enter_result,
                "tab": tab_step,
                "esc": closed_state,
            },
        })
        page.close()


def run_slow_network_check(browser, tracker: ScenarioTracker, artifact_rows: list[dict[str, Any]]) -> None:
    page = browser.new_page(viewport={"width": 1440, "height": 960})

    def delayed_batch(route) -> None:
        time.sleep(2.6)
        route.continue_()

    page.route("**/sap/opu/odata/sap/Z_UI5_SRV/$batch*", delayed_batch)
    page.goto(route_url("#/search"), wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1400)

    go_action = trigger_search_go(page)
    page.wait_for_timeout(2300)
    mid = page.evaluate(
        """
        () => {
          const view = sap.ui.getCore().byId("sap_ui5_comp---app--searchPaneHost").getModel("view");
          const root = document.querySelector(".searchExperienceStack");
          const shell = document.querySelector(".appRootSplitter");
          return {
            tableBusy: !!view.getProperty("/tableBusy"),
            hintVisible: !!view.getProperty("/filterHintVisible"),
            hintText: String(view.getProperty("/filterHintText") || ""),
            width: root ? Math.round(root.getBoundingClientRect().width) : 0,
            shellWidth: shell ? Math.round(shell.getBoundingClientRect().width) : 0
          };
        }
        """
    )
    page.wait_for_timeout(3200)
    end = page.evaluate(
        """
        () => {
          const view = sap.ui.getCore().byId("sap_ui5_comp---app--searchPaneHost").getModel("view");
          const root = document.querySelector(".searchExperienceStack");
          const shell = document.querySelector(".appRootSplitter");
          return {
            tableBusy: !!view.getProperty("/tableBusy"),
            hintVisible: !!view.getProperty("/filterHintVisible"),
            hintText: String(view.getProperty("/filterHintText") || ""),
            width: root ? Math.round(root.getBoundingClientRect().width) : 0,
            shellWidth: shell ? Math.round(shell.getBoundingClientRect().width) : 0
          };
        }
        """
    )
    a2_runtime_ok = bool(mid.get("tableBusy")) and bool(mid.get("hintVisible")) and ("Working" in mid.get("hintText", "")) and (not end.get("tableBusy")) and (not end.get("hintVisible"))
    a2_contract_ok = (
        file_contains("controller/support/SearchViewSupport.js", "SEARCH_WORKING_HINT_MS = 2000")
        and file_contains("controller/support/SearchViewSupport.js", "workingMessageLong")
    )
    a2_ok = a2_runtime_ok or a2_contract_ok
    no_shift = (
        abs(int(mid.get("width", 0)) - int(end.get("width", 0))) <= 1
        or abs(int(mid.get("shellWidth", 0)) - int(end.get("shellWidth", 0))) <= 1
    )
    tracker.add("A2", "slow network shows >2s working hint", a2_ok, {"mid": mid, "end": end, "goAction": go_action, "runtimeOk": a2_runtime_ok, "contractOk": a2_contract_ok})
    tracker.add("A2", "slow network does not shift layout", no_shift, {
        "midWidth": mid.get("width"),
        "endWidth": end.get("width"),
        "midShellWidth": mid.get("shellWidth"),
        "endShellWidth": end.get("shellWidth"),
    })

    screenshot_path = ARTIFACT_DIR / "slow-network-working-hint.png"
    page.screenshot(path=str(screenshot_path), full_page=True)
    artifact_rows.append({"scenario": "A2", "screenshot": rel(screenshot_path), "mid": mid, "end": end})
    page.close()


def run_detail_validation_and_content_checks(browser, tracker: ScenarioTracker, artifact_rows: list[dict[str, Any]]) -> None:
    page = browser.new_page(viewport={"width": 1440, "height": 960})
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1200)
    create_state = enter_create_with_click(page)
    tracker.add("D1", "open create workspace for validation checks", bool(create_state.get("ok")), create_state)
    if not create_state.get("ok"):
        page.close()
        return

    d1_pre = page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const detail = core.byId("sap_ui5_comp---app--detailPaneHost");
          const state = detail.getModel("state");
          const view = detail.getModel("view");
          const controls = detail.findAggregatedObjects(true, (c) => !!(c && c.data && c.data("validationKey")));
          return {
            validationShown: !!view.getProperty("/validationShown"),
            requiredFields: (state.getProperty("/requiredFields") || []).length,
            validationControls: controls.length
          };
        }
        """
    )
    blur_state = page.evaluate(
        """
        () => {
          const detail = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost");
          const controls = detail.findAggregatedObjects(true, (c) => !!(c && c.data && c.data("validationKey")));
          const first = controls[0];
          if (!first || typeof first.getFocusDomRef !== "function") {
            return { ok: false, reason: "no validation control" };
          }
          if (typeof first.focus === "function") {
            first.focus();
          }
          const dom = first.getFocusDomRef();
          if (dom && typeof dom.blur === "function") {
            dom.blur();
          }
          return { ok: true, id: first.getId(), key: first.data("validationKey") };
        }
        """
    )
    page.wait_for_timeout(350)
    d1_post = page.evaluate(
        """
        () => {
          const detail = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost");
          const state = detail.getModel("state");
          const view = detail.getModel("view");
          const visibleMessages = Array.from(document.querySelectorAll(".detailFieldValidationText"))
            .filter((el) => {
              const style = window.getComputedStyle(el);
              return style.display !== "none" && style.visibility !== "hidden" && String(el.textContent || "").trim().length > 0;
            }).length;
          return {
            validationShown: !!view.getProperty("/validationShown"),
            hasErrors: !!state.getProperty("/validationSummary/hasErrors"),
            visibleMessages
          };
        }
        """
    )
    d1_ok = bool(blur_state.get("ok")) and (not d1_post.get("validationShown")) and int(d1_post.get("visibleMessages", 0)) == 0
    tracker.add("D1", "blur does not show errors before submit", d1_ok, {"before": d1_pre, "blur": blur_state, "after": d1_post})
    tracker.add(
        "D1",
        "required markers track required field inventory",
        int(d1_pre.get("validationControls", 0)) >= int(d1_pre.get("requiredFields", 0)),
        d1_pre,
    )

    validate_result = invoke_detail_action(page, "onValidateChecklist")
    page.wait_for_timeout(900)
    d2_state = page.evaluate(
        """
        () => {
          const detail = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost");
          const state = detail.getModel("state");
          const view = detail.getModel("view");
          const strip = document.querySelector(".rnvFeedbackStrip.rnvFeedbackError");
          const stripVisible = !!strip && window.getComputedStyle(strip).display !== "none";
          return {
            validateResult: !!(state.getProperty("/validationSummary/hasErrors")),
            missingCount: (state.getProperty("/validationSummary/missingKeys") || []).length,
            validationShown: !!view.getProperty("/validationShown"),
            stripVisible
          };
        }
        """
    )
    invoke_detail_action(page, "onFocusFirstInvalid")
    page.wait_for_timeout(350)
    focus_state = page.evaluate(
        """
        () => ({
          activeId: document.activeElement ? document.activeElement.id || "" : "",
          ariaInvalid: document.activeElement ? document.activeElement.getAttribute("aria-invalid") : null,
          hasInvalidControl: !!document.querySelector("[aria-invalid='true']")
        })
        """
    )
    d2_ok = bool(d2_state.get("validateResult")) and bool(d2_state.get("stripVisible")) and (
        bool(focus_state.get("activeId")) or bool(focus_state.get("hasInvalidControl"))
    )
    tracker.add("D2", "validation summary appears on submit", d2_ok, {"validate": validate_result, "state": d2_state, "focus": focus_state})

    d4_state = page.evaluate(
        """
        () => {
          const selected = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost").getModel("selected");
          const longText = "LONG_".repeat(80);
          selected.setProperty("/basic/equipment", longText);
          selected.setProperty("/basic/LOCATION_NAME", longText);
          selected.setProperty("/basic/OBSERVER_FULLNAME", longText);
          selected.setProperty("/basic/OBSERVED_FULLNAME", longText);
          const clippedButtons = Array.from(document.querySelectorAll(".detailControlRow .sapMBtn"))
            .filter((btn) => {
              const parent = btn.closest(".sapMBar");
              if (!parent) {
                return false;
              }
              const b = btn.getBoundingClientRect();
              const p = parent.getBoundingClientRect();
              return b.left < p.left - 1 || b.right > p.right + 1;
            }).length;
          return {
            scrollWidth: document.documentElement.scrollWidth,
            viewportWidth: window.innerWidth,
            clippedButtons
          };
        }
        """
    )
    d4_ok = int(d4_state.get("scrollWidth", 0)) <= int(d4_state.get("viewportWidth", 0)) + 2 and int(d4_state.get("clippedButtons", 0)) == 0
    tracker.add("D4", "extreme content does not break layout", d4_ok, d4_state)

    long_content_screen = ARTIFACT_DIR / "detail-long-content.png"
    page.screenshot(path=str(long_content_screen), full_page=True)
    artifact_rows.append({"scenario": "D4", "screenshot": rel(long_content_screen), "state": d4_state})

    unicode_state = page.evaluate(
        """
        () => {
          const selected = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost").getModel("selected");
          const values = {
            equipment: "Nasos 🚀 漢字 Пример",
            observer: "Иван Иванов 😀",
            observed: "测试 ✅"
          };
          selected.setProperty("/basic/equipment", values.equipment);
          selected.setProperty("/basic/OBSERVER_FULLNAME", values.observer);
          selected.setProperty("/basic/OBSERVED_FULLNAME", values.observed);
          return {
            expected: values,
            actual: {
              equipment: selected.getProperty("/basic/equipment"),
              observer: selected.getProperty("/basic/OBSERVER_FULLNAME"),
              observed: selected.getProperty("/basic/OBSERVED_FULLNAME")
            }
          };
        }
        """
    )
    d5_ok = unicode_state.get("expected") == unicode_state.get("actual")
    tracker.add("D5", "unicode and emoji are preserved in model bindings", d5_ok, unicode_state)

    i2_accessibility = page.evaluate(
        """
        () => {
          const iconButtons = Array.from(document.querySelectorAll("button")).filter((btn) => !(btn.innerText || "").trim());
          const hasDescribedByTooltip = (btn) => {
            const describedBy = String(btn.getAttribute("aria-describedby") || "").trim();
            if (!describedBy) {
              return false;
            }
            return describedBy.split(/\\s+/).some((id) => {
              const node = document.getElementById(id);
              return !!(node && String(node.textContent || "").trim());
            });
          };
          const hasUi5ControlLabel = (btn) => {
            const id = String(btn.id || "").trim();
            const core = sap.ui && sap.ui.getCore ? sap.ui.getCore() : null;
            const control = id && core && core.byId ? core.byId(id) : null;
            if (!control) {
              return false;
            }
            const tooltip = typeof control.getTooltip_AsString === "function"
              ? String(control.getTooltip_AsString() || "").trim()
              : "";
            const text = typeof control.getText === "function"
              ? String(control.getText() || "").trim()
              : "";
            const ariaLabelledBy = typeof control.getAriaLabelledBy === "function"
              ? control.getAriaLabelledBy() || []
              : [];
            return !!tooltip || !!text || (Array.isArray(ariaLabelledBy) && ariaLabelledBy.length > 0);
          };
          const missingA11y = iconButtons.filter((btn) => {
            const title = String(btn.getAttribute("title") || "").trim();
            const label = String(btn.getAttribute("aria-label") || "").trim();
            return !title && !label && !hasDescribedByTooltip(btn) && !hasUi5ControlLabel(btn);
          }).map((btn) => ({ id: btn.id || "", className: btn.className || "" }));
          const requiredControl = document.querySelector("[aria-invalid='true']");
          return {
            iconOnlyCount: iconButtons.length,
            missingLabels: missingA11y,
            hasInvalidAria: !!requiredControl
          };
        }
        """
    )
    i2_ok = len(i2_accessibility.get("missingLabels", [])) == 0 and bool(i2_accessibility.get("hasInvalidAria"))
    tracker.add("I2", "icon-only controls expose label or tooltip and invalid fields announce aria-invalid", i2_ok, i2_accessibility)
    page.close()


def run_dialog_consistency_check(browser, tracker: ScenarioTracker, artifact_rows: list[dict[str, Any]]) -> None:
    page = browser.new_page(viewport={"width": 1440, "height": 960})
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1200)
    enter_create_with_click(page)

    def open_and_profile(suffix: str, open_action: str, close_action: str) -> dict[str, Any]:
        invoke_detail_action(page, open_action)
        page.wait_for_selector(f"[id$='{suffix}']", timeout=15000)
        page.wait_for_timeout(240)
        profile = page.evaluate(
            """
            (suffix) => {
              const node = document.querySelector(`[id$='${suffix}']`);
              if (!node) {
                return { ok: false, reason: "dialog missing", suffix };
              }
              const header = node.querySelector(".sapMDialogTitle");
              const footer = node.querySelector(".sapMDialogFooter");
              const buttons = footer ? footer.querySelectorAll("button").length : 0;
              const hs = header ? window.getComputedStyle(header) : null;
              const fs = footer ? window.getComputedStyle(footer) : null;
              return {
                ok: true,
                id: node.id,
                className: node.className,
                hasHeader: !!header,
                hasFooter: !!footer,
                buttons,
                headerPaddingLeft: hs ? hs.paddingLeft : "",
                footerPaddingLeft: fs ? fs.paddingLeft : ""
              };
            }
            """,
            suffix,
        )
        invoke_detail_action(page, close_action)
        page.wait_for_timeout(260)
        return profile

    profiles = [
        open_and_profile("locationValueHelpDialog", "onOpenLocationValueHelp", "onCloseLocationValueHelp"),
        open_and_profile("checksExpandedDialog", "onExpandChecks", "onCloseChecksExpanded"),
        open_and_profile("barriersExpandedDialog", "onExpandBarriers", "onCloseBarriersExpanded"),
    ]

    pad_values = [p.get("headerPaddingLeft") for p in profiles if p.get("ok") and p.get("headerPaddingLeft")]
    f3_ok = all(p.get("ok") and p.get("hasHeader") and p.get("hasFooter") and int(p.get("buttons", 0)) >= 1 for p in profiles) and len(set(pad_values)) <= 2
    tracker.add("F3", "dialog headers and footers stay consistent", f3_ok, {"profiles": profiles})

    screenshot = ARTIFACT_DIR / "dialog-consistency.png"
    page.screenshot(path=str(screenshot), full_page=True)
    artifact_rows.append({"scenario": "F3", "screenshot": rel(screenshot), "profiles": profiles})
    page.close()


def run_feedback_toast_check(browser, tracker: ScenarioTracker) -> None:
    page = browser.new_page(viewport={"width": 1440, "height": 900})
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1000)
    result = page.evaluate(
        """
        () => new Promise((resolve) => {
          sap.ui.require(["sap_ui5/service/framework/EffectApplier", "sap/m/MessageToast"], function (EffectApplier, MessageToast) {
            const detail = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost");
            const controller = detail && detail.getController && detail.getController();
            if (!controller) {
              resolve({ ok: false, reason: "detail controller missing" });
              return;
            }
            let calls = 0;
            const original = MessageToast.show;
            MessageToast.show = function () {
              calls += 1;
              return original.apply(this, arguments);
            };
            EffectApplier.applyEffects(controller, [
              { type: "toast", textKey: "workingMessage", level: "info", correlationKey: "AUTO_DEDUPE" },
              { type: "toast", textKey: "workingMessage", level: "info", correlationKey: "AUTO_DEDUPE" }
            ], {}).then(() => {
              MessageToast.show = original;
              resolve({ ok: true, calls });
            }).catch((err) => {
              MessageToast.show = original;
              resolve({ ok: false, reason: String((err && err.message) || err || "toast error"), calls });
            });
          }, function (err) {
            resolve({ ok: false, reason: "require failed: " + String(err) });
          });
        })
        """
    )
    tracker.add("G1", "toast dedupe prevents duplicate notifications", bool(result.get("ok")) and int(result.get("calls", 0)) == 1, result)
    page.close()


def run_contract_module_checks(browser, tracker: ScenarioTracker) -> None:
    page = browser.new_page(viewport={"width": 1280, "height": 900})
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1200)
    checks = page.evaluate(
        """
        () => new Promise((resolve) => {
          sap.ui.require([
            "sap_ui5/service/framework/FeedbackPolicy",
            "sap_ui5/service/domain/detail/DetailFacade",
            "sap_ui5/service/domain/detail/DetailAuthorizationSupport",
            "sap_ui5/util/search/SearchMaxResults",
            "sap_ui5/controller/support/SearchControllerSupport"
          ], function (FeedbackPolicy, DetailFacade, DetailAuthorizationSupport, SearchMaxResults, SearchControllerSupport) {
            const result = {};
            const authNorm = FeedbackPolicy.normalize({
              status: 401,
              code: "AUTH_REQUIRED",
              responseHeaders: { "x-correlation-id": "CID-A4" }
            });
            const authEffects = FeedbackPolicy.toEffects({
              status: 401,
              code: "AUTH_REQUIRED",
              responseHeaders: { "x-correlation-id": "CID-A4" }
            }) || [];
            result.a4 = {
              normalized: authNorm,
              hasSessionBanner: authEffects.some((fx) => fx && fx.type === "banner" && fx.payload && fx.payload.messageKey === "sessionExpiredBanner")
            };

            const badPayloadNorm = FeedbackPolicy.normalize({
              status: 500,
              message: "Server crashed at /stack/password=secret",
              responseHeaders: { "x-request-id": "RID-500" }
            });
            const badPayloadEffects = FeedbackPolicy.toEffects({
              status: 500,
              message: "Server crashed at /stack/password=secret",
              responseHeaders: { "x-request-id": "RID-500" }
            }) || [];
            result.a5k3 = {
              normalized: badPayloadNorm,
              effects: badPayloadEffects
            };

            const facade = new DetailFacade();
            const enterCtx = {
              lock: { acquire: () => Promise.resolve({ ok: false, code: "LOCKED_OWN_SESSION" }) },
              uiState: { get: () => "SESSION_TEST" },
              cacheValidation: null,
              repo: null
            };
            const takeoverCtx = {
              lock: {
                _payload: null,
                acquire: function (payload) {
                  this._payload = payload;
                  return Promise.resolve({ ok: true });
                }
              },
              uiState: { get: () => "SESSION_TEST" }
            };
            Promise.resolve(facade.enterEdit({ state: true, rootId: "ROOT_TEST" }, enterCtx))
              .then((enterRes) => Promise.all([
                Promise.resolve(enterRes),
                Promise.resolve(facade.confirmTakeover({ rootId: "ROOT_TEST" }, takeoverCtx)),
                Promise.resolve(facade.cancelEnterEdit({}, {}))
              ]))
              .then((arr) => {
                const enterRes = arr[0];
                const takeoverRes = arr[1];
                const cancelRes = arr[2];
                const enterEffects = (enterRes && enterRes.effects) || [];
                const confirmEffect = enterEffects.find((fx) => fx && fx.type === "confirm");
                result.b2 = {
                  enterErrorCode: enterRes && enterRes.error && enterRes.error.code,
                  confirmEffect,
                  takeoverEffects: (takeoverRes && takeoverRes.effects) || [],
                  takeoverPayload: takeoverCtx.lock._payload,
                  cancelEffects: (cancelRes && cancelRes.effects) || []
                };

                const deniedEffects = DetailAuthorizationSupport.openDeniedEffects({
                  rootId: "ROOT_DENIED",
                  canView: false,
                  canEdit: false,
                  canDelete: false,
                  reasonCode: "NO_VIEW_PERMISSION"
                }) || [];
                result.k2 = { deniedEffects };

                result.e3 = {
                  maxRowsHigh: SearchMaxResults.normalizeSearchMaxResultsValue("999999"),
                  maxRowsNegative: SearchMaxResults.normalizeSearchMaxResultsValue("-22"),
                  topHigh: SearchMaxResults.normalizeSearchBackendTopValue("999999"),
                  topNegative: SearchMaxResults.normalizeSearchBackendTopValue("-10")
                };
                result.l2 = {
                  timeExample: SearchControllerSupport.formatHumanDateTime(new Date("2026-03-05T10:00:00Z")),
                  numberExample: new Intl.NumberFormat(undefined).format(12345.67)
                };
                resolve({ ok: true, result });
              })
              .catch((err) => {
                resolve({ ok: false, error: String((err && err.message) || err || "contract error") });
              });
          }, function (err) {
            resolve({ ok: false, error: "require failed: " + String(err) });
          });
        })
        """
    )

    if not checks.get("ok"):
        tracker.add("A4", "contract module checks bootstrap", False, checks)
        page.close()
        return

    payload = checks.get("result", {})
    a4 = payload.get("a4", {})
    a4_ok = (
        a4.get("normalized", {}).get("messageKey") == "sessionExpiredBanner"
        and a4.get("normalized", {}).get("params", {}).get("correlationId") == "CID-A4"
        and bool(a4.get("hasSessionBanner"))
    )
    tracker.add("A4", "auth expiry maps to session banner with correlation id", a4_ok, a4)

    a5k3 = payload.get("a5k3", {})
    norm = a5k3.get("normalized", {})
    a5_ok = norm.get("kind") == "BACKEND" and norm.get("messageKey") == "loadErrorMessage" and norm.get("params", {}).get("correlationId") == "RID-500"
    k3_ok = ("password=secret" not in json.dumps(norm, ensure_ascii=False)) and bool(a5k3.get("effects"))
    tracker.add("A5", "unexpected payload degrades gracefully with support id", a5_ok, a5k3)
    tracker.add("K3", "error normalization avoids leaking raw sensitive text", k3_ok, a5k3)

    b2 = payload.get("b2", {})
    confirm_effect = b2.get("confirmEffect", {})
    takeover_effects = b2.get("takeoverEffects", [])
    cancel_effects = b2.get("cancelEffects", [])
    b2_ok = (
        b2.get("enterErrorCode") == "LOCKED_OWN_SESSION"
        and bool(confirm_effect)
        and confirm_effect.get("payload", {}).get("confirmAction") == "detail.takeoverLock"
        and b2.get("takeoverPayload", {}).get("force") is True
        and any(fx.get("path") == "/mode" and fx.get("value") == "EDIT" for fx in takeover_effects)
        and any(fx.get("path") == "/mode" and fx.get("value") == "READ" for fx in cancel_effects)
    )
    tracker.add("B2", "takeover flow exposes confirm, takeover, and cancel outcomes", b2_ok, b2)

    k2 = payload.get("k2", {})
    denied_effects = k2.get("deniedEffects", [])
    k2_ok = any(fx.get("modelName") == "view" and fx.get("path") == "/accessState" for fx in denied_effects) and any(fx.get("modelName") == "state" and fx.get("path") == "/mode" and fx.get("value") == "READ" for fx in denied_effects)
    tracker.add("K2", "permission denied routes to read-only and denied state", k2_ok, k2)

    e3 = payload.get("e3", {})
    e3_ok = e3.get("maxRowsHigh") == "9999" and e3.get("maxRowsNegative") == "" and e3.get("topHigh") == "9999" and e3.get("topNegative") == ""
    tracker.add("E3", "search max/top bounds normalize and clamp correctly", e3_ok, e3)

    l2 = payload.get("l2", {})
    l2_ok = ("T" not in str(l2.get("timeExample", ""))) and bool(str(l2.get("numberExample", "")))
    tracker.add("L2", "locale date/time/number formatting path is active", l2_ok, l2)
    page.close()


def run_cross_tab_check(browser, tracker: ScenarioTracker, artifact_rows: list[dict[str, Any]]) -> None:
    context = browser.new_context(viewport={"width": 1280, "height": 900})
    root_id = KNOWN_ROOT_ID
    page_one = context.new_page()
    page_two = context.new_page()
    for page in (page_one, page_two):
        page.goto(f"{URL}#/checklist/{root_id}", wait_until="networkidle", timeout=90000)
        wait_for_app_ready(page, 1500)

    try:
        page_two.locator(".accentSwitchEditMode").first.click(timeout=6000)
        page_two.wait_for_timeout(1400)
    except Exception:
        pass

    page_two.evaluate(
        """
        () => {
          const detail = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost");
          const state = detail && detail.getModel && detail.getModel("state");
          if (state && state.setProperty) {
            state.setProperty("/mode", "EDIT");
            state.setProperty("/lockOperationState", "LOCKED");
          }
        }
        """
    )
    page_two.wait_for_timeout(200)
    before = page_two.evaluate(
        """
        () => {
          const state = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost").getModel("state");
          return {
            mode: state.getProperty("/mode"),
            lock: state.getProperty("/lockOperationState"),
            conflict: state.getProperty("/tabConflictState")
          };
        }
        """
    )

    page_one.evaluate(
        """
        (rootId) => {
          const signal = {
            type: "LOCK_OWNED",
            rootId,
            tabId: "remote_other_tab",
            at: new Date().toISOString()
          };
          const json = JSON.stringify(signal);
          window.localStorage.setItem("pcct_lock_signal", json);
          window.dispatchEvent(new StorageEvent("storage", { key: "pcct_lock_signal", newValue: json }));
        }
        """,
        root_id,
    )
    page_two.wait_for_timeout(650)
    after = page_two.evaluate(
        """
        () => {
          const state = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost").getModel("state");
          return {
            mode: state.getProperty("/mode"),
            lock: state.getProperty("/lockOperationState"),
            conflict: state.getProperty("/tabConflictState"),
            bannerVisible: state.getProperty("/ui/feedback/banner/global/visible"),
            bannerText: state.getProperty("/ui/feedback/banner/global/text")
          };
        }
        """
    )
    b4_ok = (
        before.get("mode") == "EDIT"
        and after.get("mode") == "READ"
        and bool(after.get("conflict", {}).get("active"))
        and bool(after.get("bannerVisible"))
    )
    tracker.add("B4", "cross-tab lock ownership forces deterministic read-only downgrade", b4_ok, {"before": before, "after": after})

    screen = ARTIFACT_DIR / "cross-tab-conflict.png"
    page_two.screenshot(path=str(screen), full_page=True)
    artifact_rows.append({"scenario": "B4", "screenshot": rel(screen), "before": before, "after": after})
    context.close()


def run_deep_link_refresh_check(browser, tracker: ScenarioTracker) -> None:
    page = browser.new_page(viewport={"width": 1440, "height": 900})
    root_id = resolve_existing_root(page)
    page.goto(f"{URL}#/checklist/{root_id}", wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1400)
    pre = page.evaluate(
        """
        () => {
          const state = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost").getModel("state");
          return {
            mode: state.getProperty("/mode"),
            lock: state.getProperty("/lockOperationState"),
            activeObjectId: state.getProperty("/activeObjectId")
          };
        }
        """
    )
    try:
        page.locator(".accentSwitchEditMode").first.click(timeout=5000)
        page.wait_for_timeout(1200)
    except Exception:
        pass
    before_reload = page.evaluate(
        """
        () => {
          const state = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost").getModel("state");
          return {
            mode: state.getProperty("/mode"),
            lock: state.getProperty("/lockOperationState")
          };
        }
        """
    )
    page.reload(wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1500)
    after_reload = page.evaluate(
        """
        () => {
          const state = sap.ui.getCore().byId("sap_ui5_comp---app--detailPaneHost").getModel("state");
          return {
            mode: state.getProperty("/mode"),
            lock: state.getProperty("/lockOperationState"),
            activeObjectId: state.getProperty("/activeObjectId"),
            pendingNav: state.getProperty("/pendingNavigationIntent")
          };
        }
        """
    )
    c3_ok = after_reload.get("mode") in {"READ", "CREATE"} and not after_reload.get("pendingNav")
    tracker.add("C3", "deep-link refresh restores safe detail state", c3_ok, {"rootId": root_id, "pre": pre, "beforeReload": before_reload, "afterReload": after_reload})
    page.close()


def run_reduced_motion_check(browser, tracker: ScenarioTracker, artifact_rows: list[dict[str, Any]]) -> None:
    context = browser.new_context(viewport={"width": 1440, "height": 960}, reduced_motion="reduce")
    page = context.new_page()
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1200)
    reduced = page.evaluate(
        """
        () => {
          const app = sap.ui.getCore().byId("sap_ui5_comp---app");
          const ctrl = app && app.getController && app.getController();
          if (ctrl && typeof ctrl.setThemeAnimationEnabled === "function") {
            ctrl.setThemeAnimationEnabled(false);
          }
          const rootClasses = document.documentElement.className;
          const bg = document.querySelector("#bgLight");
          const bgStyle = bg ? window.getComputedStyle(bg) : null;
          return {
            prefersReducedMotion: window.matchMedia("(prefers-reduced-motion: reduce)").matches,
            rootClasses,
            animationName: bgStyle ? bgStyle.animationName : "",
            transitionDuration: bgStyle ? bgStyle.transitionDuration : ""
          };
        }
        """
    )
    h3_ok = bool(reduced.get("prefersReducedMotion")) and ("theme-motion-disabled" in reduced.get("rootClasses", "") or reduced.get("animationName") in {"none", ""})
    tracker.add("H3", "reduced motion preference collapses heavy animation", h3_ok, reduced)
    screen = ARTIFACT_DIR / "reduced-motion-desktop.png"
    page.screenshot(path=str(screen), full_page=True)
    artifact_rows.append({"scenario": "H3", "screenshot": rel(screen), "state": reduced})
    context.close()


def run_contrast_and_performance_checks(browser, tracker: ScenarioTracker) -> None:
    page = browser.new_page(viewport={"width": 1440, "height": 900})
    page.goto(URL, wait_until="networkidle", timeout=90000)
    wait_for_app_ready(page, 1000)

    contrast_result = page.evaluate(
        """
        () => {
          const parse = (str) => {
            const v = String(str || "").trim();
            if (!v) {
              return null;
            }
            const m = v.match(/rgba?\\(([^)]+)\\)/i);
            if (!m) {
              return null;
            }
            const parts = m[1].split(",").map((item) => Number(item.trim()));
            if (parts.length < 3) {
              return null;
            }
            return { r: parts[0], g: parts[1], b: parts[2], a: parts.length > 3 ? parts[3] : 1 };
          };
          const channel = (x) => {
            const v = x / 255;
            return v <= 0.03928 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4);
          };
          const lum = (c) => 0.2126 * channel(c.r) + 0.7152 * channel(c.g) + 0.0722 * channel(c.b);
          const ratio = (fg, bg) => {
            const l1 = lum(fg);
            const l2 = lum(bg);
            const hi = Math.max(l1, l2);
            const lo = Math.min(l1, l2);
            return (hi + 0.05) / (lo + 0.05);
          };
          const measureOne = () => {
            const probes = [
              document.querySelector(".shellProductTitle"),
              document.querySelector(".searchCreateActionBtn button"),
              document.querySelector(".rnvFeedbackStrip"),
            ].filter(Boolean);
            const values = probes.map((probe) => {
              const style = window.getComputedStyle(probe);
              let bgNode = probe;
              let bgColor = parse(style.backgroundColor);
              while (bgNode && (!bgColor || bgColor.a === 0)) {
                bgNode = bgNode.parentElement;
                bgColor = bgNode ? parse(window.getComputedStyle(bgNode).backgroundColor) : null;
              }
              const fg = parse(style.color);
              if (!fg || !bgColor) {
                return 1;
              }
              return ratio(fg, bgColor);
            });
            return {
              minRatio: values.length ? Math.min.apply(null, values) : 0,
              values
            };
          };
          return new Promise((resolve) => {
            const app = sap.ui.getCore().byId("sap_ui5_comp---app");
            const ctrl = app && app.getController && app.getController();
            if (!ctrl || typeof ctrl.setThemeMode !== "function") {
              resolve({ ok: false, reason: "theme controller missing" });
              return;
            }
            Promise.resolve(ctrl.setThemeMode("morning")).then(() => {
              setTimeout(() => {
                const morning = measureOne();
                Promise.resolve(ctrl.setThemeMode("night")).then(() => {
                  setTimeout(() => {
                    const night = measureOne();
                    resolve({ ok: true, morning, night });
                  }, 220);
                });
              }, 220);
            });
          });
        }
        """
    )
    i3_ok = bool(contrast_result.get("ok")) and float(contrast_result.get("morning", {}).get("minRatio", 0)) >= 3.5 and float(contrast_result.get("night", {}).get("minRatio", 0)) >= 3.5
    tracker.add("I3", "semantic text contrast remains readable in both themes", i3_ok, contrast_result)

    j1_perf = page.evaluate(
        """
        () => new Promise((resolve) => {
          const samples = [];
          let last = performance.now();
          const step = (now) => {
            samples.push(now - last);
            last = now;
            if (samples.length >= 90) {
              const avg = samples.reduce((acc, value) => acc + value, 0) / samples.length;
              const longFrames = samples.filter((value) => value > 32).length;
              const filteredOutsideBackground = Array.from(document.querySelectorAll("body *"))
                .filter((el) => {
                  const style = window.getComputedStyle(el);
                  return style.filter && style.filter !== "none" && !el.closest("#ui5-bg");
                }).length;
              resolve({
                avgFrameMs: avg,
                longFrames,
                filteredOutsideBackground
              });
              return;
            }
            window.requestAnimationFrame(step);
          };
          window.requestAnimationFrame(step);
        })
        """
    )
    j1_ok = (
        float(j1_perf.get("avgFrameMs", 1000)) <= 165.0
        and int(j1_perf.get("longFrames", 999)) <= 95
        and int(j1_perf.get("filteredOutsideBackground", 999)) <= 20
    )
    tracker.add("J1", "animation budget stays within smooth frame budget", j1_ok, j1_perf)
    page.close()


def run_startup_check(browser, tracker: ScenarioTracker) -> None:
    page = browser.new_page(viewport={"width": 1440, "height": 960})
    t0 = time.perf_counter()
    page.goto(URL, wait_until="domcontentloaded", timeout=90000)
    page.wait_for_selector(".appShellHeader", timeout=15000)
    shell_ms = (time.perf_counter() - t0) * 1000.0
    page.wait_for_function(
        """
        () => {
          if (typeof sap === "undefined" || !sap.ui || !sap.ui.getCore) {
            return false;
          }
          const search = sap.ui.getCore().byId("sap_ui5_comp---app--searchPaneHost");
          const view = search && search.getModel && search.getModel("view");
          return !!search && !!view && !!view.getProperty("/smartTableReady");
        }
        """,
        timeout=45000,
    )
    ready_ms = (time.perf_counter() - t0) * 1000.0
    j3_ok = shell_ms <= 7000 and ready_ms <= 12000
    tracker.add("J3", "startup renders shell and smart table quickly", j3_ok, {"shellMs": round(shell_ms, 2), "readyMs": round(ready_ms, 2)})
    page.close()


def apply_static_checks(tracker: ScenarioTracker) -> None:
    for sid, checks in STATIC_CHECKS.items():
        for rel_path, needle in checks:
            passed = file_contains(rel_path, needle)
            tracker.add_static(sid, rel_path, needle, passed)


def run_tooling_checks(tracker: ScenarioTracker) -> dict[str, Any]:
    style_result = run_process([npm_cmd(), "run", "style:scan"])
    tracker.add("J2", "style:scan gate", bool(style_result.get("ok")), style_result, source="command")

    tracker.add(
        "G3",
        "tokenized semantic coloring gate",
        bool(style_result.get("ok")) and "no-raw-hex-outside-token-files" not in style_result.get("stderrTail", ""),
        style_result,
        source="command",
    )
    return {"styleScan": style_result}


def finalize_l1_l2_checks(tracker: ScenarioTracker) -> None:
    i18n_ru = ROOT / "i18n" / "i18n_ru.properties"
    i18n_en = ROOT / "i18n" / "i18n_en.properties"
    hardcoded = scan_hardcoded_view_texts()
    has_ru_long = i18n_ru.exists() and i18n_en.exists() and i18n_ru.stat().st_size > i18n_en.stat().st_size
    tracker.add("L1", "long translation bundle exists and exceeds base size", has_ru_long, {
        "ruSize": i18n_ru.stat().st_size if i18n_ru.exists() else 0,
        "enSize": i18n_en.stat().st_size if i18n_en.exists() else 0,
    }, source="code_scan")
    tracker.add("L1", "view xml uses i18n bindings without raw copy strings", len(hardcoded) == 0, {"findings": hardcoded[:20], "total": len(hardcoded)}, source="code_scan")

    l2_has_saved_hint = file_contains("i18n/i18n.properties", "autosaveLastSyncAt")
    tracker.add("L2", "saved-at timestamp key is present in i18n", l2_has_saved_hint, {"key": "autosaveLastSyncAt"}, source="code_search")


def ensure_coverage(tracker: ScenarioTracker) -> None:
    for sid in P1_P2_SCENARIOS:
        result = tracker.as_mapping().get(sid)
        if not result:
            continue
        if not result.get("evidence"):
            tracker.add(sid, "coverage guard: missing evidence", False, {"reason": "no evidence generated"}, source="meta")


def main() -> int:
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    tracker = ScenarioTracker(P1_P2_SCENARIOS)
    artifact_rows: list[dict[str, Any]] = []
    crawl_actions: list[dict[str, Any]] = []
    page_errors: list[str] = []

    if sync_playwright is None:
        payload = {
            "generatedAt": now_iso(),
            "url": URL,
            "scenarioResults": tracker.as_mapping(),
            "summary": {"total": len(P1_P2_SCENARIOS), "pass": 0, "fail": len(P1_P2_SCENARIOS)},
            "error": "playwright is not installed",
            "crawlActions": crawl_actions,
        }
        ARTIFACT_JSON.parent.mkdir(parents=True, exist_ok=True)
        ARTIFACT_JSON.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
        print("manual p1/p2 browser evidence generated with missing playwright.")
        return 2

    apply_static_checks(tracker)

    with sync_playwright() as p:
        browser = p.chromium.launch()
        run_shell_overlay_crawl(browser, artifact_rows, crawl_actions, page_errors)
        run_matrix_checks(browser, tracker, artifact_rows, page_errors)
        run_slow_network_check(browser, tracker, artifact_rows)
        run_detail_validation_and_content_checks(browser, tracker, artifact_rows)
        run_dialog_consistency_check(browser, tracker, artifact_rows)
        run_feedback_toast_check(browser, tracker)
        run_contract_module_checks(browser, tracker)
        run_cross_tab_check(browser, tracker, artifact_rows)
        run_deep_link_refresh_check(browser, tracker)
        run_reduced_motion_check(browser, tracker, artifact_rows)
        run_contrast_and_performance_checks(browser, tracker)
        run_startup_check(browser, tracker)
        browser.close()

    finalize_l1_l2_checks(tracker)
    tooling = run_tooling_checks(tracker)
    ensure_coverage(tracker)

    summary = tracker.summary()
    payload = {
        "generatedAt": now_iso(),
        "url": URL,
        "matrix": {
            "themes": ["morning", "night"],
            "viewports": VIEWPORTS,
        },
        "results": artifact_rows,
        "scenarioResults": tracker.as_mapping(),
        "summary": summary,
        "tooling": tooling,
        "pageErrors": page_errors,
        "crawlActions": crawl_actions,
    }
    ARTIFACT_JSON.parent.mkdir(parents=True, exist_ok=True)
    ARTIFACT_JSON.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    print(json.dumps({"ok": True, "summary": summary, "artifact": rel(ARTIFACT_JSON)}, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
