#!/usr/bin/env python3
"""Runtime interaction smoke checks for local UI contract and SAP-backed contour.

Local static mode validates shell/rendering/route contracts without treating missing
Gateway metadata as a product failure. SAP-backed flows remain covered by separate
browser smoke scripts.
"""

from __future__ import annotations

import json
import sys
from typing import Any

try:
    from playwright.sync_api import sync_playwright
except ModuleNotFoundError:
    sync_playwright = None

URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
DETAIL_ROOT = "E49B679F518F4947BD7A0F2CC1C4AC46"
APP_VIEW_ID = "checklist_app_comp---app"
SEARCH_VIEW_ID = "checklist_app_comp---searchTargetPage"
DETAIL_VIEW_ID = "checklist_app_comp---detailTargetPage"
ANALYTICS_VIEW_ID = "checklist_app_comp---analyticsTargetPage"
SHELL_HEADER_HOST_ID = f"{APP_VIEW_ID}--appShellHeaderHost"
SEARCH_WORKBENCH_ID = f"{SEARCH_VIEW_ID}--searchWorkbenchDock"
SEARCH_FILTER_ID = f"{SEARCH_VIEW_ID}--searchFilterCard"
SEARCH_RESULTS_ID = f"{SEARCH_VIEW_ID}--searchResultsShell"
SEARCH_RESULTS_TOOLBAR_ID = f"{SEARCH_VIEW_ID}--searchResultsToolbarHost"
SEARCH_SMART_FILTER_ID = f"{SEARCH_VIEW_ID}--searchSmartFilterBar"
SEARCH_SMART_TABLE_ID = f"{SEARCH_VIEW_ID}--searchSmartTable"
DETAIL_OBJECT_PAGE_ID = f"{DETAIL_VIEW_ID}--detailObjectPage"
LOCAL_RESULT_PASS = "PASS_LOCAL_BASELINE"
LOCAL_RESULT_BLOCKED = "BLOCKED_BACKEND"
LOCAL_RESULT_FAIL = "FAIL_UI_CONTRACT"


def ensure(condition: bool, message: str) -> None:
    if not condition:
        raise RuntimeError(message)


def wait_for_app_ready(page, delay: int = 1200) -> None:
    page.wait_for_load_state("domcontentloaded")
    page.wait_for_load_state("networkidle")
    page.wait_for_function(
        """
        (ids) => {
          if (typeof sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
            return false;
          }
          const core = sap.ui.getCore();
          return !!core.byId(ids.app)
            && !!core.byId(ids.fcl)
            && !!core.byId(ids.shellHeaderHost)
            && !!core.byId(ids.searchWorkbench)
            && !!core.byId(ids.searchFilter)
            && !!core.byId(ids.searchResults)
            && !!core.byId(ids.searchResultsToolbar)
            && document.body.classList.contains('chkAppRoot');
        }
        """,
        arg={
            "app": APP_VIEW_ID,
            "fcl": f"{APP_VIEW_ID}--mainFcl",
            "shellHeaderHost": SHELL_HEADER_HOST_ID,
            "searchWorkbench": SEARCH_WORKBENCH_ID,
            "searchFilter": SEARCH_FILTER_ID,
            "searchResults": SEARCH_RESULTS_ID,
            "searchResultsToolbar": SEARCH_RESULTS_TOOLBAR_ID,
        },
        timeout=90000,
    )
    page.wait_for_timeout(delay)


def wait_for_detail_ready(page, root_id: str = DETAIL_ROOT, delay: int = 1400, require_data: bool = True) -> None:
    wait_for_app_ready(page, 900)
    page.wait_for_function(
        """
        ({ viewId, objectPageId, expectedRootId, requireData }) => {
          const core = sap.ui.getCore();
          const view = core.byId(viewId);
          const app = core.byId('checklist_app_comp---app');
          const objectPage = core.byId(objectPageId);
          const selected = view && view.getModel && view.getModel('selected');
          const state = app && app.getModel && app.getModel('state');
          const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
          const stateSelectedId = state && state.getProperty ? String(state.getProperty('/selectedId') || '') : '';
          return !!view
            && !!objectPage
            && (!requireData || !expectedRootId || rootId === expectedRootId || stateSelectedId === expectedRootId);
        }
        """,
        arg={
            "viewId": DETAIL_VIEW_ID,
            "objectPageId": DETAIL_OBJECT_PAGE_ID,
            "expectedRootId": root_id,
            "requireData": require_data,
        },
        timeout=30000,
    )
    page.wait_for_selector(f"#{DETAIL_OBJECT_PAGE_ID}", timeout=15000)
    page.wait_for_timeout(delay)


def wait_for_analytics_ready(page, delay: int = 1200) -> None:
    page.wait_for_function(
        """
        (ids) => {
          const core = sap.ui.getCore();
          const app = core.byId(ids.app);
          const analyticsView = core.byId(ids.analytics);
          const state = app && app.getModel && app.getModel('state');
          return !!analyticsView
            && (!!analyticsView.getDomRef() || document.getElementById(ids.analytics))
            && state
            && state.getProperty('/currentRouteName') === 'analytics';
        }
        """,
        arg={"app": APP_VIEW_ID, "analytics": ANALYTICS_VIEW_ID},
        timeout=30000,
    )
    page.wait_for_timeout(delay)


def wait_for_search_route(page, delay: int = 900) -> None:
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          return !!state
            && state.getProperty('/currentRouteName') === 'search'
            && (state.getProperty('/layout') || 'OneColumn') === 'OneColumn';
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(delay)


def route_state(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const fcl = core.byId('checklist_app_comp---app--mainFcl');
          const state = app && app.getModel && app.getModel('state');
          const currentMid = fcl && fcl.getCurrentMidColumnPage && fcl.getCurrentMidColumnPage();
          return {
            hash: String(window.location.hash || ''),
            routeName: state && state.getProperty ? String(state.getProperty('/currentRouteName') || '') : '',
            layout: state && state.getProperty ? String(state.getProperty('/layout') || '') : '',
            selectedId: state && state.getProperty ? String(state.getProperty('/selectedId') || '') : '',
            midPageId: currentMid && currentMid.getId ? currentMid.getId() : ''
          };
        }
        """
    )


def invoke_controller_method(page, view_id: str, method_name: str, *args: Any) -> Any:
    return page.evaluate(
        """
        ({ viewId, methodName, args }) => {
          const view = sap.ui.getCore().byId(viewId);
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error(methodName + ' is not available on ' + viewId);
          }
          return Promise.resolve(controller[methodName].apply(controller, args || []));
        }
        """,
        {"viewId": view_id, "methodName": method_name, "args": list(args)},
    )


def wait_for_dialog(page, dialog_id_suffix: str) -> dict[str, float]:
    selector = f"[id$='{dialog_id_suffix}']"
    page.wait_for_selector(selector, timeout=15000)
    dialog_box = page.locator(selector).first.bounding_box()
    ensure(dialog_box is not None, f"{dialog_id_suffix} geometry is not available")
    return {
        "width": round(dialog_box["width"], 2),
        "height": round(dialog_box["height"], 2),
    }


def set_theme_mode(page, mode: str) -> dict[str, Any]:
    result = page.evaluate(
        """
        (targetMode) => {
          const view = sap.ui.getCore().byId('checklist_app_comp---app');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller.setThemeMode !== 'function') {
            throw new Error('setThemeMode is not available');
          }
          return Promise.resolve(controller.setThemeMode(targetMode)).then(function (themeResult) {
            const appView = view && view.getModel && view.getModel('appView');
            return {
              requestedMode: targetMode,
              resolvedMode: themeResult && themeResult.mode ? String(themeResult.mode) : String((appView && appView.getProperty && appView.getProperty('/themeMode')) || 'morning'),
              themeResult: themeResult || null
            };
          });
        }
        """,
        mode,
    )
    page.wait_for_timeout(400)
    observed = page.evaluate(
        """
        () => {
          const bgState = window.Ui5Bg && typeof window.Ui5Bg.getState === 'function' ? window.Ui5Bg.getState() : null;
          const app = sap.ui.getCore().byId('checklist_app_comp---app');
          const appView = app && app.getModel && app.getModel('appView');
          return {
            bodyClasses: String(document.body.className || ''),
            htmlClasses: String(document.documentElement.className || ''),
            bgTheme: bgState ? String(bgState.theme || '') : '',
            themeMode: appView && appView.getProperty ? String(appView.getProperty('/themeMode') || '') : '',
            animationEnabled: appView && appView.getProperty ? !!appView.getProperty('/themeAnimationEnabled') : null
          };
        }
        """
    )
    result["observed"] = observed
    return result


def capture_resize_runtime(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const bg = document.getElementById('ui5-bg');
          const container = document.getElementById('ui5_container');
          const coreReady = typeof sap !== 'undefined'
            && sap.ui
            && sap.ui.getCore
            && !!sap.ui.getCore().byId('checklist_app_comp---app--mainFcl')
            && !!sap.ui.getCore().byId('checklist_app_comp---searchTargetPage--searchWorkbenchDock')
            && !!sap.ui.getCore().byId('checklist_app_comp---app--appShellHeaderHost');
          const bgStyle = bg ? getComputedStyle(bg) : null;
          const containerStyle = container ? getComputedStyle(container) : null;
          const bgState = window.Ui5Bg && typeof window.Ui5Bg.getState === 'function' ? window.Ui5Bg.getState() : null;
          return {
            appReady: coreReady,
            resizing: document.documentElement.classList.contains('chkResizing'),
            bgOpacity: bgStyle ? Number(bgStyle.opacity || 0) : -1,
            bgVisibility: bgStyle ? String(bgStyle.visibility || '') : '',
            containerVisibility: containerStyle ? String(containerStyle.visibility || '') : '',
            currentTheme: bgState ? String(bgState.theme || '') : '',
            bgState: bgState
          };
        }
        """
    )


def normalize_backend_blockers(items: list[dict[str, Any]]) -> list[dict[str, Any]]:
    seen = set()
    normalized = []
    for item in items:
        url = str(item.get("url") or "")
        status = int(item.get("status") or 0)
        key = (url, status)
        if key in seen:
            continue
        seen.add(key)
        normalized.append({"url": url, "status": status, "kind": str(item.get("kind") or "response")})
    return normalized


def has_backend_blockers(items: list[dict[str, Any]]) -> bool:
    return any("/sap/opu/odata/" in str(item.get("url") or "") and int(item.get("status") or 0) >= 400 for item in items)


def run_resize_trace(page, theme_mode: str) -> dict[str, Any]:
    set_theme_mode(page, theme_mode)
    page.set_viewport_size({"width": 1440, "height": 960})
    page.wait_for_timeout(180)

    samples = []
    widths = [1400, 1340, 1280, 1220, 1160, 1100, 1040, 1120, 1240, 1360, 1440]
    heights = [960, 948, 936, 924, 912, 900, 888, 900, 924, 948, 960]
    for width, height in zip(widths, heights):
        page.set_viewport_size({"width": width, "height": height})
        page.wait_for_timeout(85)
        samples.append({"width": width, "height": height, **capture_resize_runtime(page)})

    page.wait_for_timeout(900)
    final_state = capture_resize_runtime(page)
    ensure(any(sample["resizing"] for sample in samples), f"resize runtime did not enter resizing state for {theme_mode}")
    ensure(all(sample["appReady"] for sample in samples), f"app-ready marker dropped during {theme_mode} resize")
    ensure(all(sample["containerVisibility"] != "hidden" for sample in samples), f"ui5 container became hidden during {theme_mode} resize")
    ensure(final_state["containerVisibility"] != "hidden", f"ui5 container became hidden after {theme_mode} resize")

    return {
        "themeMode": theme_mode,
        "samples": samples,
        "final": final_state,
    }


def main() -> int:
    if sync_playwright is None:
        print("[error] playwright is not installed.")
        return 2

    report: dict[str, Any] = {}
    backend_blockers: list[dict[str, Any]] = []

    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": 1440, "height": 960})
        page.on(
            "response",
            lambda response: backend_blockers.append({
                "url": response.url,
                "status": response.status,
                "kind": "response"
            }) if "/sap/opu/odata/" in response.url and response.status >= 400 else None
        )

        page.goto(URL, wait_until="networkidle", timeout=90000)
        wait_for_app_ready(page, 1800)
        report["resizeMorning"] = run_resize_trace(page, "morning")
        report["resizeNight"] = run_resize_trace(page, "night")
        set_theme_mode(page, "morning")

        invoke_controller_method(page, APP_VIEW_ID, "onOpenShellAnalytics")
        wait_for_analytics_ready(page)
        shell_analytics_state = route_state(page)
        ensure(shell_analytics_state["routeName"] == "analytics", "shell analytics did not navigate to analytics route")
        ensure(shell_analytics_state["midPageId"].endswith("analyticsTargetPage"), "shell analytics did not activate analytics mid page")
        invoke_controller_method(page, ANALYTICS_VIEW_ID, "onCloseAnalytics")
        wait_for_search_route(page)
        report["shellAnalytics"] = shell_analytics_state

        page.goto(f"{URL}#/checklist/{DETAIL_ROOT}", wait_until="networkidle", timeout=90000)
        wait_for_detail_ready(page, DETAIL_ROOT, 1600, require_data=not has_backend_blockers(backend_blockers))
        report["detailRoute"] = route_state(page)

        if not has_backend_blockers(backend_blockers):
            invoke_controller_method(page, DETAIL_VIEW_ID, "onOpenWorkflowAnalytics")
            wait_for_analytics_ready(page)
            detail_analytics_state = route_state(page)
            ensure(detail_analytics_state["routeName"] == "analytics", "detail analytics did not navigate to analytics route")
            invoke_controller_method(page, ANALYTICS_VIEW_ID, "onCloseAnalytics")
            wait_for_detail_ready(page, DETAIL_ROOT, 1200)
            detail_return_state = route_state(page)
            ensure(detail_return_state["routeName"] in ("detail", "detailLayout"), "analytics close did not return to detail route")
            ensure(detail_return_state["selectedId"] == DETAIL_ROOT, "detail return lost selected root after analytics")
            report["detailAnalytics"] = {
                "opened": detail_analytics_state,
                "returned": detail_return_state,
            }

            sticky_before = page.locator(".detailControlStickyBlock").first.bounding_box()
            ensure(sticky_before is not None, "sticky control rail is not rendered")
            report["detailSticky"] = {"height": round(sticky_before["height"], 2)}

            invoke_controller_method(page, DETAIL_VIEW_ID, "onExpandChecks")
            checks_dialog = wait_for_dialog(page, "checksExpandedDialog")
            invoke_controller_method(page, DETAIL_VIEW_ID, "onCloseChecksExpanded")
            page.wait_for_timeout(350)
            report["checksExpandedDialog"] = checks_dialog

            invoke_controller_method(page, DETAIL_VIEW_ID, "onExpandBarriers")
            barriers_dialog = wait_for_dialog(page, "barriersExpandedDialog")
            invoke_controller_method(page, DETAIL_VIEW_ID, "onCloseBarriersExpanded")
            page.wait_for_timeout(350)
            report["barriersExpandedDialog"] = barriers_dialog

            invoke_controller_method(page, DETAIL_VIEW_ID, "onOpenLocationValueHelp")
            value_help_box = wait_for_dialog(page, "locationValueHelpDialog")
            ensure(value_help_box["height"] > 300, "location value help did not open correctly")
            invoke_controller_method(page, DETAIL_VIEW_ID, "onCloseLocationValueHelp")
            page.wait_for_timeout(350)
            report["locationDialog"] = value_help_box

            phone_page = browser.new_page(viewport={"width": 390, "height": 844})
            phone_page.on(
                "response",
                lambda response: backend_blockers.append({
                    "url": response.url,
                    "status": response.status,
                    "kind": "response"
                }) if "/sap/opu/odata/" in response.url and response.status >= 400 else None
            )
            phone_page.goto(f"{URL}#/checklist/{DETAIL_ROOT}", wait_until="networkidle", timeout=90000)
            wait_for_detail_ready(phone_page, DETAIL_ROOT, 1800, require_data=True)
            invoke_controller_method(phone_page, DETAIL_VIEW_ID, "onExpandChecks")
            phone_checks_dialog = wait_for_dialog(phone_page, "checksExpandedDialog")
            phone_page.close()
            report["phoneChecksDialog"] = phone_checks_dialog
        else:
            report["backendBlockers"] = normalize_backend_blockers(backend_blockers)

        browser.close()
    normalized_blockers = normalize_backend_blockers(backend_blockers)
    result_class = LOCAL_RESULT_BLOCKED if normalized_blockers else LOCAL_RESULT_PASS
    payload = {"ok": True, "resultClass": result_class, "report": report, "backendBlockers": normalized_blockers}
    print(json.dumps(payload, ensure_ascii=False, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
