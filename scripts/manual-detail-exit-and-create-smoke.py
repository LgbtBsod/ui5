#!/usr/bin/env python3
from __future__ import annotations

import json
import sys
import time
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright

from browser_route_bootstrap import (
    collect_bootstrap_diagnostics,
    invoke_controller_method,
    navigate_to_detail,
    navigate_to_search,
    safe_evaluate,
    wait_for_app_ready,
    wait_for_detail_ready,
    wait_for_search_ready,
)

ROOT = Path(__file__).resolve().parent.parent
ARTIFACT_DIR = ROOT / "docs" / "artifacts"
ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
BASE_URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html?sap-ui-xx-componentPreload=off&smoke=manual"
REPORT_PATH = ARTIFACT_DIR / "manual-detail-exit-and-create-smoke.json"


def now_iso() -> str:
    return time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())


def wait_for_function(page, script: str, arg: Any = None, timeout: int = 30000) -> None:
    if arg is None:
        page.wait_for_function(script, timeout=timeout)
        return
    page.wait_for_function(script, arg=arg, timeout=timeout)


def read_runtime_state(page) -> dict[str, Any]:
    return safe_evaluate(
        page,
        """
        () => {
          const core = sap.ui.getCore();
          const appView = core.byId('checklist_app_comp---app');
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const detailView = core.byId('checklist_app_comp---detailTargetPage')
            || all.find((item) => item
              && item.isA
              && item.isA('sap.ui.core.mvc.View')
              && item.getController
              && item.getController()
              && item.getController().getMetadata
              && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Detail')
            || null;
          const appState = appView && appView.getModel && appView.getModel('state');
          const detailState = detailView && detailView.getModel && detailView.getModel('state');
          const selected = detailView && detailView.getModel && detailView.getModel('selected');
          return {
            hash: String(window.location.hash || ''),
            routeName: appState && appState.getProperty ? String(appState.getProperty('/currentRouteName') || '') : '',
            layout: appState && appState.getProperty ? String(appState.getProperty('/layout') || '') : '',
            selectedId: appState && appState.getProperty ? String(appState.getProperty('/selectedId') || '') : '',
            activeObjectId: appState && appState.getProperty ? String(appState.getProperty('/activeObjectId') || '') : '',
            editMode: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/editMode') || '') : '',
            lockState: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/lock/state') || '') : '',
            autosaveEnabled: !!(detailState && detailState.getProperty && (
              detailState.getProperty('/workflow/autosave/enabled')
              || detailState.getProperty('/workflow/detail/autosave/enabled')
            )),
            autosaveState: detailState && detailState.getProperty ? String(
              detailState.getProperty('/workflow/detail/autosave/state')
              || detailState.getProperty('/autosaveState')
              || ''
            ) : '',
            isDirty: !!(detailState && detailState.getProperty && detailState.getProperty('/isDirty')),
            saveInFlight: !!(detailState && detailState.getProperty && detailState.getProperty('/saveInFlight')),
            rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            checklistId: selected && selected.getProperty ? String(
              selected.getProperty('/basic/checklist_id')
              || selected.getProperty('/root/checklist_id')
              || selected.getProperty('/root/CHECKLIST_ID')
              || ''
            ) : '',
            equipment: selected && selected.getProperty ? String(
              selected.getProperty('/basic/equipment')
              || selected.getProperty('/root/equipment')
              || ''
            ) : '',
            bannerText: appState && appState.getProperty ? String(appState.getProperty('/ui/feedback/banner/global/text') || '') : '',
            bannerTextKey: appState && appState.getProperty ? String(appState.getProperty('/ui/feedback/banner/global/textKey') || '') : ''
          };
        }
        """
    )


def configure_fast_timers(page) -> None:
    safe_evaluate(
        page,
        """
        () => {
          const core = sap.ui.getCore();
          const appView = core.byId('checklist_app_comp---app');
          const appState = appView && appView.getModel && appView.getModel('state');
          const component = sap.ui.core.Component.getOwnerComponentFor(appView);
          if (appState && appState.setProperty) {
            appState.setProperty('/timers/autoSaveIntervalMs', 2000);
            appState.setProperty('/timers/autoSaveDebounceMs', 1000);
            appState.setProperty('/timers/heartbeatMs', 2000);
          }
          if (component && component._oAutoSave && component._oAutoSave.setIntervals) {
            component._oAutoSave.setIntervals({ intervalMs: 2000, debounceMs: 1000 });
          }
          if (component && component._oHeartbeat && component._oHeartbeat.setIntervalMs) {
            component._oHeartbeat.setIntervalMs(2000);
          }
          if (component && component._oLockStatus && component._oLockStatus.setIntervalMs) {
            component._oLockStatus.setIntervalMs(2000);
          }
          return true;
        }
        """
    )


def set_required_create_fields(page, suffix: str) -> None:
    safe_evaluate(
        page,
        """
        (labelSuffix) => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const detail = core.byId('checklist_app_comp---detailTargetPage')
            || all.find((item) => item
              && item.isA
              && item.isA('sap.ui.core.mvc.View')
              && item.getController
              && item.getController()
              && item.getController().getMetadata
              && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Detail')
            || null;
          const selected = detail && detail.getModel && detail.getModel('selected');
          const state = detail && detail.getModel && detail.getModel('state');
          if (!selected || !state) {
            throw new Error('detail models unavailable');
          }
          selected.setProperty('/basic/date', '2026-03-19');
          selected.setProperty('/basic/time', '04:24');
          selected.setProperty('/basic/timezone', 'Europe/Saratov');
          selected.setProperty('/basic/equipment', 'Manual smoke equipment ' + labelSuffix);
          selected.setProperty('/basic/LOCATION_KEY', 'LOC-PRD-03-B');
          selected.setProperty('/basic/LOCATION_NAME', 'Manual smoke location ' + labelSuffix);
          selected.setProperty('/basic/LOCATION_TEXT', 'Manual smoke location ' + labelSuffix);
          selected.setProperty('/basic/OBSERVER_FULLNAME', 'Manual smoke Observer ' + labelSuffix);
          selected.setProperty('/basic/OBSERVED_FULLNAME', 'Manual smoke Observed ' + labelSuffix);
          selected.setProperty('/basic/LPC_KEY', 'LPC-01');
          selected.setProperty('/basic/LPC_TEXT', 'LPC 01');
          selected.setProperty('/basic/PROF_KEY', 'PROF-01');
          selected.setProperty('/basic/PROF_TEXT', 'Operator');
          state.setProperty('/isDirty', true);
          return true;
        }
        """,
        suffix
    )


def toggle_edit(page, target_state: bool) -> Any:
    return safe_evaluate(
        page,
        """
        (state) => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const app = core.byId('checklist_app_comp---app');
          const appState = app && app.getModel && app.getModel('state');
          const activeRootId = appState && appState.getProperty
            ? String(appState.getProperty('/activeObjectId') || appState.getProperty('/selectedId') || '')
            : '';
          const candidates = all.filter((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Detail');
          const view = candidates.find((item) => {
            const selected = item && item.getModel && item.getModel('selected');
            const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
            return !!(item && item.getDomRef && item.getDomRef()) && (!!activeRootId ? rootId === activeRootId : true);
          }) || candidates.find((item) => !!(item && item.getDomRef && item.getDomRef())) || candidates[0] || null;
          const controller = view && view.getController ? view.getController() : null;
          if (!controller || typeof controller.onToggleEdit !== 'function') {
            throw new Error('onToggleEdit unavailable');
          }
          return Promise.resolve(controller.onToggleEdit({
            getParameter: function (name) {
              return name === 'state' ? !!state : undefined;
            }
          }));
        }
        """,
        target_state
    )


def invoke_detail(page, method_name: str, *args: Any) -> Any:
    return invoke_controller_method(page, "PRODUCTION_CONTROL_CHECKLIST.controller.Detail", method_name, *args)


def wait_for_mode(page, mode: str, lock_state: str | None = None) -> None:
    wait_for_function(
        page,
        """
        ({ mode, lockState }) => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const view = core.byId('checklist_app_comp---detailTargetPage')
            || all.find((item) => item
              && item.isA
              && item.isA('sap.ui.core.mvc.View')
              && item.getController
              && item.getController()
              && item.getController().getMetadata
              && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Detail')
            || null;
          const state = view && view.getModel && view.getModel('state');
          const currentMode = state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '';
          const currentLock = state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : '';
          if (currentMode !== mode) {
            return false;
          }
          if (lockState && currentLock !== lockState) {
            return false;
          }
          return true;
        }
        """,
        {"mode": mode, "lockState": lock_state},
        timeout=30000
    )
    page.wait_for_timeout(1200)


def count_requests(network: list[dict[str, Any]], marker: str) -> int:
    return len([item for item in network if marker in item["url"] or marker in item.get("post_data", "")])


def take_screenshot(page, name: str) -> str:
    path = ARTIFACT_DIR / f"{name}.png"
    page.screenshot(path=str(path), full_page=True)
    return str(path)


def main() -> None:
    network: list[dict[str, Any]] = []
    report: dict[str, Any] = {
        "generatedAt": now_iso(),
        "uiUrl": BASE_URL,
        "status": "failed",
        "checks": [],
        "artifacts": {}
    }

    with sync_playwright() as pw:
        browser = pw.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1440, "height": 960})

        def on_request(req) -> None:
            if "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV" not in req.url:
                return
            network.append({
                "ts": now_iso(),
                "method": req.method,
                "url": req.url,
                "post_data": (req.post_data or "")[:8000]
            })

        page.on("request", on_request)

        try:
            page.goto(BASE_URL, wait_until="domcontentloaded", timeout=90000)
            wait_for_app_ready(page, timeout=90000)
            navigate_to_search(page)

            navigate_to_detail(page, "__CREATE")
            configure_fast_timers(page)
            create_open_state = read_runtime_state(page)
            report["checks"].append({
                "name": "create.opens_on_runtime_navigation",
                "ok": create_open_state["routeName"] == "detail" and create_open_state["selectedId"] == "__CREATE",
                "detail": create_open_state
            })

            label_suffix = str(int(time.time()))
            set_required_create_fields(page, label_suffix)
            page.wait_for_timeout(2500)
            pre_save_autosave = count_requests(network, "AutoSave")
            report["checks"].append({
                "name": "create.no_autosave_before_first_save",
                "ok": pre_save_autosave == 0,
                "detail": {"autoSaveRequests": pre_save_autosave}
            })

            invoke_detail(page, "onSaveDetail")
            wait_for_function(
                page,
                """
                () => {
                  const core = sap.ui.getCore();
                  const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
                  const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
                  const view = core.byId('checklist_app_comp---detailTargetPage')
                    || all.find((item) => item
                      && item.isA
                      && item.isA('sap.ui.core.mvc.View')
                      && item.getController
                      && item.getController()
                      && item.getController().getMetadata
                      && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Detail')
                    || null;
                  const selected = view && view.getModel && view.getModel('selected');
                  const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
                  return !!rootId && rootId !== '__CREATE';
                }
                """,
                timeout=45000
            )
            create_saved_state = read_runtime_state(page)
            report["checks"].append({
                "name": "create.first_save_replaces_create_route",
                "ok": "__CREATE" not in create_saved_state["hash"] and create_saved_state["rootId"] not in ("", "__CREATE"),
                "detail": create_saved_state
            })

            toggle_edit(page, True)
            wait_for_mode(page, "EDIT", "EDIT_LOCKED")
            edit_state = read_runtime_state(page)
            report["checks"].append({
                "name": "detail.edit_acquires_lock",
                "ok": edit_state["editMode"] == "EDIT" and edit_state["lockState"] == "EDIT_LOCKED",
                "detail": edit_state
            })

            toggle_edit(page, False)
            wait_for_mode(page, "READ")
            read_state = read_runtime_state(page)
            release_before_close = count_requests(network, "LockRelease")
            invoke_detail(page, "onCloseDetail")
            wait_for_search_ready(page, timeout=45000)
            page.wait_for_timeout(1200)
            exit_state = read_runtime_state(page)
            release_after_close = count_requests(network, "LockRelease")
            false_release_warning = "lockReleaseFailed" in (exit_state.get("bannerTextKey") or "") or "release lock" in (exit_state.get("bannerText") or "").lower()
            report["checks"].append({
                "name": "detail.exit_to_search_without_false_release_warning",
                "ok": exit_state["routeName"] == "search"
                and exit_state["activeObjectId"] == ""
                and exit_state["lockState"] in ("", "IDLE")
                and release_after_close >= release_before_close
                and not false_release_warning,
                "detail": {
                    "beforeClose": read_state,
                    "afterClose": exit_state,
                    "lockReleaseBefore": release_before_close,
                    "lockReleaseAfter": release_after_close,
                    "hasFalseReleaseWarning": false_release_warning
                }
            })

            report["artifacts"]["finalScreenshot"] = take_screenshot(page, "manual-detail-exit-and-create-smoke")
            report["artifacts"]["bootstrap"] = collect_bootstrap_diagnostics(page)
            report["networkTail"] = network[-20:]
            report["status"] = "ok" if all(item["ok"] for item in report["checks"]) else "failed"
        finally:
            browser.close()

    REPORT_PATH.write_text(json.dumps(report, indent=2), encoding="utf-8")
    print(json.dumps(report, indent=2))
    sys.exit(0 if report["status"] == "ok" else 1)


if __name__ == "__main__":
    main()
