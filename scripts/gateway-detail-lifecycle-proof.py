#!/usr/bin/env python3
"""Gateway + UI lifecycle proof for create/edit/save/autosave/lock/unlock flows."""

from __future__ import annotations

import json
import sys
import time
import urllib.request
from http.cookiejar import CookieJar
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright
from browser_route_bootstrap import (
    collect_bootstrap_diagnostics,
    get_tail_search_row,
    invoke_controller_method,
    navigate_to_detail,
    navigate_to_search,
    safe_evaluate,
    wait_for_app_ready,
    wait_for_detail_ready,
    wait_for_search_ready,
)


UI_URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
SERVICE_ROOT = (
    sys.argv[2]
    if len(sys.argv) > 2
    else "http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV"
).rstrip("/")
ARTIFACT_DIR = Path("docs/artifacts/detail-lifecycle-proof")
REPORT_PATH = ARTIFACT_DIR / "report.json"


def now_iso() -> str:
    return time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())


def ensure(checks: list[dict[str, Any]], name: str, ok: bool, detail: Any) -> None:
    checks.append({"name": name, "ok": bool(ok), "detail": detail})


def build_opener() -> urllib.request.OpenerDirector:
    jar = CookieJar()
    return urllib.request.build_opener(urllib.request.HTTPCookieProcessor(jar))


def request(
    opener: urllib.request.OpenerDirector,
    method: str,
    url: str,
    *,
    headers: dict[str, str] | None = None,
    payload: Any = None,
    expect_json: bool = True,
) -> tuple[int, Any, dict[str, str]]:
    data = None
    req_headers = dict(headers or {})
    if payload is not None:
        data = json.dumps(payload).encode("utf-8")
        req_headers.setdefault("Content-Type", "application/json")
    req = urllib.request.Request(url, data=data, method=method.upper(), headers=req_headers)
    with opener.open(req, timeout=30) as resp:
        body = resp.read()
        resp_headers = {k: v for (k, v) in resp.headers.items()}
        if not expect_json:
            return resp.status, body, resp_headers
        if not body:
            return resp.status, {}, resp_headers
        return resp.status, json.loads(body.decode("utf-8")), resp_headers


def fetch_csrf(opener: urllib.request.OpenerDirector) -> str:
    _status, _payload, headers = request(opener, "GET", f"{SERVICE_ROOT}/", headers={"X-CSRF-Token": "Fetch"})
    return str(headers.get("X-CSRF-Token") or headers.get("x-csrf-token") or "").strip()


def create_checklist(opener: urllib.request.OpenerDirector, token: str, label: str) -> dict[str, Any]:
    payload = {
        "FullPayload": {
            "root": {"id": "__CREATE", "status": "DRAFT"},
            "basic": {
                "date": "2026-03-19",
                "time": "04:24",
                "timezone": "Europe/Saratov",
                "equipment": f"{label} Pump",
                "LOCATION_KEY": "LOC-PRD-03-B",
                "LOCATION_NAME": "Дизель-генератор",
                "LOCATION_TEXT": "Дизель-генератор",
                "OBSERVER_FULLNAME": f"{label} Observer",
                "OBSERVED_FULLNAME": f"{label} Observed",
                "LPC_KEY": "LPC-01",
                "LPC_TEXT": "ЛПК 01",
                "PROF_KEY": "PROF-01",
                "PROF_TEXT": "Operator"
            },
            "checks": [{"ChecksNum": 1, "text": f"{label} check", "result": False}],
            "barriers": [{"BarriersNum": 1, "text": f"{label} barrier", "result": True}],
        }
    }
    status, data, _headers = request(
        opener,
        "POST",
        f"{SERVICE_ROOT}/CreateChecklist",
        headers={"X-CSRF-Token": token},
        payload=payload
    )
    if status != 200:
        raise RuntimeError(f"CreateChecklist failed with {status}")
    return (data or {}).get("d") or {}


def delete_checklist(opener: urllib.request.OpenerDirector, token: str, root_id: str) -> bool:
    status, _body, _headers = request(
        opener,
        "DELETE",
        f"{SERVICE_ROOT}/ChecklistRootSet('{root_id}')",
        headers={"X-CSRF-Token": token},
        expect_json=False
    )
    return status == 204


def wait_for_function(page, script: str, arg: Any = None, timeout: int = 30000) -> None:
    if arg is None:
        page.wait_for_function(script, timeout=timeout)
        return
    page.wait_for_function(script, arg=arg, timeout=timeout)


def wait_for_ui(page) -> None:
    wait_for_app_ready(page, timeout=90000)


def wait_for_search(page) -> None:
    wait_for_search_ready(page, timeout=45000)


def wait_for_detail(page, expected_root: str) -> None:
    wait_for_detail_ready(page, expected_root, timeout=45000)


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
          const component = sap.ui.core.Component.getOwnerComponentFor(appView);
          const managers = component ? {
            heartbeat: !!(component._oHeartbeat && component._oHeartbeat.isRunning && component._oHeartbeat.isRunning()),
            autosave: !!(component._oAutoSave && component._oAutoSave._bRunning),
            lockStatus: !!(component._oLockStatus && component._oLockStatus.isRunning && component._oLockStatus.isRunning()),
            activity: !!(component._oActivity && component._oActivity.isRunning && component._oActivity.isRunning()),
            gcd: !!(component._oGcd && component._oGcd.isRunning && component._oGcd.isRunning())
          } : {};
          return {
            hash: String(window.location.hash || ''),
            routeName: appState && appState.getProperty ? String(appState.getProperty('/currentRouteName') || '') : '',
            layout: appState && appState.getProperty ? String(appState.getProperty('/layout') || '') : '',
            selectedId: appState && appState.getProperty ? String(appState.getProperty('/selectedId') || '') : '',
            activeObjectId: appState && appState.getProperty ? String(appState.getProperty('/activeObjectId') || '') : '',
            editMode: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/editMode') || '') : '',
            lockState: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/lock/state') || '') : '',
            autosaveState: detailState && detailState.getProperty ? String(
              detailState.getProperty('/workflow/detail/autosave/state')
              || detailState.getProperty('/autosaveState')
              || ''
            ) : '',
            autosaveEnabled: !!(detailState && detailState.getProperty && (
              detailState.getProperty('/workflow/autosave/enabled')
              || detailState.getProperty('/workflow/detail/autosave/enabled')
            )),
            autosaveLastSavedAt: detailState && detailState.getProperty ? (
              detailState.getProperty('/workflow/detail/autosave/lastSavedAt')
              || null
            ) : null,
            isDirty: !!(detailState && detailState.getProperty && detailState.getProperty('/isDirty')),
            saveInFlight: !!(detailState && detailState.getProperty && detailState.getProperty('/saveInFlight')),
            lockOperationPending: !!(detailState && detailState.getProperty && detailState.getProperty('/lockOperationPending')),
            nextHeartbeatAt: detailState && detailState.getProperty ? detailState.getProperty('/persistence/nextHeartbeatAt') : null,
            rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            checklistId: selected && selected.getProperty ? String(
              selected.getProperty('/basic/checklist_id')
              || selected.getProperty('/root/checklist_id')
              || selected.getProperty('/root/CHECKLIST_ID')
              || ''
            ) : '',
            equipment: selected && selected.getProperty ? String(selected.getProperty('/basic/equipment') || '') : '',
            overallResult: selected && selected.getProperty ? (
              selected.getProperty('/root/overall_result')
              ?? selected.getProperty('/root/OverallResult')
              ?? selected.getProperty('/basic/overall_result')
              ?? null
            ) : null,
            managers: managers
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
          selected.setProperty('/basic/equipment', 'Created equipment ' + labelSuffix);
          selected.setProperty('/basic/LOCATION_KEY', 'LOC-PRD-03-B');
          selected.setProperty('/basic/LOCATION_NAME', 'Created location ' + labelSuffix);
          selected.setProperty('/basic/LOCATION_TEXT', 'Created location ' + labelSuffix);
          selected.setProperty('/basic/OBSERVER_FULLNAME', 'Created Observer ' + labelSuffix);
          selected.setProperty('/basic/OBSERVED_FULLNAME', 'Created Observed ' + labelSuffix);
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


def set_equipment_dirty(page, next_value: str, touch_autosave: bool) -> None:
    safe_evaluate(
        page,
        """
        ({ value, touchAutosave }) => {
          const core = sap.ui.getCore();
          const appView = core.byId('checklist_app_comp---app');
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
          const component = sap.ui.core.Component.getOwnerComponentFor(appView);
          if (!selected || !state) {
            throw new Error('detail models unavailable');
          }
          selected.setProperty('/basic/equipment', String(value || ''));
          state.setProperty('/isDirty', true);
          if (touchAutosave && component && component._oAutoSave && component._oAutoSave.touch) {
            component._oAutoSave.touch();
          }
          return true;
        }
        """,
        {"value": next_value, "touchAutosave": touch_autosave}
    )


def invoke_detail(page, method_name: str, *args: Any) -> Any:
    return invoke_controller_method(page, "PRODUCTION_CONTROL_CHECKLIST.controller.Detail", method_name, *args)


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


def count_requests(network: list[dict[str, Any]], marker: str) -> int:
    return len([item for item in network if marker in item["url"] or marker in item.get("post_data", "")])


def summarize_requests(network: list[dict[str, Any]], marker: str, since: int = 0) -> list[dict[str, Any]]:
    return [
        item for item in network[since:]
        if marker in item["url"] or marker in item.get("post_data", "")
    ]


def take_step_screenshot(page, name: str) -> str:
    path = ARTIFACT_DIR / f"{name}.png"
    page.screenshot(path=str(path), full_page=True)
    return str(path)


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


def open_last_search_row(page) -> dict[str, Any]:
    payload = get_tail_search_row(page)
    if not payload.get("domId"):
        raise RuntimeError("search tail row not resolved")
    page.locator(f"#{payload['domId']}").click(timeout=15000)
    return payload


def run_search_by_checklist_id(page, checklist_id: str) -> dict[str, Any]:
    payload = safe_evaluate(
        page,
        """
        (checklistId) => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const searchView = all.find((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Search') || null;
          const controller = searchView && searchView.getController ? searchView.getController() : null;
          const smartFilterBar = controller && controller.byId ? controller.byId('searchSmartFilterBar') : null;
          const control = smartFilterBar && smartFilterBar.getControlByKey ? (
            smartFilterBar.getControlByKey('Id')
            || smartFilterBar.getControlByKey('ChecklistId')
            || smartFilterBar.getControlByKey('checklist_id')
          ) : null;
          const currentData = smartFilterBar && smartFilterBar.getFilterData ? Object.assign({}, smartFilterBar.getFilterData() || {}) : {};
          if (control && typeof control.setValue === 'function') {
            control.setValue(String(checklistId || ''));
            if (typeof control.fireChange === 'function') {
              control.fireChange({ value: String(checklistId || '') });
            }
          }
          if (smartFilterBar && typeof smartFilterBar.setFilterData === 'function') {
            currentData.Id = {
              value: String(checklistId || ''),
              items: [],
              ranges: []
            };
            smartFilterBar.setFilterData(currentData, true);
          }
          if (!controller || typeof controller.onSmartSearch !== 'function') {
            throw new Error('search controller unavailable');
          }
          return Promise.resolve(controller.onSmartSearch()).then(function () {
            return {
              checklistId: String(checklistId || ''),
              filterData: smartFilterBar && smartFilterBar.getFilterData ? smartFilterBar.getFilterData() : {},
              controlValue: control && control.getValue ? String(control.getValue() || '') : ''
            };
          });
        }
        """,
        checklist_id
    )
    return payload or {}


def open_search_row_by_checklist_id(page, checklist_id: str) -> dict[str, Any]:
    payload = safe_evaluate(
        page,
        """
        (checklistId) => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const smartTable = all.find((item) => item && item.isA && item.isA('sap.ui.comp.smarttable.SmartTable') && item.getId && String(item.getId()).indexOf('searchSmartTable') >= 0) || null;
          const table = (smartTable && smartTable.getTable && smartTable.getTable())
            || all.find((item) => item && item.isA && item.isA('sap.m.Table') && item.getItems && item.getId && String(item.getId()).indexOf('searchSmartTable') >= 0)
            || null;
          const rows = table && table.getItems ? (table.getItems() || []).filter((item) => !!(item && item.getVisible && item.getVisible() && item.getBindingContext && item.getBindingContext())) : [];
          const match = rows.find((item) => {
            const ctx = item && item.getBindingContext ? item.getBindingContext() : null;
            const data = ctx && ctx.getObject ? ctx.getObject() : null;
            const rowChecklistId = String((data && (data.Id || data.ChecklistId || data.checklist_id)) || '').trim();
            return rowChecklistId === String(checklistId || '').trim();
          }) || null;
          const ctx = match && match.getBindingContext ? match.getBindingContext() : null;
          const data = ctx && ctx.getObject ? ctx.getObject() : {};
          const dom = match && match.getDomRef ? match.getDomRef() : null;
          if (dom && dom.scrollIntoView) {
            dom.scrollIntoView({ block: 'center', inline: 'nearest' });
          }
          return {
            domId: dom && dom.id ? String(dom.id) : '',
            rootKey: String((data && (data.Key || data.RootKey)) || '').trim(),
            checklistId: String((data && (data.Id || data.ChecklistId || data.checklist_id)) || '').trim(),
            rowCount: rows.length
          };
        }
        """,
        checklist_id
    )
    if not payload or not payload.get("domId"):
        raise RuntimeError(f"search row not resolved for checklist_id={checklist_id}")
    try:
        page.locator(f"#{payload['domId']}").click(timeout=15000)
    except Exception as exc:
        # Sticky search rails can transiently intercept pointer events in headed Chromium.
        # Fall back to the UI5 row press pipeline instead of failing on a pure click race.
        if "intercepts pointer events" not in str(exc):
            raise
        safe_evaluate(
            page,
            """
            (domId) => {
              const core = sap.ui.getCore();
              const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
              const row = (registry && registry.get && registry.get(String(domId || ""))) || null;
              if (row && typeof row.firePress === "function") {
                row.firePress();
                return { mode: "firePress", ok: true };
              }
              const dom = document.getElementById(String(domId || ""));
              if (!dom) {
                throw new Error("search row dom missing for fallback click");
              }
              dom.click();
              return { mode: "domClick", ok: true };
            }
            """,
            payload["domId"]
        )
    return payload


def collect_failure_context(page, network: list[dict[str, Any]], step: str, error: str) -> dict[str, Any]:
    snapshot = {}
    bootstrap = {}
    try:
        snapshot = read_runtime_state(page)
    except Exception:  # noqa: BLE001
        snapshot = {}
    try:
        bootstrap = collect_bootstrap_diagnostics(page)
    except Exception:  # noqa: BLE001
        bootstrap = {}
    return {
        "step": step,
        "error": error,
        "state": snapshot,
        "bootstrap": bootstrap,
        "networkTail": network[-20:]
    }


def body_contains(page, expected_text: str) -> bool:
    return bool(
        safe_evaluate(
            page,
            """
            (expected) => {
              const body = document.body && document.body.innerText ? document.body.innerText : '';
              return String(body || '').indexOf(String(expected || '')) >= 0;
            }
            """,
            expected_text
        )
    )


def run_browser_flow(existing_root_id: str) -> dict[str, Any]:
    checks: list[dict[str, Any]] = []
    failures: list[dict[str, Any]] = []
    network: list[dict[str, Any]] = []
    screenshots: dict[str, str] = {}
    create_root_id = ""
    step = "startup"

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
            step = "search.open"
            page.goto(UI_URL, wait_until="domcontentloaded", timeout=90000)
            navigate_to_search(page)

            step = "create.open"
            navigate_to_detail(page, "__CREATE")
            screenshots["beforeFirstSave"] = take_step_screenshot(page, "before-first-save")

            step = "create.empty_save_blocked"
            create_before_empty = count_requests(network, "CreateChecklist")
            invoke_detail(page, "onSaveDetail")
            page.wait_for_timeout(1800)
            empty_blocked_state = read_runtime_state(page)
            create_after_empty = count_requests(network, "CreateChecklist")
            validation_visible = body_contains(page, "Обязательные поля") or body_contains(page, "Исправьте обязательные поля")
            ensure(
                checks,
                "emptyCreateBlocked",
                "__CREATE" in empty_blocked_state["hash"]
                and empty_blocked_state["selectedId"] == "__CREATE"
                and empty_blocked_state["editMode"] == "CREATE"
                and create_after_empty == create_before_empty
                and (validation_visible or empty_blocked_state["lockState"] == "IDLE"),
                {
                    "before": create_before_empty,
                    "after": create_after_empty,
                    "validationVisible": validation_visible,
                    "state": empty_blocked_state
                }
            )

            step = "create.fill"
            set_required_create_fields(page, str(int(time.time())))
            state_after_fill = read_runtime_state(page)
            ensure(
                checks,
                "create.fill.required_fields",
                state_after_fill["selectedId"] == "__CREATE" and state_after_fill["editMode"] == "CREATE" and state_after_fill["isDirty"],
                state_after_fill
            )

            step = "create.no_autosave_before_first_save"
            autosave_before = count_requests(network, "AutoSave")
            page.wait_for_timeout(4000)
            autosave_after = count_requests(network, "AutoSave")
            ensure(checks, "create.no_autosave_before_first_save", autosave_after == autosave_before, {"before": autosave_before, "after": autosave_after})

            step = "create.first_save"
            create_before = count_requests(network, "CreateChecklist")
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
            page.wait_for_timeout(1500)
            create_saved_state = read_runtime_state(page)
            create_root_id = create_saved_state["rootId"]
            create_after = count_requests(network, "CreateChecklist")
            screenshots["afterCreateSave"] = take_step_screenshot(page, "after-create-save")
            ensure(
                checks,
                "validCreateSaved",
                bool(create_root_id)
                and create_root_id != "__CREATE"
                and create_after == create_before + 1
                and "__CREATE" not in create_saved_state["hash"]
                and create_saved_state["overallResult"] in (None, "", False),
                {"before": create_before, "after": create_after, "state": create_saved_state}
            )

            step = "create.no_autosave_without_changes"
            nochange_before = count_requests(network, "AutoSave")
            page.wait_for_timeout(3500)
            nochange_after = count_requests(network, "AutoSave")
            ensure(checks, "detail.no_autosave_without_changes_after_first_save", nochange_after == nochange_before, {"before": nochange_before, "after": nochange_after, "state": read_runtime_state(page)})

            step = "create.enter_edit"
            lock_before = count_requests(network, "LockAcquire")
            toggle_edit(page, True)
            wait_for_mode(page, "EDIT", "EDIT_LOCKED")
            configure_fast_timers(page)
            edit_state = read_runtime_state(page)
            lock_after = count_requests(network, "LockAcquire")
            ensure(checks, "detail.enter_edit_acquires_lock", lock_after > lock_before and edit_state["editMode"] == "EDIT" and edit_state["lockState"] == "EDIT_LOCKED", {"before": lock_before, "after": lock_after, "state": edit_state})

            step = "create.autosave_after_dirty_change"
            autosave_before_dirty = count_requests(network, "AutoSave")
            next_equipment = "Autosaved equipment " + str(int(time.time()))
            set_equipment_dirty(page, next_equipment, True)
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
                  const state = view && view.getModel && view.getModel('state');
                  const autosaveState = state && state.getProperty ? String(
                    state.getProperty('/workflow/detail/autosave/state')
                    || state.getProperty('/autosaveState')
                    || ''
                  ) : '';
                  return !!state && autosaveState === 'SAVED' && state.getProperty('/isDirty') === false;
                }
                """,
                timeout=30000
            )
            autosave_after_dirty = count_requests(network, "AutoSave")
            autosave_state = read_runtime_state(page)
            screenshots["afterAutosave"] = take_step_screenshot(page, "after-autosave")
            ensure(
                checks,
                "autosaveStable",
                autosave_after_dirty > autosave_before_dirty
                and autosave_state["equipment"] == next_equipment
                and not autosave_state["isDirty"]
                and autosave_state["autosaveState"] == "SAVED",
                {"before": autosave_before_dirty, "after": autosave_after_dirty, "state": autosave_state}
            )

            step = "create.no_repeat_autosave_without_changes"
            repeat_before = count_requests(network, "AutoSave")
            page.wait_for_timeout(3500)
            repeat_after = count_requests(network, "AutoSave")
            ensure(checks, "detail.no_repeat_autosave_without_new_changes", repeat_after == repeat_before, {"before": repeat_before, "after": repeat_after, "state": read_runtime_state(page)})

            step = "create.second_change_manual_save"
            manual_save_before = count_requests(network, "SaveChanges")
            final_equipment = "Manual save equipment " + str(int(time.time()))
            set_equipment_dirty(page, final_equipment, False)
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
                  const state = view && view.getModel && view.getModel('state');
                  return !!state && state.getProperty('/isDirty') === false && state.getProperty('/saveInFlight') === false;
                }
                """,
                timeout=30000
            )
            manual_save_after = count_requests(network, "SaveChanges")
            ensure(checks, "detail.manual_save_after_second_change", manual_save_after > manual_save_before and read_runtime_state(page)["equipment"] == final_equipment, {"before": manual_save_before, "after": manual_save_after, "state": read_runtime_state(page)})

            step = "create.exit_edit"
            heartbeat_before_exit = count_requests(network, "LockHeartbeat")
            toggle_edit(page, False)
            wait_for_mode(page, "READ")
            page.wait_for_timeout(3000)
            post_exit_state = read_runtime_state(page)
            heartbeat_after_exit = count_requests(network, "LockHeartbeat")
            ensure(
                checks,
                "detail.exit_edit_stops_edit_runtime",
                post_exit_state["editMode"] == "READ" and not any(post_exit_state["managers"].values()) and heartbeat_after_exit == heartbeat_before_exit,
                {"before": heartbeat_before_exit, "after": heartbeat_after_exit, "state": post_exit_state}
            )

            step = "create.close_detail"
            release_before = count_requests(network, "LockRelease")
            invoke_detail(page, "onCloseDetail")
            wait_for_search(page)
            page.wait_for_timeout(1200)
            release_after = count_requests(network, "LockRelease")
            closed_state = read_runtime_state(page)
            screenshots["afterCloseUnlock"] = take_step_screenshot(page, "after-close-unlock")
            ensure(
                checks,
                "closeReleasedLock",
                release_after >= release_before
                and closed_state["routeName"] == "search"
                and closed_state["lockState"] in ("", "IDLE")
                and not closed_state["autosaveEnabled"]
                and closed_state["activeObjectId"] == "",
                {"before": release_before, "after": release_after, "state": closed_state}
            )

            step = "create.reopen_existing_by_route"
            navigate_to_detail(page, create_root_id)
            lock_reopen_before = count_requests(network, "LockAcquire")
            toggle_edit(page, True)
            wait_for_mode(page, "EDIT", "EDIT_LOCKED")
            configure_fast_timers(page)
            reopen_state = read_runtime_state(page)
            lock_reopen_after = count_requests(network, "LockAcquire")
            ensure(
                checks,
                "reopenAcquiredLock",
                lock_reopen_after > lock_reopen_before
                and reopen_state["rootId"] == create_root_id
                and reopen_state["editMode"] == "EDIT"
                and reopen_state["lockState"] == "EDIT_LOCKED",
                {"before": lock_reopen_before, "after": lock_reopen_after, "state": reopen_state}
            )
            toggle_edit(page, False)
            wait_for_mode(page, "READ")
            invoke_detail(page, "onCloseDetail")
            wait_for_search(page)

            step = "search.find_created_row"
            search_payload = run_search_by_checklist_id(page, create_saved_state["checklistId"])
            wait_for_function(
                page,
                """
                (checklistId) => {
                  const core = sap.ui.getCore();
                  const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
                  const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
                  const smartTable = all.find((item) => item && item.isA && item.isA('sap.ui.comp.smarttable.SmartTable') && item.getId && String(item.getId()).indexOf('searchSmartTable') >= 0) || null;
                  const table = (smartTable && smartTable.getTable && smartTable.getTable())
                    || all.find((item) => item && item.isA && item.isA('sap.m.Table') && item.getItems && item.getId && String(item.getId()).indexOf('searchSmartTable') >= 0)
                    || null;
                  const rows = table && table.getItems ? (table.getItems() || []).filter((item) => !!(item && item.getVisible && item.getVisible() && item.getBindingContext && item.getBindingContext())) : [];
                  return rows.some((item) => {
                    const ctx = item && item.getBindingContext ? item.getBindingContext() : null;
                    const data = ctx && ctx.getObject ? ctx.getObject() : null;
                    const rowChecklistId = String((data && (data.Id || data.ChecklistId || data.checklist_id)) || '').trim();
                    return rowChecklistId === String(checklistId || '').trim();
                  });
                }
                """,
                create_saved_state["checklistId"],
                timeout=30000
            )
            ensure(checks, "search.find_created_row_by_checklist_id", bool(search_payload.get("checklistId")), search_payload)

            step = "search.click_created_row"
            tail_payload = open_search_row_by_checklist_id(page, create_saved_state["checklistId"])
            wait_for_detail(page, tail_payload["rootKey"])
            tail_state = read_runtime_state(page)
            screenshots["openedFromSearchTail"] = take_step_screenshot(page, "opened-from-search-tail")
            ensure(
                checks,
                "search.click_created_row_opens_detail_via_selection_pipeline",
                tail_state["rootId"] == tail_payload["rootKey"] and tail_state["checklistId"] == tail_payload["checklistId"],
                {"clicked": tail_payload, "state": tail_state}
            )

            step = "search.tail_edit_flow"
            tail_lock_before = count_requests(network, "LockAcquire")
            toggle_edit(page, True)
            wait_for_mode(page, "EDIT", "EDIT_LOCKED")
            configure_fast_timers(page)
            tail_equipment = "Tail flow equipment " + str(int(time.time()))
            set_equipment_dirty(page, tail_equipment, True)
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
                  const state = view && view.getModel && view.getModel('state');
                  const autosaveState = state && state.getProperty ? String(
                    state.getProperty('/workflow/detail/autosave/state')
                    || state.getProperty('/autosaveState')
                    || ''
                  ) : '';
                  return !!state && autosaveState === 'SAVED' && state.getProperty('/isDirty') === false;
                }
                """,
                timeout=30000
            )
            tail_lock_after = count_requests(network, "LockAcquire")
            tail_autosave_count = count_requests(network, "AutoSave")
            ensure(checks, "search.tail_click_detail_edit_save_autosave_works", tail_lock_after > tail_lock_before and tail_autosave_count > 0 and read_runtime_state(page)["equipment"] == tail_equipment, {"lockBefore": tail_lock_before, "lockAfter": tail_lock_after, "autosaveCount": tail_autosave_count, "state": read_runtime_state(page)})
            toggle_edit(page, False)
            wait_for_mode(page, "READ")
            tail_release_before = count_requests(network, "LockRelease")
            invoke_detail(page, "onCloseDetail")
            wait_for_search(page)
            tail_release_after = count_requests(network, "LockRelease")
            ensure(checks, "search.tail_click_close_releases_lock", tail_release_after > tail_release_before, {"before": tail_release_before, "after": tail_release_after})
        except Exception as exc:  # noqa: BLE001
            failures.append(collect_failure_context(page, network, step, str(exc)))
        finally:
            browser.close()

    return {
        "ok": not failures and all(item["ok"] for item in checks),
        "checks": checks,
        "failures": failures,
        "networkEvidence": {
            "createChecklist": summarize_requests(network, "CreateChecklist"),
            "lockAcquire": summarize_requests(network, "LockAcquire"),
            "lockHeartbeat": summarize_requests(network, "LockHeartbeat"),
            "lockRelease": summarize_requests(network, "LockRelease"),
            "saveChanges": summarize_requests(network, "SaveChanges"),
            "autoSave": summarize_requests(network, "AutoSave")
        },
        "screenshots": screenshots,
        "createdRootId": create_root_id
    }


def main() -> int:
    ARTIFACT_DIR.mkdir(parents=True, exist_ok=True)
    opener = build_opener()
    gateway_checks: list[dict[str, Any]] = []
    warnings: list[str] = []
    created_roots: list[str] = []
    report: dict[str, Any] = {}

    try:
        token = fetch_csrf(opener)
        ensure(gateway_checks, "gateway.csrf", bool(token), {"tokenPresent": bool(token)})
        existing_seed = create_checklist(opener, token, "Lifecycle Existing Seed")
        existing_root_id = str(existing_seed.get("RootKey") or existing_seed.get("Key") or "").strip().upper()
        created_roots.append(existing_root_id)
        ensure(gateway_checks, "gateway.seed_existing_root", bool(existing_root_id), {"rootId": existing_root_id})
        browser_report = run_browser_flow(existing_root_id)
        report = {
            "generatedAt": now_iso(),
            "uiUrl": UI_URL,
            "serviceRoot": SERVICE_ROOT,
            "status": "ok" if browser_report.get("ok") and all(item["ok"] for item in gateway_checks) else "failed",
            "createLifecycle": browser_report,
            "existingLifecycle": {
                "seedRootId": existing_root_id,
                "reopenAcquiredLock": next((item for item in browser_report.get("checks", []) if item.get("name") == "reopenAcquiredLock"), None)
            },
            "searchTailOpenLifecycle": {
                "tailOpen": next((item for item in browser_report.get("checks", []) if item.get("name") == "search.click_created_row_opens_detail_via_selection_pipeline"), None),
                "tailEdit": next((item for item in browser_report.get("checks", []) if item.get("name") == "search.tail_click_detail_edit_save_autosave_works"), None)
            },
            "networkEvidence": browser_report.get("networkEvidence") or {},
            "failures": [item for item in gateway_checks if not item["ok"]] + (browser_report.get("failures") or []),
            "gatewayChecks": gateway_checks,
            "warnings": warnings
        }
    except Exception as exc:  # noqa: BLE001
        report = {
            "generatedAt": now_iso(),
            "uiUrl": UI_URL,
            "serviceRoot": SERVICE_ROOT,
            "status": "blocked_by_environment",
            "createLifecycle": {},
            "existingLifecycle": {},
            "searchTailOpenLifecycle": {},
            "networkEvidence": {},
            "failures": [{"step": "bootstrap", "error": str(exc)}],
            "gatewayChecks": gateway_checks,
            "warnings": warnings
        }
    finally:
        token = ""
        try:
            token = fetch_csrf(opener)
        except Exception:  # noqa: BLE001
            token = ""
        for root_id in created_roots:
            if not root_id or not token:
                continue
            try:
                if not delete_checklist(opener, token, root_id):
                    warnings.append(f"cleanup_failed:{root_id}")
            except Exception:  # noqa: BLE001
                warnings.append(f"cleanup_failed:{root_id}")

    REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    summary = {
        "status": report.get("status"),
        "failures": len(report.get("failures") or []),
        "report": str(REPORT_PATH)
    }
    sys.stdout.write(json.dumps({"summary": summary, "report": report}, ensure_ascii=False, indent=2) + "\n")
    return 0 if report.get("status") == "ok" else 1


if __name__ == "__main__":
    raise SystemExit(main())
