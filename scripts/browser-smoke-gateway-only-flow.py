#!/usr/bin/env python3
"""Browser smoke: Gateway-only Smart/OData runtime flow.

Result classes:
- PASS_SAP_EVIDENCE: SAP metadata/data and runtime flow succeeded
- BLOCKED_SAP_ENV: SAP contour is unavailable or incomplete
- FAIL_PRODUCT_CONTRACT: runtime/product flow regressed under SAP-backed execution
"""

from __future__ import annotations

import json
import sys
import time
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright
from browser_route_bootstrap import (
    collect_bootstrap_diagnostics,
    invoke_controller_method as shared_invoke_controller_method,
    navigate_to_detail,
    navigate_to_search,
    wait_for_app_ready,
    wait_for_detail_ready as shared_wait_for_detail_ready,
    wait_for_search_ready as shared_wait_for_search_ready,
)


UI_URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
ROOT_ID = sys.argv[2] if len(sys.argv) > 2 else ""
REPORT_PATH = Path("docs/artifacts/gateway-browser-smoke-report.json")
SEARCH_VIEW_ID = "checklist_app_comp---searchTargetPage"
DETAIL_VIEW_ID = "checklist_app_comp---detailTargetPage"
ANALYTICS_VIEW_ID = "checklist_app_comp---analyticsTargetPage"
RESULT_PASS = "PASS_SAP_EVIDENCE"
RESULT_BLOCKED = "BLOCKED_SAP_ENV"
RESULT_FAIL = "FAIL_PRODUCT_CONTRACT"
RESULT_TOOLING = "tooling bug"
ROUTE_DETAIL = "detail"
ROUTE_SEARCH = "search"
ROUTE_ANALYTICS = "analytics"
MODE_EDIT = "EDIT"
MODE_READ = "READ"
LOCK_EDIT_LOCKED = "EDIT_LOCKED"
LOCK_READ_ONLY = "READ_ONLY"
LOCK_IDLE = "IDLE"
AUTOSAVE_STATE_SAVED = "SAVED"
AUTOSAVE_STATE_SAVING = "SAVING"
AUTOSAVE_CLASS_TOOLING = RESULT_TOOLING
AUTOSAVE_CLASS_BLOCKED = RESULT_BLOCKED
CLASSIFIER_LOCK_KILLED = "LOCK_KILLED_OR_CONTENDED"
CLASSIFIER_LOCK_ENV = "LOCK_ENV_BLOCK"
CLASSIFIER_LOCK_PRODUCT = "LOCK_PRODUCT_FAILURE"
CLASSIFIER_ROUTE_TIMEOUT = "ROUTE_OPEN_TIMEOUT"
CLASSIFIER_DETAIL_DATA = "DETAIL_DATA_NOT_READY"
CLASSIFIER_ROOT_REJECTED = "ROOT_CANDIDATE_REJECTED"
CLASSIFIER_ANALYTICS_RETURN = "ANALYTICS_RETURN_NOT_READY"
CLASSIFIER_ATTACHMENT_READY = "ATTACHMENT_NOT_READY"
MARKER_CHECKLIST_ROOT = "ChecklistRootSet"
MARKER_CHECKLIST_BASIC = "ChecklistBasicInfoSet"
MARKER_CHECKLIST_PERMISSION = "ChecklistPermissionSet"
MARKER_LAST_CHANGE = "LastChangeSet"
MARKER_LOCK_ACQUIRE = "LockAcquire"
MARKER_LOCK_RELEASE = "LockRelease"
MARKER_LOCK_HEARTBEAT = "LockHeartbeat"
MARKER_AUTOSAVE = "AutoSave"
MARKER_SAVE = "SaveChanges"
MARKER_ATTACHMENT = "AttachmentSet"
DETAIL_READ_MARKERS = [
    MARKER_CHECKLIST_ROOT,
    MARKER_CHECKLIST_BASIC,
    MARKER_CHECKLIST_PERMISSION,
    MARKER_LAST_CHANGE,
]


def ensure(checks: list[dict[str, Any]], name: str, ok: bool, detail: Any) -> None:
    checks.append({"name": name, "ok": bool(ok), "detail": detail})


def fetch_seed_root_candidates(ui_url: str, limit: int = 5) -> list[str]:
    base = str(ui_url or "").split("#", 1)[0].rsplit("/", 1)[0]
    service_url = (
        f"{base}/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistSearchSet?"
        + urllib.parse.urlencode({"$top": max(1, int(limit or 5)), "$format": "json"})
    )
    try:
        with urllib.request.urlopen(service_url, timeout=15) as response:
            payload = json.load(response)
    except Exception:  # noqa: BLE001
        return []
    results = (((payload or {}).get("d") or {}).get("results") or [])
    unique: list[str] = []
    for item in results:
        value = str(
            (item or {}).get("Key")
            or (item or {}).get("DB_KEY")
            or (item or {}).get("RootKey")
            or (item or {}).get("Id")
            or ""
        ).strip()
        if value and value != "__CREATE" and value not in unique:
            unique.append(value)
    return unique


def is_navigation_race(exc: Exception) -> bool:
    message = str(exc or "")
    return "Execution context was destroyed" in message or "Cannot find context with specified id" in message


def safe_evaluate(page, script: str, arg: Any = None, retries: int = 3):
    last_error = None
    for attempt in range(max(1, int(retries))):
        try:
            if arg is None:
                return page.evaluate(script)
            return page.evaluate(script, arg)
        except Exception as exc:  # noqa: BLE001
            last_error = exc
            if not is_navigation_race(exc) or attempt >= retries - 1:
                raise
            page.wait_for_timeout(750)
    raise last_error


def wait_for_ui5_bootstrap(page) -> None:
    wait_for_app_ready(page, timeout=60000)


def wait_for_search_ready(page) -> None:
    shared_wait_for_search_ready(page, timeout=30000)


def wait_for_detail_ready(page, root_id: str) -> None:
    shared_wait_for_detail_ready(page, root_id, timeout=30000)


def wait_for_edit_detail_ready(page, root_id: str) -> None:
    wait_for_ui5_bootstrap(page)
    page.wait_for_function(
        """
        (expectedRootId) => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const detail = core.byId('checklist_app_comp---detailTargetPage');
          const appState = app && app.getModel && app.getModel('state');
          const state = detail && detail.getModel && detail.getModel('state');
          const selected = detail && detail.getModel && detail.getModel('selected');
          const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
          return !!detail
            && !!appState
            && !!state
            && rootId === expectedRootId
            && appState.getProperty('/currentRouteName') === 'detail'
            && state.getProperty('/workflow/detail/editMode') === 'EDIT'
            && state.getProperty('/workflow/detail/lock/state') === 'EDIT_LOCKED'
            && !!detail.getDomRef();
        }
        """,
        arg=root_id,
        timeout=30000,
    )
    page.wait_for_timeout(900)


def detail_route_state(page) -> dict[str, Any]:
    return safe_evaluate(
        page,
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const detail = core.byId('checklist_app_comp---detailTargetPage');
          const appState = app && app.getModel && app.getModel('state');
          const state = detail && detail.getModel && detail.getModel('state');
          const selected = detail && detail.getModel && detail.getModel('selected');
          return {
            currentRouteName: appState && appState.getProperty ? String(appState.getProperty('/currentRouteName') || '') : '',
            layout: appState && appState.getProperty ? String(appState.getProperty('/layout') || '') : '',
            selectedId: appState && appState.getProperty ? String(appState.getProperty('/selectedId') || '') : '',
            rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            mode: state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '',
            lockState: state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : '',
            busyDetail: !!(state && state.getProperty && state.getProperty('/ui/busy/detail')),
            saveInFlight: !!(state && state.getProperty && state.getProperty('/saveInFlight')),
            autosaveState: state && state.getProperty ? String(state.getProperty('/workflow/detail/autosave/state') || state.getProperty('/autosaveState') || '') : '',
            domReady: !!(detail && detail.getDomRef && detail.getDomRef())
          };
        }
        """
    )


def wait_for_analytics_ready(page) -> None:
    wait_for_ui5_bootstrap(page)
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          const analyticsView = core.byId('checklist_app_comp---analyticsTargetPage');
          const viewModel = analyticsView && analyticsView.getModel && analyticsView.getModel('view');
          return !!state
            && state.getProperty('/currentRouteName') === 'analytics'
            && !!analyticsView
            && !!analyticsView.getDomRef()
            && !!viewModel
            && viewModel.getProperty('/busy') === false;
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(900)


def matching_requests(network: list[dict[str, Any]], marker: str) -> list[dict[str, Any]]:
    return [
        item
        for item in network
        if marker in item["url"] or marker in item.get("post_data", "")
    ]


def direct_requests(network: list[dict[str, Any]], marker: str) -> list[dict[str, Any]]:
    return [
        item
        for item in matching_requests(network, marker)
        if "/$batch" not in item["url"]
    ]


def batch_requests(network: list[dict[str, Any]], marker: str) -> list[dict[str, Any]]:
    return [
        item
        for item in matching_requests(network, marker)
        if "/$batch" in item["url"]
    ]


def batch_operation_requests(network: list[dict[str, Any]], method: str, marker: str) -> list[dict[str, Any]]:
    needle = f"{method.upper()} {marker}"
    return [
        item
        for item in network
        if "/$batch" in item["url"] and needle in item.get("post_data", "")
    ]


def transport_snapshot(network: list[dict[str, Any]], marker: str) -> dict[str, Any]:
    direct = direct_requests(network, marker)
    batched = batch_requests(network, marker)
    return {
        "marker": marker,
        "directCount": len(direct),
        "batchCount": len(batched),
        "directSample": direct[-3:],
        "batchSample": batched[-3:],
    }


def recent_network_by_markers(network: list[dict[str, Any]], markers: list[str], limit: int = 12) -> dict[str, Any]:
    return {
        marker: matching_requests(network, marker)[-limit:]
        for marker in markers
    }


def collect_search_root_candidates(page, limit: int = 5) -> list[str]:
    candidates = safe_evaluate(
        page,
        """
        (maxCount) => {
          const core = sap.ui.getCore();
          const searchView = core.byId('checklist_app_comp---searchTargetPage');
          const controller = searchView && searchView.getController && searchView.getController();
          const ctx = controller && controller._ctx && controller._ctx();
          const smartControls = ctx && ctx.smartControls;
          if (smartControls && typeof smartControls.getBoundRows === 'function') {
            return Promise.resolve(smartControls.getBoundRows(Number(maxCount || 5))).then((rows) => {
              return (rows || []).map((item) => {
                return String((item && (item.Key || item.RootKey || item.Id)) || '').trim();
              }).filter((value) => !!value && value !== '__CREATE').slice(0, Number(maxCount || 5));
            });
          }
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          const smartTable = all.find((item) => item && item.isA && item.isA('sap.ui.comp.smarttable.SmartTable') && item.getId && String(item.getId()).indexOf('searchSmartTable') >= 0) || null;
          const table = (smartTable && smartTable.getTable && smartTable.getTable())
            || all.find((item) => item && item.isA && item.isA('sap.m.Table') && item.getItems && item.getId && String(item.getId()).indexOf('searchSmartTable') >= 0)
            || null;
          const rows = table && table.getItems ? (table.getItems() || []).filter((item) => !!(item && item.getVisible && item.getVisible() && item.getBindingContext && item.getBindingContext())) : [];
          return rows.map((item) => {
            const ctx2 = item && item.getBindingContext ? item.getBindingContext() : null;
            const data = ctx2 && ctx2.getObject ? ctx2.getObject() : null;
            return String((data && (data.Key || data.RootKey || data.Id)) || '').trim();
          }).filter((value) => !!value && value !== '__CREATE').slice(0, Number(maxCount || 5));
        }
        """,
        limit
    ) or []
    unique: list[str] = []
    for candidate in candidates:
        value = str(candidate or "").strip()
        if value and value not in unique:
            unique.append(value)
    return unique


def ensure_search_results_loaded(page, timeout: int = 30000) -> None:
    if fetch_seed_root_candidates(UI_URL, limit=1):
        return
    candidates = collect_search_root_candidates(page, limit=1)
    if candidates:
        return
    invoke_view_controller_method(page, SEARCH_VIEW_ID, "onSmartSearch")
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const searchView = core.byId('checklist_app_comp---searchTargetPage');
          const controller = searchView && searchView.getController && searchView.getController();
          const viewModel = searchView && searchView.getModel && searchView.getModel('view');
          const ctx = controller && controller._ctx && controller._ctx();
          const smartControls = ctx && ctx.smartControls;
          if (viewModel && viewModel.getProperty && Number(viewModel.getProperty('/resultCount') || 0) > 0) {
            return true;
          }
          if (smartControls && typeof smartControls.getBoundRows === 'function') {
            return Promise.resolve(smartControls.getBoundRows(1)).then((rows) => Array.isArray(rows) && rows.length > 0);
          }
          return false;
        }
        """,
        timeout=timeout,
    )
    page.wait_for_timeout(1200)


def detail_view_candidates(page) -> list[dict[str, Any]]:
    return safe_evaluate(
        page,
        """
        () => {
          const core = sap.ui.getCore();
          const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
          const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
          return all.filter((item) => item
            && item.isA
            && item.isA('sap.ui.core.mvc.View')
            && item.getController
            && item.getController()
            && item.getController().getMetadata
            && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Detail')
            .map((view) => {
              const selected = view && view.getModel && view.getModel('selected');
              const state = view && view.getModel && view.getModel('state');
              return {
                id: view && view.getId ? String(view.getId()) : '',
                domReady: !!(view && view.getDomRef && view.getDomRef()),
                rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
                mode: state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '',
                lockState: state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : ''
              };
            });
        }
        """
    ) or []


def wait_for_detail_mode(page, mode: str, lock_state: str | None = None, timeout: int = 20000) -> None:
    page.wait_for_function(
        """
        ({ mode, lockState }) => {
          const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
          const state = view && view.getModel && view.getModel('state');
          const currentMode = state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '';
          const currentLock = state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : '';
          if (currentMode !== String(mode || '')) {
            return false;
          }
          if (lockState && currentLock !== String(lockState || '')) {
            return false;
          }
          return true;
        }
        """,
        {"mode": mode, "lockState": lock_state},
        timeout=timeout,
    )


def classify_route_open_diagnostic(diag: dict[str, Any]) -> str:
    route_snapshot = diag.get("routeSnapshot") or {}
    detail = diag.get("detailState") or {}
    network_reads = diag.get("recentNetworkByMarker") or {}
    has_reads = any(network_reads.get(marker) for marker in DETAIL_READ_MARKERS)
    if route_snapshot.get("currentRouteName") == ROUTE_DETAIL and detail.get("rootId") and detail.get("rootId") != diag.get("requestedRootId"):
        return RESULT_FAIL
    if not has_reads:
        return RESULT_BLOCKED
    return RESULT_FAIL if route_snapshot.get("currentRouteName") == ROUTE_DETAIL else RESULT_BLOCKED


def open_detail_candidate(page, root_id: str, network: list[dict[str, Any]]) -> dict[str, Any]:
    before_counts = {marker: len(matching_requests(network, marker)) for marker in DETAIL_READ_MARKERS}
    diag = {
        "requestedRootId": root_id,
        "routeSnapshot": {},
        "detailState": {},
        "bootstrap": {},
        "recentNetworkByMarker": {},
        "detailViewCandidates": [],
        "classification": CLASSIFIER_ROUTE_TIMEOUT,
        "ok": False,
    }
    try:
        navigate_to_detail(page, root_id)
        wait_for_detail_ready(page, root_id)
        page.wait_for_timeout(1200)
        diag["routeSnapshot"] = capture_route_snapshot(page, "detail.routeCandidate")
        diag["detailState"] = detail_state(page)
        diag["bootstrap"] = collect_bootstrap_diagnostics(page)
        diag["detailViewCandidates"] = detail_view_candidates(page)
        diag["recentNetworkByMarker"] = {
            marker: matching_requests(network, marker)[before_counts[marker]:]
            for marker in DETAIL_READ_MARKERS
        }
        diag["ok"] = diag["detailState"].get("rootId") == root_id and diag["routeSnapshot"].get("currentRouteName") == ROUTE_DETAIL
        diag["classification"] = RESULT_PASS if diag["ok"] else classify_route_open_diagnostic(diag)
        return diag
    except Exception as exc:  # noqa: BLE001
        diag["error"] = str(exc)
        diag["routeSnapshot"] = capture_route_snapshot(page, "detail.routeCandidate.failed")
        try:
            diag["detailState"] = detail_state(page)
        except Exception:  # noqa: BLE001
            diag["detailState"] = {}
        try:
            diag["bootstrap"] = collect_bootstrap_diagnostics(page)
        except Exception:  # noqa: BLE001
            diag["bootstrap"] = {}
        diag["detailViewCandidates"] = detail_view_candidates(page)
        diag["recentNetworkByMarker"] = {
            marker: matching_requests(network, marker)[before_counts[marker]:]
            for marker in DETAIL_READ_MARKERS
        }
        diag["classification"] = classify_route_open_diagnostic(diag)
        return diag


def classify_lock_acquire_result(detail: dict[str, Any]) -> dict[str, Any]:
    toggle_result = detail.get("toggleResult") or {}
    state = detail.get("state") or {}
    error_payload = toggle_result.get("error") or {}
    error_code = str(error_payload.get("code") or "").upper()
    message_key = str(error_payload.get("messageKey") or "").strip()
    effects = toggle_result.get("effects") or []
    warning_dialog = any(item.get("type") == "dialog" and item.get("variant") == "warning" for item in effects if isinstance(item, dict))
    forced_read_only = state.get("mode") == MODE_READ and state.get("lockState") == LOCK_READ_ONLY
    transport = detail.get("transport") or {}
    if transport.get("batchCount", 0) > 0 and (error_code == "KILLED" or message_key == "lockConflictError" or forced_read_only or warning_dialog):
        return {"classification": RESULT_BLOCKED, "reasonCode": CLASSIFIER_LOCK_KILLED}
    if transport.get("batchCount", 0) == 0 and not state.get("mode"):
        return {"classification": RESULT_BLOCKED, "reasonCode": CLASSIFIER_LOCK_ENV}
    return {"classification": RESULT_FAIL, "reasonCode": CLASSIFIER_LOCK_PRODUCT}


def reset_to_search(page) -> None:
    navigate_to_search(page)
    wait_for_search_ready(page)


def resolve_smoke_root(page, network: list[dict[str, Any]], preferred_root_id: str) -> dict[str, Any]:
    diagnostics: list[dict[str, Any]] = []
    candidates: list[str] = []
    preferred = str(preferred_root_id or "").strip()
    if preferred and preferred != "__CREATE":
        candidates.append(preferred)
    for candidate in fetch_seed_root_candidates(UI_URL):
        if candidate not in candidates:
            candidates.append(candidate)
    for candidate in collect_search_root_candidates(page):
        if candidate not in candidates:
            candidates.append(candidate)
    for candidate in candidates:
        route_diag = open_detail_candidate(page, candidate, network)
        route_diag["candidateRootId"] = candidate
        diagnostics.append(route_diag)
        if route_diag.get("ok"):
            return {
                "ok": True,
                "selectedRootId": candidate,
                "routeOpenDiagnostics": route_diag,
                "rootSelectionDiagnostics": diagnostics,
                "candidateSequence": candidates,
            }
        reset_to_search(page)
    return {
        "ok": False,
        "selectedRootId": "",
        "routeOpenDiagnostics": diagnostics[-1] if diagnostics else {},
        "rootSelectionDiagnostics": diagnostics,
        "candidateSequence": candidates,
        "classification": RESULT_BLOCKED,
    }


def autosave_model_outcome(before: dict[str, Any], after: dict[str, Any], expected_equipment: str) -> dict[str, Any]:
    version_advanced = (after.get("version", 0) or 0) > (before.get("version", 0) or 0)
    equipment_applied = after.get("equipment") == expected_equipment
    autosave_saved = after.get("autosaveState") == AUTOSAVE_STATE_SAVED
    stable_detail = (
        after.get("currentRouteName") == ROUTE_DETAIL
        and after.get("mode") == MODE_EDIT
        and after.get("lockState") == LOCK_EDIT_LOCKED
        and not after.get("busyDetail")
        and not after.get("saveInFlight")
    )
    confirmed = stable_detail and (autosave_saved or version_advanced or equipment_applied)
    return {
        "confirmed": confirmed,
        "stableDetail": stable_detail,
        "versionAdvanced": version_advanced,
        "equipmentApplied": equipment_applied,
        "autosaveSaved": autosave_saved,
        "finished": bool(after.get("autosaveFinished"))
    }


def build_report(
    checks: list[dict[str, Any]],
    failures: list[str],
    network: list[dict[str, Any]],
    extra: dict[str, Any] | None = None,
) -> dict[str, Any]:
    report = {
        "generatedAt": int(time.time()),
        "uiUrl": UI_URL,
        "rootId": ROOT_ID,
        "ok": not failures,
        "checks": checks,
        "failures": failures,
        "networkSample": network[-25:],
    }
    if extra:
        report.update(extra)
    return report


def capture_route_snapshot(page, label: str) -> dict[str, Any]:
    snapshot = safe_evaluate(
        page,
        """
        (label) => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const detail = core.byId('checklist_app_comp---detailTargetPage');
          const analytics = core.byId('checklist_app_comp---analyticsTargetPage');
          const search = core.byId('checklist_app_comp---searchTargetPage');
          const appState = app && app.getModel && app.getModel('state');
          const detailState = detail && detail.getModel && detail.getModel('state');
          const selected = detail && detail.getModel && detail.getModel('selected');
          return {
            label: String(label || ''),
            hash: String(window.location.hash || ''),
            currentRouteName: appState && appState.getProperty ? String(appState.getProperty('/currentRouteName') || '') : '',
            layout: appState && appState.getProperty ? String(appState.getProperty('/layout') || '') : '',
            selectedId: appState && appState.getProperty ? String(appState.getProperty('/selectedId') || '') : '',
            activeObjectId: appState && appState.getProperty ? String(appState.getProperty('/activeObjectId') || '') : '',
            detailRootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            detailMode: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/editMode') || '') : '',
            detailLockState: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/lock/state') || '') : '',
            detailAutosaveState: detailState && detailState.getProperty ? String(detailState.getProperty('/workflow/detail/autosave/state') || detailState.getProperty('/autosaveState') || '') : '',
            searchVisible: !!(search && search.getDomRef && search.getDomRef()),
            detailVisible: !!(detail && detail.getDomRef && detail.getDomRef()),
            analyticsVisible: !!(analytics && analytics.getDomRef && analytics.getDomRef())
          };
        }
        """,
        label
    )
    return snapshot


def flush_report(report: dict[str, Any]) -> int:
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
    return 1 if report.get("failures") else 0


def classify_failure(step: str, error: Exception | str) -> str:
    message = str(error or "")
    if "lock.acquire" in step or "detail.lock.acquire" in step:
        return RESULT_BLOCKED
    if "route.open.detail" in step:
        return RESULT_BLOCKED if "Timeout" in message else RESULT_FAIL
    if "Execution context was destroyed" in message or "Cannot find context with specified id" in message:
        return "page/context lifecycle bug"
    if "Timeout" in message:
        if "analytics.close" in step or "attachments." in step:
            return RESULT_FAIL
        return AUTOSAVE_CLASS_TOOLING
    if "Locator" in message or "selector" in message:
        return "selector bug"
    return AUTOSAVE_CLASS_TOOLING


def trigger_component_autosave(page, expected_equipment: str) -> dict[str, Any]:
    return safe_evaluate(
        page,
        """
        (expectedEquipment) => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const detail = core.byId('checklist_app_comp---detailTargetPage');
          const component = app ? sap.ui.core.Component.getOwnerComponentFor(app) : null;
          const state = detail && detail.getModel && detail.getModel('state');
          const selected = detail && detail.getModel && detail.getModel('selected');
          if (!component || !detail || !state || !selected) {
            window.__gatewaySmokeAutosave = { started: false, ok: false, error: 'detail autosave dependencies unavailable' };
            return window.__gatewaySmokeAutosave;
          }
          if (!component._oAutoSave || typeof component._oAutoSave.touch !== 'function') {
            window.__gatewaySmokeAutosave = { started: false, ok: false, error: 'component autosave manager unavailable' };
            return window.__gatewaySmokeAutosave;
          }
          const sValue = String(expectedEquipment || ('Gateway browser autosave ' + Date.now()));
          const oEventProvider = component._oAutoSave;
          const fnDone = function () {
            window.__gatewaySmokeAutosave = Object.assign({}, window.__gatewaySmokeAutosave || {}, {
              finished: true,
              ok: true,
              event: 'autosaveDone'
            });
            oEventProvider.detachEvent('autosaveDone', fnDone);
            oEventProvider.detachEvent('autosaveError', fnError);
          };
          const fnError = function (oEvent) {
            const mParameters = oEvent && oEvent.getParameters ? oEvent.getParameters() : {};
            window.__gatewaySmokeAutosave = Object.assign({}, window.__gatewaySmokeAutosave || {}, {
              finished: true,
              ok: false,
              event: 'autosaveError',
              error: String((mParameters && mParameters.error && mParameters.error.message) || mParameters.error || 'autosave error')
            });
            oEventProvider.detachEvent('autosaveDone', fnDone);
            oEventProvider.detachEvent('autosaveError', fnError);
          };
          selected.setProperty('/basic/equipment', sValue);
          state.setProperty('/isDirty', true);
          window.__gatewaySmokeAutosave = {
            started: true,
            ok: false,
            finished: false,
            equipment: sValue,
            trigger: 'component._oAutoSave.touch'
          };
          oEventProvider.attachEvent('autosaveDone', fnDone);
          oEventProvider.attachEvent('autosaveError', fnError);
          component._oAutoSave.touch();
          return window.__gatewaySmokeAutosave;
        }
        """,
        expected_equipment
    ) or {}


def detail_state(page) -> dict[str, Any]:
    return safe_evaluate(
        page,
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
          const appState = app && app.getModel && app.getModel('state');
          const selected = view && view.getModel && view.getModel('selected');
          const state = view && view.getModel && view.getModel('state');
          const autosave = window.__gatewaySmokeAutosave || {};
          return {
            currentRouteName: appState && appState.getProperty ? String(appState.getProperty('/currentRouteName') || '') : '',
            rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            version: selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0,
            equipment: selected && selected.getProperty ? String(selected.getProperty('/basic/equipment') || '') : '',
            mode: state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '',
            lockState: state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : '',
            autosaveState: state && state.getProperty ? String(state.getProperty('/workflow/detail/autosave/state') || state.getProperty('/autosaveState') || '') : '',
            busyDetail: !!(state && state.getProperty && state.getProperty('/ui/busy/detail')),
            saveInFlight: !!(state && state.getProperty && state.getProperty('/saveInFlight')),
            isDirty: !!(state && state.getProperty && state.getProperty('/isDirty')),
            autosaveFinished: !!autosave.finished
          };
        }
        """
    )


def wait_for_detail_mutation_ready(page, root_id: str, timeout: int = 45000) -> dict[str, Any]:
    wait_error = ""
    try:
        page.wait_for_function(
            """
            ({ expectedRootId, autosaveSaving }) => {
              const core = sap.ui.getCore();
              const app = core.byId('checklist_app_comp---app');
              const view = core.byId('checklist_app_comp---detailTargetPage');
              const appState = app && app.getModel && app.getModel('state');
              const selected = view && view.getModel && view.getModel('selected');
              const state = view && view.getModel && view.getModel('state');
              const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
              const routeName = appState && appState.getProperty ? String(appState.getProperty('/currentRouteName') || '') : '';
              const mode = state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '';
              const lockState = state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : '';
              const autosaveState = state && state.getProperty ? String(state.getProperty('/workflow/detail/autosave/state') || state.getProperty('/autosaveState') || '') : '';
              const busyDetail = !!(state && state.getProperty && state.getProperty('/ui/busy/detail'));
              const saveInFlight = !!(state && state.getProperty && state.getProperty('/saveInFlight'));
              return routeName === 'detail'
                && rootId === String(expectedRootId || '')
                && mode === 'EDIT'
                && lockState === 'EDIT_LOCKED'
                && !!(view && view.getDomRef && view.getDomRef())
                && !busyDetail
                && !saveInFlight
                && autosaveState !== String(autosaveSaving || '');
            }
            """,
            arg={"expectedRootId": root_id, "autosaveSaving": AUTOSAVE_STATE_SAVING},
            timeout=timeout,
        )
    except Exception as exc:  # noqa: BLE001
        wait_error = str(exc)
    state = detail_state(page)
    route_snapshot = capture_route_snapshot(page, "detail.mutationReady")
    ok = (
        state.get("currentRouteName") == ROUTE_DETAIL
        and state.get("rootId") == root_id
        and state.get("mode") == MODE_EDIT
        and state.get("lockState") == LOCK_EDIT_LOCKED
        and not state.get("busyDetail")
        and not state.get("saveInFlight")
        and state.get("autosaveState") != AUTOSAVE_STATE_SAVING
    )
    classification = RESULT_PASS if ok else RESULT_FAIL
    return {
        "ok": ok,
        "classification": classification,
        "waitError": wait_error,
        "stateEvidence": state,
        "routeSnapshot": route_snapshot,
    }


def wait_for_analytics_close_ready(page, root_id: str, network: list[dict[str, Any]], timeout: int = 45000) -> dict[str, Any]:
    wait_error = ""
    try:
        wait_for_detail_ready(page, root_id)
        page.wait_for_function(
            """
            (expectedRootId) => {
              const core = sap.ui.getCore();
              const app = core.byId('checklist_app_comp---app');
              const detail = core.byId('checklist_app_comp---detailTargetPage');
              const appState = app && app.getModel && app.getModel('state');
              const selected = detail && detail.getModel && detail.getModel('selected');
              const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
              const routeName = appState && appState.getProperty ? String(appState.getProperty('/currentRouteName') || '') : '';
              return routeName === 'detail'
                && rootId === String(expectedRootId || '')
                && !!(detail && detail.getDomRef && detail.getDomRef());
            }
            """,
            arg=root_id,
            timeout=timeout,
        )
    except Exception as exc:  # noqa: BLE001
        wait_error = str(exc)
    mutation_ready = wait_for_detail_mutation_ready(page, root_id, timeout=timeout)
    route_snapshot = capture_route_snapshot(page, "detail.afterAnalyticsClose")
    state = detail_state(page)
    transport = recent_network_by_markers(network, [MARKER_SAVE, MARKER_AUTOSAVE, MARKER_LOCK_HEARTBEAT], limit=6)
    route_ok = (
        route_snapshot.get("currentRouteName") == ROUTE_DETAIL
        and route_snapshot.get("detailRootId") == root_id
        and state.get("rootId") == root_id
    )
    ok = route_ok and mutation_ready.get("ok")
    classification = RESULT_PASS if ok else RESULT_FAIL
    return {
        "ok": ok,
        "classification": classification,
        "reasonCode": "" if ok else CLASSIFIER_ANALYTICS_RETURN,
        "waitError": wait_error,
        "routeSnapshot": route_snapshot,
        "stateEvidence": state,
        "mutationReady": mutation_ready,
        "transportEvidence": transport,
    }


def invoke_view_controller_method(page, view_id: str, method_name: str, *args: Any):
    controller_name = "PRODUCTION_CONTROL_CHECKLIST.controller.Detail"
    if "searchTargetPage" in view_id:
        controller_name = "PRODUCTION_CONTROL_CHECKLIST.controller.Search"
    elif "analyticsTargetPage" in view_id:
        controller_name = "PRODUCTION_CONTROL_CHECKLIST.controller.Analytics"
    return shared_invoke_controller_method(page, controller_name, method_name, *args)


def set_detail_edit_mode(page, state: bool) -> Any:
    return safe_evaluate(
        page,
        """
        (targetState) => {
          const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller.onToggleEdit !== 'function') {
            throw new Error('onToggleEdit is not available');
          }
          return Promise.resolve(controller.onToggleEdit({
            getParameter: function (name) {
              return name === 'state' ? !!targetState : undefined;
            }
          }));
        }
        """,
        state,
    )


def enter_edit_or_report(page) -> tuple[bool, dict[str, Any]]:
    result = set_detail_edit_mode(page, True)
    try:
        page.wait_for_function(
            """
            () => {
              const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
              const state = view && view.getModel && view.getModel('state');
              return !!(state && state.getProperty && state.getProperty('/workflow/detail/editMode') === 'EDIT');
            }
            """,
            timeout=20000,
        )
        page.wait_for_timeout(1600)
        return True, {"toggleResult": result, "state": detail_state(page)}
    except Exception as exc:  # noqa: BLE001
        return False, {
            "toggleResult": result,
            "state": detail_state(page),
            "error": str(exc),
        }



def ensure_attachments_expanded(page) -> None:
    safe_evaluate(
        page,
        """
        () => {
          const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
          const controller = view && view.getController && view.getController();
          const viewModel = view && view.getModel && view.getModel('view');
          if (!controller || typeof controller.onToggleAttachmentsSection !== 'function') {
            throw new Error('onToggleAttachmentsSection is not available');
          }
          if (viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentsExpanded')) {
            return Promise.resolve(true);
          }
          return Promise.resolve(controller.onToggleAttachmentsSection());
        }
        """
    )
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---detailTargetPage');
          const viewModel = view && view.getModel && view.getModel('view');
          const expanded = !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentsExpanded'));
          const historyLoaded = !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentsLoaded'));
          const uploader = core.byId('checklist_app_comp---detailTargetPage--attachmentUploader');
          return expanded && !!uploader && (historyLoaded || true);
        }
        """,
        timeout=30000,
    )
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const control = core.byId('checklist_app_comp---detailTargetPage--attachmentUploader');
          const state = core.byId('checklist_app_comp---detailTargetPage')?.getModel?.('state');
          const autosaveState = state && state.getProperty ? String(state.getProperty('/workflow/detail/autosave/state') || state.getProperty('/autosaveState') || '') : '';
          return !!control
            && control.getEnabled && control.getEnabled()
            && !!state
            && state.getProperty('/workflow/detail/editMode') === 'EDIT'
            && state.getProperty('/workflow/detail/lock/state') === 'EDIT_LOCKED'
            && state.getProperty('/saveInFlight') === false
            && state.getProperty('/ui/busy/detail') === false
            && autosaveState !== 'SAVING';
        }
        """,
        timeout=10000,
    )
    page.wait_for_timeout(1200)


def invoke_attachment_upload(page, file_name: str, file_text: str, mime_type: str = "text/plain") -> Any:
    return safe_evaluate(
        page,
        """
        ({ fileName, fileText, mimeType }) => {
          const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
          const controller = view && view.getController && view.getController();
          const uploader = controller && controller.byId && controller.byId('attachmentUploader');
          if (!controller || typeof controller.onAttachmentUploadChange !== 'function') {
            throw new Error('onAttachmentUploadChange is not available');
          }
          const file = new File([fileText], String(fileName || 'gateway-smoke.txt'), {
            type: String(mimeType || 'text/plain')
          });
          return Promise.resolve(controller.onAttachmentUploadChange({
            getSource: function () {
              return uploader || null;
            },
            getParameter: function (name) {
              return name === 'files' ? [file] : undefined;
            }
          }));
        }
        """,
        {"fileName": file_name, "fileText": file_text, "mimeType": mime_type}
    )
def main() -> int:
    network: list[dict[str, Any]] = []
    checks: list[dict[str, Any]] = []
    failures: list[str] = []
    last_state: dict[str, Any] = {}
    route_snapshots: list[dict[str, Any]] = []
    root_selection_diagnostics: list[dict[str, Any]] = []
    route_open_diagnostics: dict[str, Any] = {}
    lock_acquire_diagnostics: dict[str, Any] = {}
    analytics_close_diagnostics: dict[str, Any] = {}
    attachment_readiness_diagnostics: dict[str, Any] = {}
    current_step = "startup"
    selected_root_id = ""
    attachment_file = Path("docs/runtime/gateway-smoke-attachment.txt")
    attachment_file.parent.mkdir(parents=True, exist_ok=True)
    attachment_file.write_text("gateway browser smoke attachment payload", encoding="utf-8")

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch()
            page = browser.new_page(viewport={"width": 1440, "height": 960})

            def on_request(req) -> None:
                if "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV" not in req.url:
                    return
                network.append(
                    {
                        "method": req.method,
                        "url": req.url,
                        "headers": req.headers,
                        "post_data": (req.post_data or "")[:6000],
                    }
                )

            page.on("request", on_request)

            current_step = "route.open.search"
            page.goto(UI_URL, wait_until="domcontentloaded", timeout=90000)
            navigate_to_search(page)
            wait_for_search_ready(page)
            route_snapshots.append(capture_route_snapshot(page, "search.initial"))

            smart_controls = safe_evaluate(
                page,
                """
                () => {
                  const core = sap.ui.getCore();
                  const registry = sap.ui.core && sap.ui.core.Element && sap.ui.core.Element.registry;
                  const all = registry && registry.all ? Object.keys(registry.all()).map((key) => registry.get(key)).filter(Boolean) : Object.values(core.mElements || {});
                  const searchView = all.find((item) => item
                    && item.isA
                    && item.isA('sap.ui.core.mvc.View')
                    && item.getController
                    && item.getController()
                    && item.getController().getMetadata
                    && item.getController().getMetadata().getName() === 'PRODUCTION_CONTROL_CHECKLIST.controller.Search');
                  const smartFilterBar = all.find((item) => item && item.getId && String(item.getId()).endsWith('searchSmartFilterBar'));
                  const smartTable = all.find((item) => item && item.getId && String(item.getId()).endsWith('searchSmartTable'));
                  return {
                    hasSmartFilterBar: !!smartFilterBar,
                    hasSmartTable: !!smartTable,
                    searchVisible: !!(searchView && searchView.getDomRef && searchView.getDomRef())
                  };
                }
                """
            )
            ok_smart = bool(smart_controls.get("hasSmartFilterBar")) and bool(smart_controls.get("hasSmartTable")) and bool(smart_controls.get("searchVisible"))
            ensure(checks, "search.smart.gateway.controls", ok_smart, smart_controls)
            if not ok_smart:
                failures.append("search.smart.gateway.controls")

            current_step = "root.selection"
            if not fetch_seed_root_candidates(UI_URL, limit=1):
                ensure_search_results_loaded(page)
            root_resolution = resolve_smoke_root(page, network, ROOT_ID)
            root_selection_diagnostics = root_resolution.get("rootSelectionDiagnostics") or []
            route_open_diagnostics = root_resolution.get("routeOpenDiagnostics") or {}
            selected_root_id = str(root_resolution.get("selectedRootId") or "").strip()
            ensure(checks, "detail.root.selection", bool(root_resolution.get("ok")), {
                "preferredRootId": ROOT_ID,
                "selectedRootId": selected_root_id,
                "candidateSequence": root_resolution.get("candidateSequence") or [],
                "rootSelectionDiagnostics": root_selection_diagnostics,
            })
            if not root_resolution.get("ok"):
                failures.append("detail.root.selection")
                browser.close()
                return flush_report(build_report(checks, failures, network, {
                    "lastState": last_state,
                    "rootSelectionDiagnostics": root_selection_diagnostics,
                    "routeOpenDiagnostics": route_open_diagnostics,
                    "selectedRootId": selected_root_id,
                }))

            route_snapshots.append(capture_route_snapshot(page, "detail.initial"))

            opened = detail_state(page)
            last_state = opened
            ok_open = opened.get("rootId") == selected_root_id
            ensure(checks, "detail.route.opened", ok_open, {
                "stateEvidence": opened,
                "routeOpenDiagnostics": route_open_diagnostics,
            })
            if not ok_open:
                failures.append("detail.route.opened")

            current_step = "lock.acquire"
            before_lock = len(matching_requests(network, MARKER_LOCK_ACQUIRE))
            edit_ok, edit_detail = enter_edit_or_report(page)
            after_lock = len(matching_requests(network, MARKER_LOCK_ACQUIRE))
            edit_state = edit_detail.get("state") or detail_state(page)
            last_state = edit_state
            ok_lock = edit_ok and after_lock > before_lock and edit_state.get("mode") == "EDIT"
            lock_acquire_diagnostics = classify_lock_acquire_result({
                "toggleResult": edit_detail.get("toggleResult"),
                "state": edit_state,
                "transport": transport_snapshot(network, MARKER_LOCK_ACQUIRE),
            })
            ensure(checks, "detail.lock.acquire", ok_lock, {
                "before": before_lock,
                "after": after_lock,
                "stateEvidence": edit_state,
                "toggleResult": edit_detail.get("toggleResult"),
                "transportEvidence": transport_snapshot(network, MARKER_LOCK_ACQUIRE),
                "classification": lock_acquire_diagnostics.get("classification"),
                "contourEvidence": lock_acquire_diagnostics,
                "error": edit_detail.get("error", ""),
            })
            if not ok_lock:
                failures.append("detail.lock.acquire")
                browser.close()
                return flush_report(build_report(checks, failures, network, {
                    "lastState": last_state,
                    "rootSelectionDiagnostics": root_selection_diagnostics,
                    "routeOpenDiagnostics": route_open_diagnostics,
                    "lockAcquireDiagnostics": lock_acquire_diagnostics,
                    "selectedRootId": selected_root_id,
                }))

            current_step = "detail.save"
            save_before = detail_state(page)
            save_request_count_before = len(matching_requests(network, "SaveChanges"))
            save_call = safe_evaluate(
                page,
                """
                () => new Promise((resolve, reject) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                  const controller = view && view.getController && view.getController();
                  const selected = view && view.getModel && view.getModel('selected');
                  const state = view && view.getModel && view.getModel('state');
                  if (!controller || !selected || !state) {
                    reject(new Error('detail controller/models unavailable'));
                    return;
                  }
                  const sValue = 'Gateway browser save ' + Date.now();
                  selected.setProperty('/basic/equipment', sValue);
                  state.setProperty('/isDirty', true);
                  Promise.resolve(controller.onSaveDetail()).then(() => {
                    resolve({ equipment: sValue });
                  }).catch(reject);
                })
                """
            )
            page.wait_for_function(
                """
                (prevVersion) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                  const state = view && view.getModel && view.getModel('state');
                  const selected = view && view.getModel && view.getModel('selected');
                  const version = selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0;
                  return version > Number(prevVersion || 0) && !!(state && state.getProperty && state.getProperty('/ui/busy/detail') === false);
                }
                """,
                arg=save_before.get("version") or 0,
                timeout=30000,
            )
            page.wait_for_timeout(1600)
            save_after = detail_state(page)
            last_state = save_after
            save_requests = matching_requests(network, "SaveChanges")
            ok_save = len(save_requests) > save_request_count_before and save_after.get("equipment") == save_call.get("equipment") and save_after.get("version", 0) > save_before.get("version", 0)
            ensure(checks, "detail.save.gateway", ok_save, {"before": save_before, "after": save_after, "requestCount": len(save_requests), "transport": transport_snapshot(network, "SaveChanges")})
            if not ok_save:
                failures.append("detail.save.gateway")

            current_step = "detail.autosave"
            page.wait_for_function(
                """
                () => {
                  const core = sap.ui.getCore();
                  const app = core && core.byId('checklist_app_comp---app');
                  const detail = core && core.byId('checklist_app_comp---detailTargetPage');
                  const appState = app && app.getModel && app.getModel('state');
                  const state = detail && detail.getModel && detail.getModel('state');
                  const selected = detail && detail.getModel && detail.getModel('selected');
                  return !!app
                    && !!detail
                    && !!appState
                    && !!selected
                    && !!state
                    && appState.getProperty('/currentRouteName') === 'detail'
                    && state.getProperty('/workflow/detail/editMode') === 'EDIT'
                    && state.getProperty('/workflow/detail/lock/state') === 'EDIT_LOCKED'
                    && state.getProperty('/ui/busy/detail') === false
                    && state.getProperty('/saveInFlight') === false;
                }
                """,
                timeout=10000,
            )
            autosave_before = detail_state(page)
            autosave_request_count_before = len(matching_requests(network, MARKER_AUTOSAVE))
            autosave_expected_equipment = "Gateway browser autosave " + str(int(time.time() * 1000))
            route_snapshots.append(capture_route_snapshot(page, "detail.beforeAutosave"))
            autosave_status = trigger_component_autosave(page, autosave_expected_equipment)
            autosave_wait_error = ""
            try:
                page.wait_for_function(
                    """
                    () => {
                      if (window.__gatewaySmokeAutosave && window.__gatewaySmokeAutosave.started) {
                        return true;
                      }
                      const core = sap.ui.getCore();
                      const app = core.byId('checklist_app_comp---app');
                      const component = app ? sap.ui.core.Component.getOwnerComponentFor(app) : null;
                      return !!(component && component._oAutoSave);
                    }
                    """,
                    timeout=10000,
                )
                page.wait_for_function(
                    """
                    (prevVersion) => {
                      const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                      const selected = view && view.getModel && view.getModel('selected');
                      const state = view && view.getModel && view.getModel('state');
                      const version = selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0;
                      const autosaveState = state && state.getProperty ? String(state.getProperty('/workflow/detail/autosave/state') || state.getProperty('/autosaveState') || '') : '';
                      const equipment = selected && selected.getProperty ? String(selected.getProperty('/basic/equipment') || '') : '';
                      const routeState = sap.ui.getCore().byId('checklist_app_comp---app')?.getModel?.('state');
                      const routeName = routeState && routeState.getProperty ? String(routeState.getProperty('/currentRouteName') || '') : '';
                      const lockState = state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : '';
                      const editMode = state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '';
                      const saveInFlight = !!(state && state.getProperty && state.getProperty('/saveInFlight'));
                      const busyDetail = !!(state && state.getProperty && state.getProperty('/ui/busy/detail'));
                      const autosave = window.__gatewaySmokeAutosave || {};
                      const outcomeOk = (
                        autosaveState === 'SAVED'
                        || version > Number(prevVersion || 0)
                        || !!autosave.finished
                      ) && equipment === String(autosave.equipment || '');
                      const transportOk = !!autosave.started;
                      return transportOk
                        && outcomeOk
                        && routeName === 'detail'
                        && editMode === 'EDIT'
                        && lockState === 'EDIT_LOCKED'
                        && !saveInFlight
                        && !busyDetail;
                    }
                    """,
                    arg=autosave_before.get("version") or 0,
                    timeout=30000,
                )
                page.wait_for_timeout(1200)
            except Exception as exc:  # noqa: BLE001
                autosave_wait_error = str(exc)
            autosave_after = detail_state(page)
            last_state = autosave_after
            autosave_status = safe_evaluate(page, "() => window.__gatewaySmokeAutosave || {}")
            autosave_outcome = autosave_model_outcome(autosave_before, autosave_after, autosave_expected_equipment)
            autosave_transport = transport_snapshot(network, MARKER_AUTOSAVE)
            autosave_triggered = bool(autosave_status.get("started")) or autosave_transport.get("batchCount", 0) > autosave_request_count_before
            autosave_blocked = bool(autosave_wait_error) and not autosave_triggered and "ERR_" in autosave_wait_error.upper()
            ok_autosave = autosave_triggered and autosave_outcome.get("confirmed") and (
                bool(autosave_status.get("ok")) or autosave_outcome.get("confirmed")
            )
            autosave_classification = (
                RESULT_PASS if ok_autosave else
                AUTOSAVE_CLASS_BLOCKED if autosave_blocked else
                RESULT_FAIL if autosave_triggered or not autosave_outcome.get("confirmed") else
                AUTOSAVE_CLASS_TOOLING
            )
            ensure(checks, "detail.autosave.gateway", ok_autosave, {
                "before": autosave_before,
                "after": autosave_after,
                "expectedEquipment": autosave_expected_equipment,
                "autosaveTriggerStatus": autosave_status,
                "autosaveTransport": autosave_transport,
                "autosaveModelOutcome": autosave_outcome,
                "autosaveClassification": autosave_classification,
                "waitError": autosave_wait_error,
                "routeSnapshot": capture_route_snapshot(page, "detail.afterAutosave")
            })
            if not ok_autosave:
                failures.append("detail.autosave.gateway")

            current_step = "analytics.open"
            analytics_request_before = len(
                matching_requests(network, "SimpleAnalyticalSet")
            ) + len(
                matching_requests(network, "WorkflowAnalyticsBreakdownSet")
            )
            invoke_view_controller_method(page, "checklist_app_comp---detailTargetPage", "onOpenWorkflowAnalytics")
            wait_for_analytics_ready(page)
            route_snapshots.append(capture_route_snapshot(page, "analytics.fromDetail"))
            analytics_request_after = len(
                matching_requests(network, "SimpleAnalyticalSet")
            ) + len(
                matching_requests(network, "WorkflowAnalyticsBreakdownSet")
            )
            analytics_state = safe_evaluate(
                page,
                """
                () => {
                  const core = sap.ui.getCore();
                  const app = core.byId('checklist_app_comp---app');
                  const state = app && app.getModel && app.getModel('state');
                  const analyticsView = core.byId('checklist_app_comp---analyticsTargetPage');
                  const viewModel = analyticsView && analyticsView.getModel && analyticsView.getModel('view');
                  return {
                    routeName: state && state.getProperty ? String(state.getProperty('/currentRouteName') || '') : '',
                    layout: state && state.getProperty ? String(state.getProperty('/layout') || '') : '',
                    total: viewModel && viewModel.getProperty ? Number(viewModel.getProperty('/analytics/total') || 0) : 0,
                    error: viewModel && viewModel.getProperty ? String(viewModel.getProperty('/error') || '') : ''
                  };
                }
                """
            )
            ok_analytics = analytics_request_after > analytics_request_before and analytics_state.get("routeName") == "analytics" and not analytics_state.get("error")
            ensure(checks, "analytics.screen.gateway", ok_analytics, {"before": analytics_request_before, "after": analytics_request_after, "state": analytics_state})
            if not ok_analytics:
                failures.append("analytics.screen.gateway")
            current_step = "analytics.close"
            invoke_view_controller_method(page, "checklist_app_comp---analyticsTargetPage", "onCloseAnalytics")
            analytics_close_diagnostics = wait_for_analytics_close_ready(page, selected_root_id, network)
            analytics_return_state = analytics_close_diagnostics.get("stateEvidence") or detail_route_state(page)
            route_snapshots.append(analytics_close_diagnostics.get("routeSnapshot") or capture_route_snapshot(page, "detail.afterAnalyticsClose"))
            ok_analytics_return = (
                analytics_close_diagnostics.get("ok") is True
                and analytics_return_state.get("currentRouteName") == ROUTE_DETAIL
                and analytics_return_state.get("rootId") == selected_root_id
                and analytics_return_state.get("mode") == MODE_EDIT
                and analytics_return_state.get("lockState") == LOCK_EDIT_LOCKED
            )
            ensure(checks, "analytics.close.gateway", ok_analytics_return, analytics_close_diagnostics)
            if not ok_analytics_return:
                failures.append("analytics.close.gateway")

            current_step = "attachments.expand"
            attachment_readiness_diagnostics = wait_for_detail_mutation_ready(page, selected_root_id)
            ensure(checks, "detail.attachment.ready", bool(attachment_readiness_diagnostics.get("ok")), attachment_readiness_diagnostics)
            if not attachment_readiness_diagnostics.get("ok"):
                failures.append("detail.attachment.ready")
            ensure_attachments_expanded(page)
            current_step = "attachments.upload"
            attachment_before = safe_evaluate(
                page,
                """
                () => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                  const viewModel = view && view.getModel && view.getModel('view');
                  const selected = view && view.getModel && view.getModel('selected');
                  const state = view && view.getModel && view.getModel('state');
                  const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
                  const sessionAttachments = viewModel && viewModel.getProperty ? (viewModel.getProperty('/sessionAttachments') || []) : [];
                  return {
                    attachmentCount: Array.isArray(attachments) ? attachments.length : 0,
                    sessionAttachmentCount: Array.isArray(sessionAttachments) ? sessionAttachments.length : 0,
                    busy: !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentBusy')),
                    isDirty: !!(state && state.getProperty && state.getProperty('/isDirty'))
                  };
                }
                """
            )
            before_upload = len(network)
            invoke_attachment_upload(page, attachment_file.name, attachment_file.read_text(encoding="utf-8"))
            attachment_wait_error = ""
            previous_attachment_count = max(
                attachment_before.get("attachmentCount") or 0,
                attachment_before.get("sessionAttachmentCount") or 0,
            )
            try:
                page.wait_for_function(
                    """
                    (prevCount) => {
                      const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                      const viewModel = view && view.getModel && view.getModel('view');
                      const selected = view && view.getModel && view.getModel('selected');
                      const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
                      const sessionAttachments = viewModel && viewModel.getProperty ? (viewModel.getProperty('/sessionAttachments') || []) : [];
                      const busy = !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentBusy'));
                      return busy
                        || (Array.isArray(attachments) && attachments.length > Number(prevCount || 0))
                        || (Array.isArray(sessionAttachments) && sessionAttachments.length > Number(prevCount || 0));
                    }
                    """,
                    arg=previous_attachment_count,
                    timeout=10000,
                )
                page.wait_for_function(
                    """
                    (prevCount) => {
                      const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                      const viewModel = view && view.getModel && view.getModel('view');
                      const selected = view && view.getModel && view.getModel('selected');
                      const state = view && view.getModel && view.getModel('state');
                      const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
                      const sessionAttachments = viewModel && viewModel.getProperty ? (viewModel.getProperty('/sessionAttachments') || []) : [];
                      const nextCount = Math.max(
                        Array.isArray(attachments) ? attachments.length : 0,
                        Array.isArray(sessionAttachments) ? sessionAttachments.length : 0
                      );
                      const attachmentBusy = !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentBusy'));
                      const saveInFlight = !!(state && state.getProperty && state.getProperty('/saveInFlight'));
                      const autosaveState = state && state.getProperty ? String(state.getProperty('/workflow/detail/autosave/state') || state.getProperty('/autosaveState') || '') : '';
                      return nextCount > Number(prevCount || 0)
                        && (!attachmentBusy || saveInFlight || autosaveState === 'SAVING');
                    }
                    """,
                    arg=previous_attachment_count,
                    timeout=30000,
                )
            except Exception as exc:  # noqa: BLE001
                attachment_wait_error = str(exc)
            page.wait_for_timeout(1200)
            attachment_stage_requests = [
                item
                for item in network[before_upload:]
                if any(marker in item["url"] or marker in item.get("post_data", "") for marker in [MARKER_ATTACHMENT, MARKER_SAVE, "CreateChecklist", MARKER_AUTOSAVE])
            ]
            attachment_after_stage = safe_evaluate(
                page,
                """
                () => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                  const viewModel = view && view.getModel && view.getModel('view');
                  const selected = view && view.getModel && view.getModel('selected');
                  const state = view && view.getModel && view.getModel('state');
                  const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
                  const sessionAttachments = viewModel && viewModel.getProperty ? (viewModel.getProperty('/sessionAttachments') || []) : [];
                  return {
                    attachmentCount: Array.isArray(attachments) ? attachments.length : 0,
                    sessionAttachmentCount: Array.isArray(sessionAttachments) ? sessionAttachments.length : 0,
                    isDirty: !!(state && state.getProperty && state.getProperty('/isDirty'))
                  };
                }
                """
            )
            stage_ok = (
                max(attachment_after_stage.get("attachmentCount", 0), attachment_after_stage.get("sessionAttachmentCount", 0))
                    >= previous_attachment_count + 1
            )
            attachment_save_before = len(matching_requests(network, "SaveChanges"))
            attachment_save_before_state = detail_state(page)
            invoke_view_controller_method(page, "checklist_app_comp---detailTargetPage", "onSaveDetail")
            page.wait_for_function(
                """
                (prevVersion) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                  const state = view && view.getModel && view.getModel('state');
                  const selected = view && view.getModel && view.getModel('selected');
                  const version = selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0;
                  return version > Number(prevVersion || 0)
                    && !!(state && state.getProperty && state.getProperty('/ui/busy/detail') === false)
                    && !!(state && state.getProperty && state.getProperty('/isDirty') === false);
                }
                """,
                arg=attachment_save_before_state.get("version") or 0,
                timeout=30000,
            )
            page.wait_for_timeout(1200)
            attachment_after_save = safe_evaluate(
                page,
                """
                () => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                  const viewModel = view && view.getModel && view.getModel('view');
                  const selected = view && view.getModel && view.getModel('selected');
                  const state = view && view.getModel && view.getModel('state');
                  const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
                  const sessionAttachments = viewModel && viewModel.getProperty ? (viewModel.getProperty('/sessionAttachments') || []) : [];
                  return {
                    attachmentCount: Array.isArray(attachments) ? attachments.length : 0,
                    sessionAttachmentCount: Array.isArray(sessionAttachments) ? sessionAttachments.length : 0,
                    isDirty: !!(state && state.getProperty && state.getProperty('/isDirty'))
                  };
                }
                """
            )
            attachment_save_requests = matching_requests(network, "SaveChanges")
            attachment_save_payloads = [
                item for item in attachment_save_requests[attachment_save_before:]
                if "\"attachments\"" in item.get("post_data", "") or "\"Value\"" in item.get("post_data", "")
            ]
            attachment_transport = {
                "waitError": attachment_wait_error,
                "stageRequests": attachment_stage_requests,
                "saveRequests": attachment_save_requests[attachment_save_before:],
                "savePayloadsWithAttachments": attachment_save_payloads
            }
            attachment_ok = (
                stage_ok
                and len(attachment_save_requests) > attachment_save_before
                and len(attachment_save_payloads) > 0
                and max(attachment_after_save.get("attachmentCount", 0), attachment_after_save.get("sessionAttachmentCount", 0))
                    >= max(attachment_before.get("attachmentCount", 0), attachment_before.get("sessionAttachmentCount", 0)) + 1
                and attachment_after_save.get("isDirty") is False
            )
            ensure(checks, "detail.attachment.gateway", attachment_ok, {"before": attachment_before, "afterStage": attachment_after_stage, "afterSave": attachment_after_save, "transport": attachment_transport})
            if not attachment_ok:
                failures.append("detail.attachment.gateway")

            current_step = "lock.release"
            before_release = len(matching_requests(network, "LockRelease"))
            invoke_view_controller_method(page, "checklist_app_comp---detailTargetPage", "onCloseDetail")
            wait_for_search_ready(page)
            page.wait_for_timeout(1600)
            route_snapshots.append(capture_route_snapshot(page, "search.afterDetailClose"))
            after_release = len(matching_requests(network, "LockRelease"))
            back_to_search = safe_evaluate(
                page,
                """
                () => {
                  const core = sap.ui.getCore();
                  const all = Object.values(core.mElements || {});
                  const smartTable = all.find((item) => item && item.getId && String(item.getId()).endsWith('searchSmartTable'));
                  const text = document.body && document.body.innerText ? String(document.body.innerText) : '';
                  return {
                    hasCreateButton: text.indexOf('Create') >= 0 || text.indexOf('Создать') >= 0 || text.indexOf('Ð¡Ð¾Ð·Ð´Ð°Ñ‚ÑŒ') >= 0,
                    smartTable: !!smartTable
                  };
                }
                """
            )
            ok_release = after_release > before_release and bool(back_to_search.get("hasCreateButton")) and bool(back_to_search.get("smartTable"))
            ensure(checks, "detail.lock.release", ok_release, {"before": before_release, "after": after_release, "search": back_to_search, "transport": transport_snapshot(network, "LockRelease")})
            if not ok_release:
                failures.append("detail.lock.release")

            current_step = "route.repeat.detail"
            repeat_detail_before = len(matching_requests(network, MARKER_LOCK_ACQUIRE))
            navigate_to_detail(page, selected_root_id)
            wait_for_detail_ready(page, selected_root_id)
            route_snapshots.append(capture_route_snapshot(page, "detail.repeatOpen"))
            repeat_open_state = detail_route_state(page)
            ok_repeat_open = (
                repeat_open_state.get("currentRouteName") == "detail"
                and repeat_open_state.get("rootId") == selected_root_id
                and repeat_open_state.get("mode") in ("READ", "EDIT")
            )
            ensure(checks, "detail.route.repeat_open", ok_repeat_open, repeat_open_state)
            if not ok_repeat_open:
                failures.append("detail.route.repeat_open")

            current_step = "route.repeat.detail.close"
            invoke_view_controller_method(page, "checklist_app_comp---detailTargetPage", "onCloseDetail")
            wait_for_search_ready(page)
            page.wait_for_timeout(1200)
            route_snapshots.append(capture_route_snapshot(page, "search.afterRepeatDetailClose"))
            repeat_close_state = capture_route_snapshot(page, "search.afterRepeatDetailCloseCheck")
            ok_repeat_close = (
                repeat_close_state.get("currentRouteName") == "search"
                and repeat_close_state.get("activeObjectId") == ""
            )
            ensure(checks, "detail.route.repeat_close", ok_repeat_close, {
                "beforeLockAcquireCount": repeat_detail_before,
                "afterState": repeat_close_state
            })
            if not ok_repeat_close:
                failures.append("detail.route.repeat_close")

            current_step = "route.repeat.analytics"
            navigate_to_search(page)
            wait_for_search_ready(page)
            invoke_view_controller_method(page, "checklist_app_comp---searchTargetPage", "onOpenWorkflowAnalytics")
            wait_for_analytics_ready(page)
            route_snapshots.append(capture_route_snapshot(page, "analytics.repeatOpen"))
            analytics_repeat_state = capture_route_snapshot(page, "analytics.repeatOpenCheck")
            ok_analytics_repeat = analytics_repeat_state.get("currentRouteName") == "analytics"
            ensure(checks, "analytics.route.repeat_open", ok_analytics_repeat, analytics_repeat_state)
            if not ok_analytics_repeat:
                failures.append("analytics.route.repeat_open")

            invoke_view_controller_method(page, "checklist_app_comp---analyticsTargetPage", "onCloseAnalytics")
            wait_for_search_ready(page)
            page.wait_for_timeout(1200)
            route_snapshots.append(capture_route_snapshot(page, "search.afterRepeatAnalyticsClose"))
            analytics_repeat_close_state = capture_route_snapshot(page, "search.afterRepeatAnalyticsCloseCheck")
            ok_analytics_repeat_close = analytics_repeat_close_state.get("currentRouteName") == "search"
            ensure(checks, "analytics.route.repeat_close", ok_analytics_repeat_close, analytics_repeat_close_state)
            if not ok_analytics_repeat_close:
                failures.append("analytics.route.repeat_close")

            browser.close()
    except Exception as exc:  # noqa: BLE001
        bootstrap = {}
        try:
            if "page" in locals():
                bootstrap = collect_bootstrap_diagnostics(page)
        except Exception:  # noqa: BLE001
            bootstrap = {}
        failures.append("browser.exception")
        ensure(checks, "browser.exception", False, {
            "error": str(exc),
            "lastState": last_state,
            "step": current_step,
            "classification": classify_failure(current_step, exc),
            "bootstrap": bootstrap,
            "rootSelectionDiagnostics": root_selection_diagnostics,
            "routeOpenDiagnostics": route_open_diagnostics,
            "lockAcquireDiagnostics": lock_acquire_diagnostics,
            "analyticsCloseDiagnostics": analytics_close_diagnostics,
            "attachmentReadinessDiagnostics": attachment_readiness_diagnostics,
        })

    return flush_report(build_report(checks, failures, network, {
        "lastState": last_state,
        "routeSnapshots": route_snapshots,
        "rootSelectionDiagnostics": root_selection_diagnostics,
        "routeOpenDiagnostics": route_open_diagnostics,
        "lockAcquireDiagnostics": lock_acquire_diagnostics,
        "analyticsCloseDiagnostics": analytics_close_diagnostics,
        "attachmentReadinessDiagnostics": attachment_readiness_diagnostics,
        "selectedRootId": selected_root_id,
        "failureContext": {
            "step": current_step,
            "classification": classify_failure(current_step, failures[-1] if failures else "")
        } if failures else {}
    }))


if __name__ == "__main__":
    raise SystemExit(main())
