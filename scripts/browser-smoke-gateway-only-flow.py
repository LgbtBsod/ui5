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


def ensure(checks: list[dict[str, Any]], name: str, ok: bool, detail: Any) -> None:
    checks.append({"name": name, "ok": bool(ok), "detail": detail})


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
    page.get_by_text("Create", exact=True).wait_for(timeout=30000)
    page.wait_for_timeout(1200)


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
            autosaveState: state && state.getProperty ? String(state.getProperty('/autosaveState') || '') : '',
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
            detailAutosaveState: detailState && detailState.getProperty ? String(detailState.getProperty('/autosaveState') || '') : '',
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
    if "Execution context was destroyed" in message or "Cannot find context with specified id" in message:
        return "page/context lifecycle bug"
    if "Timeout" in message:
        if "analytics" in step or "attachment" in step:
            return "readiness/wait bug"
        return "tooling bug"
    if "Locator" in message or "selector" in message:
        return "selector bug"
    return "tooling bug"


def detail_state(page) -> dict[str, Any]:
    return safe_evaluate(
        page,
        """
        () => {
          const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
          const selected = view && view.getModel && view.getModel('selected');
          const state = view && view.getModel && view.getModel('state');
          return {
            rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            version: selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0,
            equipment: selected && selected.getProperty ? String(selected.getProperty('/basic/equipment') || '') : '',
            mode: state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '',
            lockState: state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : '',
            autosaveState: state && state.getProperty ? String(state.getProperty('/autosaveState') || '') : ''
          };
        }
        """
    )


def invoke_view_controller_method(page, view_id: str, method_name: str, *args: Any):
    controller_name = "PRODUCTION_CONTROL_CHECKLIST.controller.Detail"
    if "analyticsTargetPage" in view_id:
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
          return !!control
            && control.getEnabled && control.getEnabled()
            && !!state
            && state.getProperty('/workflow/detail/editMode') === 'EDIT'
            && state.getProperty('/workflow/detail/lock/state') === 'EDIT_LOCKED';
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
    if not ROOT_ID:
        print(json.dumps({"ok": False, "error": "ROOT_ID is required"}, ensure_ascii=False))
        return 2

    network: list[dict[str, Any]] = []
    checks: list[dict[str, Any]] = []
    failures: list[str] = []
    last_state: dict[str, Any] = {}
    route_snapshots: list[dict[str, Any]] = []
    current_step = "startup"
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
                  const all = Object.values(core.mElements || {});
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

            current_step = "route.open.detail"
            navigate_to_detail(page, ROOT_ID)
            wait_for_detail_ready(page, ROOT_ID)
            route_snapshots.append(capture_route_snapshot(page, "detail.initial"))

            opened = detail_state(page)
            last_state = opened
            ok_open = opened.get("rootId") == ROOT_ID
            ensure(checks, "detail.route.opened", ok_open, opened)
            if not ok_open:
                failures.append("detail.route.opened")

            current_step = "lock.acquire"
            before_lock = len(matching_requests(network, "LockAcquire"))
            edit_ok, edit_detail = enter_edit_or_report(page)
            after_lock = len(matching_requests(network, "LockAcquire"))
            edit_state = edit_detail.get("state") or detail_state(page)
            last_state = edit_state
            ok_lock = edit_ok and after_lock > before_lock and edit_state.get("mode") == "EDIT"
            ensure(checks, "detail.lock.acquire", ok_lock, {
                "before": before_lock,
                "after": after_lock,
                "state": edit_state,
                "toggleResult": edit_detail.get("toggleResult"),
                "transport": transport_snapshot(network, "LockAcquire"),
                "error": edit_detail.get("error", ""),
            })
            if not ok_lock:
                failures.append("detail.lock.acquire")
                browser.close()
                return flush_report(build_report(checks, failures, network, {"lastState": last_state}))

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
                  const state = detail && detail.getModel && detail.getModel('state');
                  const selected = detail && detail.getModel && detail.getModel('selected');
                  return !!app
                    && !!detail
                    && !!selected
                    && !!state
                    && state.getProperty('/currentRouteName') === 'detail'
                    && state.getProperty('/workflow/detail/editMode') === 'EDIT'
                    && state.getProperty('/workflow/detail/lock/state') === 'EDIT_LOCKED'
                    && state.getProperty('/ui/busy/detail') === false
                    && state.getProperty('/saveInFlight') === false;
                }
                """,
                timeout=10000,
            )
            autosave_before = detail_state(page)
            autosave_request_count_before = len(matching_requests(network, "AutoSave"))
            autosave_expected_equipment = "Gateway browser autosave " + str(int(time.time() * 1000))
            safe_evaluate(
                page,
                """
                (expectedEquipment) => {
                  sap.ui.require(['PRODUCTION_CONTROL_CHECKLIST/util/DeltaPayloadBuilder'], function (DeltaPayloadBuilder) {
                    const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                    const controller = view && view.getController && view.getController();
                    const selected = view && view.getModel && view.getModel('selected');
                    const uiState = view && view.getModel && view.getModel('uiState');
                    const state = view && view.getModel && view.getModel('state');
                    if (!controller || !selected || !uiState || !state) {
                      window.__gatewaySmokeAutosave = { started: false, ok: false, error: 'detail controller/models unavailable' };
                      return;
                    }
                    const sRootId = String(selected.getProperty('/root/id') || '').trim();
                    const sValue = String(expectedEquipment || ('Gateway browser autosave ' + Date.now()));
                    selected.setProperty('/basic/equipment', sValue);
                    state.setProperty('/isDirty', true);
                    const current = selected.getProperty('/') || {};
                    const snapshot = uiState.getProperty('/_detailSnapshot') || {};
                    const delta = DeltaPayloadBuilder.buildDeltaPayload(current, snapshot);
                    if (!delta) {
                      window.__gatewaySmokeAutosave = { started: false, ok: false, equipment: sValue, error: 'autosave delta is empty' };
                      return;
                    }
                    window.__gatewaySmokeAutosave = { started: true, ok: false, equipment: sValue, deltaKeys: Object.keys(delta), rootId: sRootId };
                    Promise.resolve(controller.executeFacadeMethod(controller._facade, 'autosave', { rootId: sRootId, delta: delta }, controller._ctx || {}))
                      .then(function () { window.__gatewaySmokeAutosave = { started: true, ok: true, equipment: sValue, deltaKeys: Object.keys(delta) }; })
                      .catch(function (err) { window.__gatewaySmokeAutosave = { started: true, ok: false, equipment: sValue, deltaKeys: Object.keys(delta), error: String((err && err.message) || err || 'autosave failed') }; });
                  }, function (err) {
                    window.__gatewaySmokeAutosave = { started: false, ok: false, error: String((err && err.message) || err || 'module load failed') };
                  });
                  return true;
                }
                """,
                autosave_expected_equipment
            )
            page.wait_for_function(
                """
                (prevVersion) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---detailTargetPage');
                  const selected = view && view.getModel && view.getModel('selected');
                  const state = view && view.getModel && view.getModel('state');
                  const version = selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0;
                  const autosaveState = state && state.getProperty ? String(state.getProperty('/autosaveState') || '') : '';
                  return version > Number(prevVersion || 0) && autosaveState === 'SAVED';
                }
                """,
                arg=autosave_before.get("version") or 0,
                timeout=30000,
            )
            page.wait_for_function(
                "() => !!(window.__gatewaySmokeAutosave && window.__gatewaySmokeAutosave.started)",
                timeout=10000,
            )
            page.wait_for_timeout(1200)
            autosave_after = detail_state(page)
            last_state = autosave_after
            autosave_requests = matching_requests(network, "AutoSave")
            autosave_status = safe_evaluate(page, "() => window.__gatewaySmokeAutosave || {}")
            ok_autosave = len(autosave_requests) > autosave_request_count_before and autosave_after.get("version", 0) > autosave_before.get("version", 0) and autosave_after.get("autosaveState") == "SAVED" and autosave_after.get("equipment") == autosave_expected_equipment and bool(autosave_status.get("ok"))
            ensure(checks, "detail.autosave.gateway", ok_autosave, {"before": autosave_before, "after": autosave_after, "requestCount": len(autosave_requests), "expectedEquipment": autosave_expected_equipment, "deltaKeys": sorted((autosave_status.get("deltaKeys") or [])), "autosaveStatus": autosave_status, "transport": transport_snapshot(network, "AutoSave")})
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
            wait_for_detail_ready(page, ROOT_ID)
            wait_for_edit_detail_ready(page, ROOT_ID)
            analytics_return_state = detail_route_state(page)
            route_snapshots.append(capture_route_snapshot(page, "detail.afterAnalyticsClose"))
            ok_analytics_return = (
                analytics_return_state.get("currentRouteName") == "detail"
                and analytics_return_state.get("rootId") == ROOT_ID
                and analytics_return_state.get("mode") == "EDIT"
                and analytics_return_state.get("lockState") == "EDIT_LOCKED"
            )
            ensure(checks, "analytics.close.gateway", ok_analytics_return, analytics_return_state)
            if not ok_analytics_return:
                failures.append("analytics.close.gateway")

            current_step = "attachments.expand"
            uploader_selector = ensure_attachments_expanded(page)
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
            page.locator(uploader_selector).set_input_files(str(attachment_file.resolve()))
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
                arg=max(attachment_before.get("attachmentCount") or 0, attachment_before.get("sessionAttachmentCount") or 0),
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
                  return nextCount > Number(prevCount || 0)
                    && !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentBusy') === false)
                    && !!(state && state.getProperty && state.getProperty('/isDirty') === true);
                }
                """,
                arg=max(attachment_before.get("attachmentCount") or 0, attachment_before.get("sessionAttachmentCount") or 0),
                timeout=30000,
            )
            page.wait_for_timeout(1200)
            attachment_stage_requests = [
                item
                for item in network[before_upload:]
                if any(marker in item["url"] or marker in item.get("post_data", "") for marker in ["AttachmentSet", "SaveChanges", "CreateChecklist", "AutoSave"])
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
                len(attachment_stage_requests) == 0
                and max(attachment_after_stage.get("attachmentCount", 0), attachment_after_stage.get("sessionAttachmentCount", 0))
                    == max(attachment_before.get("attachmentCount", 0), attachment_before.get("sessionAttachmentCount", 0)) + 1
                and attachment_after_stage.get("isDirty") is True
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
                  return {
                    hasCreateButton: document.body && document.body.innerText.includes('Create'),
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
            repeat_detail_before = len(matching_requests(network, "LockAcquire"))
            navigate_to_detail(page, ROOT_ID)
            wait_for_detail_ready(page, ROOT_ID)
            route_snapshots.append(capture_route_snapshot(page, "detail.repeatOpen"))
            repeat_open_state = detail_route_state(page)
            ok_repeat_open = (
                repeat_open_state.get("currentRouteName") == "detail"
                and repeat_open_state.get("rootId") == ROOT_ID
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
            "bootstrap": bootstrap
        })

    return flush_report(build_report(checks, failures, network, {
        "lastState": last_state,
        "routeSnapshots": route_snapshots,
        "failureContext": {
            "step": current_step,
            "classification": classify_failure(current_step, failures[-1] if failures else "")
        } if failures else {}
    }))


if __name__ == "__main__":
    raise SystemExit(main())
