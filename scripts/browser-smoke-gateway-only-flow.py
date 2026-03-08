#!/usr/bin/env python3
"""Browser smoke: Gateway-only Smart/OData runtime flow."""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright


UI_URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
ROOT_ID = sys.argv[2] if len(sys.argv) > 2 else ""
REPORT_PATH = Path("docs/artifacts/gateway-browser-smoke-report.json")


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


def wait_for_search_ready(page) -> None:
    page.wait_for_function(
        """
        () => {
          if (typeof sap === 'undefined' || !sap.ui || !sap.ui.getCore) {
            return false;
          }
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          const fcl = core.byId('checklist_app_comp---app--mainFcl');
          const smartFilterBar = core.byId('checklist_app_comp---app--searchPaneHost--searchSmartFilterBar');
          const smartTable = core.byId('checklist_app_comp---app--searchPaneHost--searchSmartTable');
          const appReady = document.documentElement.getAttribute('data-ui5-app-ready') === 'true';
          return !!fcl
            && !!smartFilterBar
            && !!smartTable
            && !!state
            && state.getProperty('/currentRouteName') === 'search'
            && appReady;
        }
        """,
        timeout=30000,
    )
    page.get_by_text("Create", exact=True).wait_for(timeout=30000)
    page.wait_for_timeout(1200)


def wait_for_detail_ready(page, root_id: str) -> None:
    page.wait_for_function(
        """
        (expectedRootId) => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---app--detailPaneHost');
          const objectPage = core.byId('checklist_app_comp---app--detailPaneHost--detailObjectPage');
          const selected = view && view.getModel && view.getModel('selected');
          const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '';
          return !!view && !!objectPage && rootId === expectedRootId;
        }
        """,
        arg=root_id,
        timeout=30000,
    )
    page.wait_for_timeout(1500)


def wait_for_analytics_ready(page) -> None:
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          const analyticsView = core.byId('checklist_app_comp---app--analyticsPaneHost');
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


def flush_report(report: dict[str, Any]) -> int:
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
    return 1 if report.get("failures") else 0


def detail_state(page) -> dict[str, Any]:
    return safe_evaluate(
        page,
        """
        () => {
          const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
          const selected = view && view.getModel && view.getModel('selected');
          const state = view && view.getModel && view.getModel('state');
          return {
            rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            version: selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0,
            equipment: selected && selected.getProperty ? String(selected.getProperty('/basic/equipment') || '') : '',
            mode: state && state.getProperty ? String(state.getProperty('/mode') || '') : '',
            lockState: state && state.getProperty ? String(state.getProperty('/lockOperationState') || '') : '',
            autosaveState: state && state.getProperty ? String(state.getProperty('/autosaveState') || '') : ''
          };
        }
        """
    )


def invoke_view_controller_method(page, view_id: str, method_name: str, *args: Any):
    return safe_evaluate(
        page,
        """
        ({ viewId, methodName, args }) => {
          const view = sap.ui.getCore().byId(viewId);
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error('Controller method not found: ' + viewId + ':' + methodName);
          }
          return Promise.resolve(controller[methodName].apply(controller, args || []));
        }
        """,
        {"viewId": view_id, "methodName": method_name, "args": list(args)},
    )


def set_detail_edit_mode(page, state: bool) -> Any:
    return safe_evaluate(
        page,
        """
        (targetState) => {
          const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
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
              const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
              const state = view && view.getModel && view.getModel('state');
              return !!(state && state.getProperty && state.getProperty('/mode') === 'EDIT');
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
          const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
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
          const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
          const viewModel = view && view.getModel && view.getModel('view');
          const expanded = !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentsExpanded'));
          const historyLoaded = !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentsLoaded'));
          const uploaderReady = !!document.querySelector('#checklist_app_comp---app--detailPaneHost--attachmentUploader-fu');
          return expanded && (historyLoaded || uploaderReady);
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(1200)
def main() -> int:
    if not ROOT_ID:
        print(json.dumps({"ok": False, "error": "ROOT_ID is required"}, ensure_ascii=False))
        return 2

    network: list[dict[str, Any]] = []
    checks: list[dict[str, Any]] = []
    failures: list[str] = []
    last_state: dict[str, Any] = {}
    attachment_file = Path("docs/runtime/gateway-smoke-attachment.txt")
    attachment_file.parent.mkdir(parents=True, exist_ok=True)
    attachment_file.write_text("gateway browser smoke attachment payload", encoding="utf-8")

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch()
            page = browser.new_page(viewport={"width": 1440, "height": 960})

            def on_request(req) -> None:
                if "/sap/opu/odata/sap/Z_UI5_SRV" not in req.url:
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

            page.goto(UI_URL, wait_until="networkidle", timeout=90000)
            wait_for_search_ready(page)

            smart_controls = page.evaluate(
                """
                () => {
                  const core = sap.ui.getCore();
                  const searchView = core.byId('checklist_app_comp---app--searchPaneHost');
                  return {
                    hasSmartFilterBar: !!core.byId('checklist_app_comp---app--searchPaneHost--searchSmartFilterBar'),
                    hasSmartTable: !!core.byId('checklist_app_comp---app--searchPaneHost--searchSmartTable'),
                    searchVisible: !!(searchView && searchView.getDomRef && searchView.getDomRef())
                  };
                }
                """
            )
            ok_smart = bool(smart_controls.get("hasSmartFilterBar")) and bool(smart_controls.get("hasSmartTable")) and bool(smart_controls.get("searchVisible"))
            ensure(checks, "search.smart.gateway.controls", ok_smart, smart_controls)
            if not ok_smart:
                failures.append("search.smart.gateway.controls")

            page.goto(f"{UI_URL}#/checklist/{ROOT_ID}", wait_until="networkidle", timeout=90000)
            wait_for_detail_ready(page, ROOT_ID)

            opened = detail_state(page)
            last_state = opened
            ok_open = opened.get("rootId") == ROOT_ID
            ensure(checks, "detail.route.opened", ok_open, opened)
            if not ok_open:
                failures.append("detail.route.opened")

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

            save_before = detail_state(page)
            save_request_count_before = len(matching_requests(network, "SaveChanges"))
            save_call = safe_evaluate(
                page,
                """
                () => new Promise((resolve, reject) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
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
                  const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
                  const state = view && view.getModel && view.getModel('state');
                  const selected = view && view.getModel && view.getModel('selected');
                  const version = selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0;
                  return version > Number(prevVersion || 0) && !!(state && state.getProperty && state.getProperty('/isBusy') === false);
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

            autosave_before = detail_state(page)
            autosave_request_count_before = len(matching_requests(network, "AutoSave"))
            autosave_call = safe_evaluate(
                page,
                """
                () => new Promise((resolve, reject) => {
                  sap.ui.require(['checklist/app/util/DeltaPayloadBuilder'], function (DeltaPayloadBuilder) {
                    const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
                    const controller = view && view.getController && view.getController();
                    const selected = view && view.getModel && view.getModel('selected');
                    const uiState = view && view.getModel && view.getModel('uiState');
                    const state = view && view.getModel && view.getModel('state');
                    if (!controller || !selected || !uiState || !state) {
                      reject(new Error('detail controller/models unavailable'));
                      return;
                    }
                    const sRootId = String(selected.getProperty('/root/id') || '').trim();
                    const sValue = 'Gateway browser autosave ' + Date.now();
                    selected.setProperty('/basic/equipment', sValue);
                    state.setProperty('/isDirty', true);
                    const current = selected.getProperty('/') || {};
                    const snapshot = uiState.getProperty('/_detailSnapshot') || {};
                    const delta = DeltaPayloadBuilder.buildDeltaPayload(current, snapshot);
                    if (!delta) {
                      reject(new Error('autosave delta is empty'));
                      return;
                    }
                    Promise.resolve(controller._run('autosave', { rootId: sRootId, delta: delta }))
                      .then(function () { resolve({ equipment: sValue, delta: delta }); })
                      .catch(reject);
                  }, reject);
                })
                """
            )
            page.wait_for_function(
                """
                (prevVersion) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
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
            page.wait_for_timeout(1200)
            autosave_after = detail_state(page)
            last_state = autosave_after
            autosave_requests = matching_requests(network, "AutoSave")
            ok_autosave = len(autosave_requests) > autosave_request_count_before and autosave_after.get("version", 0) > autosave_before.get("version", 0) and autosave_after.get("autosaveState") == "SAVED" and autosave_after.get("equipment") == autosave_call.get("equipment")
            ensure(checks, "detail.autosave.gateway", ok_autosave, {"before": autosave_before, "after": autosave_after, "requestCount": len(autosave_requests), "deltaKeys": sorted((autosave_call.get("delta") or {}).keys()), "transport": transport_snapshot(network, "AutoSave")})
            if not ok_autosave:
                failures.append("detail.autosave.gateway")

            analytics_request_before = len(
                matching_requests(network, "SimpleAnalyticalSet")
            ) + len(
                matching_requests(network, "WorkflowAnalyticsBreakdownSet")
            )
            invoke_view_controller_method(page, "checklist_app_comp---app--detailPaneHost", "onOpenWorkflowAnalytics")
            wait_for_analytics_ready(page)
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
                  const analyticsView = core.byId('checklist_app_comp---app--analyticsPaneHost');
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
            invoke_view_controller_method(page, "checklist_app_comp---app--analyticsPaneHost", "onCloseAnalytics")
            wait_for_detail_ready(page, ROOT_ID)

            ensure_attachments_expanded(page)
            before_upload = len([item for item in network if "AttachmentSet" in item["url"] or "AttachmentSet" in item.get("post_data", "") or "/" in item["url"]])
            page.locator("#checklist_app_comp---app--detailPaneHost--attachmentUploader-fu").set_input_files(str(attachment_file.resolve()))
            page.wait_for_timeout(3200)
            attachment_requests = [
                item
                for item in network[before_upload:]
                if "AttachmentSet" in item["url"] or "AttachmentSet" in item.get("post_data", "") or "/$batch" in item["url"]
            ]
            has_attachment_post = any(
                item["method"] == "POST"
                and (
                    "/AttachmentSet" in item["url"]
                    or ("multipart/mixed" in str(item["headers"].get("content-type", "")) and "POST AttachmentSet" in item.get("post_data", ""))
                )
                for item in attachment_requests
            )
            has_attachment_put = any(
                item["method"] == "PUT"
                and "/AttachmentSet(Key='" in item["url"]
                and "/$value" in item["url"]
                for item in attachment_requests
            )
            attachment_transport = {
                "postBatched": batch_operation_requests(attachment_requests, "POST", "AttachmentSet"),
                "putBatched": batch_operation_requests(attachment_requests, "PUT", "AttachmentSet"),
            }
            ensure(checks, "detail.attachment.gateway", has_attachment_post and has_attachment_put, {"requests": attachment_requests, "transport": attachment_transport})
            if not (has_attachment_post and has_attachment_put):
                failures.append("detail.attachment.gateway")

            before_release = len(matching_requests(network, "LockRelease"))
            invoke_view_controller_method(page, "checklist_app_comp---app--detailPaneHost", "onCloseDetail")
            wait_for_search_ready(page)
            page.wait_for_timeout(1600)
            after_release = len(matching_requests(network, "LockRelease"))
            back_to_search = safe_evaluate(
                page,
                """
                () => {
                  return {
                    hasCreateButton: document.body && document.body.innerText.includes('Create'),
                    smartTable: !!sap.ui.getCore().byId('checklist_app_comp---app--searchPaneHost--searchSmartTable')
                  };
                }
                """
            )
            ok_release = after_release > before_release and bool(back_to_search.get("hasCreateButton")) and bool(back_to_search.get("smartTable"))
            ensure(checks, "detail.lock.release", ok_release, {"before": before_release, "after": after_release, "search": back_to_search, "transport": transport_snapshot(network, "LockRelease")})
            if not ok_release:
                failures.append("detail.lock.release")

            browser.close()
    except Exception as exc:  # noqa: BLE001
        failures.append("browser.exception")
        ensure(checks, "browser.exception", False, {"error": str(exc), "lastState": last_state})

    return flush_report(build_report(checks, failures, network, {"lastState": last_state}))


if __name__ == "__main__":
    raise SystemExit(main())
