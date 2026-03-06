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


def wait_for_search_ready(page) -> None:
    page.wait_for_selector("#sap_ui5_comp---app--mainSplitter", timeout=30000)
    page.wait_for_selector("#sap_ui5_comp---app--searchPaneHost--searchSmartFilterBar", timeout=30000)
    page.wait_for_selector("#sap_ui5_comp---app--searchPaneHost--searchSmartTable", timeout=30000)
    page.get_by_text("Create", exact=True).wait_for(timeout=30000)
    page.wait_for_timeout(1200)


def wait_for_detail_ready(page) -> None:
    page.wait_for_selector("#sap_ui5_comp---app--detailPaneHost--detailObjectPage", timeout=30000)
    page.wait_for_timeout(1500)


def current_requests(network: list[dict[str, Any]], predicate) -> list[dict[str, Any]]:
    return [item for item in network if predicate(item)]


def direct_requests(network: list[dict[str, Any]], marker: str) -> list[dict[str, Any]]:
    return current_requests(
        network,
        lambda item: marker in item["url"] and "/$batch" not in item["url"],
    )


def batch_requests(network: list[dict[str, Any]], marker: str) -> list[dict[str, Any]]:
    return current_requests(
        network,
        lambda item: "/$batch" in item["url"] and marker in item.get("post_data", ""),
    )


def batch_operation_requests(network: list[dict[str, Any]], method: str, marker: str) -> list[dict[str, Any]]:
    needle = f"{method.upper()} {marker}"
    return current_requests(
        network,
        lambda item: "/$batch" in item["url"] and needle in item.get("post_data", ""),
    )


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
    return page.evaluate(
        """
        () => {
          const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
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


def invoke_detail(page, method_name: str):
    return page.evaluate(
        """
        (methodName) => {
          const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error('Detail controller method not found: ' + methodName);
          }
          return controller[methodName]();
        }
        """,
        method_name,
    )


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
                  const searchView = core.byId('sap_ui5_comp---app--searchPaneHost');
                  return {
                    hasSmartFilterBar: !!core.byId('sap_ui5_comp---app--searchPaneHost--searchSmartFilterBar'),
                    hasSmartTable: !!core.byId('sap_ui5_comp---app--searchPaneHost--searchSmartTable'),
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
            wait_for_detail_ready(page)

            opened = detail_state(page)
            last_state = opened
            ok_open = opened.get("rootId") == ROOT_ID
            ensure(checks, "detail.route.opened", ok_open, opened)
            if not ok_open:
                failures.append("detail.route.opened")

            before_lock = len(current_requests(network, lambda item: "LockAcquire" in item["url"] or "LockAcquire" in item.get("post_data", "")))
            page.locator(".accentSwitchEditMode").click(timeout=15000)
            page.wait_for_function(
                """
                () => {
                  const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
                  const state = view && view.getModel && view.getModel('state');
                  return !!(state && state.getProperty && state.getProperty('/mode') === 'EDIT');
                }
                """,
                timeout=20000,
            )
            page.wait_for_timeout(1600)
            after_lock = len(current_requests(network, lambda item: "LockAcquire" in item["url"] or "LockAcquire" in item.get("post_data", "")))
            edit_state = detail_state(page)
            last_state = edit_state
            ok_lock = after_lock > before_lock and edit_state.get("mode") == "EDIT"
            ensure(checks, "detail.lock.acquire", ok_lock, {"before": before_lock, "after": after_lock, "state": edit_state})
            if not ok_lock:
                failures.append("detail.lock.acquire")
            lock_direct_requests = direct_requests(network, "LockAcquire")
            lock_batch_requests = batch_requests(network, "LockAcquire")
            ok_lock_direct = len(lock_direct_requests) >= 1 and not lock_batch_requests
            ensure(checks, "detail.lock.acquire.direct", ok_lock_direct, {"direct": lock_direct_requests[-3:], "batch": lock_batch_requests[-3:]})
            if not ok_lock_direct:
                failures.append("detail.lock.acquire.direct")

            save_before = detail_state(page)
            save_request_count_before = len(current_requests(network, lambda item: "SaveChanges" in item["url"] or "SaveChanges" in item.get("post_data", "")))
            save_call = page.evaluate(
                """
                () => new Promise((resolve, reject) => {
                  const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
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
                (prevRequestCount) => {
                  const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
                  const state = view && view.getModel && view.getModel('state');
                  const selected = view && view.getModel && view.getModel('selected');
                  const requestCount = window.performance.getEntriesByType('resource')
                    .filter((entry) => String(entry.name || '').indexOf('/sap/opu/odata/sap/Z_UI5_SRV') >= 0)
                    .length;
                  const version = selected && selected.getProperty ? Number(selected.getProperty('/root/version_number') || selected.getProperty('/root/VersionNumber') || 0) : 0;
                  return requestCount >= Number(prevRequestCount || 0) && !!(state && state.getProperty && state.getProperty('/isBusy') === false) && !!version;
                }
                """,
                arg=save_request_count_before,
                timeout=30000,
            )
            page.wait_for_timeout(1600)
            save_after = detail_state(page)
            last_state = save_after
            save_requests = current_requests(network, lambda item: "SaveChanges" in item["url"] or "SaveChanges" in item.get("post_data", ""))
            ok_save = len(save_requests) > save_request_count_before and save_after.get("equipment") == save_call.get("equipment") and save_after.get("version", 0) > save_before.get("version", 0)
            ensure(checks, "detail.save.gateway", ok_save, {"before": save_before, "after": save_after, "requestCount": len(save_requests)})
            if not ok_save:
                failures.append("detail.save.gateway")
            save_direct_requests = direct_requests(network, "SaveChanges")
            save_batch_requests = batch_requests(network, "SaveChanges")
            ok_save_direct = len(save_direct_requests) >= 1 and not save_batch_requests
            ensure(checks, "detail.save.direct", ok_save_direct, {"direct": save_direct_requests[-3:], "batch": save_batch_requests[-3:]})
            if not ok_save_direct:
                failures.append("detail.save.direct")

            autosave_before = detail_state(page)
            autosave_request_count_before = len(current_requests(network, lambda item: "AutoSave" in item["url"] or "AutoSave" in item.get("post_data", "")))
            autosave_call = page.evaluate(
                """
                () => new Promise((resolve, reject) => {
                  sap.ui.require(['sap_ui5/util/DeltaPayloadBuilder'], function (DeltaPayloadBuilder) {
                    const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
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
                  const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
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
            autosave_requests = current_requests(network, lambda item: "AutoSave" in item["url"] or "AutoSave" in item.get("post_data", ""))
            ok_autosave = len(autosave_requests) > autosave_request_count_before and autosave_after.get("version", 0) > autosave_before.get("version", 0) and autosave_after.get("autosaveState") == "SAVED" and autosave_after.get("equipment") == autosave_call.get("equipment")
            ensure(checks, "detail.autosave.gateway", ok_autosave, {"before": autosave_before, "after": autosave_after, "requestCount": len(autosave_requests), "deltaKeys": sorted((autosave_call.get("delta") or {}).keys())})
            if not ok_autosave:
                failures.append("detail.autosave.gateway")
            autosave_direct_requests = direct_requests(network, "AutoSave")
            autosave_batch_requests = batch_requests(network, "AutoSave")
            ok_autosave_direct = len(autosave_direct_requests) >= 1 and not autosave_batch_requests
            ensure(checks, "detail.autosave.direct", ok_autosave_direct, {"direct": autosave_direct_requests[-3:], "batch": autosave_batch_requests[-3:]})
            if not ok_autosave_direct:
                failures.append("detail.autosave.direct")

            before_upload = len(current_requests(network, lambda item: "AttachmentSet" in item["url"] or "AttachmentSet" in item.get("post_data", "") or "/$batch" in item["url"]))
            page.locator("#sap_ui5_comp---app--detailPaneHost--attachmentUploader-fu").set_input_files(str(attachment_file.resolve()))
            page.wait_for_timeout(3200)
            attachment_requests = current_requests(
                network[before_upload:],
                lambda item: "AttachmentSet" in item["url"] or "AttachmentSet" in item.get("post_data", "") or "/$batch" in item["url"],
            )
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
            ensure(checks, "detail.attachment.gateway", has_attachment_post and has_attachment_put, {"requests": attachment_requests})
            if not (has_attachment_post and has_attachment_put):
                failures.append("detail.attachment.gateway")
            attachment_batch_requests = batch_operation_requests(attachment_requests, "POST", "AttachmentSet") + batch_operation_requests(attachment_requests, "PUT", "AttachmentSet")
            ok_attachment_direct = has_attachment_post and has_attachment_put and not attachment_batch_requests
            ensure(checks, "detail.attachment.direct", ok_attachment_direct, {"requests": attachment_requests, "batchedWrites": attachment_batch_requests})
            if not ok_attachment_direct:
                failures.append("detail.attachment.direct")

            before_release = len(current_requests(network, lambda item: "LockRelease" in item["url"] or "LockRelease" in item.get("post_data", "")))
            invoke_detail(page, "onCloseDetail")
            wait_for_search_ready(page)
            page.wait_for_timeout(1600)
            after_release = len(current_requests(network, lambda item: "LockRelease" in item["url"] or "LockRelease" in item.get("post_data", "")))
            back_to_search = page.evaluate(
                """
                () => {
                  return {
                    hasCreateButton: document.body && document.body.innerText.includes('Create'),
                    smartTable: !!sap.ui.getCore().byId('sap_ui5_comp---app--searchPaneHost--searchSmartTable')
                  };
                }
                """
            )
            ok_release = after_release > before_release and bool(back_to_search.get("hasCreateButton")) and bool(back_to_search.get("smartTable"))
            ensure(checks, "detail.lock.release", ok_release, {"before": before_release, "after": after_release, "search": back_to_search})
            if not ok_release:
                failures.append("detail.lock.release")
            release_direct_requests = direct_requests(network, "LockRelease")
            release_batch_requests = batch_requests(network, "LockRelease")
            ok_release_direct = len(release_direct_requests) >= 1 and not release_batch_requests
            ensure(checks, "detail.lock.release.direct", ok_release_direct, {"direct": release_direct_requests[-3:], "batch": release_batch_requests[-3:]})
            if not ok_release_direct:
                failures.append("detail.lock.release.direct")

            browser.close()
    except Exception as exc:  # noqa: BLE001
        failures.append("browser.exception")
        ensure(checks, "browser.exception", False, {"error": str(exc), "lastState": last_state})

    return flush_report(build_report(checks, failures, network, {"lastState": last_state}))


if __name__ == "__main__":
    raise SystemExit(main())
