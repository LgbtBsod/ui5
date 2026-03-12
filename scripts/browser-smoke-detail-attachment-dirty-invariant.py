#!/usr/bin/env python3
"""Browser smoke: attachment upload/delete must not set detail dirty state."""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright


UI_URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
ROOT_ID = sys.argv[2] if len(sys.argv) > 2 else ""
REPORT_PATH = Path("docs/artifacts/gateway-browser-attachment-dirty-report.json")


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
        if "attachment" in step or "attachments" in step:
            return "readiness/wait bug"
        return "tooling bug"
    if "Locator" in message or "selector" in message:
        return "selector bug"
    return "tooling bug"


def matching_requests(network: list[dict[str, Any]], marker: str) -> list[dict[str, Any]]:
    return [
        item
        for item in network
        if marker in item["url"] or marker in item.get("post_data", "")
    ]


def batch_operation_requests(network: list[dict[str, Any]], method: str, marker: str) -> list[dict[str, Any]]:
    needle = f"{method.upper()} {marker}"
    return [
        item
        for item in network
        if "/$batch" in item["url"] and needle in item.get("post_data", "")
    ]


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


def wait_for_search_ready(page) -> None:
    page.wait_for_function(
        """
        () => {
          const core = typeof sap !== 'undefined' && sap.ui && sap.ui.getCore && sap.ui.getCore();
          const app = core && core.byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          return !!core
            && !!core.byId('checklist_app_comp---app--mainFcl')
            && !!core.byId('checklist_app_comp---app--searchPaneHost--searchSmartTable')
            && !!state
            && state.getProperty('/currentRouteName') === 'search';
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(1000)


def detail_state(page) -> dict[str, Any]:
    return safe_evaluate(
        page,
        """
        () => {
          const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
          const selected = view && view.getModel && view.getModel('selected');
          const state = view && view.getModel && view.getModel('state');
          const viewModel = view && view.getModel && view.getModel('view');
          const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
          const sessionAttachments = viewModel && viewModel.getProperty ? (viewModel.getProperty('/sessionAttachments') || []) : [];
          const selectedKeys = Array.isArray(attachments)
            ? attachments.map((item) => String((item && (item.AttachmentKey || item.Key)) || '').trim()).filter(Boolean)
            : [];
          const sessionKeys = Array.isArray(sessionAttachments)
            ? sessionAttachments.map((item) => String((item && (item.AttachmentKey || item.Key)) || '').trim()).filter(Boolean)
            : [];
          return {
            rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            mode: state && state.getProperty ? String(state.getProperty('/workflow/detail/editMode') || '') : '',
            lockState: state && state.getProperty ? String(state.getProperty('/workflow/detail/lock/state') || '') : '',
            isDirty: !!(state && state.getProperty && state.getProperty('/isDirty')),
            attachmentCount: Array.isArray(attachments) ? attachments.length : 0,
            sessionAttachmentCount: Array.isArray(sessionAttachments) ? sessionAttachments.length : 0,
            attachmentKeys: selectedKeys,
            sessionAttachmentKeys: sessionKeys,
            allAttachmentKeys: selectedKeys.concat(sessionKeys.filter((key) => selectedKeys.indexOf(key) < 0))
          };
        }
        """
    )


def invoke_controller_method(page, view_id: str, method_name: str, *args: Any) -> Any:
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


def invoke_delete(page, attachment_key: str) -> None:
    safe_evaluate(
        page,
        """
        (attachmentKey) => new Promise((resolve, reject) => {
          const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
          const controller = view && view.getController && view.getController();
          const selected = view && view.getModel && view.getModel('selected');
          const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
          const rootId = selected && selected.getProperty ? String(selected.getProperty('/root/id') || '').trim() : '';
          const attachment = attachments.find((item) => String((item && (item.AttachmentKey || item.Key)) || '').trim() === attachmentKey);
          if (!controller || typeof controller._run !== 'function') {
            reject(new Error('detail controller unavailable'));
            return;
          }
          if (!attachment || !rootId) {
            reject(new Error('attachment to delete not found'));
            return;
          }
          Promise.resolve(controller._run('attachmentDelete', {
            rootId,
            attachmentId: attachmentKey,
            attachment
          })).then(() => resolve(true)).catch(reject);
        })
        """,
        attachment_key,
    )



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
          return expanded && uploaderReady && (historyLoaded || uploaderReady);
        }
        """,
        timeout=30000,
    )
    page.locator("#checklist_app_comp---app--detailPaneHost--attachmentUploader-fu").wait_for(timeout=10000)
    page.wait_for_function(
        """
        () => {
          const control = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost--attachmentUploader');
          const state = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost')?.getModel?.('state');
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
def main() -> int:
    if not ROOT_ID:
        print(json.dumps({"ok": False, "error": "ROOT_ID is required"}, ensure_ascii=False))
        return 2

    checks: list[dict[str, Any]] = []
    failures: list[str] = []
    network: list[dict[str, Any]] = []
    last_state: dict[str, Any] = {}
    current_step = "startup"
    attachment_file = Path("docs/runtime/gateway-dirty-invariant-attachment.txt")
    attachment_file.parent.mkdir(parents=True, exist_ok=True)
    attachment_file.write_text("attachment dirty invariant payload", encoding="utf-8")

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

            current_step = "route.open.detail"
            page.goto(f"{UI_URL}#/checklist/{ROOT_ID}", wait_until="networkidle", timeout=90000)
            wait_for_detail_ready(page, ROOT_ID)

            current_step = "lock.acquire"
            before_lock = len(matching_requests(network, "LockAcquire"))
            edit_ok, edit_detail = enter_edit_or_report(page)
            after_lock = len(matching_requests(network, "LockAcquire"))
            edit_state = edit_detail.get("state") or detail_state(page)
            last_state = edit_state
            ok_lock = edit_ok and after_lock > before_lock and edit_state.get("mode") == "EDIT" and edit_state.get("isDirty") is False
            ensure(checks, "detail.attachment_dirty.lock_acquired_clean", ok_lock, {
                "before": before_lock,
                "after": after_lock,
                "state": edit_state,
                "toggleResult": edit_detail.get("toggleResult"),
                "error": edit_detail.get("error", ""),
            })
            if not ok_lock:
                failures.append("detail.attachment_dirty.lock_acquired_clean")
                browser.close()
                return flush_report(report={
                    "generatedAt": int(time.time()),
                    "uiUrl": UI_URL,
                    "rootId": ROOT_ID,
                    "ok": False,
                    "checks": checks,
                    "failures": failures,
                    "networkSample": network[-25:],
                    "lastState": last_state,
                })

            current_step = "attachments.expand"
            ensure_attachments_expanded(page)
            current_step = "attachments.upload"
            upload_before = detail_state(page)
            upload_request_index = len(network)
            page.locator("#checklist_app_comp---app--detailPaneHost--attachmentUploader-fu").set_input_files(str(attachment_file.resolve()))
            page.wait_for_function(
                """
                (prevCount) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
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
                arg=upload_before.get("attachmentCount") or 0,
                timeout=10000,
            )
            page.wait_for_function(
                """
                (prevCount) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
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
                    && !!(state && state.getProperty && state.getProperty('/isDirty') === false);
                }
                """,
                arg=upload_before.get("attachmentCount") or 0,
                timeout=30000,
            )
            page.wait_for_function(
                """
                () => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
                  const viewModel = view && view.getModel && view.getModel('view');
                  return !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentBusy') === false);
                }
                """,
                timeout=10000,
            )
            page.wait_for_timeout(1500)
            upload_after = detail_state(page)
            last_state = upload_after
            upload_requests = [
                item
                for item in network[upload_request_index:]
                if "AttachmentSet" in item["url"] or "AttachmentSet" in item.get("post_data", "") or "/$batch" in item["url"]
            ]
            has_upload_post = any(
                item["method"] == "POST"
                and (
                    "/AttachmentSet" in item["url"]
                    or ("multipart/mixed" in str(item["headers"].get("content-type", "")) and "POST AttachmentSet" in item.get("post_data", ""))
                )
                for item in upload_requests
            )
            has_upload_put = any(
                item["method"] == "PUT"
                and "/AttachmentSet(Key='" in item["url"]
                and "/$value" in item["url"]
                for item in upload_requests
            )
            uploaded_keys = [key for key in upload_after.get("allAttachmentKeys", []) if key not in (upload_before.get("allAttachmentKeys", []) or [])]
            uploaded_key = uploaded_keys[-1] if uploaded_keys else ""
            ok_upload = (
                has_upload_post
                and has_upload_put
                and max(upload_after.get("attachmentCount", 0), upload_after.get("sessionAttachmentCount", 0))
                    == max(upload_before.get("attachmentCount", 0), upload_before.get("sessionAttachmentCount", 0)) + 1
                and upload_after.get("isDirty") is False
                and bool(uploaded_key)
            )
            ensure(
                checks,
                "detail.attachment_upload_keeps_clean_state",
                ok_upload,
                {
                    "before": upload_before,
                    "after": upload_after,
                    "uploadedKey": uploaded_key,
                    "requestCount": len(upload_requests),
                    "batchedWrites": {
                        "post": batch_operation_requests(upload_requests, "POST", "AttachmentSet"),
                        "put": batch_operation_requests(upload_requests, "PUT", "AttachmentSet"),
                    },
                },
            )
            if not ok_upload:
                failures.append("detail.attachment_upload_keeps_clean_state")

            current_step = "attachments.delete"
            delete_before = detail_state(page)
            delete_request_index = len(network)
            invoke_delete(page, uploaded_key)
            page.wait_for_function(
                """
                (payload) => {
                  const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
                  const selected = view && view.getModel && view.getModel('selected');
                  const state = view && view.getModel && view.getModel('state');
                  const viewModel = view && view.getModel && view.getModel('view');
                  const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
                  const sessionAttachments = viewModel && viewModel.getProperty ? (viewModel.getProperty('/sessionAttachments') || []) : [];
                  const combined = (Array.isArray(attachments) ? attachments : []).concat(
                    (Array.isArray(sessionAttachments) ? sessionAttachments : []).filter((item) => {
                      const sKey = String((item && (item.AttachmentKey || item.Key)) || '').trim();
                      return !(Array.isArray(attachments) && attachments.some((entry) => String((entry && (entry.AttachmentKey || entry.Key)) || '').trim() === sKey));
                    })
                  );
                  const attachmentGone = Array.isArray(combined)
                    && !combined.some((item) => String((item && (item.AttachmentKey || item.Key)) || '').trim() === String(payload.key || ''));
                  return Array.isArray(combined)
                    && combined.length < Number(payload.prevCount || 0)
                    && attachmentGone
                    && !!(state && state.getProperty && state.getProperty('/isDirty') === false);
                }
                """,
                arg={"prevCount": max(delete_before.get("attachmentCount") or 0, delete_before.get("sessionAttachmentCount") or 0), "key": uploaded_key},
                timeout=30000,
            )
            page.wait_for_timeout(1500)
            delete_after = detail_state(page)
            last_state = delete_after
            delete_requests = [
                item
                for item in network[delete_request_index:]
                if ("AttachmentSet(Key='" in item["url"] and item["method"] == "DELETE") or "DELETE AttachmentSet" in item.get("post_data", "")
            ]
            ok_delete = (
                len(delete_requests) > 0
                and max(delete_after.get("attachmentCount", 0), delete_after.get("sessionAttachmentCount", 0))
                    == max(0, max(delete_before.get("attachmentCount", 0), delete_before.get("sessionAttachmentCount", 0)) - 1)
                and delete_after.get("isDirty") is False
                and uploaded_key not in (delete_after.get("allAttachmentKeys", []) or [])
            )
            ensure(
                checks,
                "detail.attachment_delete_keeps_clean_state",
                ok_delete,
                {
                    "before": delete_before,
                    "after": delete_after,
                    "deletedKey": uploaded_key,
                    "requestCount": len(delete_requests),
                    "batchedWrites": batch_operation_requests(delete_requests, "DELETE", "AttachmentSet"),
                },
            )
            if not ok_delete:
                failures.append("detail.attachment_delete_keeps_clean_state")

            current_step = "lock.release"
            before_release = len(matching_requests(network, "LockRelease"))
            invoke_controller_method(page, "checklist_app_comp---app--detailPaneHost", "onCloseDetail")
            wait_for_search_ready(page)
            page.wait_for_timeout(1200)
            after_release = len(matching_requests(network, "LockRelease"))
            ok_release = after_release > before_release
            ensure(checks, "detail.attachment_dirty.lock_release", ok_release, {"before": before_release, "after": after_release})
            if not ok_release:
                failures.append("detail.attachment_dirty.lock_release")

            browser.close()
    except Exception as exc:  # noqa: BLE001
        failures.append("browser.exception")
        ensure(checks, "browser.exception", False, {
            "error": str(exc),
            "lastState": last_state,
            "step": current_step,
            "classification": classify_failure(current_step, exc)
        })

    report = {
        "generatedAt": int(time.time()),
        "uiUrl": UI_URL,
        "rootId": ROOT_ID,
        "ok": not failures,
        "checks": checks,
        "failures": failures,
        "networkSample": network[-25:],
        "lastState": last_state,
        "failureContext": {
            "step": current_step,
            "classification": classify_failure(current_step, failures[-1] if failures else "")
        } if failures else {},
    }
    return flush_report(report)


if __name__ == "__main__":
    raise SystemExit(main())
