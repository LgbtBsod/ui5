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


def flush_report(report: dict[str, Any]) -> int:
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
    return 1 if report.get("failures") else 0


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
          const view = core.byId('sap_ui5_comp---app--detailPaneHost');
          const objectPage = core.byId('sap_ui5_comp---app--detailPaneHost--detailObjectPage');
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
          const app = core && core.byId('sap_ui5_comp---app');
          const state = app && app.getModel && app.getModel('state');
          return !!core
            && !!core.byId('sap_ui5_comp---app--mainFcl')
            && !!core.byId('sap_ui5_comp---app--searchPaneHost--searchSmartTable')
            && !!state
            && state.getProperty('/currentRouteName') === 'search';
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(1000)


def detail_state(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
          const selected = view && view.getModel && view.getModel('selected');
          const state = view && view.getModel && view.getModel('state');
          const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
          return {
            rootId: selected && selected.getProperty ? String(selected.getProperty('/root/id') || '') : '',
            mode: state && state.getProperty ? String(state.getProperty('/mode') || '') : '',
            lockState: state && state.getProperty ? String(state.getProperty('/lockOperationState') || '') : '',
            isDirty: !!(state && state.getProperty && state.getProperty('/isDirty')),
            attachmentCount: Array.isArray(attachments) ? attachments.length : 0,
            attachmentKeys: Array.isArray(attachments)
              ? attachments.map((item) => String((item && (item.AttachmentKey || item.Key)) || '').trim()).filter(Boolean)
              : []
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
            throw new Error('Controller method not found: ' + viewId + ':' + methodName);
          }
          return Promise.resolve(controller[methodName].apply(controller, args || []));
        }
        """,
        {"viewId": view_id, "methodName": method_name, "args": list(args)},
    )


def set_detail_edit_mode(page, state: bool) -> Any:
    return page.evaluate(
        """
        (targetState) => {
          const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
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


def invoke_delete(page, attachment_key: str) -> None:
    page.evaluate(
        """
        (attachmentKey) => new Promise((resolve, reject) => {
          const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
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
    page.evaluate(
        """
        () => {
          const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
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
          const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
          const selected = view && view.getModel && view.getModel('selected');
          const viewModel = view && view.getModel && view.getModel('view');
          const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
          return !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentsExpanded'))
            && !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentsLoaded'))
            && Array.isArray(attachments);
        }
        """,
        timeout=30000,
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
    attachment_file = Path("docs/runtime/gateway-dirty-invariant-attachment.txt")
    attachment_file.parent.mkdir(parents=True, exist_ok=True)
    attachment_file.write_text("attachment dirty invariant payload", encoding="utf-8")

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

            page.goto(f"{UI_URL}#/checklist/{ROOT_ID}", wait_until="networkidle", timeout=90000)
            wait_for_detail_ready(page, ROOT_ID)

            before_lock = len(matching_requests(network, "LockAcquire"))
            set_detail_edit_mode(page, True)
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
            after_lock = len(matching_requests(network, "LockAcquire"))
            edit_state = detail_state(page)
            last_state = edit_state
            ok_lock = after_lock > before_lock and edit_state.get("mode") == "EDIT" and edit_state.get("isDirty") is False
            ensure(checks, "detail.attachment_dirty.lock_acquired_clean", ok_lock, {"before": before_lock, "after": after_lock, "state": edit_state})
            if not ok_lock:
                failures.append("detail.attachment_dirty.lock_acquired_clean")

            ensure_attachments_expanded(page)
            upload_before = detail_state(page)
            upload_request_index = len(network)
            page.locator("#sap_ui5_comp---app--detailPaneHost--attachmentUploader-fu").set_input_files(str(attachment_file.resolve()))
            page.wait_for_function(
                """
                (prevCount) => {
                  const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
                  const selected = view && view.getModel && view.getModel('selected');
                  const state = view && view.getModel && view.getModel('state');
                  const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
                  return Array.isArray(attachments)
                    && attachments.length > Number(prevCount || 0)
                    && !!(state && state.getProperty && state.getProperty('/isDirty') === false);
                }
                """,
                arg=upload_before.get("attachmentCount") or 0,
                timeout=30000,
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
            uploaded_keys = [key for key in upload_after.get("attachmentKeys", []) if key not in (upload_before.get("attachmentKeys", []) or [])]
            uploaded_key = uploaded_keys[-1] if uploaded_keys else ""
            ok_upload = (
                has_upload_post
                and has_upload_put
                and upload_after.get("attachmentCount", 0) == upload_before.get("attachmentCount", 0) + 1
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

            delete_before = detail_state(page)
            delete_request_index = len(network)
            invoke_delete(page, uploaded_key)
            page.wait_for_function(
                """
                (payload) => {
                  const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
                  const selected = view && view.getModel && view.getModel('selected');
                  const state = view && view.getModel && view.getModel('state');
                  const attachments = selected && selected.getProperty ? (selected.getProperty('/attachments') || []) : [];
                  const attachmentGone = Array.isArray(attachments)
                    && !attachments.some((item) => String((item && (item.AttachmentKey || item.Key)) || '').trim() === String(payload.key || ''));
                  return Array.isArray(attachments)
                    && attachments.length < Number(payload.prevCount || 0)
                    && attachmentGone
                    && !!(state && state.getProperty && state.getProperty('/isDirty') === false);
                }
                """,
                arg={"prevCount": delete_before.get("attachmentCount") or 0, "key": uploaded_key},
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
                and delete_after.get("attachmentCount", 0) == max(0, delete_before.get("attachmentCount", 0) - 1)
                and delete_after.get("isDirty") is False
                and uploaded_key not in (delete_after.get("attachmentKeys", []) or [])
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

            before_release = len(matching_requests(network, "LockRelease"))
            invoke_controller_method(page, "sap_ui5_comp---app--detailPaneHost", "onCloseDetail")
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
        ensure(checks, "browser.exception", False, {"error": str(exc), "lastState": last_state})

    report = {
        "generatedAt": int(time.time()),
        "uiUrl": UI_URL,
        "rootId": ROOT_ID,
        "ok": not failures,
        "checks": checks,
        "failures": failures,
        "networkSample": network[-25:],
        "lastState": last_state,
    }
    return flush_report(report)


if __name__ == "__main__":
    raise SystemExit(main())
