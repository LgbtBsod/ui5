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


def wait_for_detail_ready(page) -> None:
    page.wait_for_selector("#sap_ui5_comp---app--detailPaneHost--detailObjectPage", timeout=30000)
    page.wait_for_timeout(1500)


def wait_for_search_ready(page) -> None:
    page.wait_for_selector("#sap_ui5_comp---app--searchPaneHost--searchSmartTable", timeout=30000)
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
            wait_for_detail_ready(page)

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
            ok_lock = after_lock > before_lock and edit_state.get("mode") == "EDIT" and edit_state.get("isDirty") is False
            ensure(checks, "detail.attachment_dirty.lock_acquired_clean", ok_lock, {"before": before_lock, "after": after_lock, "state": edit_state})
            if not ok_lock:
                failures.append("detail.attachment_dirty.lock_acquired_clean")
            lock_direct_requests = direct_requests(network, "LockAcquire")
            lock_batch_requests = batch_requests(network, "LockAcquire")
            ok_lock_direct = len(lock_direct_requests) >= 1 and not lock_batch_requests
            ensure(checks, "detail.attachment_dirty.lock_direct", ok_lock_direct, {"direct": lock_direct_requests[-3:], "batch": lock_batch_requests[-3:]})
            if not ok_lock_direct:
                failures.append("detail.attachment_dirty.lock_direct")

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
            upload_requests = current_requests(
                network[upload_request_index:],
                lambda item: "AttachmentSet" in item["url"] or "AttachmentSet" in item.get("post_data", "") or "/$batch" in item["url"],
            )
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
                },
            )
            if not ok_upload:
                failures.append("detail.attachment_upload_keeps_clean_state")
            upload_batch_requests = batch_operation_requests(upload_requests, "POST", "AttachmentSet") + batch_operation_requests(upload_requests, "PUT", "AttachmentSet")
            ok_upload_direct = has_upload_post and has_upload_put and not upload_batch_requests
            ensure(checks, "detail.attachment_upload_direct", ok_upload_direct, {"requests": upload_requests, "batchedWrites": upload_batch_requests})
            if not ok_upload_direct:
                failures.append("detail.attachment_upload_direct")

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
            delete_requests = current_requests(
                network[delete_request_index:],
                lambda item: ("AttachmentSet(Key='" in item["url"] and item["method"] == "DELETE") or "DELETE AttachmentSet" in item.get("post_data", ""),
            )
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
                },
            )
            if not ok_delete:
                failures.append("detail.attachment_delete_keeps_clean_state")
            delete_batch_requests = batch_operation_requests(delete_requests, "DELETE", "AttachmentSet")
            ok_delete_direct = len(delete_requests) > 0 and not delete_batch_requests
            ensure(checks, "detail.attachment_delete_direct", ok_delete_direct, {"requests": delete_requests, "batchedWrites": delete_batch_requests})
            if not ok_delete_direct:
                failures.append("detail.attachment_delete_direct")

            before_release = len(current_requests(network, lambda item: "LockRelease" in item["url"] or "LockRelease" in item.get("post_data", "")))
            page.evaluate(
                """
                () => {
                  const view = sap.ui.getCore().byId('sap_ui5_comp---app--detailPaneHost');
                  const controller = view && view.getController && view.getController();
                  if (!controller || typeof controller.onCloseDetail !== 'function') {
                    throw new Error('detail close handler unavailable');
                  }
                  return controller.onCloseDetail();
                }
                """
            )
            wait_for_search_ready(page)
            page.wait_for_timeout(1200)
            after_release = len(current_requests(network, lambda item: "LockRelease" in item["url"] or "LockRelease" in item.get("post_data", "")))
            ok_release = after_release > before_release
            ensure(checks, "detail.attachment_dirty.lock_release", ok_release, {"before": before_release, "after": after_release})
            if not ok_release:
                failures.append("detail.attachment_dirty.lock_release")
            release_direct_requests = direct_requests(network, "LockRelease")
            release_batch_requests = batch_requests(network, "LockRelease")
            ok_release_direct = len(release_direct_requests) >= 1 and not release_batch_requests
            ensure(checks, "detail.attachment_dirty.lock_release_direct", ok_release_direct, {"direct": release_direct_requests[-3:], "batch": release_batch_requests[-3:]})
            if not ok_release_direct:
                failures.append("detail.attachment_dirty.lock_release_direct")

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
