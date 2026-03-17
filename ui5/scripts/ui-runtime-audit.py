#!/usr/bin/env python3
"""Runtime UI audit for high-value user flows with network capture."""

from __future__ import annotations

import json
import sys
import urllib.request
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright


URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
SERVICE_ROOT = "http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV"
REPORT_PATH = Path("docs/ui-runtime-audit-latest.json")


def fetch_existing_root_id() -> str:
    with urllib.request.urlopen(
        f"{SERVICE_ROOT}/ChecklistSearchSet?$top=1&$orderby=ChangedOn%20desc", timeout=20
    ) as resp:
        payload = json.loads(resp.read().decode("utf-8"))
    rows = (((payload or {}).get("d") or {}).get("results")) or []
    if not rows:
        raise RuntimeError("ChecklistSearchSet returned no rows")
    row = rows[0]
    root_id = str(row.get("Key") or row.get("RootKey") or row.get("Id") or "").strip().upper()
    if not root_id:
        raise RuntimeError("Could not resolve existing root id")
    return root_id


def geom_snapshot(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const splitter = document.querySelector('#checklist_app_comp---app--mainSplitter');
          const left = document.querySelector('#checklist_app_comp---app--mainSplitter-content-0');
          const right = document.querySelector('#checklist_app_comp---app--mainSplitter-content-1');
          const rect = (node) => node ? node.getBoundingClientRect().toJSON() : null;
          return {
            splitterClass: splitter?.className || '',
            left: rect(left),
            right: rect(right),
            bodyScrollHeight: document.body.scrollHeight,
            winY: window.scrollY,
            visibleText: document.body ? document.body.innerText : ''
          };
        }
        """
    )


def wait_for_search_ready(page) -> None:
    page.wait_for_selector("#checklist_app_comp---app--mainSplitter", timeout=30000)
    page.get_by_text("Create", exact=True).wait_for(timeout=30000)
    page.wait_for_timeout(1200)


def wait_for_detail_ready(page) -> None:
    page.wait_for_selector("#checklist_app_comp---app--detailPaneHost--detailObjectPage", timeout=30000)
    page.wait_for_timeout(1500)


def current_requests(network: list[dict[str, Any]], predicate) -> list[dict[str, Any]]:
    return [item for item in network if predicate(item)]


def ensure(checks: list[dict[str, Any]], name: str, ok: bool, detail: Any) -> None:
    checks.append({"name": name, "ok": bool(ok), "detail": detail})


def invoke_search(page, method_name: str) -> None:
    page.evaluate(
        """
        (methodName) => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---app--searchPaneHost');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error('Search controller method not found: ' + methodName);
          }
          controller[methodName]();
        }
        """,
        method_name,
    )


def invoke_detail(page, method_name: str) -> None:
    page.evaluate(
        """
        (methodName) => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---app--detailPaneHost');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error('Detail controller method not found: ' + methodName);
          }
          controller[methodName]();
        }
        """,
        method_name,
    )


def main() -> int:
    root_id = fetch_existing_root_id()
    network: list[dict[str, Any]] = []
    checks: list[dict[str, Any]] = []
    bugs: list[str] = []

    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": 1440, "height": 960})

        def on_request(req) -> None:
            if "/sap/" not in req.url:
                return
            payload = req.post_data or ""
            network.append(
                {
                    "method": req.method,
                    "url": req.url,
                    "headers": req.headers,
                    "post_data": payload[:4000],
                }
            )

        page.on("request", on_request)

        page.goto(URL, wait_until="networkidle", timeout=90000)
        wait_for_search_ready(page)
        startup = geom_snapshot(page)
        ensure(checks, "startup.search.visible", "Create" in startup["visibleText"], startup)

        invoke_search(page, "onCreate")
        wait_for_detail_ready(page)
        create_geom = geom_snapshot(page)
        ensure(checks, "create.opens.split", "appSplitModeSplit" in create_geom["splitterClass"], create_geom)

        create_before_dnd = page.evaluate(
            """
            () => {
              const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
              return ((view.getModel('selected').getProperty('/attachments') || []).length);
            }
            """
        )
        page.evaluate(
            """
            () => {
              const zone = document.querySelector('#checklist_app_comp---app--detailPaneHost--attachmentDropZone');
              if (!zone) {
                throw new Error('attachment drop zone not found in create mode');
              }
              const data = new DataTransfer();
              data.items.add(new File(['create dnd payload'], 'create-dnd.txt', { type: 'text/plain' }));
              ['dragenter', 'dragover', 'drop'].forEach((type) => {
                zone.dispatchEvent(new DragEvent(type, { bubbles: true, cancelable: true, dataTransfer: data }));
              });
            }
            """
        )
        page.wait_for_timeout(1200)
        create_after_dnd = page.evaluate(
            """
            () => {
              const view = sap.ui.getCore().byId('checklist_app_comp---app--detailPaneHost');
              const attachments = view.getModel('selected').getProperty('/attachments') || [];
              return {
                count: attachments.length,
                last: attachments.length ? { name: attachments[attachments.length - 1].FileName, staged: !!attachments[attachments.length - 1].staged } : null
              };
            }
            """
        )
        ensure(
            checks,
            "attachment.dnd.create.staged",
            create_after_dnd["count"] > create_before_dnd and bool((create_after_dnd.get("last") or {}).get("staged")),
            {"before": create_before_dnd, "after": create_after_dnd},
        )
        if not (create_after_dnd["count"] > create_before_dnd and bool((create_after_dnd.get("last") or {}).get("staged"))):
            bugs.append("attachment.dnd.create")

        invoke_detail(page, "onCloseDetail")
        page.wait_for_timeout(1800)
        close_create = geom_snapshot(page)
        ok_create_close = (
            "appSplitModeSingle" in close_create["splitterClass"]
            and close_create["left"]
            and close_create["left"]["width"] > 1200
            and close_create["winY"] == 0
        )
        ensure(checks, "close.create.restores.single", ok_create_close, close_create)
        if not ok_create_close:
            bugs.append("splitter.close_create.mode_or_geometry")

        page.goto(f"{URL}#/checklist/{root_id}", wait_until="networkidle", timeout=90000)
        wait_for_detail_ready(page)
        existing_open = geom_snapshot(page)
        ensure(checks, "existing.opens.split", "appSplitModeSplit" in existing_open["splitterClass"], existing_open)

        before_lock = len(current_requests(network, lambda item: ("LockAcquire" in item["url"]) or ("LockAcquire" in item.get("post_data", ""))))
        page.locator(".accentSwitchEditMode").click(timeout=10000)
        page.wait_for_timeout(1800)
        after_lock = len(current_requests(network, lambda item: ("LockAcquire" in item["url"]) or ("LockAcquire" in item.get("post_data", ""))))
        ensure(checks, "edit.lock.requested", after_lock > before_lock, {"before": before_lock, "after": after_lock})
        if after_lock <= before_lock:
            bugs.append("lock.acquire.not_observed")

        tmp_file = Path("docs/runtime/tmp_ui_attachment.txt")
        tmp_file.parent.mkdir(parents=True, exist_ok=True)
        tmp_file.write_text("ui audit attachment payload", encoding="utf-8")

        before_upload = len(current_requests(network, lambda item: "AttachmentSet" in item["url"] or "/$batch" in item["url"]))
        page.locator("#checklist_app_comp---app--detailPaneHost--attachmentUploader-fu").set_input_files(str(tmp_file.resolve()))
        page.wait_for_timeout(2500)
        attachment_requests = current_requests(
            network[before_upload:],
            lambda item: "AttachmentSet" in item["url"] or "/$batch" in item["url"],
        )
        has_metadata_create = any(
            (
                item["method"] == "POST"
                and (
                    "/AttachmentSet" in item["url"]
                    or ("multipart/mixed" in str(item["headers"].get("content-type", "")) and "POST AttachmentSet" in item.get("post_data", ""))
                )
            )
            for item in attachment_requests
        )
        has_value_put = any(
            item["method"] == "PUT" and "/AttachmentSet(Key='" in item["url"] and "/$value" in item["url"]
            for item in attachment_requests
        )
        ensure(
            checks,
            "attachment.upload.network",
            has_metadata_create and has_value_put,
            {"requests": attachment_requests},
        )
        if not (has_metadata_create and has_value_put):
            bugs.append("attachment.upload.contract")

        invoke_detail(page, "onCloseDetail")
        page.wait_for_timeout(1800)
        close_existing = geom_snapshot(page)
        ok_existing_close = (
            "appSplitModeSingle" in close_existing["splitterClass"]
            and close_existing["left"]
            and close_existing["left"]["width"] > 1200
            and close_existing["winY"] == 0
            and "Create" in close_existing["visibleText"]
        )
        ensure(checks, "close.existing.restores.search", ok_existing_close, close_existing)
        if not ok_existing_close:
            bugs.append("splitter.close_existing.mode_or_blank")

        cache_requests = current_requests(network, lambda item: ("LastChangeSet" in item["url"]) or ("LastChangeSet" in item.get("post_data", "")))
        ensure(checks, "cache.validation.called", len(cache_requests) > 0, {"count": len(cache_requests)})

        browser.close()

    report = {
        "rootId": root_id,
        "checks": checks,
        "bugs": bugs,
        "network": {
            "count": len(network),
            "sample": network[-20:],
        },
    }
    REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
    return 1 if bugs else 0


if __name__ == "__main__":
    raise SystemExit(main())
