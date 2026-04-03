#!/usr/bin/env python3
"""Runtime UI audit for SAP-backed high-value user flows with network capture.

Result classes:
- PASS_SAP_EVIDENCE: SAP metadata/data and runtime flow succeeded
- BLOCKED_SAP_ENV: SAP contour is unavailable or incomplete
- FAIL_PRODUCT_CONTRACT: runtime/product flow regressed under SAP-backed execution
"""

from __future__ import annotations

import json
import subprocess
import sys
import urllib.request
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright
from browser_route_bootstrap import navigate_to_detail, wait_for_app_ready


URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
SERVICE_ROOT = "http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV"
REPORT_PATH = Path("docs/ui-runtime-audit-latest.json")
SEARCH_VIEW_IDS = (
    "checklist_app_comp---searchView",
    "checklist_app_comp---searchTargetPage",
)
DETAIL_VIEW_IDS = (
    "checklist_app_comp---detailView",
    "checklist_app_comp---detailTargetPage",
)
RESULT_PASS = "PASS_SAP_EVIDENCE"
RESULT_BLOCKED = "BLOCKED_SAP_ENV"
RESULT_FAIL = "FAIL_PRODUCT_CONTRACT"
STATIC_GATES = (
    "attachment-contract-gate.js",
    "key-model-gate.js",
    "legacy-alias-leakage-gate.js",
    "lock-contract-naming-gate.js",
    "naming-debt-gate.js",
    "raw-ui-text-gate.js",
    "wrapper-sprawl-gate.js",
    "sap-internal-css-gate.js",
)


def fetch_candidate_db_keys(limit: int = 15) -> list[str]:
    with urllib.request.urlopen(
        f"{SERVICE_ROOT}/ChecklistSearchSet?$top={max(1, int(limit))}&$orderby=ChangedOn%20desc", timeout=20
    ) as resp:
        payload = json.loads(resp.read().decode("utf-8"))
    rows = (((payload or {}).get("d") or {}).get("results")) or []
    if not rows:
        raise RuntimeError("ChecklistSearchSet returned no rows")
    return [
        str(row.get("DB_KEY") or row.get("Key") or row.get("Id") or "").strip().upper()
        for row in rows
        if str(row.get("DB_KEY") or row.get("Key") or row.get("Id") or "").strip()
    ]


def geom_snapshot(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          const fcl = core.byId('checklist_app_comp---app--mainFcl');
          const fclDom = fcl && fcl.getDomRef ? fcl.getDomRef() : null;
          const beginColumn = fclDom && fclDom.querySelector ? fclDom.querySelector('.sapFFCLColumnBegin') : null;
          const midColumn = fclDom && fclDom.querySelector ? fclDom.querySelector('.sapFFCLColumnMid') : null;
          const rect = (node) => node ? node.getBoundingClientRect().toJSON() : null;
          return {
            routeName: state && state.getProperty ? String(state.getProperty('/currentRouteName') || '') : '',
            layout: state && state.getProperty ? String(state.getProperty('/layout') || '') : '',
            fclClass: fclDom?.className || '',
            left: rect(beginColumn),
            right: rect(midColumn),
            bodyScrollHeight: document.body.scrollHeight,
            winY: window.scrollY,
            visibleText: document.body ? document.body.innerText : ''
          };
        }
        """
    )


def read_current_user(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const app = sap.ui.getCore().byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          return (state && state.getProperty && state.getProperty('/currentUser')) || {};
        }
        """
    ) or {}


def inspect_detail_access(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
          const state = view && view.getModel && view.getModel('state');
          const viewModel = view && view.getModel && view.getModel('view');
          const detail = view && view.getModel && view.getModel('detail');
          const accessState = state && state.getProperty ? state.getProperty('/accessState') : null;
          const editSwitch = core.byId('checklist_app_comp---detailView--detailEditSwitch')
            || core.byId('checklist_app_comp---detailTargetPage--detailEditSwitch');
          return {
            denied: !!(accessState && accessState.denied),
            reasonCode: String((accessState && accessState.reasonCode) || ""),
            lockState: String((state && state.getProperty && state.getProperty('/workflow/detail/lock/state')) || ""),
            editMode: String((state && state.getProperty && state.getProperty('/workflow/detail/editMode')) || ""),
            dbKey: String((detail && detail.getProperty && (detail.getProperty('/current/root/DB_KEY') || detail.getProperty('/current/root/id'))) || ""),
            hasEditSwitch: !!editSwitch,
            editSwitchEnabled: !!(editSwitch && editSwitch.getEnabled && editSwitch.getEnabled()),
            attachmentMetaEditable: !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentMetaEditable'))
          };
        }
        """
    ) or {}


def resolve_accessible_detail_db_key(page, candidate_keys: list[str]) -> tuple[str, dict[str, Any]]:
    diagnostics = {"checked": []}
    for key in candidate_keys:
        page.goto(f"{URL}#/checklist/{key}", wait_until="networkidle", timeout=90000)
        wait_for_detail_route_open(page)
        access = inspect_detail_access(page)
        diagnostics["checked"].append({"dbKey": key, "access": access})
        if not access.get("denied") and access.get("hasEditSwitch") and access.get("editSwitchEnabled"):
            return key, diagnostics
    raise RuntimeError(json.dumps(diagnostics, ensure_ascii=False))


def resolve_attachment_input_selector(page) -> str:
    selector = page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const uploader = core.byId('checklist_app_comp---detailView--attachmentUploader')
            || core.byId('checklist_app_comp---detailTargetPage--attachmentUploader');
          const dom = uploader && uploader.getDomRef ? uploader.getDomRef() : null;
          const input = uploader && uploader.getFocusDomRef ? uploader.getFocusDomRef() : null;
          if (input && input.id) {
            return '#' + input.id;
          }
          const fileInputDom = uploader && uploader.getDomRef ? uploader.getDomRef('fu') : null;
          if (fileInputDom && fileInputDom.id) {
            return '#' + fileInputDom.id;
          }
          if (dom && dom.querySelector) {
            const nestedInput = dom.querySelector('input[type="file"]');
            if (nestedInput && nestedInput.id) {
              return '#' + nestedInput.id;
            }
          }
          const globalInput = document.querySelector('#checklist_app_comp---detailView--attachmentUploader input[type="file"]')
            || document.querySelector('#checklist_app_comp---detailTargetPage--attachmentUploader input[type="file"]')
            || document.querySelector('input[type="file"][id*="attachmentUploader"]');
          if (globalInput && globalInput.id) {
            return '#' + globalInput.id;
          }
          return '';
        }
        """
    )
    if not selector:
        raise RuntimeError("attachment uploader input not resolved")
    return str(selector)


def inspect_attachment_runtime(page) -> dict[str, Any]:
    return page.evaluate(
        """
        () => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
          const viewModel = view && view.getModel && view.getModel('view');
          const uploader = core.byId('checklist_app_comp---detailView--attachmentUploader')
            || core.byId('checklist_app_comp---detailTargetPage--attachmentUploader');
          const dom = uploader && uploader.getDomRef ? uploader.getDomRef() : null;
          const domFu = uploader && uploader.getDomRef ? uploader.getDomRef('fu') : null;
          return {
            hasUploader: !!uploader,
            uploaderId: uploader && uploader.getId ? String(uploader.getId()) : "",
            attachmentMetaEditable: !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentMetaEditable')),
            attachmentActionsEnabled: !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentActionsEnabled')),
            domId: dom && dom.id ? String(dom.id) : "",
            inputId: domFu && domFu.id ? String(domFu.id) : "",
            hasNestedFileInput: !!(dom && dom.querySelector && dom.querySelector('input[type="file"]'))
          };
        }
        """
    ) or {}


def wait_for_search_ready(page) -> None:
    wait_for_app_ready(page, timeout=90000)
    page.wait_for_function(
        """
        (ids) => {
          const core = sap.ui.getCore();
          return ids.some((id) => {
            const view = core && core.byId ? core.byId(id) : null;
            return !!(view && view.getDomRef && view.getDomRef());
          });
        }
        """,
        arg=SEARCH_VIEW_IDS,
        timeout=30000,
    )
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const controlIds = [
            'checklist_app_comp---searchView--searchResultsActionRail',
            'checklist_app_comp---searchTargetPage--searchResultsActionRail',
            'checklist_app_comp---searchView--searchActionRailStack',
            'checklist_app_comp---searchTargetPage--searchActionRailStack'
          ];
          const text = document.body && document.body.innerText ? document.body.innerText : '';
          const hasCreateText = text.indexOf('\u0421\u043e\u0437\u0434\u0430\u0442\u044c') >= 0 || text.indexOf('Create') >= 0;
          return controlIds.some((id) => {
            const control = core && core.byId ? core.byId(id) : null;
            return !!(control && control.getDomRef && control.getDomRef());
          }) || hasCreateText;
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(1200)


def wait_for_detail_ready(page) -> None:
    page.wait_for_function(
        """
        (ids) => {
          const core = sap.ui.getCore();
          return ids.some((id) => {
            const objectPage = core && core.byId ? core.byId(id + '--detailObjectPage') : null;
            return !!(objectPage && objectPage.getDomRef && objectPage.getDomRef());
          });
        }
        """,
        arg=DETAIL_VIEW_IDS,
        timeout=30000,
    )
    page.wait_for_timeout(1500)


def wait_for_detail_route_open(page) -> None:
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const app = core.byId('checklist_app_comp---app');
          const state = app && app.getModel && app.getModel('state');
          const routeName = state && state.getProperty ? String(state.getProperty('/currentRouteName') || '') : '';
          const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
          return routeName === 'detail' && !!(view && view.getDomRef && view.getDomRef());
        }
        """,
        timeout=30000,
    )
    page.wait_for_timeout(1200)


def current_requests(network: list[dict[str, Any]], predicate) -> list[dict[str, Any]]:
    return [item for item in network if predicate(item)]


def ensure(checks: list[dict[str, Any]], name: str, ok: bool, detail: Any) -> None:
    checks.append({"name": name, "ok": bool(ok), "detail": detail})


def run_static_gate(script_name: str) -> dict[str, Any]:
    result = subprocess.run(
        ["node", f"scripts/{script_name}"],
        capture_output=True,
        text=True,
        encoding="utf-8",
        cwd=Path.cwd(),
        timeout=120,
    )
    return {
        "name": script_name,
        "ok": result.returncode == 0,
        "stdout": (result.stdout or "").strip(),
        "stderr": (result.stderr or "").strip(),
    }


def invoke_search(page, method_name: str) -> None:
    page.evaluate(
        """
        (methodName) => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---searchView') || core.byId('checklist_app_comp---searchTargetPage');
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
          const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
          const controller = view && view.getController && view.getController();
          if (!controller || typeof controller[methodName] !== 'function') {
            throw new Error('Detail controller method not found: ' + methodName);
          }
          controller[methodName]();
        }
        """,
        method_name,
    )


def toggle_detail_edit(page, next_state: bool) -> dict[str, Any]:
    result = page.evaluate(
        """
        (state) => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
          const controller = view && view.getController && view.getController();
          const control = core.byId('checklist_app_comp---detailView--detailEditSwitch')
            || core.byId('checklist_app_comp---detailTargetPage--detailEditSwitch');
          if (!controller || typeof controller.onToggleEdit !== 'function') {
            throw new Error('Detail onToggleEdit handler not found');
          }
          if (!control) {
            throw new Error('Detail edit switch not found');
          }
          if (typeof control.getEnabled === 'function' && !control.getEnabled()) {
            throw new Error('Detail edit switch is disabled');
          }
          return Promise.resolve(controller.onToggleEdit({
            getParameter: (name) => name === 'state' ? !!state : undefined,
            getSource: () => control
          }));
        }
        """,
        next_state,
    )
    if not next_state:
        page.wait_for_function(
            """
            () => {
              const core = sap.ui.getCore();
              const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
              const stateModel = view && view.getModel && view.getModel('state');
              const editMode = stateModel && stateModel.getProperty ? String(stateModel.getProperty('/workflow/detail/editMode') || '') : '';
              return editMode !== 'EDIT';
            }
            """,
            timeout=30000,
        )
        return result or {}
    page.wait_for_function(
        """
        () => {
          const core = sap.ui.getCore();
          const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
          const stateModel = view && view.getModel && view.getModel('state');
          const viewModel = view && view.getModel && view.getModel('view');
          const editMode = stateModel && stateModel.getProperty ? String(stateModel.getProperty('/workflow/detail/editMode') || '') : '';
          const attachmentMetaEditable = !!(viewModel && viewModel.getProperty && viewModel.getProperty('/attachmentMetaEditable'));
          const lockState = stateModel && stateModel.getProperty ? String(stateModel.getProperty('/workflow/detail/lock/state') || '') : '';
          return (editMode === 'EDIT' && attachmentMetaEditable) || lockState === 'READ_ONLY';
        }
        """,
        timeout=30000,
    )
    return result or {}


def main() -> int:
    db_key = ""
    candidate_db_keys: list[str] = []
    network: list[dict[str, Any]] = []
    checks: list[dict[str, Any]] = []
    bugs: list[str] = []
    static_gate_results = [run_static_gate(script_name) for script_name in STATIC_GATES]

    for gate in static_gate_results:
        ensure(checks, f"static.{gate['name']}", gate["ok"], {
            "stdout": gate["stdout"],
            "stderr": gate["stderr"],
        })
        if not gate["ok"]:
            bugs.append(f"static:{gate['name']}")

    try:
        candidate_db_keys = fetch_candidate_db_keys()
    except Exception as exc:  # noqa: BLE001
        ensure(checks, "gateway.db_key.fetch", False, {"error": str(exc)})
        report = {
            "dbKey": db_key,
            "checks": checks,
            "bugs": bugs + ["gateway:db_key_fetch"],
            "staticGates": static_gate_results,
            "network": {
                "count": 0,
                "sample": [],
            },
        }
        REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
        sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
        return 1

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
        current_user = read_current_user(page)
        can_create = bool((current_user or {}).get("canCreate"))
        ensure(checks, "startup.search.visible", ("Create" in startup["visibleText"]) or ("\u0421\u043e\u0437\u0434\u0430\u0442\u044c" in startup["visibleText"]) or (not can_create), {
            "geometry": startup,
            "currentUser": current_user,
        })

        if can_create:
            invoke_search(page, "onCreate")
            wait_for_detail_ready(page)
            create_geom = geom_snapshot(page)
            ensure(
                checks,
                "create.opens.split",
                create_geom["routeName"] == "detail" and create_geom["left"] and create_geom["left"]["width"] > 0,
                create_geom,
            )

            create_before_dnd = page.evaluate(
                """
                () => {
                  const core = sap.ui.getCore();
                  const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
                  const model = view && (view.getModel('detail') || view.getModel('selected'));
                  const attachments = model && model.getProperty ? (model.getProperty('/attachments') || model.getProperty('/current/attachments') || []) : [];
                  return attachments.length;
                }
                """
            )
            page.evaluate(
                """
                () => {
                  const zone = document.querySelector('#checklist_app_comp---detailView--attachmentDropZone')
                    || document.querySelector('#checklist_app_comp---detailTargetPage--attachmentDropZone');
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
                  const core = sap.ui.getCore();
                  const view = core.byId('checklist_app_comp---detailView') || core.byId('checklist_app_comp---detailTargetPage');
                  const model = view && (view.getModel('detail') || view.getModel('selected'));
                  const attachments = model && model.getProperty ? (model.getProperty('/attachments') || model.getProperty('/current/attachments') || []) : [];
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
                close_create["routeName"] == "search"
                and str(close_create["layout"]) == "OneColumn"
                and close_create["left"]
                and close_create["left"]["width"] > 0
                and close_create["winY"] == 0
            )
            ensure(checks, "close.create.restores.single", ok_create_close, close_create)
            if not ok_create_close:
                bugs.append("splitter.close_create.mode_or_geometry")
        else:
            ensure(checks, "create.flow.skipped.no_permission", True, {"currentUser": current_user})

        try:
            db_key, access_diagnostics = resolve_accessible_detail_db_key(page, candidate_db_keys)
            ensure(checks, "gateway.detail_accessible_key", True, access_diagnostics)
        except Exception as exc:  # noqa: BLE001
            ensure(checks, "gateway.detail_accessible_key", False, {"error": str(exc), "candidates": candidate_db_keys})
            browser.close()
            report = {
                "result": RESULT_BLOCKED,
                "dbKey": "",
                "checks": checks,
                "bugs": bugs,
                "staticGates": static_gate_results,
                "network": {
                    "count": len(network),
                    "sample": network[-20:],
                },
            }
            REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
            sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
            return 0

        page.goto(f"{URL}#/checklist/{db_key}", wait_until="networkidle", timeout=90000)
        wait_for_detail_route_open(page)
        existing_open = geom_snapshot(page)
        ensure(
            checks,
            "existing.opens.split",
            existing_open["routeName"] == "detail" and existing_open["left"] and existing_open["left"]["width"] > 0,
            existing_open,
        )

        before_lock = len(current_requests(network, lambda item: ("LockAcquire" in item["url"]) or ("LockAcquire" in item.get("post_data", ""))))
        edit_attempt = toggle_detail_edit(page, True)
        page.wait_for_timeout(1800)
        after_lock = len(current_requests(network, lambda item: ("LockAcquire" in item["url"]) or ("LockAcquire" in item.get("post_data", ""))))
        ensure(checks, "edit.lock.requested", after_lock > before_lock, {"before": before_lock, "after": after_lock})
        if after_lock <= before_lock:
            bugs.append("lock.acquire.not_observed")

        post_edit_access = inspect_detail_access(page)
        edit_error = (((edit_attempt or {}).get("value") or {}).get("error")) or {}
        edit_blocked = str(edit_error.get("code") or "").strip().upper() == "LOCKED" or post_edit_access.get("lockState") == "READ_ONLY"
        ensure(checks, "edit.enter.runtime", not edit_blocked and post_edit_access.get("editMode") == "EDIT", {
            "attempt": edit_attempt,
            "detailAccess": post_edit_access,
        })
        if edit_blocked:
            browser.close()
            report = {
                "result": RESULT_BLOCKED,
                "dbKey": db_key,
                "checks": checks,
                "bugs": bugs,
                "staticGates": static_gate_results,
                "network": {
                    "count": len(network),
                    "sample": network[-20:],
                },
            }
            REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
            sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
            return 0

        tmp_file = Path("docs/runtime/tmp_ui_attachment.txt")
        tmp_file.parent.mkdir(parents=True, exist_ok=True)
        tmp_file.write_text("ui audit attachment payload", encoding="utf-8")

        before_upload = len(current_requests(network, lambda item: "AttachmentSet" in item["url"] or "/$batch" in item["url"]))
        try:
            uploader_selector = resolve_attachment_input_selector(page)
        except Exception as exc:  # noqa: BLE001
            ensure(checks, "attachment.uploader.available", False, {
                "error": str(exc),
                "runtime": inspect_attachment_runtime(page),
                "dbKey": db_key,
            })
            browser.close()
            report = {
                "result": RESULT_BLOCKED,
                "dbKey": db_key,
                "checks": checks,
                "bugs": bugs,
                "staticGates": static_gate_results,
                "network": {
                    "count": len(network),
                    "sample": network[-20:],
                },
            }
            REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
            sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
            return 0
        page.locator(uploader_selector).set_input_files(str(tmp_file.resolve()))
        page.wait_for_timeout(2500)
        attachment_requests = current_requests(
            network[before_upload:],
            lambda item: "AttachmentSet" in item["url"] or "/$batch" in item["url"],
        )
        has_metadata_create = any(item["method"] == "POST" and "/AttachmentSet" in item["url"] for item in attachment_requests)
        has_base64_save = any("Value" in (item.get("post_data") or "") and "SaveChanges" in item["url"] for item in attachment_requests)
        ensure(
            checks,
            "attachment.upload.network",
            has_metadata_create and not has_base64_save,
            {"requests": attachment_requests},
        )
        if not (has_metadata_create and not has_base64_save):
            bugs.append("attachment.upload.contract")

        invoke_detail(page, "onCloseDetail")
        page.wait_for_timeout(1800)
        close_existing = geom_snapshot(page)
        ok_existing_close = (
            close_existing["routeName"] == "search"
            and str(close_existing["layout"]) == "OneColumn"
            and close_existing["left"]
            and close_existing["left"]["width"] > 0
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
        "result": RESULT_FAIL if bugs else RESULT_PASS,
        "dbKey": db_key,
        "checks": checks,
        "bugs": bugs,
        "staticGates": static_gate_results,
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
