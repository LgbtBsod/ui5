#!/usr/bin/env python3
"""SAP Gateway-only smoke pack: API + browser flow over mock Gateway."""

from __future__ import annotations

import json
import subprocess
import sys
import urllib.parse
import urllib.request
import uuid
import base64
from http.cookiejar import CookieJar
from pathlib import Path
from typing import Any


UI_URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
SERVICE_ROOT = (sys.argv[2] if len(sys.argv) > 2 else "http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV").rstrip("/")
REPORT_PATH = Path("docs/artifacts/gateway-only-smoke-report.json")
ROOT_DELETE_WARNING = "cleanup.delete.failed"


def ensure(checks: list[dict[str, Any]], name: str, ok: bool, detail: Any) -> None:
    checks.append({"name": name, "ok": bool(ok), "detail": detail})


def build_opener() -> tuple[urllib.request.OpenerDirector, CookieJar]:
    jar = CookieJar()
    opener = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(jar))
    return opener, jar


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
        if isinstance(payload, (bytes, bytearray)):
            data = bytes(payload)
        else:
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


def fetch_csrf(opener) -> str:
    _status, _payload, headers = request(opener, "GET", f"{SERVICE_ROOT}/", headers={"X-CSRF-Token": "Fetch"})
    return str(headers.get("X-CSRF-Token") or headers.get("x-csrf-token") or "").strip()


def existing_root(opener) -> str:
    status, payload, _headers = request(opener, "GET", f"{SERVICE_ROOT}/ChecklistSearchSet?$top=1&$orderby=ChangedOn%20desc")
    if status != 200:
        raise RuntimeError("ChecklistSearchSet request failed")
    rows = (((payload or {}).get("d") or {}).get("results")) or []
    if not rows:
        raise RuntimeError("ChecklistSearchSet returned no rows")
    return str(rows[0].get("Key") or rows[0].get("RootKey") or rows[0].get("Id") or "").strip().upper()


def create_checklist(opener, token: str) -> dict[str, Any]:
    payload = {
        "FullPayload": {
            "root": {"id": "__CREATE", "status": "DRAFT"},
            "basic": {
                "date": "2026-03-04",
                "equipment": "Gateway Smoke Pump",
                "BUKRS": "3000",
                "LOCATION_KEY": "LOC-001-01-01",
                "LOCATION_NAME": "Area A",
                "LOCATION_TEXT": "Area A",
                "OBSERVER_FULLNAME": "Gateway Smoke Observer",
                "OBSERVER_ORGUNIT": "Production Shift A",
                "OBSERVED_FULLNAME": "Gateway Smoke Observed",
                "LPC_KEY": "L2",
                "PROF_KEY": "PR1",
            },
            "checks": [{"ChecksNum": 1, "text": "Gateway smoke create", "result": False}],
            "barriers": [{"BarriersNum": 1, "comment": "Gateway smoke barrier", "result": True}],
        }
    }
    status, data, _headers = request(opener, "POST", f"{SERVICE_ROOT}/CreateChecklist", headers={"X-CSRF-Token": token}, payload=payload)
    if status != 200:
        raise RuntimeError(f"CreateChecklist failed with {status}")
    return (data or {}).get("d") or {}


def delete_checklist(opener, token: str, root_id: str) -> bool:
    status, _body, _headers = request(
        opener,
        "DELETE",
        f"{SERVICE_ROOT}/ChecklistRootSet('{root_id}')",
        headers={"X-CSRF-Token": token},
        expect_json=False,
    )
    return status == 204


def run_browser_smoke_script(script_name: str, *script_args: str) -> dict[str, Any]:
    script = Path("scripts") / script_name
    result = subprocess.run(
        [sys.executable, str(script), *script_args],
        capture_output=True,
        text=True,
        encoding="utf-8",
        cwd=Path.cwd(),
        timeout=240,
    )
    output = (result.stdout or "").strip()
    if not output:
        output = (result.stderr or "").strip()
    if not output:
        parsed = {"ok": False, "failures": ["browser.output.empty"], "checks": []}
    else:
        try:
            parsed = json.loads(output)
        except json.JSONDecodeError:
            parsed = {
                "ok": False,
                "failures": ["browser.output.invalid_json"],
                "checks": [],
                "stdout": (result.stdout or "").strip(),
            }
    parsed["exitCode"] = result.returncode
    if result.stderr:
        parsed["stderr"] = result.stderr.strip()
    return parsed


def combine_browser_reports(reports: dict[str, dict[str, Any]]) -> dict[str, Any]:
    checks: list[dict[str, Any]] = []
    failures: list[str] = []
    network_sample: list[dict[str, Any]] = []
    last_state: dict[str, Any] = {}
    for report_name, report in reports.items():
        checks.extend((report or {}).get("checks") or [])
        failures.extend([f"{report_name}:{item}" for item in ((report or {}).get("failures") or [])])
        network_sample.extend(((report or {}).get("networkSample") or [])[-10:])
        if (report or {}).get("lastState"):
            last_state = report.get("lastState") or last_state
    return {
        "ok": not failures and all(bool((report or {}).get("ok", False)) for report in reports.values()),
        "checks": checks,
        "failures": failures,
        "networkSample": network_sample[-25:],
        "lastState": last_state,
        "flows": reports,
    }


def main() -> int:
    opener, _jar = build_opener()
    api_checks: list[dict[str, Any]] = []
    warnings: list[str] = []
    created_root_id = ""
    browser_root_id = ""
    browser_attachment_root_id = ""
    browser_flow_root_id = ""
    browser_report: dict[str, Any] = {}
    token = ""
    browser_failures: list[str] = []

    try:
        token = fetch_csrf(opener)
        ensure(api_checks, "csrf.fetch", bool(token), {"tokenPresent": bool(token)})
        existing = existing_root(opener)
        ensure(api_checks, "search.root.available", bool(existing), {"rootId": existing})

        status, runtime_payload, _headers = request(opener, "GET", f"{SERVICE_ROOT}/RuntimeSettingsSet('GLOBAL')")
        runtime_data = (runtime_payload or {}).get("d") or {}
        ensure(api_checks, "runtime.settings.gateway", status == 200 and bool(runtime_data.get("Key")), {"status": status, "key": runtime_data.get("Key")})

        created = create_checklist(opener, token)
        created_root_id = str(created.get("RootKey") or created.get("Key") or "").strip().upper()
        created_version = int(created.get("VersionNumber") or 1)
        ensure(api_checks, "create.gateway", bool(created_root_id) and created_version == 1, {"rootId": created_root_id, "version": created_version})

        session_guid = f"GW-SMOKE-{uuid.uuid4().hex[:12].upper()}"
        query = urllib.parse.urlencode({"RootId": created_root_id, "SessionGuid": session_guid})
        acquire_status, acquire_payload, _headers = request(opener, "POST", f"{SERVICE_ROOT}/LockAcquire?{query}", headers={"X-CSRF-Token": token})
        heartbeat_status, heartbeat_payload, _headers = request(opener, "POST", f"{SERVICE_ROOT}/LockHeartbeat?{query}", headers={"X-CSRF-Token": token})
        ensure(
            api_checks,
            "lock.acquire.heartbeat.gateway",
            acquire_status == 200 and heartbeat_status == 200 and bool(((acquire_payload or {}).get("d") or {}).get("Ok")),
            {"acquireStatus": acquire_status, "heartbeatStatus": heartbeat_status},
        )

        autosave_payload = {
            "root": {"pcct_uuid": created_root_id},
            "checks": [{
                "client_row_id": uuid.uuid4().hex.upper(),
                "edit_mode": "C",
                "checks_num": 2,
                "text": "Gateway smoke autosave",
                "comment": "Autosave via smoke pack",
                "result": False,
            }],
            "barriers": [],
            "client_version": created_version,
            "SessionGuid": session_guid,
        }
        autosave_status, autosave_data, _headers = request(opener, "POST", f"{SERVICE_ROOT}/AutoSave", headers={"X-CSRF-Token": token}, payload=autosave_payload)
        autosave_body = (autosave_data or {}).get("d") or {}
        autosave_version = int(autosave_body.get("version_number") or 0)
        ensure(api_checks, "autosave.gateway", autosave_status == 200 and autosave_version == 2, {"status": autosave_status, "version": autosave_version})

        save_payload = {
            "root": {"pcct_uuid": created_root_id, "equipment": "Gateway Smoke Saved"},
            "checks": [],
            "barriers": [],
            "client_version": autosave_version,
            "SessionGuid": session_guid,
        }
        save_status, save_data, _headers = request(opener, "POST", f"{SERVICE_ROOT}/SaveChanges", headers={"X-CSRF-Token": token}, payload=save_payload)
        save_body = (save_data or {}).get("d") or {}
        save_version = int(save_body.get("version_number") or 0)
        ensure(api_checks, "save.gateway", save_status == 200 and save_version == 3, {"status": save_status, "version": save_version})

        attachment_body = b"gateway smoke attachment"
        attachment_save_payload = {
            "root": {"pcct_uuid": created_root_id, "equipment": "Gateway Smoke Saved"},
            "checks": [],
            "barriers": [],
            "client_version": save_version,
            "SessionGuid": session_guid,
            "attachments": [{
                "Key": "GW-ATT-1",
                "RootKey": created_root_id,
                "ParentKey": created_root_id,
                "FolderKey": created_root_id,
                "CategoryKey": "GEN",
                "Type": "GEN",
                "FileName": "gateway-smoke.txt",
                "Name": "gateway-smoke.txt",
                "MimeType": "text/plain",
                "Description": "Gateway smoke attachment",
                "FileSize": len(attachment_body),
                "FileSizeContent": len(attachment_body),
                "Value": base64.b64encode(attachment_body).decode("ascii"),
            }],
        }
        attachment_save_status, attachment_save_data, _headers = request(
            opener,
            "POST",
            f"{SERVICE_ROOT}/SaveChanges",
            headers={"X-CSRF-Token": token},
            payload=attachment_save_payload,
        )
        attachment_saved_body = (attachment_save_data or {}).get("d") or {}
        attachment_list_status, attachment_list_body, _headers = request(
            opener,
            "GET",
            f"{SERVICE_ROOT}/AttachmentSet?$filter=RootKey%20eq%20'{created_root_id}'",
            headers={"X-CSRF-Token": token},
        )
        attachment_rows = (((attachment_list_body or {}).get("d") or {}).get("results")) or []
        attachment_key = str((attachment_rows[0] if attachment_rows else {}).get("AttachmentKey") or "").strip()
        attachment_get_status, attachment_get_payload, _headers = request(
            opener,
            "GET",
            f"{SERVICE_ROOT}/AttachmentSet(AttachmentKey='{attachment_key}')",
            headers={"X-CSRF-Token": token},
        )
        attachment_get_body = ((attachment_get_payload or {}).get("d") or {})
        attachment_delete_status, _attachment_delete_body, _headers = request(
            opener,
            "DELETE",
            f"{SERVICE_ROOT}/AttachmentSet(AttachmentKey='{attachment_key}')",
            headers={"X-CSRF-Token": token},
            expect_json=False,
        )
        ensure(
            api_checks,
            "attachment.gateway",
            attachment_save_status == 200
            and int(attachment_saved_body.get("version_number") or 0) == 4
            and attachment_list_status == 200
            and len(attachment_rows) == 1
            and attachment_get_status == 200
            and attachment_get_body.get("Value") == base64.b64encode(attachment_body).decode("ascii")
            and attachment_delete_status == 204,
            {
                "saveStatus": attachment_save_status,
                "saveVersion": attachment_saved_body.get("version_number"),
                "listStatus": attachment_list_status,
                "getStatus": attachment_get_status,
                "deleteStatus": attachment_delete_status,
                "attachmentCount": len(attachment_rows),
            },
        )

        release_status, release_payload, _headers = request(opener, "POST", f"{SERVICE_ROOT}/LockRelease?{query}", headers={"X-CSRF-Token": token})
        release_ok = release_status == 200 and bool(((release_payload or {}).get("d") or {}).get("Ok"))
        ensure(api_checks, "lock.release.gateway", release_ok, {"status": release_status})

        browser_created = create_checklist(opener, token)
        browser_root_id = str(browser_created.get("RootKey") or browser_created.get("Key") or "").strip().upper()
        ensure(api_checks, "browser.root.created", bool(browser_root_id), {"rootId": browser_root_id})

        browser_attachment_created = create_checklist(opener, token)
        browser_attachment_root_id = str(browser_attachment_created.get("RootKey") or browser_attachment_created.get("Key") or "").strip().upper()
        ensure(api_checks, "browser.attachment.root.created", bool(browser_attachment_root_id), {"rootId": browser_attachment_root_id})

        browser_flow_created = create_checklist(opener, token)
        browser_flow_root_id = str(browser_flow_created.get("RootKey") or browser_flow_created.get("Key") or "").strip().upper()
        ensure(api_checks, "browser.flow.root.created", bool(browser_flow_root_id), {"rootId": browser_flow_root_id})

        browser_report = combine_browser_reports({
            "facadeContract": run_browser_smoke_script("browser-smoke-domain-facade-contract.py", UI_URL),
            "attachmentDirtyInvariant": run_browser_smoke_script("browser-smoke-detail-attachment-dirty-invariant.py", UI_URL, browser_attachment_root_id),
            "gatewayOnlyFlow": run_browser_smoke_script("browser-smoke-gateway-only-flow.py", UI_URL, browser_flow_root_id),
        })
        browser_failures = list(browser_report.get("failures") or [])
    except Exception as exc:  # noqa: BLE001
        ensure(api_checks, "gateway.pack.exception", False, {"error": str(exc)})
        browser_failures.append("pack.exception")
    finally:
        for root_id in [created_root_id, browser_root_id, browser_attachment_root_id, browser_flow_root_id]:
            if not root_id or not token:
                continue
            try:
                if not delete_checklist(opener, token, root_id):
                    warnings.append(ROOT_DELETE_WARNING)
            except Exception:  # noqa: BLE001
                warnings.append(ROOT_DELETE_WARNING)

    api_failures = [item["name"] for item in api_checks if not item["ok"]]
    ok = not api_failures and not browser_failures and bool(browser_report.get("ok", False))
    report = {
        "generatedAt": Path.cwd().stat().st_mtime,
        "uiUrl": UI_URL,
        "serviceRoot": SERVICE_ROOT,
        "status": "ok" if ok else "failed",
        "createdRootId": created_root_id,
        "browserRootId": browser_root_id,
        "api": {
            "ok": not api_failures,
            "checks": api_checks,
            "failures": api_failures,
        },
        "browser": browser_report,
        "warnings": warnings,
    }
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
    return 1 if not ok else 0


if __name__ == "__main__":
    raise SystemExit(main())
