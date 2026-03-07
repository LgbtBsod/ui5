#!/usr/bin/env python3
"""Browser smoke: facade contract coverage for domain APIs."""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright


UI_URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"
REPORT_PATH = Path("docs/artifacts/browser-smoke-domain-facade-contract.json")


def ensure(checks: list[dict[str, Any]], name: str, ok: bool, detail: Any) -> None:
    checks.append({"name": name, "ok": bool(ok), "detail": detail})


def main() -> int:
    checks: list[dict[str, Any]] = []
    failures: list[str] = []

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch()
            page = browser.new_page(viewport={"width": 1440, "height": 900})
            page.goto(UI_URL, wait_until="networkidle", timeout=90000)

            result = page.evaluate(
                """
                () => new Promise((resolve) => {
                    sap.ui.require([
                        'checklist/app/service/domain/detail/DetailFacade',
                        'checklist/app/service/domain/search/SearchFacade',
                        'checklist/app/service/domain/search/ExportFacade',
                        'checklist/app/facades/AppFacade',
                        'checklist/app/facades/LockFacade'
                    ], function (DetailFacade, SearchFacade, ExportFacade, AppFacade, LockFacade) {
                        var detail = new DetailFacade();
                        var search = new SearchFacade();
                        var exp = new ExportFacade();
                        var out = {
                            detailMethods: [
                                'open', 'enterEdit', 'confirmTakeover', 'cancelEnterEdit',
                                'discardChanges', 'onLockLost', 'forceReadOnly', 'closeDetail',
                                'save', 'validate', 'autosave', 'close', 'deleteChecklist',
                                'changeStatus', 'resolveConflict', 'attachmentUpload',
                                'attachmentDelete', 'rowOps', 'valueHelpLocation', 'personSuggest'
                            ],
                            searchMethods: [
                                'bootstrap', 'buildFilter', 'executeSearch', 'rebind',
                                'selectRow', 'selectionChanged', 'exportFlow',
                                'analytics', 'applyRebindPolicy'
                            ],
                            exportMethods: ['exportFlow', 'exportEntity'],
                            appMethods: ['confirmTestUser'],
                            lockMethods: ['release']
                        };

                        function hasMethods(obj, names) {
                            return names.every(function (name) { return typeof obj[name] === 'function'; });
                        }

                        resolve({
                            detailOk: hasMethods(detail, out.detailMethods),
                            searchOk: hasMethods(search, out.searchMethods),
                            exportOk: hasMethods(exp, out.exportMethods),
                            appOk: hasMethods(AppFacade, out.appMethods),
                            lockOk: hasMethods(LockFacade, out.lockMethods),
                            ok: hasMethods(detail, out.detailMethods)
                                && hasMethods(search, out.searchMethods)
                                && hasMethods(exp, out.exportMethods)
                                && hasMethods(AppFacade, out.appMethods)
                                && hasMethods(LockFacade, out.lockMethods),
                            matrix: out
                        });
                    });
                })
                """
            )

            browser.close()

            matrix = result.get("matrix") or {}
            detail_ok = bool(result.get("detailOk"))
            search_ok = bool(result.get("searchOk"))
            export_ok = bool(result.get("exportOk"))
            app_ok = bool(result.get("appOk"))
            lock_ok = bool(result.get("lockOk"))

            ensure(checks, "facade.detail.contract", detail_ok, {"methods": matrix.get("detailMethods", [])})
            ensure(checks, "facade.search.contract", search_ok, {"methods": matrix.get("searchMethods", [])})
            ensure(checks, "facade.export.contract", export_ok, {"methods": matrix.get("exportMethods", [])})
            ensure(checks, "facade.app.contract", app_ok, {"methods": matrix.get("appMethods", [])})
            ensure(checks, "facade.lock.contract", lock_ok, {"methods": matrix.get("lockMethods", [])})

            if not result.get("ok"):
                failures.append("facade.contract.mismatch")
    except Exception as exc:  # noqa: BLE001
        failures.append("facade.contract.exception")
        ensure(checks, "facade.contract.exception", False, {"error": str(exc)})

    report = {
        "generatedAt": int(time.time()),
        "uiUrl": UI_URL,
        "ok": not failures,
        "checks": checks,
        "failures": failures,
    }
    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)
    REPORT_PATH.write_text(json.dumps(report, ensure_ascii=False, indent=2), encoding="utf-8")
    sys.stdout.buffer.write((json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
