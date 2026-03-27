#!/usr/bin/env python3
"""Browser smoke: facade contract coverage for domain APIs."""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path
from typing import Any

from playwright.sync_api import sync_playwright
from browser_route_bootstrap import navigate_to_search, safe_evaluate, wait_for_search_ready


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
            page.goto(UI_URL, wait_until="domcontentloaded", timeout=90000)
            navigate_to_search(page)
            wait_for_search_ready(page, timeout=30000)

            result = safe_evaluate(
                page,
                """
                () => new Promise((resolve) => {
                    sap.ui.require([
                        'PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailFacade',
                        'PRODUCTION_CONTROL_CHECKLIST/service/domain/search/SearchFacade'
                    ], function (DetailFacade, SearchFacade) {
                        var detail = new DetailFacade();
                        var search = new SearchFacade();
                        var out = {
                            detailMethods: [
                                'open', 'enterEdit', 'confirmTakeover', 'cancelEnterEdit',
                                'discardChanges', 'onLockLost', 'forceReadOnly', 'closeDetail',
                                'save', 'validate', 'autosave', 'close', 'deleteChecklist',
                                'resolveConflict', 'attachmentLoad', 'attachmentUpload',
                                'attachmentDelete', 'rowOps', 'valueHelpLocation', 'personSuggest'
                            ],
                            searchMethods: [
                                'bootstrap', 'buildFilter', 'executeSearch', 'rebind',
                                'selectRow', 'selectionChanged', 'exportFlow',
                                'analytics', 'applyRebindPolicy'
                            ]
                        };

                        function hasMethods(obj, names) {
                            return names.every(function (name) { return typeof obj[name] === 'function'; });
                        }

                        resolve({
                            detailOk: hasMethods(detail, out.detailMethods),
                            searchOk: hasMethods(search, out.searchMethods),
                            ok: hasMethods(detail, out.detailMethods)
                                && hasMethods(search, out.searchMethods),
                            matrix: out
                        });
                    });
                })
                """,
            )
            page.wait_for_load_state("networkidle", timeout=30000)
            page.wait_for_timeout(1500)

            browser.close()

            matrix = result.get("matrix") or {}
            detail_ok = bool(result.get("detailOk"))
            search_ok = bool(result.get("searchOk"))

            ensure(checks, "facade.detail.contract", detail_ok, {"methods": matrix.get("detailMethods", [])})
            ensure(checks, "facade.search.contract", search_ok, {"methods": matrix.get("searchMethods", [])})

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
