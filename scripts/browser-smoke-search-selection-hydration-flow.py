#!/usr/bin/env python3
"""Browser smoke: search selection hydration flow (missing-id/backend-miss/backend-error)."""

import json
import sys
from playwright.sync_api import sync_playwright

URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"


def main():
    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": 1440, "height": 900})
        page.goto(URL, wait_until="networkidle", timeout=90000)

        result = page.evaluate(
            """
            () => new Promise((resolve) => {
                sap.ui.require([
                    'sap_ui5/service/usecase/SearchSelectionHydrationUseCase'
                ], function (mod) {
                    var selected = { data: null, setData: function (v) { this.data = v; } };
                    var view = { m: {}, setProperty: function (k, v) { this.m[k] = v; } };

                    mod.runSelectionHydration({ id: '', selectedModel: selected, viewModel: view }).then(function (missingId) {
                        mod.runSelectionHydration({
                            id: 'CHK-MISS',
                            selectedModel: selected,
                            viewModel: view,
                            loadChecklistById: function () { return Promise.resolve({}); }
                        }).then(function (backendMiss) {
                            mod.runSelectionHydration({
                                id: 'CHK-ERR',
                                selectedModel: selected,
                                viewModel: view,
                                loadChecklistById: function () { return Promise.reject(new Error('backend-down')); }
                            }).then(function (backendErr) {
                                resolve({
                                    ok: missingId.reason === 'missing_id'
                                      && backendMiss.ok === true
                                      && backendMiss.checklist.root.id === 'CHK-MISS'
                                      && backendErr.ok === false
                                      && backendErr.reason === 'load_error'
                                      && backendErr.checklist.root.id === 'CHK-ERR'
                                      && view.m['/hasSelection'] === true,
                                    trace: {
                                        missingId: missingId.reason,
                                        backendMissId: backendMiss.checklist.root.id,
                                        backendError: backendErr.reason,
                                        selectedId: selected.data && selected.data.root && selected.data.root.id
                                    }
                                });
                            });
                        });
                    });
                });
            })
            """
        )

        browser.close()
        print(json.dumps(result, ensure_ascii=False))
        if not result.get("ok"):
            return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
