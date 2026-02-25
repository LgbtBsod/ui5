#!/usr/bin/env python3
"""Browser smoke: search delete orchestration flow (missing-id/success/error branches)."""

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
                    'sap_ui5/service/usecase/SearchDeleteOrchestrationUseCase'
                ], function (mod) {
                    var trace = { applyRows: 0, rebind: 0, clearSelection: 0, reloadSelectionState: 0 };

                    Promise.resolve()
                        .then(function () {
                            return mod.runDeleteFlow({
                                resolveSelectedId: function () { return ''; }
                            });
                        })
                        .then(function (missingId) {
                            return mod.runDeleteFlow({
                                resolveSelectedId: function () { return 'CHK-100'; },
                                runWithLoading: function (fnTask) { return fnTask(); },
                                deleteAndReload: function (sId) { return Promise.resolve([{ root: { id: sId } }]); },
                                applyRows: function () { trace.applyRows += 1; },
                                rebind: function () { trace.rebind += 1; },
                                applySelectedChecklist: function (oChecklist) {
                                    if (!oChecklist || Object.keys(oChecklist).length === 0) {
                                        trace.clearSelection += 1;
                                    }
                                },
                                reloadSelectionState: function () { trace.reloadSelectionState += 1; }
                            }).then(function (successResult) {
                                return { missingId: missingId, successResult: successResult };
                            });
                        })
                        .then(function (state) {
                            return mod.runDeleteFlow({
                                resolveSelectedId: function () { return 'CHK-ERR'; },
                                runWithLoading: function (fnTask) { return fnTask(); },
                                deleteAndReload: function () { return Promise.reject(new Error('backend-delete-failed')); }
                            }).then(function (errorResult) {
                                resolve({
                                    ok: state.missingId.reason === 'missing_id'
                                      && state.successResult.ok === true
                                      && state.successResult.rows.length === 1
                                      && trace.applyRows === 1
                                      && trace.rebind === 1
                                      && trace.clearSelection === 1
                                      && trace.reloadSelectionState === 1
                                      && errorResult.ok === false
                                      && errorResult.reason === 'delete_error',
                                    trace: {
                                        missingReason: state.missingId.reason,
                                        successRows: state.successResult.rows.length,
                                        applyRows: trace.applyRows,
                                        rebind: trace.rebind,
                                        clearSelection: trace.clearSelection,
                                        reloadSelectionState: trace.reloadSelectionState,
                                        errorReason: errorResult.reason
                                    }
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
