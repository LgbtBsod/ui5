#!/usr/bin/env python3
"""Browser smoke: detail unsaved decision flow (clean/save/discard/cancel branches)."""

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
                    'sap_ui5/service/usecase/DetailUnsavedDecisionFlowUseCase'
                ], function (mod) {
                    var proceedCalls = 0;

                    mod.runUnsavedCloseFlow({
                        isDirty: false,
                        proceed: function () { proceedCalls += 1; }
                    }).then(function (cleanResult) {
                        mod.runUnsavedCloseFlow({
                            isDirty: true,
                            host: {},
                            onSave: function () {},
                            confirmUnsavedAndHandle: function () { return Promise.resolve('CANCEL'); },
                            proceed: function () { proceedCalls += 1; }
                        }).then(function (cancelResult) {
                            mod.runUnsavedCloseFlow({
                                isDirty: true,
                                host: {},
                                onSave: function () {},
                                confirmUnsavedAndHandle: function () { return Promise.resolve('DISCARD'); },
                                proceed: function () { proceedCalls += 1; }
                            }).then(function (discardResult) {
                                var action = mod.buildConfirmUnsavedAction({
                                    host: {},
                                    onSave: function () {},
                                    confirmUnsavedAndHandle: function () { return Promise.resolve('SAVE'); }
                                });
                                action().then(function (saveDecision) {
                                    resolve({
                                        ok: cleanResult.proceeded === true
                                          && cleanResult.decision === 'CLEAN'
                                          && cancelResult.proceeded === false
                                          && cancelResult.decision === 'CANCEL'
                                          && discardResult.proceeded === true
                                          && discardResult.decision === 'DISCARD'
                                          && saveDecision === 'SAVE'
                                          && proceedCalls === 2,
                                        trace: {
                                            cleanDecision: cleanResult.decision,
                                            cancelDecision: cancelResult.decision,
                                            discardDecision: discardResult.decision,
                                            saveDecision: saveDecision,
                                            proceedCalls: proceedCalls
                                        }
                                    });
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
