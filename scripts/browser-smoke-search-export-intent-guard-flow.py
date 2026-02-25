#!/usr/bin/env python3
"""Browser smoke: search export intent guard flow."""

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
                    'sap_ui5/service/usecase/SearchExportIntentGuardUseCase'
                ], function (mod) {
                    var runCalls = 0;

                    mod.runExportIntent({
                        defaultEntity: 'screen',
                        runExport: null
                    }).then(function (missingRunner) {
                        mod.runExportIntent({
                            defaultEntity: 'screen',
                            isEnabled: function () { return false; },
                            runExport: function () { runCalls += 1; return Promise.resolve(); }
                        }).then(function (disabled) {
                            mod.runExportIntent({
                                event: { getParameter: function () { return { data: function () { return 'check'; } }; } },
                                defaultEntity: 'screen',
                                allowedEntities: ['screen', 'barrier', 'check'],
                                resolveEntityFromMenuEvent: function (event, fallback) {
                                    var item = event.getParameter('item');
                                    return item ? item.data('entity') : fallback;
                                },
                                isEnabled: function () { return true; },
                                runExport: function (entity) {
                                    runCalls += 1;
                                    return entity === 'check' ? Promise.resolve() : Promise.reject(new Error('wrong-entity'));
                                }
                            }).then(function (applied) {
                                resolve({
                                    ok: missingRunner.reason === 'missing_run_export'
                                      && disabled.reason === 'disabled'
                                      && applied.ok === true
                                      && applied.reason === 'applied'
                                      && runCalls === 1,
                                    trace: {
                                        missingRunner: missingRunner.reason,
                                        disabled: disabled.reason,
                                        applied: applied.reason,
                                        runCalls: runCalls,
                                        entity: applied.intent && applied.intent.entity
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
