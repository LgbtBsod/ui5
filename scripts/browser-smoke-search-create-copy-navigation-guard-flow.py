#!/usr/bin/env python3
"""Browser smoke: search create/copy navigation guard flow."""

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
                    'sap_ui5/service/usecase/SearchCreateCopyNavigationGuardUseCase'
                ], function (mod) {
                    var nav = 0;

                    mod.runCopyNavigationFlow({
                        resolveSelectedId: function () { return ''; },
                        confirmNavigation: function () { return Promise.resolve(true); },
                        buildCopyIntent: function (id) { return { route: 'detail', routeParams: { id: id } }; },
                        applyIntent: function () { nav += 1; return true; }
                    }).then(function (missingSelection) {
                        mod.runCreateNavigationFlow({
                            confirmNavigation: function () { return Promise.resolve(false); },
                            buildCreateIntent: function () { return { route: 'detail', routeParams: { id: '__create' } }; },
                            applyIntent: function () { nav += 1; return true; }
                        }).then(function (cancelledCreate) {
                            mod.runCopyNavigationFlow({
                                resolveSelectedId: function () { return 'CHK-1'; },
                                confirmNavigation: function () { return Promise.resolve(true); },
                                buildCopyIntent: function (id) { return { route: 'detail', routeParams: { id: id } }; },
                                applyIntent: function () { nav += 1; return true; }
                            }).then(function (proceedCopy) {
                                resolve({
                                    ok: missingSelection.reason === 'missing_selection'
                                      && cancelledCreate.reason === 'cancelled'
                                      && proceedCopy.ok === true
                                      && proceedCopy.reason === 'applied'
                                      && nav === 1,
                                    trace: {
                                        missingSelection: missingSelection.reason,
                                        cancelledCreate: cancelledCreate.reason,
                                        proceedCopy: proceedCopy.reason,
                                        navCalls: nav
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
