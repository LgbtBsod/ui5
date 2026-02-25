#!/usr/bin/env python3
"""Browser smoke: search open-detail guard flow (missing-id/cancel/proceed branches)."""

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
                    'sap_ui5/service/usecase/SearchOpenDetailGuardUseCase'
                ], function (mod) {
                    var navCalls = 0;

                    mod.runOpenDetailFlow({ id: '' }).then(function (missingId) {
                        mod.runOpenDetailFlow({
                            id: 'CHK-CANCEL',
                            confirmNavigation: function () { return Promise.resolve(false); },
                            buildIntent: function (id) { return { route: 'detail', routeParams: { id: id } }; },
                            applyIntent: function () { navCalls += 1; return true; }
                        }).then(function (cancelled) {
                            mod.runOpenDetailFlow({
                                id: 'CHK-NO-ROUTER',
                                confirmNavigation: function () { return Promise.resolve(true); },
                                buildIntent: function (id) { return { route: 'detail', routeParams: { id: id } }; },
                                applyIntent: function () { return false; }
                            }).then(function (missingRouter) {
                                mod.runOpenDetailFlow({
                                    id: 'CHK-PROCEED',
                                    confirmNavigation: function () { return Promise.resolve(true); },
                                    buildIntent: function (id) { return { route: 'detail', routeParams: { id: id } }; },
                                    applyIntent: function () { navCalls += 1; return true; }
                                }).then(function (proceed) {
                                    resolve({
                                        ok: missingId.reason === 'missing_id'
                                          && cancelled.reason === 'cancelled'
                                          && missingRouter.reason === 'missing_router_adapter'
                                          && proceed.ok === true
                                          && proceed.reason === 'applied'
                                          && navCalls === 1,
                                        trace: {
                                            missingId: missingId.reason,
                                            cancelled: cancelled.reason,
                                            missingRouter: missingRouter.reason,
                                            proceed: proceed.reason,
                                            navCalls: navCalls
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
