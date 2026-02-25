#!/usr/bin/env python3
"""Browser smoke: integration edit warning flow (yes/no + non-integration branches)."""

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
                    'sap_ui5/service/usecase/DetailIntegrationEditWarningUseCase'
                ], function (mod) {
                    var warningCalls = 0;
                    var capturedTitle = '';

                    mod.confirmIntegrationEdit({
                        selectedRoot: { this_is_integration_data: false },
                        messageBox: null,
                        bundle: { getText: function (k) { return k; } }
                    }).then(function (nonIntegrationAllowed) {
                        mod.confirmIntegrationEdit({
                            selectedRoot: { this_is_integration_data: true },
                            messageBox: {
                                Action: { YES: 'YES', NO: 'NO' },
                                warning: function (_text, opts) {
                                    warningCalls += 1;
                                    capturedTitle = opts.title;
                                    opts.onClose('NO');
                                }
                            },
                            bundle: { getText: function (k) { return k; } }
                        }).then(function (integrationRejected) {
                            mod.confirmIntegrationEdit({
                                selectedRoot: { integrationFlag: true },
                                messageBox: {
                                    Action: { YES: 'YES', NO: 'NO' },
                                    warning: function (_text, opts) {
                                        warningCalls += 1;
                                        opts.onClose('YES');
                                    }
                                },
                                bundle: { getText: function (k) { return k; } }
                            }).then(function (integrationAllowed) {
                                resolve({
                                    ok: nonIntegrationAllowed === true
                                      && integrationRejected === false
                                      && integrationAllowed === true
                                      && warningCalls === 2
                                      && capturedTitle === 'integrationEditTitle',
                                    trace: {
                                        nonIntegrationAllowed: nonIntegrationAllowed,
                                        integrationRejected: integrationRejected,
                                        integrationAllowed: integrationAllowed,
                                        warningCalls: warningCalls,
                                        capturedTitle: capturedTitle
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
