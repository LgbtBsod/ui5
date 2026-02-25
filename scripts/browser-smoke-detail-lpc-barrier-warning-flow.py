#!/usr/bin/env python3
"""Browser smoke: LPC barrier warning flow (prompt + confirm/reject branches)."""

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
                    'sap_ui5/service/usecase/DetailLpcBarrierWarningFlowUseCase'
                ], function (mod) {
                    var stateReject = {};
                    var stateConfirm = {};
                    var warningCalls = 0;
                    var warningText = '';

                    var messageBoxReject = {
                        Action: { YES: 'YES', NO: 'NO' },
                        warning: function (text, opts) {
                            warningCalls += 1;
                            warningText = text;
                            opts.onClose('NO');
                        }
                    };

                    var messageBoxConfirm = {
                        Action: { YES: 'YES', NO: 'NO' },
                        warning: function (_text, opts) {
                            warningCalls += 1;
                            opts.onClose('YES');
                        }
                    };

                    mod.openWarningDialog({
                        messageBox: messageBoxReject,
                        promptText: 'Barrier reset warning',
                        barrierAllowed: false,
                        barriers: [{ id: 'B-1' }],
                        selectedModel: { setProperty: function (path, value) { stateReject[path] = value; } }
                    }).then(function (openedReject) {
                        mod.openWarningDialog({
                            messageBox: messageBoxConfirm,
                            promptText: 'Barrier reset warning',
                            barrierAllowed: false,
                            barriers: [{ id: 'B-1' }],
                            selectedModel: { setProperty: function (path, value) { stateConfirm[path] = value; } }
                        }).then(function (openedConfirm) {
                            mod.openWarningDialog({
                                messageBox: messageBoxConfirm,
                                promptText: 'should-not-open',
                                barrierAllowed: true,
                                barriers: [{ id: 'B-1' }],
                                selectedModel: { setProperty: function () {} }
                            }).then(function (openedSkipped) {
                                resolve({
                                    ok: openedReject === true
                                      && openedConfirm === true
                                      && openedSkipped === false
                                      && stateReject['/basic/LPC_KEY'] === ''
                                      && stateReject['/basic/LPC_TEXT'] === ''
                                      && Array.isArray(stateConfirm['/barriers'])
                                      && stateConfirm['/barriers'].length === 0
                                      && warningText === 'Barrier reset warning'
                                      && warningCalls === 2,
                                    trace: {
                                        openedReject: openedReject,
                                        openedConfirm: openedConfirm,
                                        openedSkipped: openedSkipped,
                                        warningCalls: warningCalls,
                                        rejectLpcKey: stateReject['/basic/LPC_KEY'] || '',
                                        confirmBarriersLength: (stateConfirm['/barriers'] || []).length
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
