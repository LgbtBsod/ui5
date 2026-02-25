#!/usr/bin/env python3
"""Browser smoke: analytics dialog lifecycle (open -> load success/fallback -> close/reopen)."""

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
                    'sap_ui5/service/usecase/SearchWorkflowAnalyticsDialogUseCase',
                    'sap_ui5/service/usecase/SearchAnalyticsDialogExportFlowUseCase'
                ], function (AnalyticsDialogUseCase, DialogExportFlowUseCase) {
                    var vmState = {};
                    var viewModel = {
                        setProperty: function (path, value) { vmState[path] = value; }
                    };

                    var opened = 0;
                    var closed = 0;
                    var dialog = {
                        open: function () { opened += 1; },
                        close: function () { closed += 1; }
                    };

                    AnalyticsDialogUseCase.openDialogLifecycle({
                        dialog: dialog,
                        runLoad: function () {
                            return AnalyticsDialogUseCase.runAnalyticsLoadFlow({
                                viewModel: viewModel,
                                loadAnalytics: function () {
                                    return Promise.resolve({ source: 'backend', total: 4 });
                                }
                            });
                        },
                        openDialog: DialogExportFlowUseCase.openAnalyticsDialog
                    });

                    Promise.resolve().then(function () {
                        var firstOpenOk = opened === 1
                          && vmState['/analyticsBusy'] === false
                          && vmState['/analyticsError'] === ''
                          && vmState['/analytics'].source === 'backend';

                        AnalyticsDialogUseCase.closeDialogLifecycle({
                            dialog: dialog,
                            closeDialog: DialogExportFlowUseCase.closeAnalyticsDialog
                        });

                        return AnalyticsDialogUseCase.runAnalyticsLoadFlow({
                            viewModel: viewModel,
                            loadAnalytics: function () {
                                return Promise.resolve({ total: 2 }); // source should normalize to fallback
                            }
                        }).then(function () {
                            return AnalyticsDialogUseCase.runAnalyticsLoadFlow({
                                viewModel: viewModel,
                                loadAnalytics: function () {
                                    return Promise.reject(new Error('analytics-load-failed'));
                                }
                            }).then(function () {
                                AnalyticsDialogUseCase.openDialogLifecycle({
                                    dialog: dialog,
                                    runLoad: function () {},
                                    openDialog: DialogExportFlowUseCase.openAnalyticsDialog
                                });

                                resolve({
                                    ok: firstOpenOk
                                      && closed === 1
                                      && vmState['/analytics'].source === 'fallback'
                                      && vmState['/analyticsError'] === 'analytics-load-failed'
                                      && vmState['/analyticsBusy'] === false
                                      && opened === 2,
                                    trace: {
                                        opened: opened,
                                        closed: closed,
                                        analytics: vmState['/analytics'],
                                        analyticsError: vmState['/analyticsError'],
                                        analyticsBusy: vmState['/analyticsBusy']
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
