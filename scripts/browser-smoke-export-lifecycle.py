#!/usr/bin/env python3
"""Browser smoke: export lifecycle (default/specific entity + empty/success/error branches)."""

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
                    'sap_ui5/service/usecase/SearchExportOrchestrationUseCase',
                    'sap_ui5/service/usecase/SearchAnalyticsExportUseCase',
                    'sap_ui5/service/usecase/SearchAnalyticsDialogExportFlowUseCase'
                ], function (ExportOrchestrationUseCase, AnalyticsExportUseCase, DialogExportFlowUseCase) {
                    var trace = {
                        defaultEntity: '',
                        specificEntity: '',
                        successRows: 0,
                        emptyCalls: 0,
                        errorCalls: 0,
                        filenamePrefixOk: false
                    };

                    trace.defaultEntity = ExportOrchestrationUseCase.resolveExportEntityFromAction({
                        defaultEntity: 'screen'
                    });

                    trace.specificEntity = ExportOrchestrationUseCase.resolveExportEntityFromAction({
                        event: {
                            getParameter: function () {
                                return {
                                    data: function (key) { return key === 'entity' ? 'check' : null; }
                                };
                            }
                        },
                        defaultEntity: 'screen',
                        resolveEntityFromMenuEvent: function (evt, fallback) {
                            var item = evt.getParameter('item');
                            return item ? item.data('entity') : fallback;
                        }
                    });

                    var filename = ExportOrchestrationUseCase.buildExportFilename('barrier', function () { return 123; });
                    trace.filenamePrefixOk = filename === 'checklist_barrier_123';

                    ExportOrchestrationUseCase.runExportLifecycle({
                        runExportFlow: DialogExportFlowUseCase.runExportFlow,
                        runWithLoading: function (fnTask) { return fnTask(); },
                        buildExportPromise: function () {
                            return AnalyticsExportUseCase.buildExportPromise('screen', function () {
                                return [{ id: 'S-1' }, { id: 'S-2' }];
                            }, function () { return Promise.resolve({ rows: [] }); });
                        },
                        onEmpty: function () { trace.emptyCalls += 1; },
                        onSuccess: function (rows) { trace.successRows += rows.length; },
                        onError: function () { trace.errorCalls += 1; }
                    }).then(function () {
                        return ExportOrchestrationUseCase.runExportLifecycle({
                            runExportFlow: DialogExportFlowUseCase.runExportFlow,
                            runWithLoading: function (fnTask) { return fnTask(); },
                            buildExportPromise: function () { return Promise.resolve({ rows: [] }); },
                            onEmpty: function () { trace.emptyCalls += 1; },
                            onSuccess: function () {},
                            onError: function () {}
                        });
                    }).then(function () {
                        return ExportOrchestrationUseCase.runExportLifecycle({
                            runExportFlow: DialogExportFlowUseCase.runExportFlow,
                            runWithLoading: function (fnTask) { return fnTask(); },
                            buildExportPromise: function () { return Promise.reject(new Error('export-failed')); },
                            onEmpty: function () {},
                            onSuccess: function () {},
                            onError: function () { trace.errorCalls += 1; }
                        });
                    }).then(function () {
                        resolve({
                            ok: trace.defaultEntity === 'screen'
                              && trace.specificEntity === 'check'
                              && trace.successRows === 2
                              && trace.emptyCalls === 1
                              && trace.errorCalls === 1
                              && trace.filenamePrefixOk,
                            trace: trace
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
