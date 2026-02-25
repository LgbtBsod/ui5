#!/usr/bin/env python3
"""Browser smoke: detail expanded rows lifecycle (open/add/delete/close + selected-model sync)."""

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
                    'sap_ui5/service/usecase/DetailExpandedRowsFlowUseCase'
                ], function (ExpandedRowsFlowUseCase) {
                    var selectedState = {
                        '/checks': [{ id: 'c-1', selected: false }],
                        '/barriers': [{ id: 'b-1', selected: false }]
                    };
                    var trace = {
                        dialogOpened: 0,
                        dialogClosed: 0,
                        addDelta: 0,
                        deleteResult: null,
                        syncCalls: 0
                    };

                    var selectedModel = {
                        getProperty: function (path) {
                            return selectedState[path];
                        },
                        setProperty: function (path, value) {
                            selectedState[path] = value;
                        }
                    };

                    var host = {
                        _pChecksDialog: null
                    };
                    var fakeDialog = {
                        open: function () { trace.dialogOpened += 1; },
                        close: function () { trace.dialogClosed += 1; }
                    };

                    ExpandedRowsFlowUseCase.openExpandedDialogByType({
                        type: 'checks',
                        host: host,
                        view: { getId: function () { return 'V'; }, addDependent: function () {} },
                        controller: {}
                    }).then(function () {
                        // override with deterministic close target for the close call below
                        ExpandedRowsFlowUseCase.closeExpandedDialogByType({
                            type: 'checks',
                            byId: function () { return fakeDialog; }
                        });

                        var beforeAdd = (selectedState['/checks'] || []).length;
                        ExpandedRowsFlowUseCase.addRowByType({
                            type: 'checks',
                            selectedModel: selectedModel,
                            onMutated: function () { trace.syncCalls += 1; }
                        });
                        var afterAdd = (selectedState['/checks'] || []).length;
                        trace.addDelta = afterAdd - beforeAdd;

                        trace.deleteResult = ExpandedRowsFlowUseCase.deleteRowByType({
                            event: { id: 'event' },
                            type: 'checks',
                            deleteRowFromEvent: function () { return { deleted: true }; },
                            onSyncSelectionMeta: function () { trace.syncCalls += 1; }
                        });

                        resolve({
                            ok: trace.dialogOpened === 1
                              && trace.dialogClosed === 1
                              && trace.addDelta === 1
                              && trace.deleteResult && trace.deleteResult.deleted === true
                              && trace.syncCalls === 2,
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
