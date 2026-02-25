#!/usr/bin/env python3
"""Browser smoke: analytics dialog + export flow usecase sanity on Search page."""

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
                    'sap_ui5/service/usecase/SearchAnalyticsDialogExportFlowUseCase'
                ], function (UseCase) {
                    var state = {
                        opened: 0,
                        closed: 0,
                        loaded: 0,
                        emptyCalls: 0,
                        successCalls: 0,
                        errorCalls: 0
                    };

                    var dialog = {
                        open: function () { state.opened += 1; },
                        close: function () { state.closed += 1; }
                    };

                    UseCase.openAnalyticsDialog(dialog, function () { state.loaded += 1; });
                    UseCase.closeAnalyticsDialog(dialog);

                    UseCase.runExportFlow({
                        runWithLoading: function (fn) { return fn(); },
                        buildExportPromise: function () { return Promise.resolve({ rows: [] }); },
                        onEmpty: function () { state.emptyCalls += 1; },
                        onSuccess: function () { state.successCalls += 1; },
                        onError: function () { state.errorCalls += 1; }
                    }).then(function () {
                        return UseCase.runExportFlow({
                            runWithLoading: function (fn) { return fn(); },
                            buildExportPromise: function () { return Promise.reject(new Error('forced export fail')); },
                            onEmpty: function () { state.emptyCalls += 1; },
                            onSuccess: function () { state.successCalls += 1; },
                            onError: function () { state.errorCalls += 1; }
                        });
                    }).then(function () {
                        var ok = state.opened === 1
                            && state.loaded === 1
                            && state.closed === 1
                            && state.emptyCalls === 1
                            && state.errorCalls === 1
                            && state.successCalls === 0;
                        resolve({ ok: ok, state: state });
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
