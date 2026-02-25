#!/usr/bin/env python3
"""Browser smoke: search load retry flow (missing loader, empty rows, retry success/error)."""

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
                    'sap_ui5/service/usecase/SearchRetryLoadPresentationUseCase'
                ], function (mod) {
                    var state = {};
                    var data = {};
                    var failCalls = 0;

                    var stateModel = { setProperty: function (p, v) { state[p] = v; } };
                    var dataModel = { setProperty: function (p, v) { data[p] = v; } };

                    mod.runRetryFlow({
                        stateModel: stateModel,
                        dataModel: dataModel,
                        getCheckLists: null
                    }).then(function (missingLoaderRes) {
                        mod.runRetryFlow({
                            stateModel: stateModel,
                            dataModel: dataModel,
                            getCheckLists: function () { return Promise.resolve([]); },
                            treatEmptyAsError: true,
                            emptyRowsMessage: 'empty-rows'
                        }).then(function (emptyRowsRes) {
                            mod.runRetryFlow({
                                stateModel: stateModel,
                                dataModel: dataModel,
                                getCheckLists: function () {
                                    failCalls += 1;
                                    return Promise.reject(new Error('still-failing'));
                                },
                                maxAttempts: 2
                            }).then(function (errorRes) {
                                mod.runRetryFlow({
                                    stateModel: stateModel,
                                    dataModel: dataModel,
                                    getCheckLists: function () { return Promise.resolve([{ id: 'A' }, { id: 'B' }]); }
                                }).then(function (okRes) {
                                    resolve({
                                        ok: missingLoaderRes.reason === 'missing_loader'
                                          && emptyRowsRes.reason === 'empty_rows'
                                          && errorRes.reason === 'error'
                                          && errorRes.attempts === 2
                                          && okRes.ok === true
                                          && (data['/visibleCheckLists'] || []).length === 2
                                          && state['/loadError'] === false,
                                        trace: {
                                            missingLoaderReason: missingLoaderRes.reason,
                                            emptyRowsReason: emptyRowsRes.reason,
                                            errorAttempts: errorRes.attempts,
                                            visibleCount: (data['/visibleCheckLists'] || []).length,
                                            failCalls: failCalls
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
