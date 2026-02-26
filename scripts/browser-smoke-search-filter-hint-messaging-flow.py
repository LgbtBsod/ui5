#!/usr/bin/env python3
"""Browser smoke: search filter-hint messaging flow."""

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
                    'sap_ui5/service/usecase/SearchFilterHintPresentationUseCase'
                ], function (mod) {
                    var viewData = {};
                    var viewModel = {
                        setProperty: function (k, v) { viewData[k] = v; },
                        getProperty: function (k) { return viewData[k]; }
                    };
                    var stateData = {};
                    var stateModel = {
                        getProperty: function (k) { return stateData[k]; }
                    };

                    var bundle = {
                        getText: function (k) {
                            var m = {
                                filterHintSmartActive: 'smart-active',
                                filterHintFallbackActive: 'fallback-active',
                                filterHintCleared: 'cleared'
                            };
                            if (!m[k]) { throw new Error('missing-key'); }
                            return m[k];
                        }
                    };

                    var smart = mod.applyHintPresentation({
                        viewModel: viewModel,
                        stateModel: stateModel,
                        useSmartControls: true,
                        hasSmartFilters: true,
                        fallbackPayload: { filterId: '', filterLpc: '', filterFailedChecks: 'ALL', filterFailedBarriers: 'ALL' },
                        bundle: bundle
                    });

                    var fallback = mod.applyHintPresentation({
                        viewModel: viewModel,
                        stateModel: stateModel,
                        useSmartControls: false,
                        hasSmartFilters: false,
                        fallbackPayload: { filterId: 'A-1', filterLpc: '', filterFailedChecks: 'ALL', filterFailedBarriers: 'ALL' },
                        bundle: bundle
                    });

                    var cleared = mod.applyHintPresentation({
                        viewModel: viewModel,
                        stateModel: stateModel,
                        useSmartControls: true,
                        hasSmartFilters: false,
                        fallbackPayload: { filterId: '', filterLpc: '', filterFailedChecks: 'ALL', filterFailedBarriers: 'ALL' },
                        bundle: bundle
                    });

                    resolve({
                        ok: smart.text === 'smart-active'
                          && smart.visible === true
                          && fallback.text === 'fallback-active'
                          && fallback.type === 'Warning'
                          && cleared.visible === false
                          && viewData['/filterHintVisible'] === false,
                        trace: {
                            smart: smart,
                            fallback: fallback,
                            cleared: cleared,
                            viewData: viewData
                        }
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
