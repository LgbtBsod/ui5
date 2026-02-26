#!/usr/bin/env python3
"""Browser smoke: inline analytics rail presentation lifecycle."""

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
                    'sap_ui5/service/usecase/SearchInlineAnalyticsPresentationUseCase'
                ], function (mod) {
                    var viewData = {};
                    var viewModel = {
                        setProperty: function (k, v) { viewData[k] = v; },
                        getProperty: function (k) { return viewData[k]; }
                    };
                    var bundle = {
                        getText: function (k) {
                            var m = {
                                analyticsSourceBackend: 'Backend aggregate',
                                analyticsSourceFallback: 'Client fallback'
                            };
                            return m[k] || k;
                        }
                    };

                    var backendMapped = mod.applyInlineAnalyticsPresentation({
                        viewModel: viewModel,
                        bundle: bundle,
                        analytics: {
                            total: 25,
                            failedChecks: 3,
                            failedBarriers: 1,
                            healthy: 21,
                            avgChecksRate: 91,
                            avgBarriersRate: 98,
                            refreshedAt: '10:22:33',
                            source: 'backend'
                        }
                    });

                    var fallbackMapped = mod.applyInlineAnalyticsPresentation({
                        viewModel: viewModel,
                        bundle: bundle,
                        analytics: {
                            total: 10,
                            failedChecks: 2,
                            failedBarriers: 2,
                            healthy: 8,
                            avgChecksRate: 75,
                            avgBarriersRate: 80,
                            refreshedAt: '',
                            source: 'fallback'
                        }
                    });

                    resolve({
                        ok: backendMapped.sourceText === 'Backend aggregate'
                          && backendMapped.total === 25
                          && fallbackMapped.sourceText === 'Client fallback'
                          && fallbackMapped.refreshedAt === '-'
                          && viewData['/analyticsRail'].source === 'fallback',
                        trace: {
                            backendMapped: backendMapped,
                            fallbackMapped: fallbackMapped,
                            viewRail: viewData['/analyticsRail']
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
