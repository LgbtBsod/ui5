#!/usr/bin/env python3
"""Browser smoke: search summary/workflow stage sync flow."""

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
                    'sap_ui5/service/usecase/SearchSummaryPresentationUseCase'
                ], function (mod) {
                    var viewData = {};
                    var viewModel = { setProperty: function (p, v) { viewData[p] = v; } };
                    var bundle = {
                        getText: function (key, params) {
                            if (key === 'resultSummary') {
                                return String((params || [])[0] || 0) + ' of ' + String((params || [])[1] || 0);
                            }
                            throw new Error('missing-key');
                        }
                    };

                    var searchApplied = mod.applySummaryPresentation({
                        viewModel: viewModel,
                        bundle: bundle,
                        kpi: { visible: 4, total: 10, failedChecks: 3, failedBarriers: 2, healthy: 5, workflowStage: 'ANALYZE' },
                        visible: 4,
                        total: 10,
                        lastUpdatedAt: '10:00:01'
                    });

                    var resetApplied = mod.applySummaryPresentation({
                        viewModel: viewModel,
                        bundle: bundle,
                        kpi: { visible: 0, total: 0, failedChecks: 0, failedBarriers: 0, healthy: 0, workflowStage: 'DISCOVER' },
                        visible: 0,
                        total: 0,
                        lastUpdatedAt: '10:00:02'
                    });

                    var retryApplied = mod.applySummaryPresentation({
                        viewModel: viewModel,
                        bundle: bundle,
                        kpi: { visible: 1, total: 3, failedChecks: 0, failedBarriers: 0, healthy: 1, workflowStage: 'REVIEW' },
                        visible: 1,
                        total: 3,
                        lastUpdatedAt: '10:00:03'
                    });

                    var dataReceivedApplied = mod.applySummaryPresentation({
                        viewModel: viewModel,
                        bundle: bundle,
                        kpi: { visible: 'broken', total: 8, failedChecks: 2, failedBarriers: 1, healthy: 5, workflowStage: 'UNKNOWN' },
                        visible: 'bad',
                        total: 8,
                        lastUpdatedAt: '10:00:04'
                    });

                    resolve({
                        ok: searchApplied.ok === true
                          && resetApplied.ok === true
                          && retryApplied.ok === true
                          && dataReceivedApplied.ok === true
                          && viewData['/resultSummary'] === '0 of 8'
                          && viewData['/workflowStage'] === 'DISCOVER'
                          && viewData['/lastUpdatedAt'] === '10:00:04',
                        trace: {
                            resultSummary: viewData['/resultSummary'],
                            workflowStage: viewData['/workflowStage'],
                            lastUpdatedAt: viewData['/lastUpdatedAt']
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
