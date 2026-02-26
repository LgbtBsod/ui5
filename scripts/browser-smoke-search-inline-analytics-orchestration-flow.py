#!/usr/bin/env python3
"""Browser smoke: inline analytics refresh orchestration trigger matrix + idempotent behavior."""

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
                    'sap_ui5/service/usecase/SearchInlineAnalyticsRefreshOrchestrationUseCase'
                ], function (mod) {
                    var viewData = { '/analytics': { total: 1 } };
                    var viewModel = {
                        setProperty: function (k, v) { viewData[k] = v; },
                        getProperty: function (k) { return viewData[k]; }
                    };

                    var state = mod.ensureRefreshState({});
                    var applied = [];
                    var calls = 0;

                    function delayedLoad(value, delayMs) {
                        return function () {
                            calls += 1;
                            return new Promise(function (resolveLoad) {
                                setTimeout(function () {
                                    resolveLoad(value);
                                }, delayMs);
                            });
                        };
                    }

                    Promise.all([
                        mod.runRefreshLifecycle({
                            refreshState: state,
                            viewModel: viewModel,
                            loadAnalytics: delayedLoad({ total: 1 }, 25),
                            applyPresentation: function (a) { applied.push('first-' + (a.total || 0)); }
                        }),
                        mod.runRefreshLifecycle({
                            refreshState: state,
                            viewModel: viewModel,
                            loadAnalytics: delayedLoad({ total: 2 }, 5),
                            applyPresentation: function (a) { applied.push('second-' + (a.total || 0)); }
                        })
                    ]).then(function (pair) {
                        var supported = [
                            'ROUTE_MATCHED',
                            'SMART_SEARCH',
                            'FALLBACK_SEARCH',
                            'RETRY_LOAD',
                            'RESET_FILTERS',
                            'SEARCH_MODE_TOGGLE'
                        ].every(function (k) { return mod.shouldRefreshForTrigger(k); });

                        var unsupported = mod.shouldRefreshForTrigger('UNKNOWN_TRIGGER') === false;
                        var staleDetected = pair.some(function (r) { return !!r.stale; });

                        resolve({
                            ok: supported
                              && unsupported
                              && staleDetected
                              && applied.length === 1
                              && applied[0] === 'second-2'
                              && calls === 2,
                            trace: {
                                pair: pair,
                                applied: applied,
                                calls: calls,
                                latestRequestId: state.latestRequestId
                            }
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
