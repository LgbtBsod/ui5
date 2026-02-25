#!/usr/bin/env python3
"""Browser smoke: search empty-state message flow."""

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
                    'sap_ui5/service/usecase/SearchEmptyStatePresentationUseCase'
                ], function (mod) {
                    var viewData = { '/useSmartControls': true, '/loadError': false };
                    var view = {
                        setProperty: function (k, v) { viewData[k] = v; },
                        getProperty: function (k) { return viewData[k]; }
                    };
                    var data = { values: [], getProperty: function () { return this.values; } };
                    var table = { text: '', setNoDataText: function (t) { this.text = t; } };
                    var bundle = {
                        getText: function (k) {
                            var m = { noDataDefault: 'No checklists found.', noDataLoadError: 'Unable to load data.' };
                            if (!m[k]) { throw new Error('missing-key'); }
                            return m[k];
                        }
                    };

                    data.values = [{ id: '1' }];
                    var hasData = mod.applyEmptyStatePresentation({ viewModel: view, dataModel: data, bundle: bundle, table: table });

                    data.values = [];
                    viewData['/loadError'] = true;
                    var loadError = mod.applyEmptyStatePresentation({ viewModel: view, dataModel: data, bundle: bundle, table: table });

                    viewData['/loadError'] = false;
                    viewData['/useSmartControls'] = false;
                    var degraded = mod.applyEmptyStatePresentation({ viewModel: view, dataModel: data, bundle: bundle, table: table });

                    var unknown = mod.resolveNoDataText({ kind: 'unknown-kind', bundle: bundle, unknownFallbackKey: 'missing-key' });

                    resolve({
                        ok: hasData.ok === true
                          && loadError.kind === 'load_error'
                          && degraded.kind === 'smart_degraded_empty'
                          && viewData['/noDataText'] === 'No checklists found.'
                          && table.text === 'No checklists found.'
                          && unknown === 'No data',
                        trace: {
                            hasDataKind: hasData.kind,
                            loadErrorKind: loadError.kind,
                            degradedKind: degraded.kind,
                            noDataText: viewData['/noDataText'],
                            unknownText: unknown
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
