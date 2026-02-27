#!/usr/bin/env python3
"""Browser smoke: SmartFilter/SmartTable are disabled on metadata outage and recover after."""

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
                    'sap_ui5/util/SearchSmartControlCoordinator'
                ], function (SearchSmartControlCoordinator) {
                    var vmState = { '/useSmartControls': true, '/smartControlsReason': '' };
                    var stateBag = { '/mainServiceMetadataOk': true, '/mainServiceMetadataError': '' };
                    var bootstrapCalls = 0;

                    var viewModel = {
                        getProperty: function (k) { return vmState[k]; },
                        setProperty: function (k, v) { vmState[k] = v; }
                    };
                    var stateModel = {
                        getProperty: function (k) { return stateBag[k]; }
                    };

                    function snapshot(tag) {
                        var useSmart = !!vmState['/useSmartControls'];
                        return {
                            tag: tag,
                            useSmartControls: useSmart,
                            smartFilterVisible: useSmart,
                            smartTableVisible: useSmart,
                            reason: vmState['/smartControlsReason'] || ''
                        };
                    }

                    var trace = [];
                    SearchSmartControlCoordinator.syncAvailability({
                        stateModel: stateModel,
                        viewModel: viewModel,
                        unavailableText: 'smartControlsUnavailable',
                        bootstrap: function () { bootstrapCalls += 1; }
                    });
                    trace.push(snapshot('ok-initial'));

                    stateBag['/mainServiceMetadataOk'] = false;
                    stateBag['/mainServiceMetadataError'] = 'metadata-down';
                    SearchSmartControlCoordinator.syncAvailability({
                        stateModel: stateModel,
                        viewModel: viewModel,
                        unavailableText: 'smartControlsUnavailable',
                        bootstrap: function () { bootstrapCalls += 1; }
                    });
                    trace.push(snapshot('degraded'));

                    stateBag['/mainServiceMetadataOk'] = true;
                    stateBag['/mainServiceMetadataError'] = '';
                    SearchSmartControlCoordinator.syncAvailability({
                        stateModel: stateModel,
                        viewModel: viewModel,
                        unavailableText: 'smartControlsUnavailable',
                        bootstrap: function () { bootstrapCalls += 1; }
                    });
                    trace.push(snapshot('recovered'));

                    var degraded = trace[1] || {};
                    var recovered = trace[2] || {};

                    resolve({
                        ok: trace[0].smartFilterVisible === true
                          && degraded.smartFilterVisible === false
                          && recovered.smartFilterVisible === true
                          && bootstrapCalls >= 1,
                        bootstrapCalls: bootstrapCalls,
                        trace: trace
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
