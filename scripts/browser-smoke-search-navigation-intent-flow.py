#!/usr/bin/env python3
"""Browser smoke: search navigation intent flow (create/copy/open-detail branches)."""

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
                    'sap_ui5/service/usecase/SearchNavigationIntentUseCase'
                ], function (mod) {
                    var state = {};
                    var navCalls = [];

                    var stateModel = { setProperty: function (k, v) { state[k] = v; } };
                    var navTo = function (route, params) { navCalls.push({ route: route, params: params }); };

                    var createIntent = mod.buildCreateIntent();
                    var copyIntent = mod.buildCopyIntent({ selectedId: 'CHK-1' });
                    var detailIntent = mod.buildOpenDetailIntent({ id: 'CHK-2' });
                    var copyMissing = mod.buildCopyIntent({ selectedId: '' });

                    var applyCreate = mod.applyIntent({ intent: createIntent, stateModel: stateModel, navTo: navTo });
                    var applyCopy = mod.applyIntent({ intent: copyIntent, stateModel: stateModel, navTo: navTo });
                    var applyDetail = mod.applyIntent({ intent: detailIntent, stateModel: stateModel, navTo: navTo });
                    var applyMissingRouter = mod.applyIntent({ intent: copyIntent, stateModel: stateModel, navTo: null });

                    resolve({
                        ok: createIntent.routeParams.id === '__create'
                          && copyIntent.routeParams.id === 'CHK-1'
                          && detailIntent.routeParams.id === 'CHK-2'
                          && copyMissing === null
                          && applyCreate === true
                          && applyCopy === true
                          && applyDetail === true
                          && applyMissingRouter === false
                          && navCalls.length === 3
                          && state['/layout'] === 'TwoColumnsMidExpanded',
                        trace: {
                            navCalls: navCalls.length,
                            lastRoute: navCalls.length ? navCalls[navCalls.length - 1].route : '',
                            objectAction: state['/objectAction'] || ''
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
