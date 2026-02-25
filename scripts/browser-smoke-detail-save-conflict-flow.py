#!/usr/bin/env python3
"""Browser smoke: detail save-conflict flow adapter routing (reload/overwrite)."""

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
                    'sap_ui5/service/usecase/DetailSaveConflictFlowUseCase'
                ], function (FlowUseCase) {
                    var state = { reloadCalls: 0, overwriteCalls: 0 };
                    var fn = FlowUseCase.buildConflictHandler({
                        reloadLabel: 'reload',
                        overwriteLabel: 'overwrite',
                        onReload: function () { state.reloadCalls += 1; return Promise.resolve(); },
                        onOverwrite: function () { state.overwriteCalls += 1; return Promise.resolve(); }
                    });

                    fn('reload').then(function () {
                        return fn('overwrite');
                    }).then(function () {
                        resolve({
                            ok: state.reloadCalls === 1 && state.overwriteCalls === 1,
                            state: state
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
