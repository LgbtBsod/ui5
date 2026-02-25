#!/usr/bin/env python3
"""Browser smoke: detail close-navigation flow (clean/dirty + release fallback branches)."""

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
                    'sap_ui5/service/usecase/DetailCloseNavigationFlowUseCase'
                ], function (mod) {
                    var prep = 0;
                    var nav = 0;
                    var rel = 0;
                    var relErr = 0;

                    mod.runCloseNavigation({
                        stateModel: { getProperty: function () { return ''; } },
                        prepareCloseNavigation: function () { prep += 1; },
                        navigateToSearch: function () { nav += 1; }
                    }).then(function (skipRes) {
                        mod.runCloseNavigation({
                            stateModel: { getProperty: function (k) { return k === '/activeObjectId' ? 'CHK-1' : 'S-1'; } },
                            releaseLock: function () { rel += 1; return Promise.resolve(); },
                            prepareCloseNavigation: function () { prep += 1; },
                            navigateToSearch: function () { nav += 1; }
                        }).then(function (okRes) {
                            mod.runCloseNavigation({
                                stateModel: { getProperty: function (k) { return k === '/activeObjectId' ? 'CHK-2' : 'S-2'; } },
                                releaseLock: function () { return Promise.reject(new Error('x')); },
                                prepareCloseNavigation: function () { prep += 1; },
                                navigateToSearch: function () { nav += 1; },
                                onReleaseError: function () { relErr += 1; }
                            }).then(function (errRes) {
                                resolve({
                                    ok: skipRes.skippedRelease === true
                                      && okRes.released === true
                                      && errRes.releaseError === true
                                      && prep === 3
                                      && nav === 3
                                      && rel === 1
                                      && relErr === 1,
                                    trace: {
                                        skip: skipRes,
                                        ok: okRes,
                                        err: errRes,
                                        prep: prep,
                                        nav: nav,
                                        rel: rel,
                                        relErr: relErr
                                    }
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
