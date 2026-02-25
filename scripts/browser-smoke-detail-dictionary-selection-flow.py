#!/usr/bin/env python3
"""Browser smoke: detail dictionary selection policy (resolve/apply/barrier confirmation)."""

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
                    'sap_ui5/service/usecase/DetailDictionarySelectionUseCase'
                ], function (mod) {
                    var selectedState = {};

                    var key = mod.resolveSelectedKey({
                        getParameter: function (name) {
                            return name === 'selectedItem'
                                ? { getKey: function () { return 'L2'; } }
                                : null;
                        },
                        getSource: function () {
                            return { getSelectedKey: function () { return 'fallback'; } };
                        }
                    });

                    mod.applyDictionarySelection({
                        key: key,
                        dictionary: [
                            { key: 'L1', text: 'Area 1' },
                            { key: 'L2', text: 'Area 2' }
                        ],
                        keyPath: '/basic/LPC_KEY',
                        textPath: '/basic/LPC_TEXT',
                        selectedModel: { setProperty: function (path, value) { selectedState[path] = value; } }
                    });

                    var shouldConfirm = mod.shouldConfirmBarrierReset({
                        barrierAllowed: false,
                        barriers: [{ id: 'B1' }]
                    });

                    mod.applyLpcDecision({
                        confirmed: false,
                        selectedModel: { setProperty: function (path, value) { selectedState[path] = value; } }
                    });

                    var rolledBackKey = selectedState['/basic/LPC_KEY'];

                    mod.applyLpcDecision({
                        confirmed: true,
                        selectedModel: { setProperty: function (path, value) { selectedState[path] = value; } }
                    });

                    resolve({
                        ok: key === 'L2'
                          && selectedState['/basic/LPC_TEXT'] === ''
                          && shouldConfirm === true
                          && rolledBackKey === ''
                          && Array.isArray(selectedState['/barriers'])
                          && selectedState['/barriers'].length === 0,
                        trace: {
                            resolvedKey: key,
                            shouldConfirm: shouldConfirm,
                            rolledBackKey: rolledBackKey,
                            barriersLength: (selectedState['/barriers'] || []).length
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
