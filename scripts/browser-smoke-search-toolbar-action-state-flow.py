#!/usr/bin/env python3
"""Browser smoke: search toolbar action-state flow (selection + smart/fallback mode branches)."""

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
                    'sap_ui5/service/usecase/SearchToolbarActionStateUseCase'
                ], function (mod) {
                    var viewState = {};
                    var selected = { getData: function () { return { root: { id: 'CHK-1' } }; } };
                    var rowsMatch = { getProperty: function () { return [{ root: { id: 'CHK-1' } }]; } };
                    var rowsDiff = { getProperty: function () { return [{ root: { id: 'CHK-X' } }]; } };

                    var smartState = mod.resolveActionState({
                        selectedModel: selected,
                        dataModel: rowsMatch,
                        isLoading: false,
                        useSmartControls: true
                    });

                    var fallbackState = mod.resolveActionState({
                        selectedModel: selected,
                        dataModel: rowsDiff,
                        isLoading: false,
                        useSmartControls: false
                    });

                    var loadingState = mod.resolveActionState({
                        selectedModel: selected,
                        dataModel: rowsMatch,
                        isLoading: true,
                        useSmartControls: true
                    });

                    mod.applyActionStateToViewModel({
                        selectedModel: selected,
                        dataModel: rowsMatch,
                        viewModel: { setProperty: function (path, value) { viewState[path] = value; } },
                        isLoading: false,
                        useSmartControls: true
                    });

                    resolve({
                        ok: smartState.hasSelection === true
                          && smartState.canCopy === true
                          && fallbackState.hasSelection === false
                          && fallbackState.canDelete === false
                          && loadingState.canRetryLoad === false
                          && viewState['/canExport'] === true
                          && viewState['/hasSelection'] === true,
                        trace: {
                            smartCanCopy: smartState.canCopy,
                            fallbackHasSelection: fallbackState.hasSelection,
                            loadingCanRetry: loadingState.canRetryLoad,
                            viewCanExport: viewState['/canExport']
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
