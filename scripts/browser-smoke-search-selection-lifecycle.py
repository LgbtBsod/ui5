#!/usr/bin/env python3
"""Browser smoke: search selection lifecycle (select -> navigation intent -> cancel/back -> restore state)."""

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
                    'sap_ui5/service/usecase/SearchSelectionNavigationUseCase'
                ], function (SearchSelectionNavigationUseCase) {
                    var viewState = {};
                    var selectedState = {};
                    var trace = {
                        selectedInitial: false,
                        persistedAfterBack: false,
                        droppedOnFilteredOut: false,
                        cancelBlocked: false,
                        navTargetId: ''
                    };

                    var selectedModel = {
                        setData: function (value) { selectedState = value || {}; },
                        getData: function () { return selectedState; }
                    };
                    var viewModel = {
                        setProperty: function (path, value) { viewState[path] = value; }
                    };
                    var dataModel = {
                        data: {
                            '/checkLists': [{ root: { id: 'SEL-1' } }, { root: { id: 'SEL-2' } }],
                            '/visibleCheckLists': [{ root: { id: 'SEL-1' } }, { root: { id: 'SEL-2' } }]
                        },
                        getProperty: function (path) { return this.data[path]; }
                    };

                    SearchSelectionNavigationUseCase.applySelectedChecklist({
                        checklist: { root: { id: 'SEL-1' } },
                        selectedModel: selectedModel,
                        viewModel: viewModel
                    });
                    trace.selectedInitial = viewState['/hasSelection'] === true;

                    trace.persistedAfterBack = SearchSelectionNavigationUseCase.syncSelectionState({
                        selectedModel: selectedModel,
                        dataModel: dataModel,
                        viewModel: viewModel
                    }) === true;

                    dataModel.data['/visibleCheckLists'] = [{ root: { id: 'SEL-2' } }];
                    trace.droppedOnFilteredOut = SearchSelectionNavigationUseCase.syncSelectionState({
                        selectedModel: selectedModel,
                        dataModel: dataModel,
                        viewModel: viewModel
                    }) === false && viewState['/hasSelection'] === false;

                    trace.cancelBlocked = SearchSelectionNavigationUseCase.shouldProceedAfterUnsavedDecision('CANCEL') === false;
                    trace.navTargetId = SearchSelectionNavigationUseCase.buildNavigationState('SEL-2').routeParams.id;

                    resolve({
                        ok: trace.selectedInitial
                          && trace.persistedAfterBack
                          && trace.droppedOnFilteredOut
                          && trace.cancelBlocked
                          && trace.navTargetId === 'SEL-2',
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
