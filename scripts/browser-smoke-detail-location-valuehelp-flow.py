#!/usr/bin/env python3
"""Browser smoke: detail location value-help flow (open/search/select/close + selected sync)."""

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
                    'sap_ui5/service/usecase/DetailLocationValueHelpUseCase'
                ], function (LocationVhUseCase) {
                    var viewState = {};
                    var selectedState = {};
                    var trace = {
                        opened: 0,
                        closed: 0,
                        treeSizeAfterSearch: 0,
                        selectedKey: '',
                        selectedName: '',
                        selectedText: ''
                    };

                    var rows = [
                        { node_id: 'ROOT', parent_id: '', location_name: 'Root Area' },
                        { node_id: 'A-100', parent_id: 'ROOT', location_name: 'Assembly Line' },
                        { node_id: 'B-200', parent_id: 'ROOT', location_name: 'Buffer Zone' }
                    ];

                    var dialog = {
                        open: function () { trace.opened += 1; },
                        close: function () { trace.closed += 1; }
                    };
                    var viewModel = {
                        setProperty: function (path, value) { viewState[path] = value; }
                    };
                    var selectedModel = {
                        setProperty: function (path, value) { selectedState[path] = value; }
                    };

                    var buildTree = function (flatRows) {
                        return (flatRows || []).map(function (row) {
                            return { node_id: row.node_id, parent_id: row.parent_id, location_name: row.location_name, children: [] };
                        });
                    };
                    var normalize = function (v) {
                        return String(v || '').trim().toLowerCase();
                    };

                    LocationVhUseCase.openValueHelp({
                        dialog: dialog,
                        locations: rows,
                        viewModel: viewModel,
                        buildLocationTree: buildTree
                    });

                    LocationVhUseCase.applyFilteredTreeToViewModel({
                        query: 'assem',
                        locations: rows,
                        viewModel: viewModel,
                        buildLocationTree: buildTree,
                        normalizeText: normalize
                    });
                    trace.treeSizeAfterSearch = (viewState['/locationVhTree'] || []).length;

                    var nodeFromTree = LocationVhUseCase.resolveNodeFromTreeSelectionEvent({
                        getParameter: function (name) {
                            if (name === 'rowContext') {
                                return { getObject: function () { return rows[1]; } };
                            }
                            return null;
                        }
                    });

                    LocationVhUseCase.applyLocationSelection({
                        node: nodeFromTree,
                        selectedModel: selectedModel
                    });

                    trace.selectedKey = selectedState['/basic/LOCATION_KEY'] || '';
                    trace.selectedName = selectedState['/basic/LOCATION_NAME'] || '';
                    trace.selectedText = selectedState['/basic/LOCATION_TEXT'] || '';

                    LocationVhUseCase.closeValueHelp({ dialog: dialog });

                    resolve({
                        ok: trace.opened === 1
                          && trace.closed === 1
                          && trace.treeSizeAfterSearch >= 1
                          && trace.selectedKey === 'A-100'
                          && trace.selectedName === 'Assembly Line'
                          && trace.selectedText === 'Assembly Line',
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
