#!/usr/bin/env python3
"""Browser smoke: detail person suggestion lifecycle (suggest/query/select/apply)."""

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
                    'sap_ui5/service/usecase/DetailPersonSuggestionUseCase'
                ], function (PersonSuggestionUseCase) {
                    var viewState = {};
                    var selectedState = {};

                    var persons = [
                        { perner: '', fullName: 'Alex Stone', position: 'Inspector', orgUnit: 'Q1', integrationName: 'INT-A' },
                        { perner: '10002', fullName: 'Alex Stone', position: 'Inspector', orgUnit: 'Q2', integrationName: 'INT-B' },
                        { perner: '10003', fullName: 'Mira Lane', position: 'Observer', orgUnit: 'Q3', integrationName: 'INT-C' }
                    ];

                    var normalize = function (v) { return String(v || '').trim().toLowerCase(); };
                    var filtered = PersonSuggestionUseCase.filterSuggestions({
                        query: 'alex',
                        persons: persons,
                        normalizeText: normalize,
                        limit: 10
                    });

                    PersonSuggestionUseCase.applySuggestionsToViewModel({
                        target: 'observer',
                        suggestions: filtered,
                        viewModel: { setProperty: function (path, value) { viewState[path] = value; } }
                    });

                    var resolved = PersonSuggestionUseCase.resolvePersonFromSuggestionEvent({
                        getParameter: function (name) {
                            if (name !== 'selectedItem') {
                                return null;
                            }
                            return {
                                getBindingContext: function () {
                                    return { getObject: function () { return filtered[0]; } };
                                }
                            };
                        },
                        getSource: function () {
                            return { data: function (key) { return key === 'target' ? 'observed' : ''; } };
                        }
                    });

                    PersonSuggestionUseCase.applyPersonSelection({
                        target: resolved.target,
                        person: resolved.person,
                        selectedModel: { setProperty: function (path, value) { selectedState[path] = value; } }
                    });

                    resolve({
                        ok: Array.isArray(filtered)
                          && filtered.length === 2
                          && PersonSuggestionUseCase.shouldFetchRemoteSuggestions({ query: 'ax', localCount: 1, minLocalBeforeRemote: 3 }) === true
                          && PersonSuggestionUseCase.shouldFetchRemoteSuggestions({ query: '', localCount: 0, minLocalBeforeRemote: 3 }) === false
                          && (viewState['/observerSuggestions'] || []).length === 2
                          && selectedState['/basic/OBSERVED_FULLNAME'] === 'Alex Stone'
                          && selectedState['/basic/OBSERVED_POSITION'] === 'Inspector'
                          && selectedState['/basic/OBSERVED_INTEGRATION_NAME'] === 'INT-A',
                        trace: {
                            filteredLength: filtered.length,
                            observerSuggestionsLength: (viewState['/observerSuggestions'] || []).length,
                            observedFullName: selectedState['/basic/OBSERVED_FULLNAME'] || ''
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
