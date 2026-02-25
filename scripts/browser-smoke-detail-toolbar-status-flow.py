#!/usr/bin/env python3
"""Browser smoke: detail toolbar status switch flow (validate -> apply -> save-success toast)."""

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
                    'sap_ui5/service/usecase/DetailStatusCommandUseCase',
                    'sap_ui5/service/usecase/DetailToolbarValidationUseCase'
                ], function (StatusCommandUseCase, ToolbarValidationUseCase) {
                    var viewState = {};
                    var selectedState = {};
                    var appState = {};
                    var trace = {
                        validateCalls: 0,
                        appliedCalls: 0,
                        toastCalls: 0,
                        warningCount: 0,
                        validationShown: null,
                        finalStatus: null,
                        dirty: null
                    };

                    var viewModel = {
                        setProperty: function (path, value) {
                            viewState[path] = value;
                        }
                    };
                    var selectedModel = {
                        setProperty: function (path, value) {
                            selectedState[path] = value;
                        }
                    };
                    var stateModel = {
                        setProperty: function (path, value) {
                            appState[path] = value;
                        }
                    };

                    // First pass: emulate validation apply and warning count computation path.
                    ToolbarValidationUseCase.applyValidationState(
                        viewModel,
                        { valid: false, missingPaths: ['basic/date'], hasAtLeastOneCheck: false },
                        function (paths) { return { count: paths.length }; }
                    );
                    trace.warningCount = ToolbarValidationUseCase.resolveValidationWarningCount({
                        missingPaths: ['basic/date'],
                        hasAtLeastOneCheck: false
                    });
                    trace.validationShown = viewState['/validationShown'];

                    StatusCommandUseCase.runStatusChangeFlow({
                        targetStatus: 'REGISTERED',
                        validateChecklist: function () {
                            trace.validateCalls += 1;
                            return Promise.resolve(true);
                        },
                        getSelectedRoot: function () {
                            return { status: 'DRAFT', this_is_integration_data: false };
                        },
                        shouldApplyStatusChange: function () { return true; },
                        requiresIntegrationConfirmation: function () { return false; },
                        confirmIntegrationEdit: function () { return Promise.resolve(true); },
                        applyStatusAndSave: function () {
                            trace.appliedCalls += 1;
                            ToolbarValidationUseCase.markDirtyStatusAndNormalize(selectedModel, stateModel, 'REGISTERED');
                            trace.toastCalls += 1; // emulate statusChanged toast call after save
                            return Promise.resolve({ saved: true });
                        }
                    }).then(function () {
                        trace.finalStatus = selectedState['/root/status'];
                        trace.dirty = appState['/isDirty'];
                        resolve({
                            ok: trace.validateCalls === 1
                              && trace.appliedCalls === 1
                              && trace.toastCalls === 1
                              && trace.validationShown === true
                              && trace.warningCount === 2
                              && trace.finalStatus === 'REGISTERED'
                              && trace.dirty === true,
                            trace: trace
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
