#!/usr/bin/env python3
"""Browser smoke: detail lock lifecycle usecases (acquire/release) sanity."""

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
                    'sap_ui5/service/usecase/DetailLockReleaseUseCase',
                    'sap_ui5/service/usecase/DetailLockEditFlowUseCase'
                ], function (ReleaseUseCase, EditUseCase) {
                    var state = { idle: 0, acquired: 0, releaseCalled: 0, pending: 0 };

                    ReleaseUseCase.runReleaseFlow({
                        stateModel: { setProperty: function () {} },
                        releaseLock: function () { state.releaseCalled += 1; return Promise.resolve(); },
                        setLockUiIdle: function () { state.idle += 1; }
                    }).then(function () {
                        return EditUseCase.runToggleEditFlow({
                            editMode: true,
                            isDirty: false,
                            confirmUnsaved: function () { return Promise.resolve('SAVE'); },
                            runPendingRelease: function () { return Promise.resolve(); },
                            runPendingToggle: function (fn) { state.pending += 1; return fn(); },
                            releaseEdit: function () { return Promise.resolve(); },
                            ensureFreshBeforeEdit: function () { return Promise.resolve(); },
                            confirmIntegrationEdit: function () { return Promise.resolve(true); },
                            onStayReadOnly: function () {},
                            acquireLock: function () { return Promise.resolve({ success: true }); },
                            onLockAcquired: function () { state.acquired += 1; },
                            tryRecoverFromAcquireError: function () { return Promise.resolve(false); },
                            onAcquireFailed: function () { return Promise.resolve(); }
                        });
                    }).then(function () {
                        var ok = state.releaseCalled === 1 && state.idle === 1 && state.pending === 1 && state.acquired === 1;
                        resolve({ ok: ok, state: state });
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
