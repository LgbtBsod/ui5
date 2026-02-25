#!/usr/bin/env python3
"""Browser smoke: detail save-conflict UI decision wiring assertions (reload/overwrite/message)."""

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
                    'sap_ui5/service/usecase/DetailSaveConflictFlowUseCase',
                    'sap_ui5/service/usecase/DetailSaveConflictUseCase'
                ], function (FlowUseCase, ConflictUseCase) {
                    var labels = { reload: 'reload', overwrite: 'overwrite' };
                    var state = {
                        reloadCalls: 0,
                        overwriteCalls: 0,
                        unknownChoiceResult: 'uninitialized',
                        decisionChecks: {
                            reloadMatch: ConflictUseCase.shouldReloadChoice(labels.reload, labels.reload),
                            overwriteMatch: ConflictUseCase.shouldOverwriteChoice(labels.overwrite, labels.overwrite),
                            mismatch: ConflictUseCase.shouldOverwriteChoice(labels.reload, labels.overwrite)
                        },
                        message: ''
                    };

                    var fn = FlowUseCase.buildConflictHandler({
                        reloadLabel: labels.reload,
                        overwriteLabel: labels.overwrite,
                        onReload: function () { state.reloadCalls += 1; state.message = 'runtime:reload'; return Promise.resolve('reloaded'); },
                        onOverwrite: function () { state.overwriteCalls += 1; state.message = 'runtime:overwrite'; return Promise.resolve('overwritten'); }
                    });

                    fn('reload').then(function () {
                        return fn('overwrite');
                    }).then(function () {
                        return fn('cancel');
                    }).then(function (vUnknown) {
                        state.unknownChoiceResult = vUnknown;
                        resolve({
                            ok: state.reloadCalls === 1
                              && state.overwriteCalls === 1
                              && state.unknownChoiceResult === null
                              && state.decisionChecks.reloadMatch === true
                              && state.decisionChecks.overwriteMatch === true
                              && state.decisionChecks.mismatch === false,
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
