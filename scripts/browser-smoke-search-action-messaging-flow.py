#!/usr/bin/env python3
"""Browser smoke: search action messaging flow (copy/delete/id-missing/success/error branches)."""

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
                    'sap_ui5/service/usecase/SearchActionMessagePresentationUseCase'
                ], function (mod) {
                    var calls = [];
                    var bundle = {
                        getText: function (key, params) {
                            var map = {
                                checklistIdMissing: 'Checklist id not found',
                                nothingToCopy: 'Select a checklist to copy.',
                                deleted: 'Checklist deleted.',
                                nothingToDelete: 'Select a checklist to delete.',
                                deleteFailed: 'Delete failed: ' + (((params || [])[0]) || '')
                            };
                            return map[key] || '';
                        }
                    };

                    var showToast = function (text) { calls.push(text); };

                    var idMissingOk = mod.presentMissingChecklistId({ bundle: bundle, showToast: showToast });
                    var copyMissingOk = mod.presentCopyMissingSelection({ bundle: bundle, showToast: showToast });
                    var deleteOk = mod.presentDeleteFlowResult({ bundle: bundle, showToast: showToast, result: { ok: true } });
                    var missingDeleteOk = mod.presentDeleteFlowResult({ bundle: bundle, showToast: showToast, result: { ok: false, reason: 'missing_id' } });
                    var errorDeleteOk = mod.presentDeleteFlowResult({
                        bundle: bundle,
                        showToast: showToast,
                        result: { ok: false, reason: 'delete_error', error: { message: 'backend-failed' } }
                    });

                    var fallbackCalls = [];
                    var fallbackOk = mod.presentToastMessage({
                        bundle: { getText: function () { throw new Error('bundle-failed'); } },
                        showToast: function (text) { fallbackCalls.push(text); },
                        messageKey: 'deleted',
                        fallbackText: 'fallback-message'
                    });

                    resolve({
                        ok: idMissingOk === true
                          && copyMissingOk === true
                          && deleteOk === true
                          && missingDeleteOk === true
                          && errorDeleteOk === true
                          && fallbackOk === true
                          && calls.length === 5
                          && calls[0] === 'Checklist id not found'
                          && calls[1] === 'Select a checklist to copy.'
                          && calls[2] === 'Checklist deleted.'
                          && calls[3] === 'Select a checklist to delete.'
                          && calls[4] === 'Delete failed: backend-failed'
                          && fallbackCalls.length === 1
                          && fallbackCalls[0] === 'fallback-message',
                        trace: {
                            calls: calls,
                            fallbackCalls: fallbackCalls
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
