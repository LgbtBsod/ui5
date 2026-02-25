#!/usr/bin/env python3
"""Browser smoke: search retry messaging flow."""

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
                    'sap_ui5/service/usecase/SearchRetryMessagePresentationUseCase'
                ], function (mod) {
                    var state = {};
                    var toasts = [];
                    var stateModel = { setProperty: function (p, v) { state[p] = v; } };
                    var bundle = {
                        getText: function (key, params) {
                            var m = {
                                retryLoadSuccess: 'Data reloaded: ' + ((params || [])[0] || 0) + ' rows',
                                retryLoadEmpty: 'No rows loaded.',
                                retryLoadUnavailable: 'Retry is unavailable.',
                                retryLoadEmptyError: 'No rows returned from backend.',
                                retryLoadFailed: 'Retry failed: ' + (((params || [])[0]) || ''),
                                retryLoadFailedAfterRetries: 'Retry failed after ' + (((params || [])[0]) || 0) + ' attempts: ' + (((params || [])[1]) || ''),
                                loadErrorMessage: 'Fallback load error message'
                            };
                            if (!m[key]) {
                                throw new Error('missing-key');
                            }
                            return m[key];
                        }
                    };

                    var success = mod.presentRetryOutcome({
                        result: { ok: true, rows: [{ id: 1 }] },
                        bundle: bundle,
                        stateModel: stateModel,
                        showToast: function (t) { toasts.push(t); }
                    });

                    var empty = mod.presentRetryOutcome({
                        result: { ok: true, rows: [] },
                        bundle: bundle,
                        stateModel: stateModel,
                        showToast: function (t) { toasts.push(t); }
                    });

                    var failed = mod.presentRetryOutcome({
                        result: { ok: false, reason: 'error', attempts: 2, error: new Error('network') },
                        bundle: bundle,
                        stateModel: stateModel,
                        showToast: function (t) { toasts.push(t); }
                    });

                    var unknown = mod.presentRetryOutcome({
                        result: { ok: false, reason: 'unknown' },
                        bundle: bundle,
                        stateModel: stateModel,
                        showToast: function (t) { toasts.push(t); },
                        unknownFallbackKey: 'not-existing-key'
                    });

                    resolve({
                        ok: success.ok === true
                          && empty.ok === true
                          && failed.ok === true
                          && unknown.ok === true
                          && toasts.length === 4
                          && state['/loadError'] === true
                          && state['/loadErrorMessage'] === 'Unexpected retry result',
                        trace: {
                            toasts: toasts,
                            finalLoadError: state['/loadError'],
                            finalLoadErrorMessage: state['/loadErrorMessage']
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
