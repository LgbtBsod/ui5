#!/usr/bin/env python3
"""Browser smoke: metadata failure/recovery toggles smart-controls fallback on Search view."""

import json
import sys
from playwright.sync_api import sync_playwright

URL = sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080/index.html"


def main():
    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": 1440, "height": 900})
        page.goto(URL, wait_until="networkidle", timeout=90000)

        page.wait_for_function(
            "() => !!(window.sap && sap.ui && sap.ui.getCore && sap.ui.getCore())",
            timeout=30000,
        )

        result = page.evaluate(
            """
            () => {
                var mElements = (sap.ui.getCore() && sap.ui.getCore().mElements) || {};
                var oController = null;
                Object.keys(mElements).some(function (sId) {
                    var oControl = mElements[sId];
                    if (!oControl || typeof oControl.getViewName !== 'function' || typeof oControl.getController !== 'function') {
                        return false;
                    }
                    var sViewName = oControl.getViewName() || '';
                    if (sViewName.indexOf('Search') < 0) {
                        return false;
                    }
                    oController = oControl.getController();
                    return !!oController;
                });

                if (!oController) {
                    return { ok: false, reason: 'Search controller not found' };
                }

                var oState = oController.getModel('state');
                var oViewModel = oController.getView().getModel('view');
                if (!oState || !oViewModel) {
                    return { ok: false, reason: 'Required models not found' };
                }

                oState.setProperty('/mainServiceMetadataOk', false);
                oState.setProperty('/mainServiceMetadataError', 'smoke-meta-fail');
                oController._syncSmartControlAvailability();
                var bFallbackEnabled = oViewModel.getProperty('/useSmartControls') === false;
                var sReason = oViewModel.getProperty('/smartControlsReason') || '';

                oState.setProperty('/mainServiceMetadataOk', true);
                oState.setProperty('/mainServiceMetadataError', '');
                oController._syncSmartControlAvailability();
                var bRecovered = oViewModel.getProperty('/useSmartControls') === true;

                return {
                    ok: !!(bFallbackEnabled && bRecovered),
                    fallbackEnabled: bFallbackEnabled,
                    recovered: bRecovered,
                    reason: sReason
                };
            }
            """
        )

        browser.close()

        print(json.dumps(result, ensure_ascii=False))
        if not result.get("ok"):
            return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
