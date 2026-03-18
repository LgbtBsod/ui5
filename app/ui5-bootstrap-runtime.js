/* global sap */
(function () {
    "use strict";

    // Productive baseline is UI5 1.71.28; temporary sandbox/test harnesses may still point to 1.71.70 by external contour constraint.
    var UI5_BOOTSTRAP_SRC = window.__ui5BootstrapSrc || "https://ui5.sap.com/1.71.70/resources/sap-ui-core.js";
    var DEFAULT_THEME = "sap_fiori_3";
    function splitClassNames(sClassNames) {
        return String(sClassNames || "").split(/\s+/).map(function (sName) {
            return sName.trim();
        }).filter(Boolean);
    }

    function applyStyleClass(oTarget, sClassNames, sMethodName) {
        if (!oTarget || typeof oTarget[sMethodName] !== "function") {
            return oTarget;
        }
        splitClassNames(sClassNames).forEach(function (sClassName) {
            oTarget[sMethodName](sClassName);
        });
        return oTarget;
    }

    function normalizeStoredThemeProfile() {
        try {
            window.localStorage.setItem("checklist_app_theme_profile", JSON.stringify({
                mode: "morning",
                animationEnabled: true
            }));
            window.localStorage.setItem("checklist_app_theme", "morning");
        } catch (_e) {
            // Best-effort normalization only.
        }
    }

    function mountComponent() {
        sap.ui.require([
            "sap/ui/core/ComponentContainer",
            "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/Ui5StyleAdapter"
        ], function (ComponentContainer, Ui5StyleAdapter) {
            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.enableOn !== "function") {
                Ui5StyleAdapter.enableOn = function (oTarget, sClassNames) {
                    return applyStyleClass(oTarget, sClassNames, "addStyleClass");
                };
            }

            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.disableOn !== "function") {
                Ui5StyleAdapter.disableOn = function (oTarget, sClassNames) {
                    return applyStyleClass(oTarget, sClassNames, "removeStyleClass");
                };
            }

            if (Ui5StyleAdapter && typeof Ui5StyleAdapter.restartOn !== "function") {
                Ui5StyleAdapter.restartOn = function (oTarget, sClassName) {
                    var oDomRef;
                    applyStyleClass(oTarget, sClassName, "removeStyleClass");
                    oDomRef = oTarget && oTarget.getDomRef && oTarget.getDomRef();
                    if (oDomRef) {
                        void oDomRef.offsetWidth;
                    }
                    return applyStyleClass(oTarget, sClassName, "addStyleClass");
                };
            }

            new ComponentContainer({
                name: "PRODUCTION_CONTROL_CHECKLIST",
                manifest: true,
                settings: { id: "checklist_app_comp" },
                async: true,
                height: "100%"
            }).placeAt("ui5_container");
        });
    }

    function attachInit() {
        sap.ui.require(["sap/ui/core/Core"], function (Core) {
            if (Core && typeof Core.ready === "function") {
                Core.ready().then(mountComponent);
                return;
            }
            mountComponent();
        });
    }

    function loadBootstrapScript() {
        var oScript = document.createElement("script");

        oScript.id = "sap-ui-bootstrap";
        oScript.src = UI5_BOOTSTRAP_SRC;
        oScript.async = true;
        oScript.setAttribute("data-sap-ui-theme", DEFAULT_THEME);
        oScript.setAttribute("data-sap-ui-resourceroots", "{\"PRODUCTION_CONTROL_CHECKLIST\":\"./\"}");
        oScript.setAttribute("data-sap-ui-compatVersion", "edge");
        oScript.setAttribute("data-sap-ui-async", "true");
        oScript.setAttribute("data-sap-ui-preload", "async");
        oScript.onload = function () {
            normalizeStoredThemeProfile();
            if (window.sap && sap.ui && sap.ui.require) {
                attachInit();
                return;
            }
            window.console.error("UI5 bootstrap loaded but sap.ui.getCore is unavailable.");
        };
        oScript.onerror = function () {
            window.console.error("UI5 bootstrap failed to load.");
        };
        document.head.appendChild(oScript);
    }

    normalizeStoredThemeProfile();

    if (window.sap && sap.ui && sap.ui.require) {
        attachInit();
        return;
    }
    loadBootstrapScript();
}());
