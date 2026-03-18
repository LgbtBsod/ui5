/* global sap */
(function () {
    "use strict";

    var UI5_BOOTSTRAP_SRC = window.__ui5BootstrapSrc || "/resources/sap-ui-core.js";
    var DEFAULT_THEME = "sap_fiori_3";
    var THEME_PROFILE_KEY = "checklist_app_theme_profile";
    var APP_THEME_KEY = "checklist_app_theme";

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

    function ensureStoredThemeProfile() {
        var oStoredProfile;
        try {
            oStoredProfile = JSON.parse(window.localStorage.getItem(THEME_PROFILE_KEY) || "null");
        } catch (_profileParseError) {
            oStoredProfile = null;
        }

        if (!oStoredProfile || typeof oStoredProfile !== "object") {
            try {
                window.localStorage.setItem(THEME_PROFILE_KEY, JSON.stringify({
                    mode: "morning",
                    animationEnabled: true
                }));
            } catch (_profilePersistError) {
                // Best-effort normalization only.
            }
        }

        try {
            if (!window.localStorage.getItem(APP_THEME_KEY)) {
                window.localStorage.setItem(APP_THEME_KEY, "morning");
            }
        } catch (_themePersistError) {
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
            ensureStoredThemeProfile();
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

    ensureStoredThemeProfile();

    if (window.sap && sap.ui && sap.ui.require) {
        attachInit();
        return;
    }
    loadBootstrapScript();
}());
