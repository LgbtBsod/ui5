/* global sap */
(function () {
    "use strict";

    var UI5_BOOTSTRAP_SRC = window.__ui5BootstrapSrc || "https://ui5.sap.com/1.71.70/resources/sap-ui-core.js";

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

    function resolveStoredThemeMode() {
        var sRawProfile = "";
        var oParsedProfile = null;
        var sMode = "morning";
        try {
            sRawProfile = window.localStorage.getItem("checklist_app_theme_profile")
                || window.localStorage.getItem("sap_ui5_theme_profile")
                || "";
            if (sRawProfile) {
                oParsedProfile = JSON.parse(sRawProfile);
                sMode = String(oParsedProfile && oParsedProfile.mode || sMode).toLowerCase();
            } else {
                sMode = String(window.localStorage.getItem("sap_ui5_theme") || sMode).toLowerCase();
            }
        } catch (_e) {
            sMode = "morning";
        }
        return sMode === "night" || sMode === "sap_horizon_dark" || sMode === "sap_fiori_3_dark"
            ? "night"
            : "morning";
    }

    function scheduleAlternateThemePrefetch() {
        var sThemeRoot = String(UI5_BOOTSTRAP_SRC || "").replace(/sap-ui-core\.js(?:\?.*)?$/i, "");
        if (!sThemeRoot) {
            return;
        }
        var sAlternateTheme = resolveStoredThemeMode() === "night" ? "sap_horizon" : "sap_horizon_dark";
        var aLibraries = [
            "sap/ui/core",
            "sap/m",
            "sap/f",
            "sap/ui/layout",
            "sap/ui/comp",
            "sap/uxap",
            "sap/ui/table",
            "sap/ui/unified",
            "sap/viz"
        ];
        aLibraries.forEach(function (sLibraryPath) {
            var oLink = document.createElement("link");
            oLink.rel = "prefetch";
            oLink.as = "style";
            oLink.href = sThemeRoot + sLibraryPath + "/themes/" + sAlternateTheme + "/library.css";
            document.head.appendChild(oLink);
        });
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
        sap.ui.getCore().attachInit(mountComponent);
    }

    function loadBootstrapScript() {
        var oScript = document.createElement("script");

        oScript.id = "sap-ui-bootstrap";
        oScript.src = UI5_BOOTSTRAP_SRC;
        oScript.async = true;
        oScript.setAttribute("data-sap-ui-theme", "sap_horizon");
        oScript.setAttribute("data-sap-ui-libs", "sap.ui.core,sap.m,sap.f,sap.ui.layout,sap.ui.comp,sap.ui.table,sap.ui.unified,sap.uxap,sap.viz");
        oScript.setAttribute("data-sap-ui-resourceroots", "{\"PRODUCTION_CONTROL_CHECKLIST\":\"./\"}");
        oScript.setAttribute("data-sap-ui-compatVersion", "edge");
        oScript.setAttribute("data-sap-ui-async", "true");
        oScript.setAttribute("data-sap-ui-preload", "async");
        oScript.setAttribute("data-sap-ui-xx-componentPreload", "off");
        oScript.onload = function () {
            if (window.sap && sap.ui && sap.ui.getCore) {
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

    if (window.sap && sap.ui && sap.ui.getCore) {
        attachInit();
        return;
    }

    scheduleAlternateThemePrefetch();
    loadBootstrapScript();
}());

