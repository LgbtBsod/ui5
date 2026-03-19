/* global sap */
(function () {
    "use strict";

    // Productive support baseline: Chromium-based Edge on SAP UI 754 / UI5 1.71.
    // No Internet Explorer compatibility branches are kept in this bootstrap.
    var UI5_BOOTSTRAP_SRC = window.__ui5BootstrapSrc || "/resources/sap-ui-core.js";
    var DEFAULT_THEME = "sap_fiori_3";
    var THEME_PROFILE_KEY = "checklist_app_theme_profile";
    var APP_THEME_KEY = "checklist_app_theme";
    var DEV_HOSTS = Object.freeze({
        "127.0.0.1": true,
        "localhost": true
    });

    function isDevHost() {
        var oLocation = typeof window !== "undefined" ? window.location : null;
        var sHost = String(oLocation && oLocation.hostname || "").trim().toLowerCase();
        return !!DEV_HOSTS[sHost];
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
            "sap/ui/core/ComponentContainer"
        ], function (ComponentContainer) {
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
        var bDevHost = isDevHost();

        oScript.id = "sap-ui-bootstrap";
        oScript.src = UI5_BOOTSTRAP_SRC;
        oScript.async = true;
        oScript.setAttribute("data-sap-ui-theme", DEFAULT_THEME);
        oScript.setAttribute("data-sap-ui-resourceroots", "{\"PRODUCTION_CONTROL_CHECKLIST\":\"./\"}");
        oScript.setAttribute("data-sap-ui-compatVersion", "edge");
        oScript.setAttribute("data-sap-ui-async", "true");
        oScript.setAttribute("data-sap-ui-preload", bDevHost ? "off" : "async");
        if (bDevHost) {
            oScript.setAttribute("data-sap-ui-xx-componentPreload", "off");
        }
        oScript.onload = function () {
            ensureStoredThemeProfile();
            if (window.sap && sap.ui && sap.ui.require) {
                attachInit();
                return;
            }
            window.console.error("UI5 bootstrap loaded but sap.ui.require is unavailable.");
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
