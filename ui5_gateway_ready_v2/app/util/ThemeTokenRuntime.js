sap.ui.define([
    "checklist/app/util/ThemeDomRuntime"
], function (ThemeDomRuntime) {
    "use strict";

    function asRgbTriplet(sColor) {
        var sValue = String(sColor || "").trim();
        var aMatch;
        if (!sValue) {
            return "";
        }
        if (sValue.charAt(0) === "#") {
            if (sValue.length === 4) {
                return [
                    parseInt(sValue.charAt(1) + sValue.charAt(1), 16),
                    parseInt(sValue.charAt(2) + sValue.charAt(2), 16),
                    parseInt(sValue.charAt(3) + sValue.charAt(3), 16)
                ].join(", ");
            }
            if (sValue.length === 7) {
                return [
                    parseInt(sValue.slice(1, 3), 16),
                    parseInt(sValue.slice(3, 5), 16),
                    parseInt(sValue.slice(5, 7), 16)
                ].join(", ");
            }
        }
        aMatch = sValue.match(/rgba?\(([^)]+)\)/i);
        return aMatch ? aMatch[1].split(",").slice(0, 3).map(function (sPart) {
            return String(parseInt(sPart, 10) || 0);
        }).join(", ") : "";
    }

    function firstThemeParameter(Parameters, aNames) {
        var i;
        var sValue;
        for (i = 0; i < aNames.length; i += 1) {
            sValue = Parameters.get(aNames[i]);
            if (sValue) {
                return sValue;
            }
        }
        return "";
    }

    function syncTokensFromUI5(Core, Parameters, fnSyncDocumentRootClasses) {
        var oRoot = document && document.documentElement;
        if (!Parameters || typeof Parameters.get !== "function") {
            return;
        }
        var sCurrentTheme = Core && Core.getConfiguration && Core.getConfiguration() && Core.getConfiguration().getTheme
            ? String(Core.getConfiguration().getTheme() || "").toLowerCase()
            : "";
        var bHorizonThemeActive = sCurrentTheme.indexOf("sap_horizon") === 0;
        var sBrand = Parameters.get("sapUiBrand") || Parameters.get("sapUiHighlight");
        var sBase = Parameters.get("sapBackgroundColor");
        var sText = Parameters.get("sapTextColor");
        var sShell = Parameters.get("sapUiShellBackground");
        var sInfo = firstThemeParameter(Parameters, ["sapUiInformativeText", "sapUiInformativeElement", "sapInformativeColor", "sapInformationColor"]);
        var sWarning = firstThemeParameter(Parameters, ["sapUiCriticalText", "sapUiCriticalElement", "sapCriticalColor", "sapWarningColor"]);
        var sSuccess = firstThemeParameter(Parameters, ["sapUiPositiveText", "sapUiPositiveElement", "sapPositiveColor", "sapSuccessColor"]);
        var sDanger = firstThemeParameter(Parameters, ["sapUiNegativeText", "sapUiNegativeElement", "sapNegativeColor", "sapErrorColor"]);
        var sInfoRgb;
        var sWarningRgb;
        var sSuccessRgb;
        var sDangerRgb;
        if (!oRoot) {
            return;
        }
        if (typeof fnSyncDocumentRootClasses === "function") {
            fnSyncDocumentRootClasses();
        }
        if (sBrand && bHorizonThemeActive) {
            ThemeDomRuntime.setStyleProperties([oRoot], {
                "--chk-brand": sBrand,
                "--accent": sBrand,
                "--accent-rgb": asRgbTriplet(sBrand) || "84, 149, 255"
            });
        }
        if (sBase) {
            ThemeDomRuntime.setStyleProperty([oRoot], "--chk-ui5-bg", sBase);
        }
        if (sShell) {
            ThemeDomRuntime.setStyleProperty([oRoot], "--chk-ui5-shell", sShell);
        }
        if (sText) {
            ThemeDomRuntime.setStyleProperty([oRoot], "--chk-ui5-text", sText);
        }
        if (bHorizonThemeActive) {
            sInfoRgb = asRgbTriplet(sInfo);
            sWarningRgb = asRgbTriplet(sWarning);
            sSuccessRgb = asRgbTriplet(sSuccess);
            sDangerRgb = asRgbTriplet(sDanger);
            if (sInfoRgb) {
                ThemeDomRuntime.setStyleProperty([oRoot], "--semantic-info-rgb", sInfoRgb);
            }
            if (sWarningRgb) {
                ThemeDomRuntime.setStyleProperty([oRoot], "--semantic-warning-rgb", sWarningRgb);
            }
            if (sSuccess) {
                ThemeDomRuntime.setStyleProperty([oRoot], "--semantic-success", sSuccess);
            }
            if (sSuccessRgb) {
                ThemeDomRuntime.setStyleProperty([oRoot], "--semantic-success-rgb", sSuccessRgb);
            }
            if (sDanger) {
                ThemeDomRuntime.setStyleProperty([oRoot], "--semantic-danger", sDanger);
            }
            if (sDangerRgb) {
                ThemeDomRuntime.setStyleProperty([oRoot], "--semantic-danger-rgb", sDangerRgb);
            }
        }
    }

    return {
        syncTokensFromUI5: syncTokensFromUI5
    };
});
