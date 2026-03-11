sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/CurrentUserProfile"
], function (CurrentUserProfile) {
    "use strict";

    var DEFAULT_CODE_ORDER = ["01", "02", "03", "06"];

    function buildPermissionSheets(aPermissionRules, mOptions) {
        var mGroups = {};
        var aOrder = (mOptions && mOptions.codeOrder) || DEFAULT_CODE_ORDER;
        var fnScopeLabel = (mOptions && mOptions.scopeLabel) || function (oRule) {
            return oRule.scopeKind === "bukrs" && oRule.scopeValue.toUpperCase() !== "ALL" ? ("BE " + oRule.scopeValue) : "ALL";
        };
        var fnCodeLabel = (mOptions && mOptions.codeLabel) || function (oRule) {
            return oRule.code;
        };

        (Array.isArray(aPermissionRules) ? aPermissionRules : []).map(CurrentUserProfile.normalizePermissionRule).forEach(function (oRule) {
            var sKey;
            var oGroup;
            if (!oRule.code) {
                return;
            }
            sKey = oRule.scopeKind + ":" + oRule.scopeValue.toUpperCase();
            oGroup = mGroups[sKey];
            if (!oGroup) {
                oGroup = {
                    title: fnScopeLabel(oRule),
                    description: "",
                    scope: "",
                    info: "",
                    codes: [],
                    labels: []
                };
                mGroups[sKey] = oGroup;
            }
            if (oGroup.codes.indexOf(oRule.code) === -1) {
                oGroup.codes.push(oRule.code);
                oGroup.labels.push(fnCodeLabel(oRule));
            }
        });

        return Object.keys(mGroups).map(function (sKey) {
            var oGroup = mGroups[sKey];
            oGroup.codes.sort(function (a, b) {
                return aOrder.indexOf(a) - aOrder.indexOf(b);
            });
            oGroup.description = oGroup.codes.join(", ");
            oGroup.scope = oGroup.labels.join(" / ");
            oGroup.info = oGroup.scope;
            return oGroup;
        }).sort(function (a, b) {
            if (a.title === "ALL") {
                return -1;
            }
            if (b.title === "ALL") {
                return 1;
            }
            return String(a.title).localeCompare(String(b.title));
        });
    }

    function buildSummaryText(sBackendSummary, aPermissionSheets, sEmptyText) {
        if (String(sBackendSummary || "").trim()) {
            return String(sBackendSummary || "").trim();
        }
        if (!aPermissionSheets || !aPermissionSheets.length) {
            return String(sEmptyText || "").trim();
        }
        return aPermissionSheets.map(function (oSheet) {
            return oSheet.title + ": " + oSheet.description;
        }).join("; ");
    }

    function buildHeaderLabel(sFullName, aPermissionSheets, sSeparator) {
        var aShort = (aPermissionSheets || []).map(function (oSheet) {
            return oSheet.title + ": " + oSheet.description;
        }).filter(Boolean);
        if (!aShort.length) {
            return sFullName;
        }
        return String(sFullName || "") + String(sSeparator || " | ") + aShort.join(" | ");
    }

    return {
        buildPermissionSheets: buildPermissionSheets,
        buildSummaryText: buildSummaryText,
        buildHeaderLabel: buildHeaderLabel
    };
});
