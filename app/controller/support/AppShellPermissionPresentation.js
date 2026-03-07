sap.ui.define([
    "checklist/app/controller/support/AppShellTextSupport"
], function (AppShellTextSupport) {
    "use strict";

    var getText = AppShellTextSupport.getText;
    var PERMISSION_TEXT_KEY_MAP = {
        "01": "shellPermissionView",
        "02": "shellPermissionEditCreate",
        "03": "shellPermissionDelete"
    };

    function mapPermissionPresentation(oController, oPermission) {
        var sCode = String(oPermission && oPermission.code || "").trim();
        var sScopeKind = String(oPermission && oPermission.scopeKind || "all").trim().toLowerCase() || "all";
        var sScopeValue = String(oPermission && oPermission.scopeValue || "ALL").trim() || "ALL";
        var sTitle = getText(oController, PERMISSION_TEXT_KEY_MAP[sCode] || "shellPermissionUnknown", [sCode], sCode);
        var sScopeLabel = sScopeKind === "bukrs" && sScopeValue.toUpperCase() !== "ALL"
            ? getText(oController, "shellPermissionScopeBukrs", [sScopeValue], "BUKRS " + sScopeValue)
            : getText(oController, "shellPermissionScopeAll", null, "ALL");
        return {
            code: sCode,
            title: sCode + " " + sTitle,
            description: getText(oController, "shellPermissionDescription", [sTitle, sScopeLabel], "You have permissions for " + sTitle + " " + sScopeLabel),
            scope: sScopeLabel
        };
    }

    return {
        mapPermissionPresentation: mapPermissionPresentation
    };
});
