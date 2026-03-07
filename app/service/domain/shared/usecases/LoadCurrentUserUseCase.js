sap.ui.define([
    "sap_ui5/service/framework/Result",
    "sap_ui5/service/backend/GatewayBackendService",
    "sap_ui5/util/GatewayTextNormalizer"
], function (Result, GatewayBackendService, GatewayTextNormalizer) {
    "use strict";

    function normalizePermissionRules(vValue) {
        var aItems = [];
        try {
            aItems = JSON.parse(String(vValue || "[]"));
        } catch (_oError) {
            aItems = [];
        }
        if (!Array.isArray(aItems)) {
            return [];
        }
        return aItems.map(function (oItem) {
            return {
                code: String(oItem && oItem.code || "").trim(),
                scopeKind: String(oItem && (oItem.scope_kind || oItem.scopeKind) || "all").trim().toLowerCase() || "all",
                scopeValue: String(oItem && (oItem.scope_value || oItem.scopeValue) || "ALL").trim() || "ALL"
            };
        }).filter(function (oItem) {
            return !!oItem.code;
        });
    }

    function buildSummary(oProfile) {
        var aLabels = [];
        if (oProfile.canView) {
            aLabels.push("view");
        }
        if (oProfile.canEdit) {
            aLabels.push("edit");
        }
        if (oProfile.canDelete) {
            aLabels.push("delete");
        }
        return aLabels;
    }

    function normalizeProfileText(vValue) {
        return GatewayTextNormalizer.normalize(vValue);
    }

    function applyCurrentUserState(oStateModel, oProfile, sLogin) {
        if (!oStateModel) {
            return oProfile;
        }
        oStateModel.setProperty("/testUser", oProfile.uname || sLogin || "");
        oStateModel.setProperty("/testUserLogin", oProfile.uname || sLogin || "");
        oStateModel.setProperty("/requiresUserLogin", false);
        oStateModel.setProperty("/currentUser", {
            uname: oProfile.uname || "",
            fullName: oProfile.fullName || "",
            permissions: oProfile.permissions || [],
            permissionRules: oProfile.permissionRules || [],
            canView: !!oProfile.canView,
            canEdit: !!oProfile.canEdit,
            canDelete: !!oProfile.canDelete,
            summaryText: oProfile.summaryText || "",
            fetchedAt: new Date().toISOString()
        });
        return oProfile;
    }

    function readCurrentUser(sLogin) {
        return GatewayBackendService.readEntity("CurrentUserSet", "'CURRENT'", {
            __ts: Date.now()
        }).then(function (oData) {
            return {
                uname: normalizeProfileText(oData && oData.Uname || sLogin || ""),
                fullName: normalizeProfileText(oData && oData.FullName || sLogin || ""),
                permissions: String(oData && oData.PermissionsCsv || "").split(",").map(function (sCode) {
                    return String(sCode || "").trim();
                }).filter(Boolean),
                permissionRules: normalizePermissionRules(oData && oData.PermissionRulesJson),
                canView: !!(oData && oData.CanView),
                canEdit: !!(oData && oData.CanEdit),
                canDelete: !!(oData && oData.CanDelete),
                summaryText: normalizeProfileText(oData && oData.SummaryText || "")
            };
        });
    }

    return {
        execute: function (mInput, mDeps) {
            var sLogin = String(mInput && mInput.login || "").trim();
            var oStateModel = mDeps && mDeps.stateModel;
            return readCurrentUser(sLogin).then(function (oProfile) {
                applyCurrentUserState(oStateModel, oProfile, sLogin);
                oProfile.permissionKeys = buildSummary(oProfile);
                return Result.ok({
                    user: oProfile.uname,
                    profile: oProfile
                });
            });
        },
        refresh: function (mDeps) {
            var oStateModel = mDeps && mDeps.stateModel;
            var sLogin = String(oStateModel && (oStateModel.getProperty("/testUserLogin") || oStateModel.getProperty("/testUser")) || "").trim();
            return this.execute({ login: sLogin }, mDeps);
        }
    };
});
