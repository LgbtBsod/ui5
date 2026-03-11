sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/util/GatewayTextNormalizer"
], function (GatewayTextNormalizer) {
    "use strict";

    function normalizePermissionRule(oPermission) {
        return {
            code: String(oPermission && oPermission.code || "").trim(),
            scopeKind: String(oPermission && (oPermission.scopeKind || oPermission.scope_kind) || "all").trim().toLowerCase() || "all",
            scopeValue: String(oPermission && (oPermission.scopeValue || oPermission.scope_value) || "ALL").trim() || "ALL"
        };
    }

    function normalizePermissionRules(vValue) {
        var aItems = [];
        if (Array.isArray(vValue)) {
            aItems = vValue;
        } else {
            try {
                aItems = JSON.parse(String(vValue || "[]"));
            } catch (_oError) {
                aItems = [];
            }
        }
        if (!Array.isArray(aItems)) {
            return [];
        }
        return aItems.map(normalizePermissionRule).filter(function (oRule) {
            return !!oRule.code;
        });
    }

    function normalizeText(vValue) {
        return GatewayTextNormalizer.normalize(vValue);
    }

    function normalizePermissionCodes(vValue) {
        return String(vValue || "").split(",").map(function (sCode) {
            return String(sCode || "").trim();
        }).filter(Boolean);
    }

    function normalizeCurrentUser(vPayload, sFallbackLogin) {
        var oData = vPayload && vPayload.d ? vPayload.d : (vPayload || {});
        return {
            fullName: normalizeText(oData.FullName || oData.fullName || ""),
            permissions: normalizePermissionCodes(oData.PermissionsCsv || oData.permissionsCsv),
            permissionRules: normalizePermissionRules(oData.PermissionRulesJson || oData.permissionRulesJson || oData.permissionRules),
            canView: !!(oData.CanView || oData.canView),
            canEdit: !!(oData.CanEdit || oData.canEdit),
            canDelete: !!(oData.CanDelete || oData.canDelete),
            summaryText: normalizeText(oData.SummaryText || oData.summaryText || "")
        };
    }

    function withFetchedAt(oProfile, sTimestamp) {
        return Object.assign({}, oProfile || {}, {
            fetchedAt: String(sTimestamp || new Date().toISOString())
        });
    }

    function applyCurrentUserState(oStateModel, oProfile, sTimestamp) {
        var oResolved = withFetchedAt(oProfile, sTimestamp);
        if (oStateModel && typeof oStateModel.setProperty === "function") {
            oStateModel.setProperty("/currentUser", oResolved);
        }
        return oResolved;
    }

    function permissionKeys(oProfile) {
        var aLabels = [];
        if (oProfile && oProfile.canView) {
            aLabels.push("view");
        }
        if (oProfile && oProfile.canEdit) {
            aLabels.push("edit");
        }
        if (oProfile && oProfile.canDelete) {
            aLabels.push("delete");
        }
        return aLabels;
    }

    return {
        normalizePermissionRule: normalizePermissionRule,
        normalizePermissionRules: normalizePermissionRules,
        normalizeCurrentUser: normalizeCurrentUser,
        applyCurrentUserState: applyCurrentUserState,
        permissionKeys: permissionKeys,
        withFetchedAt: withFetchedAt
    };
});
