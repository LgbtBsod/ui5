sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/AccessPayload",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayClient, AccessPayload, ODataAdapterUtils, ODataKeyContracts, GatewayContractConstants) {
    "use strict";

    function parseGrantedOperations(vValue) {
        return String(vValue || "").split(",").map(function (sCode) {
            return String(sCode || "").trim();
        }).filter(Boolean);
    }

    function normalizePermissionResponse(oPermission, sDbKey, sActivity) {
        var aGranted = parseGrantedOperations(oPermission && oPermission.GrantedOperations);
        var bCanCreate = !!(oPermission && oPermission.CanCreate) || aGranted.indexOf("01") >= 0;
        var bCanView = !!(oPermission && oPermission.CanView) || aGranted.indexOf("03") >= 0;
        var bCanEdit = !!(oPermission && oPermission.CanEdit) || aGranted.indexOf("02") >= 0;
        var bCanDelete = !!(oPermission && oPermission.CanDelete) || aGranted.indexOf("06") >= 0;
        return AccessPayload.normalizePermission({
            dbKey: sDbKey,
            userId: String((oPermission && (oPermission.UserId || oPermission.userId)) || "").trim(),
            canCreate: bCanCreate,
            canView: bCanView,
            canEdit: bCanEdit,
            canDelete: bCanDelete,
            reasonCode: String((oPermission && (oPermission.ReasonCode || oPermission.reasonCode)) || "AUTHORIZED").trim() || "AUTHORIZED",
            message: String((oPermission && (oPermission.Message || oPermission.message)) || "").trim(),
            requestedActivity: sActivity
        }, sDbKey, {
            requestedActivity: sActivity
        });
    }

    function checkCreatePermission(sActivity, mDeps) {
        var firstRow = mDeps.firstRow;
        return GatewayClient.rawRead(ODataAdapterUtils.buildEntityPath("ChecklistCreatePermissionSet", "CURRENT", {
            type: ODataKeyContracts.TYPES.CURRENT_ALIAS_KEY
        }), {
            "__ts": Date.now()
        }).then(function (oResponse) {
            return normalizePermissionResponse(firstRow(oResponse), "", sActivity);
        });
    }

    function checkChecklistPermission(mArgs, mDeps) {
        var firstRow = mDeps.firstRow;
        var sDbKey = mDeps.normalizeDbKey(mDeps.dbKey(mArgs));
        var sActivity = String((mArgs && (mArgs.activity || mArgs.ACTVT)) || "").trim();
        if (!sDbKey) {
            if (sActivity !== "01") {
                return Promise.resolve(AccessPayload.normalizePermission({
                    dbKey: "",
                    userId: "",
                    canCreate: false,
                    canView: false,
                    canEdit: false,
                    canDelete: false,
                    reasonCode: "INVALID_PERMISSION_TARGET",
                    message: "",
                    requestedActivity: sActivity
                }, "", {
                    requestedActivity: sActivity
                }));
            }
            return checkCreatePermission(sActivity, mDeps);
        }
        return GatewayClient.rawRead(ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.CHECKLIST_PERMISSION, sDbKey, {
            name: "DB_KEY",
            type: ODataKeyContracts.TYPES.DB_KEY
        }), {
            ACTVT: sActivity,
            "$select": ODataKeyContracts.SELECTS.CHECKLIST_PERMISSION
        }).then(function (oResponse) {
            return normalizePermissionResponse(firstRow(oResponse), sDbKey, sActivity);
        });
    }

    return {
        checkChecklistPermission: checkChecklistPermission
    };
});
