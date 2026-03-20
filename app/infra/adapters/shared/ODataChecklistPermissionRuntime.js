sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/AccessPayload",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataKeyContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, AccessPayload, ODataAdapterUtils, ODataKeyContracts, GatewayContractConstants) {
    "use strict";

    function parseGrantedOperations(vValue) {
        return String(vValue || "").split(",").map(function (sCode) {
            return String(sCode || "").trim();
        }).filter(Boolean);
    }

    function normalizePermissionResponse(oPermission, sRootId, sActivity) {
        var aGranted = parseGrantedOperations(oPermission && oPermission.GrantedOperations);
        var bCanCreate = !!(oPermission && oPermission.CanCreate) || aGranted.indexOf("01") >= 0;
        var bCanView = !!(oPermission && oPermission.CanView) || aGranted.indexOf("03") >= 0;
        var bCanEdit = !!(oPermission && oPermission.CanEdit) || aGranted.indexOf("02") >= 0;
        var bCanDelete = !!(oPermission && oPermission.CanDelete) || aGranted.indexOf("06") >= 0;
        return AccessPayload.normalizePermission({
            rootId: sRootId,
            userId: String((oPermission && (oPermission.UserId || oPermission.userId)) || "").trim(),
            canCreate: bCanCreate,
            canView: bCanView,
            canEdit: bCanEdit,
            canDelete: bCanDelete,
            reasonCode: String((oPermission && (oPermission.ReasonCode || oPermission.reasonCode)) || "AUTHORIZED").trim() || "AUTHORIZED",
            message: String((oPermission && (oPermission.Message || oPermission.message)) || "").trim(),
            requestedActivity: sActivity
        }, sRootId, {
            requestedActivity: sActivity
        });
    }

    function checkCreatePermission(sActivity, mDeps) {
        var firstRow = mDeps.firstRow;
        return GatewayODataClient.get(ODataAdapterUtils.buildEntityPath("ChecklistCreatePermissionSet", "CURRENT", {
            type: ODataKeyContracts.TYPES.CURRENT_ALIAS_KEY
        }).replace(/^\//, ""), {
            "__ts": Date.now()
        }).then(function (oResponse) {
            return normalizePermissionResponse(firstRow(oResponse), "", sActivity);
        });
    }

    function checkChecklistPermission(mArgs, mDeps) {
        var normalizeRootKey = mDeps.normalizeRootKey;
        var firstRow = mDeps.firstRow;
        var sRootId = normalizeRootKey(mDeps.rootId(mArgs));
        var sActivity = String((mArgs && (mArgs.activity || mArgs.ACTVT)) || "").trim();
        if (!sRootId) {
            if (sActivity !== "01") {
                return Promise.resolve(AccessPayload.normalizePermission({
                    rootId: "",
                    userId: "",
                    canCreate: false,
                    canView: false,
                    canEdit: false,
                    canDelete: false,
                    reasonCode: "INVALID_PERMISSION_TARGET",
                    message: "RootKey is required for non-create permissions",
                    requestedActivity: sActivity
                }, "", {
                    requestedActivity: sActivity
                }));
            }
            return checkCreatePermission(sActivity, mDeps);
        }
        return GatewayODataClient.get(ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.CHECKLIST_PERMISSION, sRootId, {
            type: ODataKeyContracts.TYPES.ROOT_KEY,
            "$select": "RootKey,CanCreate,CanView,CanEdit,CanDelete,ReasonCode,Message"}).replace(/^\//, ""), {
            ACTVT: sActivity
        }).then(function (oResponse) {
            return normalizePermissionResponse(firstRow(oResponse), sRootId, sActivity);
        });
    }

    return {
        checkChecklistPermission: checkChecklistPermission
    };
});
