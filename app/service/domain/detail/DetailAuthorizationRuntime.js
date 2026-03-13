sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Effects",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ViewPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/AccessPayload",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/WorkflowTelemetry",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/UiAssetPaths"
], function (Effects, ViewPathContracts, AccessPayload, StatePaths, WorkflowTelemetry, WorkflowContracts, UiAssetPaths) {
    "use strict";

    var OPERATIONS = {
        CREATE: "01",
        CHANGE: "02",
        DISPLAY: "03",
        DELETE: "06"
    };

    function resolveRequestedActivity(mOptions) {
        var sActivity = String((mOptions && (mOptions.activity || mOptions.actvt)) || "").trim();
        return sActivity || OPERATIONS.DISPLAY;
    }

    function activityAllowed(oPermission, sActivity) {
        var oResolved = oPermission || {};
        switch (String(sActivity || "").trim()) {
        case OPERATIONS.CREATE:
            return !!oResolved.canCreate;
        case OPERATIONS.CHANGE:
            return !!oResolved.canEdit;
        case OPERATIONS.DELETE:
            return !!oResolved.canDelete;
        case OPERATIONS.DISPLAY:
        default:
            return !!oResolved.canView;
        }
    }

    function defaultDeniedReason(sActivity) {
        switch (String(sActivity || "").trim()) {
        case OPERATIONS.CREATE:
            return "NO_CREATE_PERMISSION";
        case OPERATIONS.CHANGE:
            return "NO_EDIT_PERMISSION";
        case OPERATIONS.DELETE:
            return "NO_DELETE_PERMISSION";
        case OPERATIONS.DISPLAY:
        default:
            return "NO_VIEW_PERMISSION";
        }
    }

    function normalizePermission(oPermission, sRootId, mOptions) {
        var sRequestedActivity = resolveRequestedActivity(mOptions);
        var oResolved = AccessPayload.normalizePermission(oPermission, sRootId, {
            reasonCode: "AUTHORIZED",
            requestedActivity: sRequestedActivity
        });
        var bAllowed = activityAllowed(oResolved, sRequestedActivity);

        if (sRequestedActivity === OPERATIONS.CREATE && !Object.prototype.hasOwnProperty.call(oResolved, "canCreate")) {
            oResolved.canCreate = !!(oPermission && (oPermission.canCreate || oPermission.CanCreate));
            bAllowed = activityAllowed(oResolved, sRequestedActivity);
        }
        if (!bAllowed) {
            oResolved.reasonCode = String(oResolved.reasonCode || defaultDeniedReason(sRequestedActivity)).trim() || defaultDeniedReason(sRequestedActivity);
        }
        oResolved.allowed = bAllowed;
        return oResolved;
    }

    function deniedPermission(sRootId, sRequestedActivity, sReasonCode, sMessage) {
        return normalizePermission({
            rootId: sRootId,
            canCreate: false,
            canView: false,
            canEdit: false,
            canDelete: false,
            reasonCode: sReasonCode || defaultDeniedReason(sRequestedActivity),
            message: sMessage || ""
        }, sRootId, {
            activity: sRequestedActivity
        });
    }

    function emitPermissionDenied(mCtx, oPermission, sRequestedActivity) {
        var oResolved = oPermission || {};
        if (oResolved.allowed) {
            return;
        }
        WorkflowTelemetry.emit("permission.denied", {
            stateModel: mCtx && mCtx.stateModel,
            payload: {
                rootId: String(oResolved.rootId || "").trim(),
                activity: String(sRequestedActivity || "").trim(),
                reasonCode: String(oResolved.reasonCode || "").trim(),
                message: String(oResolved.message || "").trim()
            }
        });
    }

    function buildAccessState(oPermission, bDenied) {
        var oResolved = normalizePermission(oPermission, (oPermission && oPermission.rootId) || "");
        return {
            denied: !!bDenied,
            rootId: oResolved.rootId,
            userId: oResolved.userId,
            canView: !!oResolved.canView,
            canEdit: !!oResolved.canEdit,
            canDelete: !!oResolved.canDelete,
            reasonCode: oResolved.reasonCode,
            message: oResolved.message,
            titleKey: bDenied ? "detailAccessDeniedTitle" : "",
            messageKey: bDenied ? "detailAccessDeniedText" : "",
            illustrationSrc: UiAssetPaths.resolveDetailAccessDeniedIllustration()
        };
    }

    function contentAccessEffects(oPermission) {
        return [
            Effects.modelPatch("view", ViewPathContracts.ACCESS_STATE, buildAccessState(oPermission, false))
        ];
    }

    function openDeniedEffects(oPermission) {
        var oResolved = normalizePermission(oPermission, (oPermission && oPermission.rootId) || "", {
            activity: OPERATIONS.DISPLAY
        });
        return [
            Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, WorkflowContracts.EDIT_MODES.READ),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, WorkflowContracts.LOCK_STATES.READ_ONLY),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, WorkflowContracts.AUTOSAVE_STATES.IDLE),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
            Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
            Effects.modelPatch("selected", "/", {}),
            Effects.modelPatch("snapshot", "/", {}),
            Effects.modelPatch("view", ViewPathContracts.DETAIL_SKELETON_BUSY, false),
            Effects.modelPatch("view", ViewPathContracts.ACCESS_STATE, buildAccessState(oResolved, true)),
            Effects.toast("detailViewPermissionDenied", "warning")
        ];
    }

    function deniedActionEffects(oPermission, sTextKey, aExtraEffects) {
        return contentAccessEffects(oPermission).concat(aExtraEffects || [], [
            Effects.toast(sTextKey, "warning")
        ]);
    }

    function fetchPermission(mCtx, sRootId, mOptions) {
        var oRepo = mCtx && mCtx.repo;
        var sResolvedRootId = String(sRootId || "").trim();
        var sRequestedActivity = resolveRequestedActivity(mOptions);

        if (!oRepo || typeof oRepo.checkChecklistPermission !== "function") {
            var oMissingPermission = deniedPermission(
                sResolvedRootId,
                sRequestedActivity,
                "PERMISSION_CHECK_UNAVAILABLE",
                "Permission backend is unavailable"
            );
            emitPermissionDenied(mCtx, oMissingPermission, sRequestedActivity);
            return Promise.resolve(oMissingPermission);
        }

        return Promise.resolve(oRepo.checkChecklistPermission({
            rootId: sResolvedRootId,
            activity: sRequestedActivity
        })).then(function (oPermission) {
            var oResolved = normalizePermission(oPermission, sResolvedRootId, {
                activity: sRequestedActivity
            });
            emitPermissionDenied(mCtx, oResolved, sRequestedActivity);
            return oResolved;
        }).catch(function () {
            var oFailedPermission = deniedPermission(
                sResolvedRootId,
                sRequestedActivity,
                "PERMISSION_CHECK_FAILED",
                "Permission could not be confirmed"
            );
            emitPermissionDenied(mCtx, oFailedPermission, sRequestedActivity);
            return oFailedPermission;
        });
    }

    return {
        OPERATIONS: OPERATIONS,
        buildAccessState: buildAccessState,
        contentAccessEffects: contentAccessEffects,
        deniedActionEffects: deniedActionEffects,
        fetchPermission: fetchPermission,
        openDeniedEffects: openDeniedEffects
    };
});
