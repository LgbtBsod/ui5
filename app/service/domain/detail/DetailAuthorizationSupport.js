sap.ui.define([
    "checklist/app/service/framework/Effects",
    "checklist/app/model/StatePaths",
    "checklist/app/util/CreateSentinel"
], function (Effects, StatePaths, CreateSentinel) {
    "use strict";

    var DENIED_ILLUSTRATION = "assets/illustrations/detail-access-denied.svg";

    function normalizePermission(oPermission, sRootId) {
        var oResolved = oPermission || {};
        return {
            rootId: String(oResolved.rootId || oResolved.RootKey || sRootId || "").trim(),
            userId: String(oResolved.userId || oResolved.UserId || "").trim(),
            canView: oResolved.canView !== false && oResolved.CanView !== false,
            canEdit: oResolved.canEdit !== false && oResolved.CanEdit !== false,
            canDelete: oResolved.canDelete !== false && oResolved.CanDelete !== false,
            reasonCode: String(oResolved.reasonCode || oResolved.ReasonCode || "AUTHORIZED").trim() || "AUTHORIZED",
            message: String(oResolved.message || oResolved.Message || "").trim()
        };
    }

    function buildAccessState(oPermission, bDenied) {
        var oResolved = normalizePermission(oPermission);
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
            illustrationSrc: DENIED_ILLUSTRATION
        };
    }

    function contentAccessEffects(oPermission) {
        return [
            Effects.modelPatch("view", "/accessState", buildAccessState(oPermission, false))
        ];
    }

    function openDeniedEffects(oPermission) {
        return [
            Effects.modelPatch("state", StatePaths.UI_BUSY_GLOBAL, false),
            Effects.modelPatch("state", StatePaths.UI_BUSY_DETAIL, false),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "READ"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "IDLE"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, "IDLE"),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, null),
            Effects.modelPatch("state", StatePaths.WORKFLOW_AUTOSAVE_ENABLED, false),
            Effects.modelPatch("state", StatePaths.WORKFLOW_DIRTY, false),
            Effects.modelPatch("selected", "/", {}),
            Effects.modelPatch("uiState", "/_detailSnapshot", {}),
            Effects.modelPatch("uiState", "/_detailCurrent", {}),
            Effects.modelPatch("view", "/detailSkeletonBusy", false),
            Effects.modelPatch("view", "/accessState", buildAccessState(oPermission, true)),
            Effects.toast("detailViewPermissionDenied", "warning")
        ];
    }

    function deniedActionEffects(oPermission, sTextKey, aExtraEffects) {
        return contentAccessEffects(oPermission).concat(aExtraEffects || [], [
            Effects.toast(sTextKey, "warning")
        ]);
    }

    function fetchPermission(mCtx, sRootId) {
        var oRepo = mCtx && mCtx.repo;
        var sResolvedRootId = String(sRootId || "").trim();

        if (!sResolvedRootId || CreateSentinel.isCreateId(sResolvedRootId)) {
            return Promise.resolve({
                rootId: sResolvedRootId,
                canView: true,
                canEdit: true,
                canDelete: false,
                reasonCode: "CREATE_DRAFT"
            });
        }
        if (!oRepo || typeof oRepo.checkChecklistPermission !== "function") {
            return Promise.resolve({
                rootId: sResolvedRootId,
                canView: true,
                canEdit: true,
                canDelete: true,
                reasonCode: "CHECK_SKIPPED"
            });
        }

        return Promise.resolve(oRepo.checkChecklistPermission({ rootId: sResolvedRootId })).then(function (oPermission) {
            return normalizePermission(oPermission, sResolvedRootId);
        }).catch(function () {
            // ChecklistPermissionSet may not exist on all Gateway backends - grant access and let server enforce
            return {
                rootId: sResolvedRootId,
                canView: true,
                canEdit: true,
                canDelete: true,
                reasonCode: "PERMISSION_CHECK_UNAVAILABLE"
            };
        });
    }

    return {
        contentAccessEffects: contentAccessEffects,
        deniedActionEffects: deniedActionEffects,
        fetchPermission: fetchPermission,
        openDeniedEffects: openDeniedEffects
    };
});
