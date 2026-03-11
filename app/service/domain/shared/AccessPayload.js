sap.ui.define([], function () {
    "use strict";

    function text(vValue, sFallback) {
        var sValue = String(vValue || "").trim();
        return sValue || String(sFallback || "").trim();
    }

    function normalizePermission(oPermission, sRootId, mDefaults) {
        var oResolved = oPermission || {};
        var oOptions = mDefaults || {};
        return {
            rootId: text(oResolved.rootId || oResolved.RootKey, sRootId),
            userId: text(oResolved.userId || oResolved.UserId, ""),
            canCreate: !!(oResolved.canCreate || oResolved.CanCreate || oOptions.canCreate),
            canView: !!(oResolved.canView || oResolved.CanView || oOptions.canView),
            canEdit: !!(oResolved.canEdit || oResolved.CanEdit || oOptions.canEdit),
            canDelete: !!(oResolved.canDelete || oResolved.CanDelete || oOptions.canDelete),
            reasonCode: text(oResolved.reasonCode || oResolved.ReasonCode, oOptions.reasonCode || "AUTHORIZED"),
            message: text(oResolved.message || oResolved.Message, oOptions.message || ""),
            requestedActivity: text(oResolved.requestedActivity || oResolved.RequestedActivity, oOptions.requestedActivity || "")
        };
    }

    function buildGuard(oPermission, sRootId, mDefaults) {
        var oResolved = normalizePermission(oPermission, sRootId, mDefaults);
        return {
            rootId: oResolved.rootId,
            userId: oResolved.userId,
            canCreate: !!oResolved.canCreate,
            canView: !!oResolved.canView,
            canEdit: !!oResolved.canEdit,
            canDelete: !!oResolved.canDelete,
            reasonCode: oResolved.reasonCode,
            message: oResolved.message,
            requestedActivity: oResolved.requestedActivity
        };
    }

    function buildDeniedViewState(oPermission, sRootId) {
        var oGuard = buildGuard(oPermission, sRootId, {
            canView: false,
            reasonCode: "NO_VIEW_PERMISSION"
        });
        return {
            busy: false,
            rootId: oGuard.rootId,
            reasonCode: oGuard.reasonCode,
            message: oGuard.message
        };
    }

    return {
        normalizePermission: normalizePermission,
        buildGuard: buildGuard,
        buildDeniedViewState: buildDeniedViewState
    };
});
