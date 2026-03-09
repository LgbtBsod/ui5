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
            canView: oResolved.canView !== false && oResolved.CanView !== false && oOptions.canView !== false,
            canEdit: oResolved.canEdit !== false && oResolved.CanEdit !== false && oOptions.canEdit !== false,
            canDelete: oResolved.canDelete !== false && oResolved.CanDelete !== false && oOptions.canDelete !== false,
            reasonCode: text(oResolved.reasonCode || oResolved.ReasonCode, oOptions.reasonCode || "AUTHORIZED"),
            message: text(oResolved.message || oResolved.Message, oOptions.message || "")
        };
    }

    function buildGuard(oPermission, sRootId, mDefaults) {
        var oResolved = normalizePermission(oPermission, sRootId, mDefaults);
        return {
            rootId: oResolved.rootId,
            userId: oResolved.userId,
            canView: !!oResolved.canView,
            canEdit: !!oResolved.canEdit,
            canDelete: !!oResolved.canDelete,
            reasonCode: oResolved.reasonCode,
            message: oResolved.message
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
