sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants"
], function (MessageCodeConstants) {
    "use strict";

    var DETAIL_CODES = MessageCodeConstants.DETAIL;

    function text(vValue, sFallback) {
        var sValue = String(vValue || "").trim();
        return sValue || String(sFallback || "").trim();
    }

    function normalizePermission(oPermission, sDbKey, mDefaults) {
        var oResolved = oPermission || {};
        var oOptions = mDefaults || {};
        var sCanonicalDbKey = text(oResolved.dbKey || oResolved.DB_KEY || oResolved.rootId, sDbKey);
        return {
            dbKey: sCanonicalDbKey,
            userId: text(oResolved.userId || oResolved.UserId, ""),
            canCreate: !!(oResolved.canCreate || oResolved.CanCreate || oOptions.canCreate),
            canView: !!(oResolved.canView || oResolved.CanView || oOptions.canView),
            canEdit: !!(oResolved.canEdit || oResolved.CanEdit || oOptions.canEdit),
            canDelete: !!(oResolved.canDelete || oResolved.CanDelete || oOptions.canDelete),
            reasonCode: text(oResolved.reasonCode || oResolved.ReasonCode, oOptions.reasonCode || DETAIL_CODES.AUTHORIZED),
            message: text(oResolved.message || oResolved.Message, oOptions.message || ""),
            requestedActivity: text(oResolved.requestedActivity || oResolved.RequestedActivity, oOptions.requestedActivity || "")
        };
    }

    function buildGuard(oPermission, sDbKey, mDefaults) {
        var oResolved = normalizePermission(oPermission, sDbKey, mDefaults);
        return {
            dbKey: oResolved.dbKey,
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

    function buildDeniedViewState(oPermission, sDbKey) {
        var oGuard = buildGuard(oPermission, sDbKey, {
            canView: false,
            reasonCode: DETAIL_CODES.NO_VIEW_PERMISSION
        });
        return {
            busy: false,
            dbKey: oGuard.dbKey,
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
