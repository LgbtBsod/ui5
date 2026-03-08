sap.ui.define([
    "checklist/app/service/framework/ModelStateRuntime"
], function (ModelStateRuntime) {
    "use strict";

    /**
     * Resolves the current SAP user name from the state model.
     * Required for function imports (LockAcquire, SaveChanges, etc.) on real SAP Gateway.
     *
     * @param {object} mDeps - dependencies object containing stateModel
     * @returns {string} SAP user name (Uname), empty string if unavailable
     */
    function resolveUserName(mDeps) {
        var oStateModel = mDeps && mDeps.stateModel;
        if (!oStateModel) {
            return "";
        }
        var oCurrentUser = ModelStateRuntime.readOnModel(oStateModel, "/currentUser", null);
        return String((oCurrentUser && oCurrentUser.uname) || "").trim();
    }

    /**
     * Enriches a payload with the current SAP user name (Uname).
     * SAP Gateway function imports require Uname for authorization checks.
     *
     * @param {object} oPayload - the OData function import payload
     * @param {object} mDeps - dependencies object containing stateModel
     * @returns {object} payload enriched with Uname (if available)
     */
    function withUserName(oPayload, mDeps) {
        var sUname = resolveUserName(mDeps);
        var oResult = Object.assign({}, oPayload || {});
        if (sUname) {
            oResult.Uname = sUname;
        }
        return oResult;
    }

    return {
        resolveUserName: resolveUserName,
        withUserName: withUserName
    };
});
