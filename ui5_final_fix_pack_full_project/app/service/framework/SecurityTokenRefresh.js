sap.ui.define([], function () {
    "use strict";

    function normalizeRefreshError(oError) {
        var oResolved = oError instanceof Error ? oError : new Error(String((oError && oError.message) || oError || "security_token_refresh_failed"));
        if (!oResolved.code) {
            oResolved.code = "SECURITY_TOKEN_REFRESH_FAILED";
        }
        return oResolved;
    }

    function refresh(oModel) {
        if (!oModel || typeof oModel.refreshSecurityToken !== "function") {
            return Promise.reject(normalizeRefreshError("security_token_refresh_unavailable"));
        }
        return new Promise(function (resolve, reject) {
            oModel.refreshSecurityToken(function () { resolve(true); }, function (oError) { reject(normalizeRefreshError(oError)); }, true);
        });
    }

    return {
        refresh: refresh
    };
});
