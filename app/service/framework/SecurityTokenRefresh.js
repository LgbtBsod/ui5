sap.ui.define([], function () {
    "use strict";

    function refresh(oModel) {
        if (!oModel || typeof oModel.refreshSecurityToken !== "function") {
            return Promise.resolve(false);
        }
        return new Promise(function (resolve) {
            oModel.refreshSecurityToken(function () { resolve(true); }, function () { resolve(false); }, true);
        });
    }

    return {
        refresh: refresh
    };
});
