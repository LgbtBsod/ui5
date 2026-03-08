sap.ui.define([], function () {
    "use strict";

    function readUserNameFromModel(oModel) {
        if (!oModel || typeof oModel.getProperty !== "function") {
            return "";
        }
        return String(oModel.getProperty("/currentUser/uname") || "").trim();
    }

    function resolveUserName(mDeps) {
        return String(
            readUserNameFromModel(mDeps && mDeps.stateModel) ||
            readUserNameFromModel(mDeps && mDeps.state) ||
            readUserNameFromModel(mDeps && mDeps.uiState) ||
            ""
        ).trim();
    }

    function withUserName(oPayload, mDeps) {
        var sUserName = resolveUserName(mDeps);
        var oNextPayload = Object.assign({}, oPayload || {});

        if (!sUserName) {
            return oNextPayload;
        }

        if (!Object.prototype.hasOwnProperty.call(oNextPayload, "Uname")) {
            oNextPayload.Uname = sUserName;
        }
        if (!Object.prototype.hasOwnProperty.call(oNextPayload, "UserId")) {
            oNextPayload.UserId = sUserName;
        }

        return oNextPayload;
    }

    return {
        resolveUserName: resolveUserName,
        withUserName: withUserName
    };
});
