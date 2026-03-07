sap.ui.define([
    "sap_ui5/model/StatePaths",
    "sap_ui5/service/domain/shared/usecases/LoadCurrentUserUseCase"
], function (StatePaths, LoadCurrentUserUseCase) {
    "use strict";

    function confirmTestUser(oStateModel, sLogin) {
        var sUser = String(sLogin || "").trim();
        if (!oStateModel || !sUser) {
            return Promise.resolve({ ok: false, messageKey: "testUserEmpty" });
        }
        return LoadCurrentUserUseCase.execute({ login: sUser }, {
            stateModel: oStateModel
        }).then(function (oResult) {
            if (oResult && oResult.ok) {
                oResult.user = oResult.data && oResult.data.user || sUser;
                oResult.sessionId = oStateModel.getProperty(StatePaths.SESSION_ID) || "";
            }
            return oResult;
        });
    }

    return { confirmTestUser: confirmTestUser };
});
