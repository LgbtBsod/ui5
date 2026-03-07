sap.ui.define([
    "sap_ui5/service/domain/shared/usecases/LoadCurrentUserUseCase"
], function (LoadCurrentUserUseCase) {
    "use strict";

    function refreshCurrentUser(oController) {
        var oState = oController && oController._getStateModel && oController._getStateModel();
        var oAppView = oController && oController._getAppViewModel && oController._getAppViewModel();
        if (!oState) {
            return Promise.resolve(false);
        }
        if (oAppView) {
            oAppView.setProperty("/shell/userRefreshBusy", true);
        }
        return LoadCurrentUserUseCase.refresh({
            stateModel: oState
        }).then(function (oResult) {
            oController._syncShellState();
            oController._syncShellMetrics();
            if (typeof oController.showI18nToast === "function" && oResult && oResult.ok) {
                oController.showI18nToast("shellContextRefreshed");
            }
            return !!(oResult && oResult.ok);
        }).catch(function (oError) {
            if (typeof oController.showI18nToast === "function") {
                oController.showI18nToast("testUserApplyFailed", [oError && oError.message || "Unknown error"]);
            }
            return false;
        }).finally(function () {
            if (oAppView) {
                oAppView.setProperty("/shell/userRefreshBusy", false);
            }
        });
    }

    return {
        refreshCurrentUser: refreshCurrentUser
    };
});
