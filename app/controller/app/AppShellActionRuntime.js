sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/LoadCurrentUserUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/RetryCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants"
], function (LoadCurrentUserUseCase, FeedbackBannerRuntime, ControllerModelRuntime, ModelStateRuntime, RetryCoordinator, UiDecisionCoordinator, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var SHELL_MODEL = MODELS.SHELL;

    var SHELL_OVERLAY_FRAGMENTS = {
        help: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.ShellHelpPopover",
        settings: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.ShellSettingsPopover",
        user: "PRODUCTION_CONTROL_CHECKLIST.views.fragment.ShellUserPopover"
    };

    function openShellOverlayByKey(oController, oEvent, sKey) {
        var sFragment = SHELL_OVERLAY_FRAGMENTS[sKey];
        if (!sFragment) {
            return Promise.resolve();
        }
        return oController._openShellOverlay(oEvent, sKey, sFragment);
    }

    function refreshCurrentUser(oController) {
        var oState = ControllerModelRuntime.state(oController);
        var oShellModel = ControllerModelRuntime.shell(oController);
        var bAlreadyBusy = !!ModelStateRuntime.read(oController, SHELL_MODEL, MODEL_PATHS.SHELL_USER_REFRESH_BUSY, false);
        if (!oState || bAlreadyBusy) {
            return Promise.resolve(false);
        }
        if (oShellModel) {
            ModelStateRuntime.writeOnModel(oShellModel, MODEL_PATHS.SHELL_USER_REFRESH_BUSY, true);
        }
        return LoadCurrentUserUseCase.refresh({
            stateModel: oState
        }).then(function (oResult) {
            oController._syncShellState();
            oController._syncShellMetrics();
            if (oResult && oResult.ok) {
                UiDecisionCoordinator.notifyShellRefreshSuccess({ controller: oController });
            }
            return !!(oResult && oResult.ok);
        }).catch(function (oError) {
            UiDecisionCoordinator.notifyShellRefreshFailure({ controller: oController, error: oError });
            return false;
        }).finally(function () {
            if (oShellModel) {
                ModelStateRuntime.writeOnModel(oShellModel, MODEL_PATHS.SHELL_USER_REFRESH_BUSY, false);
            }
        });
    }

    function runRetry(oController, vRetryAction) {
        return RetryCoordinator.runRetry(oController, vRetryAction);
    }

    function runGlobalBannerRetry(oController) {
        var oState = ControllerModelRuntime.state(oController);
        var sAction = String(FeedbackBannerRuntime.getBannerProperty(oState, "global", "retryAction") || "").trim();
        return runRetry(oController, sAction);
    }

    function refreshSecurityToken(oModel) {
        if (!oModel || typeof oModel.refreshSecurityToken !== "function") {
            return Promise.reject(new Error("security_token_refresh_unavailable"));
        }
        return new Promise(function (resolve, reject) {
            oModel.refreshSecurityToken(function () { resolve(true); }, reject, true);
        });
    }

    function refreshShellUserContext(oController) {
        var oModel = ControllerModelRuntime.defaultModel(oController);
        function syncShellUi() {
            oController._syncShellState();
            oController._syncShellMetrics();
        }
        return refreshSecurityToken(oModel).then(function () {
            return refreshCurrentUser(oController);
        }).then(function (bRefreshed) {
            if (bRefreshed) {
                oController._closeShellOverlay("user");
            }
            syncShellUi();
            return bRefreshed;
        }).catch(function (oError) {
            UiDecisionCoordinator.notifyShellRefreshFailure({ controller: oController, error: oError });
            syncShellUi();
            throw oError;
        });
    }

    return {
        openShellOverlayByKey: openShellOverlayByKey,
        refreshCurrentUser: refreshCurrentUser,
        runRetry: runRetry,
        runGlobalBannerRetry: runGlobalBannerRetry,
        refreshShellUserContext: refreshShellUserContext
    };
});
