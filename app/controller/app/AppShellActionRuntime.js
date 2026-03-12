sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/LoadCurrentUserUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ControllerModelRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RetryCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SecurityTokenRefresh",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/UiDecisionCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/contracts/ModelContracts"
], function (LoadCurrentUserUseCase, FeedbackBannerRuntime, ControllerModelRuntime, ModelStateRuntime, RetryCoordinator, SecurityTokenRefresh, UiDecisionCoordinator, ModelContracts) {
    "use strict";

    var MODELS = ModelContracts.MODELS;
    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var APP_VIEW_MODEL = MODELS.APP_VIEW;

    var SHELL_OVERLAY_FRAGMENTS = {
        help: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.ShellHelpPopover",
        settings: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.ShellSettingsPopover",
        user: "PRODUCTION_CONTROL_CHECKLIST.view.fragment.ShellUserPopover"
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
        var oAppView = ControllerModelRuntime.appView(oController);
        var bAlreadyBusy = !!ModelStateRuntime.read(oController, APP_VIEW_MODEL, "/shell/userRefreshBusy", false);
        if (!oState || bAlreadyBusy) {
            return Promise.resolve(false);
        }
        if (oAppView) {
            ModelStateRuntime.writeOnModel(oAppView, "/shell/userRefreshBusy", true);
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
            if (oAppView) {
                ModelStateRuntime.writeOnModel(oAppView, "/shell/userRefreshBusy", false);
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

    function refreshShellUserContext(oController) {
        var oModel = ControllerModelRuntime.defaultModel(oController);
        function finalize() {
            oController._closeShellOverlay("user");
            oController._syncShellState();
            oController._syncShellMetrics();
        }
        return SecurityTokenRefresh.refresh(oModel).then(function () {
            return refreshCurrentUser(oController).then(function () {
                finalize();
            });
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
