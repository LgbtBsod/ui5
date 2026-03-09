sap.ui.define([
    "checklist/app/service/domain/shared/usecases/LoadCurrentUserUseCase",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/ControllerModelRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/RetryCoordinator",
    "checklist/app/service/framework/SecurityTokenRefresh",
    "checklist/app/service/framework/UiDecisionCoordinator"
], function (LoadCurrentUserUseCase, FeedbackBannerRuntime, ControllerModelRuntime, ModelStateRuntime, RetryCoordinator, SecurityTokenRefresh, UiDecisionCoordinator) {
    "use strict";

    var SHELL_OVERLAY_FRAGMENTS = {
        notifications: "checklist.app.view.fragment.ShellNotificationsPopover",
        help: "checklist.app.view.fragment.ShellHelpPopover",
        settings: "checklist.app.view.fragment.ShellSettingsPopover",
        user: "checklist.app.view.fragment.ShellUserPopover"
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
        var bAlreadyBusy = !!ModelStateRuntime.read(oController, "appView", "/shell/userRefreshBusy", false);
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
