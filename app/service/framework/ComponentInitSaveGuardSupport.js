sap.ui.define([
    "checklist/app/service/framework/ActionContract",
    "checklist/app/service/framework/SecurityTokenRefresh",
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/RootIdRuntime",
    "checklist/app/service/framework/TelemetryRuntime"
], function (ActionContract, SecurityTokenRefresh, FeedbackBannerRuntime, RootIdRuntime, TelemetryRuntime) {
    "use strict";

    var SAVE_WORKING_BANNER_DELAY_MS = 2000;

    function buildSaveBannerPayload(oStateModel, mOptions) {
        var bSessionExpired = !!mOptions.sessionExpired;
        var bOffline = oStateModel.getProperty("/networkOnline") === false;
        var sDetail = String(mOptions.detail || "save_failed");
        return FeedbackBannerRuntime.createRetryBannerInput(
            bSessionExpired ? "error" : (bOffline ? "warning" : "error"),
            bSessionExpired ? "sessionExpiredBanner" : (bOffline ? "networkUnavailable" : "objectSaveFailed"),
            {
                textArgs: bSessionExpired ? [] : [sDetail],
                details: sDetail,
                correlationId: String(mOptions.correlationId || ""),
                retryAction: ActionContract.RETRY_ACTIONS.SAVE,
                retryTextKey: "retryNowButton"
            }
        );
    }

    function createRunGuardedSave(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oMainServiceModel = mOptions.mainServiceModel;
        var oStatePaths = mOptions.statePaths || {};
        var oDetailFacade = mOptions.detailFacade;
        var fnBuildLatestCtx = mOptions.buildLatestCtx;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var fnResumePendingNavigationIntent = mOptions.resumePendingNavigationIntent;
        var fnResolveCorrelationId = mOptions.resolveCorrelationId;
        var fnIsSessionExpiredError = mOptions.isSessionExpiredError;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnClearGlobalBanner = mOptions.clearGlobalBanner;

        return function () {
            var sRootId;
            if (oStateModel.getProperty(oStatePaths.SAVE_IN_FLIGHT)) {
                return oComponent._pGuardedSavePromise || Promise.resolve(false);
            }
            sRootId = RootIdRuntime.resolveFromStateModel(oStateModel);
            if (!sRootId) {
                return Promise.resolve(false);
            }
            oStateModel.setProperty(oStatePaths.SAVE_IN_FLIGHT, true);
            oStateModel.setProperty(oStatePaths.UI_BUSY_DETAIL, true);
            oStateModel.setProperty(oStatePaths.UI_BUSY_GLOBAL, true);
            oStateModel.setProperty("/autosaveState", "SAVING");
            if (oComponent._iSaveWorkingTimer) {
                clearTimeout(oComponent._iSaveWorkingTimer);
                oComponent._iSaveWorkingTimer = null;
            }
            oComponent._iSaveWorkingTimer = setTimeout(function () {
                if (oStateModel.getProperty(oStatePaths.SAVE_IN_FLIGHT)) {
                    fnSetGlobalBanner(FeedbackBannerRuntime.createRetryBannerInput("info", "workingMessageLong", {
                        retryAction: ActionContract.RETRY_ACTIONS.SAVE,
                        retryTextKey: "retryNowButton"
                    }));
                }
            }, SAVE_WORKING_BANNER_DELAY_MS);
            oComponent._pGuardedSavePromise = oDetailFacade.save({ rootId: sRootId }, fnBuildLatestCtx()).then(function (oResult) {
                fnApplyFacadeResult(oResult);
                if (!oResult || oResult.ok === false) {
                    return Promise.reject((oResult && oResult.error) || new Error("Save failed"));
                }
                fnClearGlobalBanner();
                fnEmitTelemetry("detail.save.guarded.success", { rootId: sRootId });
                fnResumePendingNavigationIntent();
                return true;
            }).catch(function (oError) {
                var sDetail = String((oError && oError.message) || "save_failed");
                var sCorrelationId = fnResolveCorrelationId(oError);
                var bSessionExpired = fnIsSessionExpiredError(oError);
                if (bSessionExpired) {
                    oStateModel.setProperty("/requiresUserLogin", true);
                    if (!oComponent._bSessionRefreshInFlight) {
                        oComponent._bSessionRefreshInFlight = true;
                        if (!oMainServiceModel) {
                            oComponent._bSessionRefreshInFlight = false;
                        } else {
                            SecurityTokenRefresh.refresh(oMainServiceModel).finally(function () {
                                oComponent._bSessionRefreshInFlight = false;
                            });
                        }
                    }
                }
                fnSetGlobalBanner(buildSaveBannerPayload(oStateModel, {
                    sessionExpired: bSessionExpired,
                    detail: sDetail,
                    correlationId: sCorrelationId
                }));
                fnEmitTelemetry("detail.save.guarded.failed", TelemetryRuntime.saveFailure(sRootId, oError, sCorrelationId));
                return false;
            }).finally(function () {
                if (oComponent._iSaveWorkingTimer) {
                    clearTimeout(oComponent._iSaveWorkingTimer);
                    oComponent._iSaveWorkingTimer = null;
                }
                oStateModel.setProperty(oStatePaths.SAVE_IN_FLIGHT, false);
                oStateModel.setProperty(oStatePaths.UI_BUSY_DETAIL, false);
                oStateModel.setProperty(oStatePaths.UI_BUSY_GLOBAL, false);
                oComponent._pGuardedSavePromise = null;
            });
            return oComponent._pGuardedSavePromise;
        };
    }

    return {
        createRunGuardedSave: createRunGuardedSave
    };
});
