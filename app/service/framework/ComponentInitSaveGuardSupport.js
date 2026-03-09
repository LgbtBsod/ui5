sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ActionContract",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SecurityTokenRefresh",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime"
], function (ActionContract, SecurityTokenRefresh, FeedbackBannerRuntime, ModelStateRuntime, RootIdRuntime, TelemetryRuntime, SchedulingRuntime) {
    "use strict";

    var SAVE_WORKING_BANNER_DELAY_MS = 2000;

    function buildSaveBannerPayload(oStateModel, mOptions) {
        var bSessionExpired = !!mOptions.sessionExpired;
        var bOffline = ModelStateRuntime.readOnModel(oStateModel, "/networkOnline", true) === false;
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
            if (ModelStateRuntime.readOnModel(oStateModel, oStatePaths.SAVE_IN_FLIGHT, false)) {
                return oComponent._pGuardedSavePromise || Promise.resolve(false);
            }
            sRootId = RootIdRuntime.resolveFromStateModel(oStateModel);
            if (!sRootId) {
                return Promise.resolve(false);
            }
            ModelStateRuntime.setManyOnModel(oStateModel, (function () {
                var m = {};
                m[oStatePaths.SAVE_IN_FLIGHT] = true;
                m[oStatePaths.UI_BUSY_DETAIL] = true;
                m[oStatePaths.UI_BUSY_GLOBAL] = true;
                m["/autosaveState"] = "SAVING";
                return m;
            }()));
            oComponent._iSaveWorkingTimer = SchedulingRuntime.restartTimer(oComponent._iSaveWorkingTimer, function () {
                if (ModelStateRuntime.readOnModel(oStateModel, oStatePaths.SAVE_IN_FLIGHT, false)) {
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
                    ModelStateRuntime.writeOnModel(oStateModel, "/requiresUserLogin", true);
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
                oComponent._iSaveWorkingTimer = SchedulingRuntime.clearTimer(oComponent._iSaveWorkingTimer);
                ModelStateRuntime.setManyOnModel(oStateModel, (function () {
                    var m = {};
                    m[oStatePaths.SAVE_IN_FLIGHT] = false;
                    m[oStatePaths.UI_BUSY_DETAIL] = false;
                    m[oStatePaths.UI_BUSY_GLOBAL] = false;
                    return m;
                }()));
                oComponent._pGuardedSavePromise = null;
            });
            return oComponent._pGuardedSavePromise;
        };
    }

    return {
        createRunGuardedSave: createRunGuardedSave
    };
});
