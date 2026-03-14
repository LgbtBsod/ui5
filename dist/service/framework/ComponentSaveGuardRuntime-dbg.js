sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SecurityTokenRefresh",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RootIdRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TelemetryRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentSaveGuardContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentSaveGuardPolicy"
], function (SecurityTokenRefresh, ModelStateRuntime, RootIdRuntime, TelemetryRuntime, SchedulingRuntime, ComponentSaveGuardContracts, ComponentSaveGuardPolicy) {
    "use strict";

    var AUTOSAVE_STATE = ComponentSaveGuardContracts.AUTOSAVE_STATE;
    var BANNER_DETAIL = ComponentSaveGuardContracts.BANNER_DETAIL;
    var DELAY_MS = ComponentSaveGuardContracts.DELAY_MS;
    var ERROR_MESSAGE = ComponentSaveGuardContracts.ERROR_MESSAGE;
    var PATHS = ComponentSaveGuardContracts.PATHS;
    var TELEMETRY_EVENT = ComponentSaveGuardContracts.TELEMETRY_EVENT;

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
                m["/autosaveState"] = AUTOSAVE_STATE.SAVING;
                return m;
            }()));
            oComponent._iSaveWorkingTimer = SchedulingRuntime.restartTimer(oComponent._iSaveWorkingTimer, function () {
                if (ModelStateRuntime.readOnModel(oStateModel, oStatePaths.SAVE_IN_FLIGHT, false)) {
                    fnSetGlobalBanner(ComponentSaveGuardPolicy.buildWorkingBannerPayload());
                }
            }, DELAY_MS.SAVE_WORKING_BANNER);
            oComponent._pGuardedSavePromise = oDetailFacade.save({ rootId: sRootId }, fnBuildLatestCtx()).then(function (oResult) {
                fnApplyFacadeResult(oResult);
                if (!oResult || oResult.ok === false) {
                    return Promise.reject((oResult && oResult.error) || new Error(ERROR_MESSAGE.SAVE_FAILED));
                }
                fnClearGlobalBanner();
                fnEmitTelemetry(TELEMETRY_EVENT.GUARDED_SUCCESS, { rootId: sRootId });
                fnResumePendingNavigationIntent();
                return true;
            }).catch(function (oError) {
                var sDetail = String((oError && oError.message) || BANNER_DETAIL.SAVE_FAILED);
                var sCorrelationId = fnResolveCorrelationId(oError);
                var bSessionExpired = fnIsSessionExpiredError(oError);
                if (bSessionExpired) {
                    ModelStateRuntime.writeOnModel(oStateModel, PATHS.REQUIRES_USER_LOGIN, true);
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
                fnSetGlobalBanner(ComponentSaveGuardPolicy.buildSaveBannerPayload(oStateModel, {
                    sessionExpired: bSessionExpired,
                    detail: sDetail,
                    correlationId: sCorrelationId
                }));
                fnEmitTelemetry(TELEMETRY_EVENT.GUARDED_FAILED, TelemetryRuntime.saveFailure(sRootId, oError, sCorrelationId));
                return false;
            }).finally(function () {
                oComponent._iSaveWorkingTimer = SchedulingRuntime.clearTimer(oComponent._iSaveWorkingTimer);
                ModelStateRuntime.setManyOnModel(oStateModel, (function () {
                    var m = {};
                    m[oStatePaths.SAVE_IN_FLIGHT] = false;
                    m[oStatePaths.UI_BUSY_DETAIL] = false;
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
