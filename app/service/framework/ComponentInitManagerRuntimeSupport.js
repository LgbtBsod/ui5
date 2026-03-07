sap.ui.define([
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/ModelStateRuntime",
    "checklist/app/service/framework/TelemetryRuntime",
    "checklist/app/util/CloneUtil"
], function (FeedbackBannerRuntime, ModelStateRuntime, TelemetryRuntime, CloneUtil) {
    "use strict";

    function attach(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var mTimerDefaults = mOptions.timerDefaults;
        var mManagers = mOptions.managers;
        var StatePaths = mOptions.statePaths;
        var DeltaPayloadBuilder = mOptions.deltaPayloadBuilder;
        var fnResolveDetailCurrent = mOptions.resolveDetailCurrent;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var DebugLogger = mOptions.debugLogger;
        var ActionContract = mOptions.actionContract;
        var fnBundleText = mOptions.bundleText;

        oComponent._oSmartCache = new mManagers.SmartCacheManager({ freshMs: mTimerDefaults.cacheFreshMs, staleOkMs: mTimerDefaults.cacheStaleOkMs });
        oComponent._oHeartbeat = new mManagers.HeartbeatManager({
            intervalMs: Number(mTimerDefaults.heartbeatMs),
            heartbeatFn: function () {
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") !== "EDIT"
                    || ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") !== "LOCKED") {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock.heartbeat !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock.heartbeat" });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock.heartbeat({
                    rootId: sRootId,
                    sessionGuid: sSessionGuid
                }).then(function (oRes) {
                    return Object.assign({ rootId: sRootId, sessionGuid: sSessionGuid }, oRes || {});
                });
            }
        });
        oComponent._oGcd = new mManagers.GCDManager({ intervalMs: Number(mTimerDefaults.gcdMs) });
        oComponent._oActivity = new mManagers.ActivityMonitor({ idleMs: Number(mTimerDefaults.idleMs) });
        oComponent._oAutoSave = new mManagers.AutoSaveCoordinator({
            intervalMs: Number(mTimerDefaults.autoSaveIntervalMs),
            debounceMs: Number(mTimerDefaults.autoSaveDebounceMs),
            lockGuardFn: function () {
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") === "LOCKED";
            },
            guardFn: function () {
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") === "EDIT"
                    && ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") === "LOCKED"
                    && !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
            },
            shouldSave: function () {
                var bIsLocked = ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") === "LOCKED";
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") === "EDIT"
                    && bIsLocked
                    && !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false)
                    && !!ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "")
                    && ModelStateRuntime.readOnModel(oStateModel, "/networkOnline", true) !== false;
            },
            buildPayload: function () {
                var sId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var oCurrent = fnResolveDetailCurrent();
                var oBase = ModelStateRuntime.readOnModel(oUiStateModel, "/_detailSnapshot", {}) || {};
                if (!sId || !oCurrent || !oCurrent.root || oCurrent.root.id !== sId) {
                    return null;
                }
                var oDelta = DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oBase);
                if (!oDelta) {
                    return null;
                }
                return { id: sId, payload: oDelta, fullPayload: CloneUtil.clone(oCurrent, {}) };
            },
            saveFn: function (oPayload) {
                if (!oComponent._detailFacade || !oComponent._ctx) {
                    return Promise.reject(new Error("Autosave unavailable: detail context missing"));
                }
                return oComponent._detailFacade.autosave({ rootId: oPayload.id, delta: oPayload.payload }, oComponent._ctx).then(function (oResult) {
                    fnApplyFacadeResult(oResult);
                    if (!oResult || oResult.ok === false) {
                        return Promise.reject((oResult && oResult.error) || new Error("Autosave usecase failed"));
                    }
                    ModelStateRuntime.writeOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
                    return oResult.data || {};
                });
            }
        });
        oComponent._oAutoSave.attachEvent("autosaveStart", function () {
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/autosaveState": "SAVING",
                [StatePaths.SAVE_IN_FLIGHT]: true
            });
            if (ModelStateRuntime.readOnModel(oStateModel, "/networkOnline", true) === false) {
                fnSetGlobalBanner(FeedbackBannerRuntime.createNetworkRetryBannerInput(
                    ActionContract.RETRY_ACTIONS.SAVE,
                    "retryNowButton"
                ));
            }
            DebugLogger.info("Component", "autosave start", TelemetryRuntime.objectRefFromStateModel(oStateModel));
            fnEmitTelemetry("autosave.triggered", TelemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveDone", function () {
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/autosaveState": "SAVED",
                "/autosaveAt": new Date().toISOString(),
                [StatePaths.SAVE_IN_FLIGHT]: false
            });
            DebugLogger.info("Component", "autosave done", TelemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveError", function (oEvent) {
            ModelStateRuntime.setManyOnModel(oStateModel, {
                "/autosaveState": "ERROR",
                [StatePaths.SAVE_IN_FLIGHT]: false
            });
            fnSetGlobalBanner(
                ModelStateRuntime.readOnModel(oStateModel, "/networkOnline", true) === false
                    ? FeedbackBannerRuntime.createNetworkRetryBannerInput(
                        ActionContract.RETRY_ACTIONS.SAVE,
                        "retryNowButton"
                    )
                    : FeedbackBannerRuntime.createRetryBannerInput("error", "objectSaveFailed", {
                        textArgs: [fnBundleText("autosaveError")],
                        retryAction: ActionContract.RETRY_ACTIONS.SAVE,
                        retryTextKey: "retryNowButton"
                    })
            );
            DebugLogger.info("Component", "autosave error", oEvent && oEvent.getParameters ? oEvent.getParameters() : {});
            fnEmitTelemetry("autosave.failed", mOptions.componentRuntimeSupport.eventPayload(oEvent));
        });

        oComponent._oConnectivity = new mManagers.ConnectivityCoordinator({ graceMs: Number(mTimerDefaults.networkGraceMs) });
        oComponent._oLockStatus = new mManagers.LockStatusMonitor({
            intervalMs: Number(mTimerDefaults.lockStatusMs),
            checkFn: function () {
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") !== "EDIT"
                    || ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") !== "LOCKED") {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock.status !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock.status" });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock.status({
                    rootId: sRootId,
                    sessionGuid: sSessionGuid
                }).then(function (oRes) {
                    return Object.assign({ rootId: sRootId, sessionGuid: sSessionGuid }, oRes || {});
                });
            }
        });
    }

    return {
        attach: attach
    };
});
