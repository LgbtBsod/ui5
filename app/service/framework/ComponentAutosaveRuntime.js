sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentSaveGuardContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (ModelStateRuntime, FeedbackBannerRuntime, ComponentSaveGuardContracts, CloneUtil, WorkflowContracts) {
    "use strict";

    var BANNER_LEVEL = ComponentSaveGuardContracts.BANNER_LEVEL;
    var BANNER_TEXT_KEY = ComponentSaveGuardContracts.BANNER_TEXT_KEY;

    function createAutoSaveManager(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oSnapshotModel = mOptions.snapshotModel;
        var mTimerDefaults = mOptions.timerDefaults;
        var StatePaths = mOptions.statePaths;
        var DeltaPayloadBuilder = mOptions.deltaPayloadBuilder;
        var fnResolveDetailCurrent = mOptions.resolveDetailCurrent;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var DebugLogger = mOptions.debugLogger;
        var ActionContract = mOptions.actionContract;
        var fnBundleText = mOptions.bundleText;
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;

        oComponent._oAutoSave = new mOptions.managers.AutoSaveCoordinator({
            intervalMs: Number(mTimerDefaults.autoSaveIntervalMs),
            debounceMs: Number(mTimerDefaults.autoSaveDebounceMs),
            lockGuardFn: function () {
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
            },
            guardFn: function () {
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") === WorkflowContracts.EDIT_MODES.EDIT &&
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") === WorkflowContracts.LOCK_STATES.EDIT_LOCKED &&
                    !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
            },
            shouldSave: function () {
                var bIsLocked = ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") === WorkflowContracts.EDIT_MODES.EDIT &&
                    bIsLocked &&
                    !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false) &&
                    !!ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
            },
            buildPayload: function () {
                var sId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var oCurrent = fnResolveDetailCurrent();
                var oBase = ModelStateRuntime.readOnModel(oSnapshotModel, "/", {}) || {};
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
            var mStart = { "/autosaveState": WorkflowContracts.AUTOSAVE_STATES.SAVING };
            mStart[StatePaths.SAVE_IN_FLIGHT] = true;
            ModelStateRuntime.setManyOnModel(oStateModel, mStart);
            DebugLogger.info("Component", "autosave start", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
            fnEmitTelemetry("autosave.triggered", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveDone", function () {
            var mDone = { "/autosaveState": WorkflowContracts.AUTOSAVE_STATES.SAVED, "/autosaveAt": new Date().toISOString() };
            mDone[StatePaths.SAVE_IN_FLIGHT] = false;
            ModelStateRuntime.setManyOnModel(oStateModel, mDone);
            DebugLogger.info("Component", "autosave done", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveError", function (oEvent) {
            var mErr = { "/autosaveState": WorkflowContracts.AUTOSAVE_STATES.FAILED };
            mErr[StatePaths.SAVE_IN_FLIGHT] = false;
            ModelStateRuntime.setManyOnModel(oStateModel, mErr);
            fnSetGlobalBanner(FeedbackBannerRuntime.createRetryBannerInput(BANNER_LEVEL.ERROR, BANNER_TEXT_KEY.OBJECT_SAVE_FAILED, {
                textArgs: [fnBundleText("autosaveError")],
                retryAction: ActionContract.RETRY_ACTIONS.SAVE,
                retryTextKey: BANNER_TEXT_KEY.RETRY_NOW
            }));
            DebugLogger.info("Component", "autosave error", oEvent && oEvent.getParameters ? oEvent.getParameters() : {});
            fnEmitTelemetry("autosave.failed", ComponentRuntimeSupport.eventPayload(oEvent));
        });
        return oComponent._oAutoSave;
    }

    return {
        createAutoSaveManager: createAutoSaveManager
    };
});
