sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentSaveGuardContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/detail/DetailPersistenceRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts"
], function (ModelStateRuntime, FeedbackBannerRuntime, ComponentSaveGuardContracts, CloneUtil, CreateSentinel, WorkflowContracts, DetailPersistenceRuntime, ModelPathContracts, DetailMessageKeyConstants) {
    "use strict";

    var BANNER_LEVEL = ComponentSaveGuardContracts.BANNER_LEVEL;
    var BANNER_TEXT_KEY = ComponentSaveGuardContracts.BANNER_TEXT_KEY;

    function createUiStateAdapter(oStateModel) {
        return {
            get: function (_sModelName, sPath) {
                return ModelStateRuntime.readOnModel(oStateModel, sPath, null);
            }
        };
    }

    function hasHealthyAutosaveLockState(oStateModel, StatePaths) {
        var oUiState = createUiStateAdapter(oStateModel);
        var sActiveId = String(ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "") || "").trim();
        return DetailPersistenceRuntime.canAutosaveFromState(oUiState, {
            rootId: sActiveId,
            isCreateDraft: CreateSentinel.isCreateId(sActiveId)
        });
    }

    function createAutoSaveManager(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oDetailModel = mOptions.detailModel;
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
        var fnBuildLatestCtx = typeof mOptions.buildLatestCtx === "function" ? mOptions.buildLatestCtx : null;
        function rescheduleHeartbeat() {
            var iIntervalMs;
            var sNextHeartbeatAt;
            if (!oComponent._oHeartbeat || typeof oComponent._oHeartbeat.start !== "function") {
                return;
            }
            oComponent._oHeartbeat.start();
            iIntervalMs = Number(oComponent._oHeartbeat._iIntervalMs || 0) || 0;
            if (iIntervalMs < 1000) {
                return;
            }
            sNextHeartbeatAt = new Date(Date.now() + iIntervalMs).toISOString();
            ModelStateRuntime.writeOnModel(oStateModel, StatePaths.PERSISTENCE_NEXT_HEARTBEAT_AT, sNextHeartbeatAt);
        }

        oComponent._oAutoSave = new mOptions.managers.AutoSaveCoordinator({
            intervalMs: Number(mTimerDefaults.autoSaveIntervalMs),
            debounceMs: Number(mTimerDefaults.autoSaveDebounceMs),
            lockGuardFn: function () {
                return hasHealthyAutosaveLockState(oStateModel, StatePaths);
            },
            guardFn: function () {
                var oUiState = createUiStateAdapter(oStateModel);
                var sActiveId = String(ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "") || "").trim();
                return DetailPersistenceRuntime.canScheduleAutosave(oUiState, {
                    rootId: sActiveId,
                    isCreateDraft: CreateSentinel.isCreateId(sActiveId)
                });
            },
            shouldSave: function () {
                var oUiState = createUiStateAdapter(oStateModel);
                var sActiveId = String(ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "") || "").trim();
                return DetailPersistenceRuntime.canScheduleAutosave(oUiState, {
                    rootId: sActiveId,
                    isCreateDraft: CreateSentinel.isCreateId(sActiveId)
                });
            },
            buildPayload: function () {
                var sId = ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "");
                var oCurrent = fnResolveDetailCurrent();
                var oBase = ModelStateRuntime.readOnModel(oDetailModel, "/base", {}) || {};
                if (!sId || CreateSentinel.isCreateId(sId) || !oCurrent || !oCurrent.root || oCurrent.root.id !== sId) {
                    return null;
                }
                var oDelta = DeltaPayloadBuilder.buildDeltaPayload(oCurrent, oBase);
                if (!oDelta) {
                    return null;
                }
                return { id: sId, payload: oDelta, fullPayload: CloneUtil.clone(oCurrent, {}) };
            },
            saveFn: function (oPayload) {
                var oLatestCtx = fnBuildLatestCtx ? fnBuildLatestCtx() : oComponent._ctx;
                if (!oComponent._detailFacade || !oLatestCtx) {
                    return Promise.reject(new Error("Autosave unavailable: detail context missing"));
                }
                return oComponent._detailFacade.autosave({ rootId: oPayload.id, delta: oPayload.payload }, oLatestCtx).then(function (oResult) {
                    fnApplyFacadeResult(oResult);
                    if (!oResult || oResult.ok === false) {
                        return Promise.reject((oResult && oResult.error) || new Error("Autosave usecase failed"));
                    }
                    return oResult.data || {};
                });
            }
        });
        oComponent._oAutoSave.attachEvent("autosaveStart", function () {
            var mStart = {};
            DetailPersistenceRuntime.startEffects("auto").forEach(function (oEffect) {
                if (oEffect && oEffect.type === "modelPatch" && oEffect.modelName === "state") {
                    mStart[oEffect.path] = oEffect.value;
                }
            });
            ModelStateRuntime.setManyOnModel(oStateModel, mStart);
            DebugLogger.info("Component", "autosave start", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
            fnEmitTelemetry("autosave.triggered", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveDone", function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/autosaveAt", new Date().toISOString());
            if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") === WorkflowContracts.EDIT_MODES.EDIT &&
                ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") === WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
                rescheduleHeartbeat();
            }
            DebugLogger.info("Component", "autosave done", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveError", function (oEvent) {
            var oPayload = ComponentRuntimeSupport.eventPayload(oEvent);
            var oClassification = DetailPersistenceRuntime.classifyError(oPayload && oPayload.error);
            if (DetailPersistenceRuntime.isLockFailure(oClassification.taxonomy) && typeof oComponent._handleForceReadOnly === "function") {
                oComponent._handleForceReadOnly({
                    reason: oClassification.taxonomy,
                    messageKey: oClassification.messageKey,
                    source: WorkflowContracts.SOURCES.AUTOSAVE
                });
                return;
            }
            if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.PERSISTENCE_STATE, "") !== DetailPersistenceRuntime.STATES.LOCK_LOST) {
                fnSetGlobalBanner(FeedbackBannerRuntime.createRetryBannerInput(BANNER_LEVEL.ERROR, BANNER_TEXT_KEY.OBJECT_SAVE_FAILED, {
                    scope: "global",
                    textArgs: [fnBundleText(DetailMessageKeyConstants.AUTOSAVE_ERROR)],
                    retryAction: ActionContract.RETRY_ACTIONS.SAVE,
                    retryTextKey: BANNER_TEXT_KEY.RETRY_NOW
                }));
            }
            DebugLogger.info("Component", "autosave error", oEvent && oEvent.getParameters ? oEvent.getParameters() : {});
            fnEmitTelemetry("autosave.failed", oPayload);
        });
        return oComponent._oAutoSave;
    }

    return {
        createAutoSaveManager: createAutoSaveManager,
        hasHealthyAutosaveLockState: hasHealthyAutosaveLockState
    };
});
