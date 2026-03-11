sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/CloneUtil"
], function (ModelStateRuntime, FeedbackBannerRuntime, CloneUtil) {
    "use strict";

    function attachLockRuntime(mOptions) {
        var oComponent = mOptions.component;
        var oMainServiceModel = mOptions.mainServiceModel;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var oCacheModel = mOptions.cacheModel;
        var oStatePaths = mOptions.statePaths || {};
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;
        var TimeConfigService = mOptions.timeConfigService;
        var DebugLogger = mOptions.debugLogger;
        var fnBundleText = mOptions.bundleText;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var fnSetGlobalBanner = mOptions.setGlobalBanner;
        var fnHandleForceReadOnly = mOptions.handleForceReadOnly;
        var fnApplyFacadeResult = mOptions.applyFacadeResult;

        oComponent._handleKilledLock = function (oPayload) {
            var bHadUnsavedChanges = !!ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_DIRTY, false);
            oComponent._oHeartbeat.stop();
            oComponent._oLockStatus.stop();
            oComponent._oAutoSave.stop();
            oComponent._oGcd.destroyManager();
            if (bHadUnsavedChanges) {
                fnSetGlobalBanner(FeedbackBannerRuntime.createBannerInput({
                    severity: "warning",
                    textKey: "lockLostMessage",
                    details: fnBundleText("tabConflictCopyHint")
                }));
            }
            return oComponent._detailFacade.onLockLost({
                rootId: ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", ""),
                reason: (oPayload && (oPayload.code || oPayload.reason_code)) || "KILLED",
                preserveDirty: bHadUnsavedChanges
            }, oComponent._ctx).then(function (oResult) {
                fnApplyFacadeResult(oResult);
                ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
                fnEmitTelemetry("lock.lost.detected", mOptions.telemetryRuntime.lockLost(
                    (oPayload && (oPayload.code || oPayload.reason_code)) || "KILLED",
                    "lock_probe"
                ));
                return oResult;
            });
        };

        oComponent._bLeaveReleaseSent = false;
        oComponent._fnUnregisterBeacon = oComponent._registerLockReleaseBeacon(oStateModel, oMainServiceModel);

        function applyOwnedLockState(oLockState, bResetConflict) {
            ModelStateRuntime.writeOnModel(oStateModel, "/lockExpires", oLockState.lockExpires);
            ModelStateRuntime.writeOnModel(oUiStateModel, "/lock", { ok: true, reason: "OWNED_BY_YOU", isKilled: false });
            if (bResetConflict) {
                ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", false);
            }
        }

        function onLockProbePayload(oPayload, bResetConflict) {
            var oLockState = ComponentRuntimeSupport.applyLockProbeState(oPayload, oStateModel);
            if (oLockState.killed || oLockState.lost) {
                oComponent._handleKilledLock(oPayload);
                return;
            }
            applyOwnedLockState(oLockState, bResetConflict);
        }

        oComponent._oHeartbeat.attachEvent("heartbeat", function (oEvent) {
            var oPayload = ComponentRuntimeSupport.eventPayload(oEvent);
            DebugLogger.info("Component", "lock heartbeat", oPayload);
            onLockProbePayload(oPayload, false);
            var sCheckedAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
            ModelStateRuntime.writeOnModel(oCacheModel, "/lastServerState", {
                lastChangeSet: oPayload.last_change_set || null,
                serverChangedOn: oPayload.server_changed_on || null,
                checkedAt: sCheckedAt
            });
            ModelStateRuntime.writeOnModel(oStateModel, "/cacheValidationAt", sCheckedAt);
        });
        oComponent._oHeartbeat.attachEvent("heartbeatError", function (oEvent) {
            ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", true);
            DebugLogger.info("Component", "lock heartbeat error", ComponentRuntimeSupport.eventPayload(oEvent));
        });
        oComponent._oGcd.attachEvent("gcdExpired", function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", true);
        });
        oComponent._oLockStatus.attachEvent("status", function (oEvent) {
            onLockProbePayload(ComponentRuntimeSupport.eventPayload(oEvent), true);
        });
        oComponent._oLockStatus.attachEvent("statusError", function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", true);
        });
        oComponent._oActivity.attachEvent("idleTimeout", function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/idleExpires", new Date().toISOString());
            fnHandleForceReadOnly({
                reason: "IDLE_TIMEOUT",
                messageKey: "idleReadOnlyMessage",
                source: "activityMonitor"
            });
        });
        oComponent._oActivity.attachEvent("activity", function (oEvent) {
            var sAt = (ComponentRuntimeSupport.eventPayload(oEvent) || {}).at || new Date().toISOString();
            ModelStateRuntime.setManyOnModel(oUiStateModel, {
                "/activity/lastActiveAt": sAt,
                "/activity/idleUntil": new Date(Date.parse(sAt) + Number(TimeConfigService.read(oStateModel, "idleMs"))).toISOString()
            });
        });
    }

    function attachManagerRuntime(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oUiStateModel = mOptions.uiStateModel;
        var oSnapshotModel = mOptions.snapshotModel;
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
        var ComponentRuntimeSupport = mOptions.componentRuntimeSupport;

        oComponent._oHeartbeat = new mManagers.HeartbeatManager({
            intervalMs: Number(mTimerDefaults.heartbeatMs),
            heartbeatFn: function () {
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") !== "EDIT" ||
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") !== "LOCKED") {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock.heartbeat !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock.heartbeat" });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock.heartbeat({ rootId: sRootId, sessionGuid: sSessionGuid }).then(function (oRes) {
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
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") === "EDIT" &&
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") === "LOCKED" &&
                    !!ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_DIRTY, false);
            },
            shouldSave: function () {
                var bIsLocked = ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") === "LOCKED";
                return ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") === "EDIT" &&
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
            var mStart = { "/autosaveState": "SAVING" };
            mStart[StatePaths.SAVE_IN_FLIGHT] = true;
            ModelStateRuntime.setManyOnModel(oStateModel, mStart);
            DebugLogger.info("Component", "autosave start", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
            fnEmitTelemetry("autosave.triggered", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveDone", function () {
            var mDone = { "/autosaveState": "SAVED", "/autosaveAt": new Date().toISOString() };
            mDone[StatePaths.SAVE_IN_FLIGHT] = false;
            ModelStateRuntime.setManyOnModel(oStateModel, mDone);
            DebugLogger.info("Component", "autosave done", mOptions.telemetryRuntime.objectRefFromStateModel(oStateModel));
        });
        oComponent._oAutoSave.attachEvent("autosaveError", function (oEvent) {
            var mErr = { "/autosaveState": "ERROR" };
            mErr[StatePaths.SAVE_IN_FLIGHT] = false;
            ModelStateRuntime.setManyOnModel(oStateModel, mErr);
            fnSetGlobalBanner(FeedbackBannerRuntime.createRetryBannerInput("error", "objectSaveFailed", {
                textArgs: [fnBundleText("autosaveError")],
                retryAction: ActionContract.RETRY_ACTIONS.SAVE,
                retryTextKey: "retryNowButton"
            }));
            DebugLogger.info("Component", "autosave error", oEvent && oEvent.getParameters ? oEvent.getParameters() : {});
            fnEmitTelemetry("autosave.failed", ComponentRuntimeSupport.eventPayload(oEvent));
        });
        oComponent._oLockStatus = new mManagers.LockStatusMonitor({
            intervalMs: Number(mTimerDefaults.lockStatusMs),
            checkFn: function () {
                if (ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_EDIT_MODE, "") !== "EDIT" ||
                    ModelStateRuntime.readOnModel(oStateModel, StatePaths.WORKFLOW_LOCK_STATUS, "") !== "LOCKED") {
                    return Promise.resolve({ success: true, is_killed: false, skipped: true });
                }
                if (!oComponent._ctx || !oComponent._ctx.lock || typeof oComponent._ctx.lock.status !== "function") {
                    return Promise.resolve({ success: false, is_killed: false, skipped: true, missing: "ctx.lock.status" });
                }
                var sRootId = ModelStateRuntime.readOnModel(oStateModel, "/activeObjectId", "");
                var sSessionGuid = ModelStateRuntime.readOnModel(oStateModel, StatePaths.SESSION_ID, "");
                return oComponent._ctx.lock.status({ rootId: sRootId, sessionGuid: sSessionGuid }).then(function (oRes) {
                    return Object.assign({ rootId: sRootId, sessionGuid: sSessionGuid }, oRes || {});
                });
            }
        });
    }

    return {
        attachLockRuntime: attachLockRuntime,
        attachManagerRuntime: attachManagerRuntime
    };
});
