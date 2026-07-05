sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/EditSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageCodeConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/MessageKeyConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/RuntimeEventContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentDetailMetaSyncRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentListenerContracts"
], function (ModelStateRuntime, FeedbackBannerRuntime, EditSessionRuntime, StatePaths, ModelContracts, WorkflowContracts, MessageCodeConstants, MessageKeyConstants, ShellStateRuntime, RuntimeEventContracts, ComponentDetailMetaSyncRuntime, ComponentListenerContracts) {
    "use strict";

    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var DETAIL_MESSAGE_KEYS = MessageKeyConstants.DETAIL;
    var DETAIL_CODES = MessageCodeConstants.FLOW;
    var PATHS = ComponentListenerContracts.PATHS;
    var TELEMETRY_EVENT = ComponentListenerContracts.TELEMETRY_EVENT;
    var VALUES = ComponentListenerContracts.VALUES;

    function readCurrentLockScope(oStateModel, oStatePaths) {
        return {
            dbKey: String(ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, "") || "").trim(),
            sessionGuid: String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.SESSION_ID, "") || "").trim(),
            editMode: String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") || "").trim(),
            lockState: String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") || "").trim()
        };
    }

    function isActiveEditLockScope(oScope) {
        return oScope.editMode === "EDIT" && oScope.lockState === "EDIT_LOCKED" && !!oScope.dbKey;
    }

    function shouldIgnoreProbePayload(oPayload, oStateModel, oStatePaths) {
        var oScope = readCurrentLockScope(oStateModel, oStatePaths);
        var sPayloadDbKey = String((oPayload && (oPayload.dbKey || oPayload.DB_KEY)) || "").trim();
        var sPayloadSessionGuid = String((oPayload && (oPayload.sessionGuid || oPayload.SessionGuid)) || "").trim();

        if (!isActiveEditLockScope(oScope)) {
            return true;
        }
        if (sPayloadDbKey && oScope.dbKey && sPayloadDbKey !== oScope.dbKey) {
            return true;
        }
        if (sPayloadSessionGuid && oScope.sessionGuid && sPayloadSessionGuid !== oScope.sessionGuid) {
            return true;
        }
        return false;
    }

    function updateCacheValidation(oCacheState, oStateModel, ComponentRuntimeSupport, oPayload) {
        var sCheckedAt = ComponentRuntimeSupport.formatHumanDateTime(new Date());
        if (oCacheState) {
            oCacheState.lastServerState = {
                lastChangeSet: oPayload.last_change_set || null,
                serverChangedOn: oPayload.server_changed_on || null,
                checkedAt: sCheckedAt
            };
        }
        ModelStateRuntime.writeOnModel(oStateModel, "/cacheValidationAt", sCheckedAt);
    }

    function invalidateProbeLockHealth(oStateModel, oShellModel, oStatePaths, ModelPaths) {
        ModelStateRuntime.setManyOnModel(oStateModel, (function () {
            var mPatch = {
                "/hasConflict": true
            };
            mPatch[oStatePaths.PERSISTENCE_HAS_VALID_LOCK] = false;
            mPatch[oStatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES] = false;
            return mPatch;
        }()));
        ModelStateRuntime.writeOnModel(oShellModel, ModelPaths.SHELL_LOCK, {
            ok: false,
            reason: WorkflowContracts.REASONS.UNKNOWN,
            isKilled: false
        });
    }

    function attachLockRuntime(mOptions) {
        var oComponent = mOptions.component;
        var oMainServiceModel = mOptions.mainServiceModel;
        var oStateModel = mOptions.stateModel;
        var oShellModel = mOptions.shellModel;
        var oCacheState = mOptions.cacheState;
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
            EditSessionRuntime.stopLockScoped(oComponent._collectManagers());
            if (bHadUnsavedChanges) {
                fnSetGlobalBanner(FeedbackBannerRuntime.createBannerInput({
                    scope: "global",
                    severity: "warning",
                    textKey: DETAIL_MESSAGE_KEYS.LOCK_LOST,
                    details: fnBundleText("tabConflictCopyHint")
                }));
            }
            return oComponent._detailFacade.onLockLost({
                dbKey: ModelStateRuntime.readOnModel(oStateModel, StatePaths.ACTIVE_OBJECT_ID, ""),
                reason: (oPayload && (oPayload.code || oPayload.reason_code)) || DETAIL_CODES.KILLED,
                preserveDirty: false
            }, oComponent._ctx).then(function (oResult) {
                fnApplyFacadeResult(oResult);
                ShellStateRuntime.syncRuntimeShellState(oStateModel, oShellModel);
                fnEmitTelemetry("lock.lost.detected", mOptions.telemetryRuntime.lockLost(
                    (oPayload && (oPayload.code || oPayload.reason_code)) || DETAIL_CODES.KILLED,
                    "lock_probe"
                ));
                return oResult;
            });
        };

        oComponent._bLeaveReleaseSent = false;
        oComponent._bLeaveReleasePending = false;
        oComponent._bLeaveReleaseAttempted = false;
        oComponent._fnUnregisterBeacon = oComponent._registerLockReleaseBeacon(oStateModel, oMainServiceModel);

        function applyOwnedLockState(oLockState, bResetConflict) {
            ModelStateRuntime.writeOnModel(oStateModel, "/lockExpires", oLockState.lockExpires);
            ModelStateRuntime.writeOnModel(oStateModel, oStatePaths.PERSISTENCE_HAS_VALID_LOCK, true);
            ModelStateRuntime.writeOnModel(oStateModel, oStatePaths.PERSISTENCE_LOCK_OWNER_SESSION_MATCHES, true);
            ModelStateRuntime.writeOnModel(oStateModel, oStatePaths.PERSISTENCE_LAST_LOCK_REFRESH_AT, new Date().toISOString());
            ModelStateRuntime.writeOnModel(oShellModel, MODEL_PATHS.SHELL_LOCK, {
                ok: true,
                reason: WorkflowContracts.REASONS.OWNED_BY_YOU,
                isKilled: false
            });
            if (bResetConflict) {
                ModelStateRuntime.writeOnModel(oStateModel, "/hasConflict", false);
            }
        }

        function onLockProbePayload(oPayload, bResetConflict) {
            if (shouldIgnoreProbePayload(oPayload, oStateModel, oStatePaths)) {
                return;
            }
            var oLockState = ComponentRuntimeSupport.applyLockProbeState(oPayload, oStateModel);
            if (oLockState.killed || oLockState.lost) {
                oComponent._handleKilledLock(oPayload);
                return;
            }
            applyOwnedLockState(oLockState, bResetConflict);
        }

        oComponent._oHeartbeat.attachEvent(RuntimeEventContracts.HEARTBEAT, function (oEvent) {
            var oPayload = ComponentRuntimeSupport.eventPayload(oEvent);
            DebugLogger.info("Component", "lock heartbeat", oPayload);
            onLockProbePayload(oPayload, false);
            updateCacheValidation(oCacheState, oStateModel, ComponentRuntimeSupport, oPayload);
        });
        oComponent._oHeartbeat.attachEvent(RuntimeEventContracts.HEARTBEAT_ERROR, function (oEvent) {
            invalidateProbeLockHealth(oStateModel, oShellModel, oStatePaths, MODEL_PATHS);
            DebugLogger.info("Component", "lock heartbeat error", ComponentRuntimeSupport.eventPayload(oEvent));
        });
        oComponent._oGcd.attachEvent(RuntimeEventContracts.GCD_EXPIRED, function () {
            invalidateProbeLockHealth(oStateModel, oShellModel, oStatePaths, MODEL_PATHS);
        });
        oComponent._oLockStatus.attachEvent(RuntimeEventContracts.STATUS, function (oEvent) {
            onLockProbePayload(ComponentRuntimeSupport.eventPayload(oEvent), true);
        });
        oComponent._oLockStatus.attachEvent(RuntimeEventContracts.STATUS_ERROR, function () {
            invalidateProbeLockHealth(oStateModel, oShellModel, oStatePaths, MODEL_PATHS);
        });
        oComponent._oActivity.attachEvent(RuntimeEventContracts.IDLE_TIMEOUT, function () {
            ModelStateRuntime.writeOnModel(oStateModel, "/idleExpires", new Date().toISOString());
            fnHandleForceReadOnly({
                reason: "IDLE_TIMEOUT",
                messageKey: "idleReadOnlyMessage",
                source: "activityMonitor"
            });
        });
        oComponent._oActivity.attachEvent(RuntimeEventContracts.ACTIVITY, function (oEvent) {
            var sAt = (ComponentRuntimeSupport.eventPayload(oEvent) || {}).at || new Date().toISOString();
            ModelStateRuntime.setManyOnModel(oShellModel, {
                [MODEL_PATHS.SHELL_ACTIVITY_LAST_ACTIVE_AT]: sAt,
                [MODEL_PATHS.SHELL_ACTIVITY_IDLE_UNTIL]: new Date(Date.parse(sAt) + Number(TimeConfigService.read(oStateModel, "idleMs"))).toISOString()
            });
        });
    }

    function attachLifecycleBindings(mOptions) {
        var oComponent = mOptions.component;
        var oStateModel = mOptions.stateModel;
        var oShellModel = mOptions.shellModel;
        var StatePaths = mOptions.statePaths || {};
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var fnPublishTabSignal = mOptions.publishTabSignal;

        oComponent._oStateLifecycleModel = oStateModel;
        oComponent._fnStateModelPropertyChange = function (oEvent) {
            var sCurrentDbKey;
            var sCurrentMode;
            var sCurrentLockState;
            var sPath = oEvent.getParameter("path") || "";
            if ([PATHS.IS_LOADING, PATHS.ACTIVE_OBJECT_ID, StatePaths.SESSION_ID, StatePaths.UI_BUSY_DETAIL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE].indexOf(sPath) >= 0) {
                ShellStateRuntime.syncRuntimeShellState(oStateModel, oShellModel);
            }
            if ([PATHS.ACTIVE_OBJECT_ID, StatePaths.READINESS_DETAIL, StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_STATE, StatePaths.WORKFLOW_DETAIL_AUTOSAVE_LAST_SAVED_AT, StatePaths.WORKFLOW_DIRTY, StatePaths.VALIDATION_SUMMARY].indexOf(sPath) >= 0) {
                ComponentDetailMetaSyncRuntime.syncDetailMeta(oStateModel, StatePaths);
            }
            if (sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE) {
                fnEmitTelemetry(TELEMETRY_EVENT.WORKFLOW_MODE_CHANGED, mOptions.telemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if (sPath === StatePaths.WORKFLOW_DETAIL_LOCK_STATE) {
                fnEmitTelemetry(TELEMETRY_EVENT.LOCK_STATE_CHANGED, mOptions.telemetryRuntime.stateValue(oEvent.getParameter("value")));
            }
            if ([StatePaths.WORKFLOW_DETAIL_EDIT_MODE, StatePaths.WORKFLOW_DETAIL_LOCK_STATE, PATHS.ACTIVE_OBJECT_ID].indexOf(sPath) >= 0) {
                sCurrentDbKey = String(ModelStateRuntime.readOnModel(oStateModel, PATHS.ACTIVE_OBJECT_ID, "") || "").trim();
                sCurrentMode = mOptions.layoutStateRuntime.readMode(oStateModel, "");
                sCurrentLockState = mOptions.layoutStateRuntime.readLockState(oStateModel, "");
                if (sCurrentDbKey && sCurrentMode === WorkflowContracts.EDIT_MODES.EDIT && sCurrentLockState === WorkflowContracts.LOCK_STATES.EDIT_LOCKED) {
                    ModelStateRuntime.writeOnModel(oStateModel, StatePaths.TAB_CONFLICT_STATE, { active: false, source: "", at: "" });
                    fnPublishTabSignal(VALUES.LOCK_OWNED, { dbKey: sCurrentDbKey });
                } else if (sCurrentDbKey && sPath === StatePaths.WORKFLOW_DETAIL_EDIT_MODE && sCurrentMode !== WorkflowContracts.EDIT_MODES.EDIT) {
                    fnPublishTabSignal(VALUES.LOCK_RELEASED, { dbKey: sCurrentDbKey });
                }
            }
        };
        oStateModel.attachPropertyChange(oComponent._fnStateModelPropertyChange, oComponent);
        oComponent._detachInitRuntimeListeners = function () {
            if (oComponent._oStateLifecycleModel && oComponent._fnStateModelPropertyChange) {
                oComponent._oStateLifecycleModel.detachPropertyChange(oComponent._fnStateModelPropertyChange, oComponent);
            }
            if (oComponent._fnBeforeUnload) {
                window.removeEventListener("beforeunload", oComponent._fnBeforeUnload);
            }
        };
    }

    return {
        attachLifecycleBindings: attachLifecycleBindings,
        attachLockRuntime: attachLockRuntime,
        invalidateProbeLockHealth: invalidateProbeLockHealth
    };
});
