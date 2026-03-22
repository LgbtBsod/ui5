sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/execution/FeedbackBannerRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/EditSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/ModelPathContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/ModelConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/DetailContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/shell/runtime/ShellStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/RuntimeEventContracts"
], function (ModelStateRuntime, FeedbackBannerRuntime, EditSessionRuntime, ModelPathContracts, ModelContracts, WorkflowContracts, DetailContracts, ShellStateRuntime, RuntimeEventContracts) {
    "use strict";

    var MODEL_PATHS = ModelContracts.MODEL_PATHS;
    var DETAIL_MESSAGE_KEYS = DetailContracts;
    var DETAIL_CODES = DetailContracts.CODES;

    function readCurrentLockScope(oStateModel, oStatePaths) {
        return {
            rootId: String(ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, "") || "").trim(),
            sessionGuid: String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.SESSION_ID, "") || "").trim(),
            editMode: String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_DETAIL_EDIT_MODE, "") || "").trim(),
            lockState: String(ModelStateRuntime.readOnModel(oStateModel, oStatePaths.WORKFLOW_DETAIL_LOCK_STATE, "") || "").trim()
        };
    }

    function isActiveEditLockScope(oScope) {
        return oScope.editMode === "EDIT" && oScope.lockState === "EDIT_LOCKED" && !!oScope.rootId;
    }

    function shouldIgnoreProbePayload(oPayload, oStateModel, oStatePaths) {
        var oScope = readCurrentLockScope(oStateModel, oStatePaths);
        var sPayloadRootId = String((oPayload && (oPayload.rootId || oPayload.RootId || oPayload.objectUuid || oPayload.ObjectUuid)) || "").trim();
        var sPayloadSessionGuid = String((oPayload && (oPayload.sessionGuid || oPayload.SessionGuid)) || "").trim();

        if (!isActiveEditLockScope(oScope)) {
            return true;
        }
        if (sPayloadRootId && oScope.rootId && sPayloadRootId !== oScope.rootId) {
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
                rootId: ModelStateRuntime.readOnModel(oStateModel, ModelPathContracts.ACTIVE_OBJECT_ID, ""),
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

    return {
        attachLockRuntime: attachLockRuntime,
        invalidateProbeLockHealth: invalidateProbeLockHealth
    };
});
