sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeTimerSanitizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentLockReleaseRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentFormattingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentDetailStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/LockAdapter"
], function (RuntimeTimerSanitizer, TimeConfigService, ComponentLockReleaseRuntime, SchedulingRuntime, ComponentSessionRuntime, ComponentFormattingRuntime, ComponentDetailStateRuntime, GatewayClient, StatePaths, WorkflowContracts, LockAdapter) {
    "use strict";

    function buildComponentRuntimeSupport() {
        return {
            resolveBootDetailId: ComponentDetailStateRuntime.resolveBootDetailId,
            isCreateBootHash: ComponentDetailStateRuntime.isCreateBootHash,
            ensureSessionId: ComponentSessionRuntime.ensureSessionId,
            ensureTabSessionId: ComponentSessionRuntime.ensureTabSessionId,
            formatHumanDateTime: ComponentFormattingRuntime.formatHumanDateTime,
            eventPayload: ComponentFormattingRuntime.eventPayload,
            applyLockProbeState: ComponentDetailStateRuntime.applyLockProbeState,
            resolveDetailCurrent: ComponentDetailStateRuntime.resolveDetailCurrent
        };
    }

    function collectManagers(oComponent) {
        return {
            heartbeat: oComponent._oHeartbeat,
            activity: oComponent._oActivity,
            autosave: oComponent._oAutoSave,
            lockStatus: oComponent._oLockStatus,
            gcd: oComponent._oGcd
        };
    }

    function isLockRuntimeActive(oStateModel) {
        return oStateModel.getProperty(StatePaths.WORKFLOW_DETAIL_EDIT_MODE) === WorkflowContracts.EDIT_MODES.EDIT
            && oStateModel.getProperty(StatePaths.WORKFLOW_DETAIL_LOCK_STATE) === WorkflowContracts.LOCK_STATES.EDIT_LOCKED;
    }

    function syncLockScopedManagers(oComponent, oStateModel) {
        if (isLockRuntimeActive(oStateModel)) {
            oComponent._startLockScopedManagers();
            return;
        }
        oComponent._stopLockScopedManagers();
    }

    function applyManagersTimerConfig(oComponent, mTimers) {
        if (oComponent._oHeartbeat && oComponent._oHeartbeat.setIntervalMs) {
            oComponent._oHeartbeat.setIntervalMs(mTimers.heartbeatMs);
        }
        if (oComponent._oLockStatus && oComponent._oLockStatus.setIntervalMs) {
            oComponent._oLockStatus.setIntervalMs(mTimers.lockStatusMs);
        }
        if (oComponent._oGcd && oComponent._oGcd.setIntervalMs) {
            oComponent._oGcd.setIntervalMs(mTimers.gcdMs);
        }
        if (oComponent._oActivity && oComponent._oActivity.setIdleMs) {
            oComponent._oActivity.setIdleMs(mTimers.idleMs);
        }
        if (oComponent._oAutoSave && oComponent._oAutoSave.setIntervals) {
            oComponent._oAutoSave.setIntervals({
                intervalMs: mTimers.autoSaveIntervalMs,
                debounceMs: mTimers.autoSaveDebounceMs
            });
        }
    }

    function applyFrontendRuntimeConfig(oComponent, oFrontendConfig, oStateModel, oEnvState, oMasterDataModel, ApplyRuntimeSettingsUseCase) {
        RuntimeTimerSanitizer.sanitizeTimers((oFrontendConfig && oFrontendConfig.runtimeSettingsPayload) || {}, oStateModel.getProperty("/timers") || {});
        oStateModel.setProperty("/timers", TimeConfigService.normalize((oFrontendConfig && oFrontendConfig.runtimeSettingsPayload) || {}, oStateModel.getProperty("/timers") || {}));
        return ApplyRuntimeSettingsUseCase.execute({ frontendConfig: oFrontendConfig || {} }, {
            stateModel: oStateModel,
            envState: oEnvState,
            masterDataModel: oMasterDataModel
        }).then(function () {
            applyManagersTimerConfig(oComponent, oStateModel.getProperty("/timers") || {});
        });
    }

    function isDocumentHidden() {
        try {
            return typeof document !== "undefined" && document.visibilityState === "hidden";
        } catch (_visibilityError) {
            return false;
        }
    }

    function hasLeaveReleaseInFlight(oComponent) {
        return !!(oComponent && (oComponent._bLeaveReleaseAttempted || oComponent._bLeaveReleasePending));
    }

    function markLeaveReleaseAttempted(oComponent) {
        if (!oComponent) {
            return false;
        }
        oComponent._bLeaveReleasePending = true;
        oComponent._bLeaveReleaseAttempted = true;
        oComponent._bLeaveReleaseSent = false;
        return true;
    }

    function resetLeaveReleaseState(oComponent) {
        if (!oComponent) {
            return false;
        }
        oComponent._bLeaveReleasePending = false;
        oComponent._bLeaveReleaseAttempted = false;
        return true;
    }

    function registerLockReleaseBeacon(oComponent, oStateModel, oMainServiceModel) {
        function tryReleaseOnPageLeave() {
            if (hasLeaveReleaseInFlight(oComponent)) {
                return;
            }
            if (!ComponentLockReleaseRuntime.readActiveLockPayload(oStateModel)) {
                return;
            }
            releaseActiveLockOnLeave(oComponent, oStateModel, oMainServiceModel);
        }

        function tryReleaseOnVisibilityChange() {
            if (isDocumentHidden()) {
                tryReleaseOnPageLeave();
                return;
            }
            resetLeaveReleaseState(oComponent);
        }

        window.addEventListener("pagehide", tryReleaseOnPageLeave);
        document.addEventListener("visibilitychange", tryReleaseOnVisibilityChange);
        return function () {
            window.removeEventListener("pagehide", tryReleaseOnPageLeave);
            document.removeEventListener("visibilitychange", tryReleaseOnVisibilityChange);
        };
    }

    function releaseActiveLockOnLeave(oComponent, oStateModel, oMainServiceModel) {
        var oPayload = ComponentLockReleaseRuntime.readActiveLockPayload(oStateModel);
        if (!oPayload || hasLeaveReleaseInFlight(oComponent)) {
            return false;
        }
        if (!markLeaveReleaseAttempted(oComponent)) {
            return false;
        }
        oComponent._bLeaveReleaseSent = LockAdapter.releaseOnPageLeave({
            rootId: oPayload.RootId,
            objectUuid: oPayload.RootId,
            sessionGuid: oPayload.SessionGuid,
            model: oMainServiceModel
        });
        return oComponent._bLeaveReleaseSent;
    }

    function clearComponentTimers(oComponent) {
        oComponent._iSaveWorkingTimer = SchedulingRuntime.clearTimer(oComponent._iSaveWorkingTimer);
    }

    function destroyComponentRuntime(oComponent) {
        if (oComponent._oLifecycleRouter && oComponent._fnBeforeRouteMatched && oComponent._oLifecycleRouter.detachBeforeRouteMatched) {
            oComponent._oLifecycleRouter.detachBeforeRouteMatched(oComponent._fnBeforeRouteMatched, oComponent);
        }
        if (oComponent._oDirtyStateBinding && oComponent._fnDirtyStateBindingChange) {
            oComponent._oDirtyStateBinding.detachChange(oComponent._fnDirtyStateBindingChange);
            oComponent._oDirtyStateBinding.destroy();
        }
        (oComponent._aLockScopedStateBindings || []).forEach(function (oEntry) {
            if (oEntry && oEntry.binding && oEntry.handler) {
                oEntry.binding.detachChange(oEntry.handler);
                oEntry.binding.destroy();
            }
        });
        if (typeof oComponent._detachInitRuntimeListeners === "function") {
            oComponent._detachInitRuntimeListeners();
        }
        if (oComponent._oInteractionFxHandle && oComponent._oInteractionFxHandle.destroy) {
            oComponent._oInteractionFxHandle.destroy();
            oComponent._oInteractionFxHandle = null;
        }
        oComponent._stopAllManagers();
        if (oComponent._fnUnregisterBeacon) {
            oComponent._fnUnregisterBeacon();
        }
        if (oComponent._fnCrossTabStorage) {
            window.removeEventListener("storage", oComponent._fnCrossTabStorage);
        }
        if (oComponent._oCrossTabChannel && typeof oComponent._oCrossTabChannel.close === "function") {
            oComponent._oCrossTabChannel.close();
        }
        if (oComponent._fnOnFullSave) {
            window.removeEventListener("pcct:fullSave", oComponent._fnOnFullSave);
        }
        if (typeof oComponent._fnUnsubscribeRuntimeSettings === "function") {
            oComponent._fnUnsubscribeRuntimeSettings();
        }
        clearComponentTimers(oComponent);
        GatewayClient.reset();
        oComponent._fnCrossTabStorage = null;
        oComponent._oCrossTabChannel = null;
        oComponent._oLifecycleRouter = null;
        oComponent._fnBeforeRouteMatched = null;
        oComponent._oDirtyStateBinding = null;
        oComponent._fnDirtyStateBindingChange = null;
        oComponent._aLockScopedStateBindings = null;
        oComponent._oStateLifecycleModel = null;
        oComponent._fnStateModelPropertyChange = null;
        oComponent._detachInitRuntimeListeners = null;
        oComponent._fnUnsubscribeRuntimeSettings = null;
    }

    var runtimeSupport = Object.freeze({
        applyFrontendRuntimeConfig: applyFrontendRuntimeConfig,
        applyManagersTimerConfig: applyManagersTimerConfig,
        buildComponentRuntimeSupport: buildComponentRuntimeSupport,
        clearComponentTimers: clearComponentTimers,
        collectManagers: collectManagers,
        isLockRuntimeActive: isLockRuntimeActive,
        syncLockScopedManagers: syncLockScopedManagers
    });

    var pageLeave = Object.freeze({
        markLeaveReleaseAttempted: markLeaveReleaseAttempted,
        registerLockReleaseBeacon: registerLockReleaseBeacon,
        releaseActiveLockOnLeave: releaseActiveLockOnLeave,
        resetLeaveReleaseState: resetLeaveReleaseState
    });

    var teardown = Object.freeze({
        clearComponentTimers: clearComponentTimers,
        destroyComponentRuntime: destroyComponentRuntime
    });

    return {
        runtimeSupport: runtimeSupport,
        pageLeave: pageLeave,
        teardown: teardown,
        applyFrontendRuntimeConfig: applyFrontendRuntimeConfig,
        applyManagersTimerConfig: applyManagersTimerConfig,
        buildComponentRuntimeSupport: buildComponentRuntimeSupport,
        clearComponentTimers: clearComponentTimers,
        collectManagers: collectManagers,
        destroyComponentRuntime: destroyComponentRuntime,
        isLockRuntimeActive: isLockRuntimeActive,
        markLeaveReleaseAttempted: markLeaveReleaseAttempted,
        registerLockReleaseBeacon: registerLockReleaseBeacon,
        releaseActiveLockOnLeave: releaseActiveLockOnLeave,
        resetLeaveReleaseState: resetLeaveReleaseState,
        syncLockScopedManagers: syncLockScopedManagers
    };
});
