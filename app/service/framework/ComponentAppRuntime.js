sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/RuntimeTimerSanitizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentLockReleaseRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/SchedulingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentFormattingRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentDetailStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/model/StatePaths",
    "PRODUCTION_CONTROL_CHECKLIST/contracts/WorkflowContracts"
], function (RuntimeTimerSanitizer, TimeConfigService, ComponentLockReleaseRuntime, SchedulingRuntime, ComponentSessionRuntime, ComponentFormattingRuntime, ComponentDetailStateRuntime, StatePaths, WorkflowContracts) {
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
            syncUiStateMode: ComponentDetailStateRuntime.syncUiStateMode,
            syncDetailCurrentFromSelected: ComponentDetailStateRuntime.syncDetailCurrentFromSelected,
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

    function applyFrontendRuntimeConfig(oComponent, oFrontendConfig, oStateModel, oEnvModel, oMasterDataModel, ApplyRuntimeSettingsUseCase) {
        RuntimeTimerSanitizer.sanitizeTimers((oFrontendConfig && oFrontendConfig.runtimeSettingsPayload) || {}, oStateModel.getProperty("/timers") || {});
        oStateModel.setProperty("/timers", TimeConfigService.normalize((oFrontendConfig && oFrontendConfig.runtimeSettingsPayload) || {}, oStateModel.getProperty("/timers") || {}));
        return ApplyRuntimeSettingsUseCase.execute({ frontendConfig: oFrontendConfig || {} }, {
            stateModel: oStateModel,
            envModel: oEnvModel,
            masterDataModel: oMasterDataModel
        }).then(function () {
            applyManagersTimerConfig(oComponent, oStateModel.getProperty("/timers") || {});
        });
    }

    function registerLockReleaseBeacon(oComponent, oStateModel, oMainServiceModel) {
        var fnPageHide = function () {
            releaseActiveLockOnLeave(oComponent, oStateModel, oMainServiceModel);
        };
        window.addEventListener("pagehide", fnPageHide);
        return function () {
            window.removeEventListener("pagehide", fnPageHide);
        };
    }

    function releaseActiveLockOnLeave(oComponent, oStateModel, oMainServiceModel) {
        var oPayload;
        var sUrl;
        var sToken;
        if (oComponent._bLeaveReleaseSent) {
            return;
        }
        oPayload = ComponentLockReleaseRuntime.readActiveLockPayload(oStateModel);
        sUrl = ComponentLockReleaseRuntime.buildLockReleaseUrl(oStateModel);
        sToken = oMainServiceModel && oMainServiceModel.getSecurityToken ? String(oMainServiceModel.getSecurityToken() || "").trim() : "";
        if (!oPayload || !sUrl) {
            return;
        }
        oComponent._bLeaveReleaseSent = true;
        ComponentLockReleaseRuntime.tryBeaconLockRelease(sUrl, oPayload, sToken);
    }

    function clearComponentTimers(oComponent) {
        oComponent._iSaveWorkingTimer = SchedulingRuntime.clearTimer(oComponent._iSaveWorkingTimer);
    }

    return {
        applyFrontendRuntimeConfig: applyFrontendRuntimeConfig,
        applyManagersTimerConfig: applyManagersTimerConfig,
        buildComponentRuntimeSupport: buildComponentRuntimeSupport,
        clearComponentTimers: clearComponentTimers,
        collectManagers: collectManagers,
        isLockRuntimeActive: isLockRuntimeActive,
        registerLockReleaseBeacon: registerLockReleaseBeacon,
        releaseActiveLockOnLeave: releaseActiveLockOnLeave,
        syncLockScopedManagers: syncLockScopedManagers
    };
});
