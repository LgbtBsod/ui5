sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/FeedbackBannerRuntime"
], function (ModelStateRuntime, FeedbackBannerRuntime) {
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

    return {
        attachLockRuntime: attachLockRuntime
    };
});
