sap.ui.define([
    "checklist/app/service/framework/FeedbackBannerRuntime",
    "checklist/app/service/framework/TelemetryRuntime"
], function (FeedbackBannerRuntime, TelemetryRuntime) {
    "use strict";

    function attach(mOptions) {
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

        // Centralized transition used by both heartbeat and lock-status probe.
        oComponent._handleKilledLock = function (oPayload) {
            var bHadUnsavedChanges = !!oStateModel.getProperty(oStatePaths.WORKFLOW_DIRTY);
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
                rootId: oStateModel.getProperty("/activeObjectId"),
                reason: (oPayload && (oPayload.code || oPayload.reason_code)) || "KILLED",
                preserveDirty: bHadUnsavedChanges
            }, oComponent._ctx).then(function (oResult) {
                fnApplyFacadeResult(oResult);
                ComponentRuntimeSupport.syncUiStateMode(oStateModel, oUiStateModel);
                fnEmitTelemetry("lock.lost.detected", TelemetryRuntime.lockLost(
                    (oPayload && (oPayload.code || oPayload.reason_code)) || "KILLED",
                    "lock_probe"
                ));
                return oResult;
            });
        };

        oComponent._bLeaveReleaseSent = false;
        oComponent._fnUnregisterBeacon = oComponent._registerLockReleaseBeacon(oStateModel, oMainServiceModel);

        function applyOwnedLockState(oLockState, bResetConflict) {
            oStateModel.setProperty("/lockExpires", oLockState.lockExpires);
            oUiStateModel.setProperty("/lock", { ok: true, reason: "OWNED_BY_YOU", isKilled: false });
            if (bResetConflict) {
                oStateModel.setProperty("/hasConflict", false);
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
            oCacheModel.setProperty("/lastServerState", {
                lastChangeSet: oPayload.last_change_set || null,
                serverChangedOn: oPayload.server_changed_on || null,
                checkedAt: sCheckedAt
            });
            oStateModel.setProperty("/cacheValidationAt", sCheckedAt);
        });
        oComponent._oHeartbeat.attachEvent("heartbeatError", function (oEvent) {
            oStateModel.setProperty("/hasConflict", true);
            DebugLogger.info("Component", "lock heartbeat error", ComponentRuntimeSupport.eventPayload(oEvent));
        });
        oComponent._oGcd.attachEvent("gcdExpired", function () {
            oStateModel.setProperty("/hasConflict", true);
        });
        oComponent._oLockStatus.attachEvent("status", function (oEvent) {
            var oPayload = ComponentRuntimeSupport.eventPayload(oEvent);
            onLockProbePayload(oPayload, true);
        });

        oComponent._oLockStatus.attachEvent("statusError", function () {
            // status probe is best-effort; heartbeat remains the source of truth.
            oStateModel.setProperty("/hasConflict", true);
        });

        oComponent._oActivity.attachEvent("idleTimeout", function () {
            oStateModel.setProperty("/idleExpires", new Date().toISOString());
            fnHandleForceReadOnly({
                reason: "IDLE_TIMEOUT",
                messageKey: "idleReadOnlyMessage",
                source: "activityMonitor"
            });
        });

        oComponent._oActivity.attachEvent("activity", function (oEvent) {
            var sAt = (ComponentRuntimeSupport.eventPayload(oEvent) || {}).at || new Date().toISOString();
            oUiStateModel.setProperty("/activity/lastActiveAt", sAt);
            oUiStateModel.setProperty("/activity/idleUntil", new Date(Date.parse(sAt) + Number(TimeConfigService.read(oStateModel, "idleMs"))).toISOString());
        });
    }

    return {
        attach: attach
    };
});
