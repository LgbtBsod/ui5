sap.ui.define([
    "sap/ui/core/UIComponent",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentManagers",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentAppRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/InteractionFX"
], function (UIComponent, ComponentBootstrap, ComponentManagers, ComponentAppRuntime, InteractionFX) {
    "use strict";

    return UIComponent.extend("PRODUCTION_CONTROL_CHECKLIST.Component", {
        metadata: {
            manifest: "json"
        },
        init: function () {
            this._oInteractionFX = InteractionFX;
            return ComponentBootstrap.init(this, arguments);
        },
        _startCoreManagers: function () {
            return ComponentManagers.startCore(this);
        },
        _stopLockScopedManagers: function () {
            return ComponentManagers.stopLockScoped(this);
        },
        _startLockScopedManagers: function () {
            return ComponentManagers.startLockScoped(this);
        },
        _stopAllManagers: function () {
            return ComponentManagers.stopAll(this);
        },
        _collectManagers: function () {
            return ComponentManagers.collect(this);
        },
        _isLockRuntimeActive: function (oStateModel) {
            return ComponentManagers.isLockRuntimeActive(oStateModel);
        },
        _syncLockScopedManagers: function (oStateModel) {
            return ComponentManagers.syncLockScopedManagers(this, oStateModel);
        },
        _applyFrontendRuntimeConfig: function (oFrontendConfig, oStateModel, oEnvModel, oMasterDataModel) {
            return ComponentManagers.applyFrontendRuntimeConfig(this, oFrontendConfig, oStateModel, oEnvModel, oMasterDataModel);
        },
        _applyManagersTimerConfig: function (mTimers) {
            return ComponentManagers.applyManagersTimerConfig(this, mTimers);
        },
        _registerLockReleaseBeacon: function (oStateModel, oMainServiceModel) {
            return ComponentManagers.registerLockReleaseBeacon(this, oStateModel, oMainServiceModel);
        },
        _releaseActiveLockOnLeave: function (oStateModel, oMainServiceModel) {
            return ComponentManagers.releaseActiveLockOnLeave(this, oStateModel, oMainServiceModel);
        },
        attachInteractionFxToApp: function (oDomRef) {
            if (this._oInteractionFxHandle && this._oInteractionFxHandle.destroy) {
                this._oInteractionFxHandle.destroy();
                this._oInteractionFxHandle = null;
            }
            if (this._oInteractionFX && typeof this._oInteractionFX.attach === "function") {
                this._oInteractionFxHandle = this._oInteractionFX.attach(oDomRef);
            }
        },
        exit: function () {
            ComponentAppRuntime.destroyComponentRuntime(this);
        }
    });
});
