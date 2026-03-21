sap.ui.define([
    "sap/ui/core/UIComponent",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentBootstrap",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentAppRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/StartManagersUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/ApplyRuntimeSettingsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/EditSessionRuntime"
], function (UIComponent, ComponentBootstrap, ComponentAppRuntime, StartManagersUseCase, ApplyRuntimeSettingsUseCase, EditSessionRuntime) {
    "use strict";

    function collectManagers(oComponent) {
        return ComponentAppRuntime.collectManagers(oComponent);
    }

    return UIComponent.extend("PRODUCTION_CONTROL_CHECKLIST.Component", {
        metadata: {
            manifest: "json"
        },
        init: function () {
            return ComponentBootstrap.init(this, arguments);
        },
        _startCoreManagers: function () {
            return StartManagersUseCase.execute({ scope: "core" }, { managers: collectManagers(this) });
        },
        _stopLockScopedManagers: function () {
            return StartManagersUseCase.execute({ scope: "lock", lockRuntimeActive: false }, { managers: collectManagers(this) });
        },
        _startLockScopedManagers: function () {
            return StartManagersUseCase.execute({ scope: "lock", lockRuntimeActive: true }, { managers: collectManagers(this) });
        },
        _stopAllManagers: function () {
            EditSessionRuntime.stopAll(collectManagers(this));
        },
        _collectManagers: function () {
            return collectManagers(this);
        },
        _isLockRuntimeActive: function (oStateModel) {
            return ComponentAppRuntime.isLockRuntimeActive(oStateModel);
        },
        _syncLockScopedManagers: function (oStateModel) {
            return ComponentAppRuntime.syncLockScopedManagers(this, oStateModel);
        },
        _applyFrontendRuntimeConfig: function (oFrontendConfig, oStateModel, oEnvState, oMasterDataModel) {
            return ComponentAppRuntime.applyFrontendRuntimeConfig(
                this,
                oFrontendConfig,
                oStateModel,
                oEnvState,
                oMasterDataModel,
                ApplyRuntimeSettingsUseCase
            );
        },
        _applyManagersTimerConfig: function (mTimers) {
            return ComponentAppRuntime.applyManagersTimerConfig(this, mTimers);
        },
        _registerLockReleaseBeacon: function (oStateModel, oMainServiceModel) {
            return ComponentAppRuntime.registerLockReleaseBeacon(this, oStateModel, oMainServiceModel);
        },
        _releaseActiveLockOnLeave: function (oStateModel, oMainServiceModel) {
            return ComponentAppRuntime.releaseActiveLockOnLeave(this, oStateModel, oMainServiceModel);
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
