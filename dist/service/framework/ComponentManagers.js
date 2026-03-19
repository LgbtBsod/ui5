sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/EditSessionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/StartManagersUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/usecases/ApplyRuntimeSettingsUseCase",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ComponentAppRuntime"
], function (EditSessionRuntime, StartManagersUseCase, ApplyRuntimeSettingsUseCase, ComponentAppRuntime) {
    "use strict";

    function collect(oComponent) {
        return ComponentAppRuntime.collectManagers(oComponent);
    }

    return {
        startCore: function (oComponent) {
            return StartManagersUseCase.execute({ scope: "core" }, { managers: collect(oComponent) });
        },
        stopLockScoped: function (oComponent) {
            return StartManagersUseCase.execute({ scope: "lock", lockRuntimeActive: false }, { managers: collect(oComponent) });
        },
        startLockScoped: function (oComponent) {
            return StartManagersUseCase.execute({ scope: "lock", lockRuntimeActive: true }, { managers: collect(oComponent) });
        },
        stopAll: function (oComponent) {
            EditSessionRuntime.stopAll(collect(oComponent));
        },
        collect: collect,
        isLockRuntimeActive: ComponentAppRuntime.isLockRuntimeActive,
        syncLockScopedManagers: function (oComponent, oStateModel) {
            return ComponentAppRuntime.syncLockScopedManagers(oComponent, oStateModel);
        },
        applyFrontendRuntimeConfig: function (oComponent, oFrontendConfig, oStateModel, oEnvModel, oMasterDataModel) {
            return ComponentAppRuntime.applyFrontendRuntimeConfig(
                oComponent,
                oFrontendConfig,
                oStateModel,
                oEnvModel,
                oMasterDataModel,
                ApplyRuntimeSettingsUseCase
            );
        },
        applyManagersTimerConfig: function (oComponent, mTimers) {
            return ComponentAppRuntime.applyManagersTimerConfig(oComponent, mTimers);
        },
        registerLockReleaseBeacon: function (oComponent, oStateModel, oMainServiceModel) {
            return ComponentAppRuntime.registerLockReleaseBeacon(oComponent, oStateModel, oMainServiceModel);
        },
        releaseActiveLockOnLeave: function (oComponent, oStateModel, oMainServiceModel) {
            return ComponentAppRuntime.releaseActiveLockOnLeave(oComponent, oStateModel, oMainServiceModel);
        }
    };
});
