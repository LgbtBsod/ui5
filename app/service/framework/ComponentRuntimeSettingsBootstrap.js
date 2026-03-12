sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/util/runtime/FrontendConfigConstants"
], function (ModelStateRuntime, FrontendConfigConstants) {
    "use strict";

    function normalizeRuntimeSettingsError(oError) {
        return {
            message: String((oError && oError.message) || "").trim(),
            code: String((oError && oError.code) || "").trim(),
            status: Number((oError && (oError.statusCode || oError.status)) || 0) || 0
        };
    }

    function createRuntimeSettingsRuntime(oComponent, mOptions) {
        var oStateModel = mOptions.stateModel;
        var oEnvModel = mOptions.envModel;
        var oMasterDataModel = mOptions.masterDataModel;
        var SettingsManager = mOptions.settingsManager;
        var GatewayBackendService = mOptions.gatewayBackendService;
        var TelemetryRuntime = mOptions.telemetryRuntime;
        var fnEmitTelemetry = mOptions.emitTelemetry;
        var oRuntimeApplyQueue = Promise.resolve();

        function applyRuntimeSettings(oRuntime) {
            oRuntimeApplyQueue = oRuntimeApplyQueue.catch(function () {
                return null;
            }).then(function () {
                return oComponent._applyFrontendRuntimeConfig({
                    source: FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL,
                    runtimeSettingsPayload: oRuntime || {}
                }, oStateModel, oEnvModel, oMasterDataModel).then(function () {
                    ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", "gateway_runtime");
                    fnEmitTelemetry("runtime.config.loaded", TelemetryRuntime.runtimeConfig(FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL));
                    return oRuntime || {};
                });
            });
            return oRuntimeApplyQueue;
        }

        oComponent._fnUnsubscribeRuntimeSettings = SettingsManager.subscribe(function (oRuntime, mMeta) {
            if (!mMeta || !mMeta.refreshed) {
                return;
            }
            applyRuntimeSettings(oRuntime).catch(function () {
                ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", "gateway_runtime_error");
            });
        });

        return {
            applyRuntimeSettings: applyRuntimeSettings,
            loadRuntimeSettings: function (mLoadOptions) {
                var bForce = !!(mLoadOptions && mLoadOptions.force);
                var pLoad = bForce ? SettingsManager.reload(GatewayBackendService) : SettingsManager.load(GatewayBackendService);
                return pLoad.then(function (oRuntime) {
                    return applyRuntimeSettings(oRuntime);
                }).catch(function (oError) {
                    var oOriginalError = normalizeRuntimeSettingsError(oError);
                    ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", "gateway_runtime_error");
                    fnEmitTelemetry("runtime.config.fallback_applied", TelemetryRuntime.runtimeConfig(
                        FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL,
                        "runtime_settings_fallback_applied",
                        oOriginalError
                    ));
                    fnEmitTelemetry("runtime.config.load_failed", TelemetryRuntime.runtimeConfig(
                        FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL,
                        oOriginalError.message || "runtime_settings_load_failed",
                        oOriginalError
                    ));
                    throw oError || new Error("runtime_settings_load_failed");
                });
            }
        };
    }

    return {
        createRuntimeSettingsRuntime: createRuntimeSettingsRuntime
    };
});
