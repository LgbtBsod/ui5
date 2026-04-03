sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/ModelStateRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/runtime/component/ComponentBootstrapContracts"
], function (ModelStateRuntime, FrontendConfigConstants, ComponentBootstrapContracts) {
    "use strict";

    function normalizeRuntimeSettingsError(oError) {
        return {
            message: String((oError && oError.message) || "").trim(),
            code: String((oError && oError.code) || "").trim(),
            status: Number((oError && (oError.statusCode || oError.status)) || 0) || 0
        };
    }

    function initializeRuntimeSettings(oComponent, mOptions) {
        var oStateModel = mOptions.stateModel;
        var oEnvState = mOptions.envState;
        var oMasterDataModel = mOptions.masterDataModel;
        var SettingsManager = mOptions.settingsManager;
        var GatewayClient = mOptions.gatewayBackendService;
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
                }, oStateModel, oEnvState, oMasterDataModel).then(function () {
                    ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", ComponentBootstrapContracts.FRONTEND_CONFIG_SOURCE.GATEWAY_RUNTIME);
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
                ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", ComponentBootstrapContracts.FRONTEND_CONFIG_SOURCE.GATEWAY_RUNTIME_ERROR);
            });
        });

        return {
            applyRuntimeSettings: applyRuntimeSettings,
            loadRuntimeSettings: function (mLoadOptions) {
                var bForce = !!(mLoadOptions && mLoadOptions.force);
                var pLoad = bForce ? SettingsManager.reload(GatewayClient) : SettingsManager.load(GatewayClient);
                return pLoad.then(function (oRuntime) {
                    return applyRuntimeSettings(oRuntime);
                }).catch(function (oError) {
                    var oOriginalError = normalizeRuntimeSettingsError(oError);
                    var sFallbackCode = ComponentBootstrapContracts.STAGE_ERRORS.RUNTIME_SETTINGS_LOAD_FAILED;
                    ModelStateRuntime.writeOnModel(oStateModel, "/frontendConfigSource", ComponentBootstrapContracts.FRONTEND_CONFIG_SOURCE.GATEWAY_RUNTIME_ERROR);
                    fnEmitTelemetry("runtime.config.fallback_applied", TelemetryRuntime.runtimeConfig(
                        FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL,
                        sFallbackCode,
                        oOriginalError
                    ));
                    fnEmitTelemetry("runtime.config.load_failed", TelemetryRuntime.runtimeConfig(
                        FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL,
                        oOriginalError.message || sFallbackCode,
                        oOriginalError
                    ));
                    if (oError) {
                        throw oError;
                    }
                    throw new Error(sFallbackCode);
                });
            }
        };
    }

    return {
        initializeRuntimeSettings: initializeRuntimeSettings
    };
});
