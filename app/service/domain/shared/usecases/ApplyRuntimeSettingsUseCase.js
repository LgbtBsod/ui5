sap.ui.define([
    "sap_ui5/service/framework/Result",
    "sap_ui5/util/TimeConfigService",
    "sap_ui5/util/DebugLogger",
    "sap_ui5/util/AttachmentUploadPolicy"
], function (Result, TimeConfigService, DebugLogger, AttachmentUploadPolicy) {
    "use strict";

    var _runtimeTimersLogDone = false;

    function parseJsonObject(vRaw, vFallback) {
        if (vRaw == null || vRaw === "") {
            return vFallback;
        }
        if (typeof vRaw === "object") {
            return vRaw;
        }
        try {
            return JSON.parse(String(vRaw));
        } catch (e) {
            return vFallback;
        }
    }

    return {
        execute: function (input, ctx) {
            var oConfig = input && input.frontendConfig ? input.frontendConfig : {};
            var oStateModel = ctx.stateModel;
            var oEnvModel = ctx.envModel;
            var oMasterDataModel = ctx.masterDataModel;

            var mTimers = TimeConfigService.normalize(oConfig.runtimeSettingsPayload || {}, oStateModel.getProperty("/timers") || {});
            var oUploadPolicy = AttachmentUploadPolicy.normalizeUploadPolicy(
                parseJsonObject((oConfig.runtimeSettingsPayload || {}).UploadPolicyJson, AttachmentUploadPolicy.DEFAULT_UPLOAD_POLICY)
            );
            oStateModel.setProperty("/timers", mTimers);
            oEnvModel.setProperty("/source", oConfig.source || "RuntimeSettingsSet(GLOBAL)");
            oEnvModel.setProperty("/loadedAt", new Date().toISOString());
            oEnvModel.setProperty("/timers", mTimers);
            if (oMasterDataModel && oMasterDataModel.setProperty) {
                oMasterDataModel.setProperty("/runtime/timers", mTimers);
                oMasterDataModel.setProperty("/runtime/uploadPolicy", oUploadPolicy || AttachmentUploadPolicy.DEFAULT_UPLOAD_POLICY);
            }

            if (DebugLogger.isEnabled() && !_runtimeTimersLogDone && oConfig.source === "RuntimeSettingsSet(GLOBAL)") {
                _runtimeTimersLogDone = true;
                DebugLogger.info("RUNTIME_TIMERS_APPLIED", "[RUNTIME_TIMERS_APPLIED]", { source: "RuntimeSettingsSet(GLOBAL)", timers: mTimers, loadedAt: new Date().toISOString() });
            }

            return Promise.resolve(Result.ok({ timers: mTimers }, []));
        }
    };
});
