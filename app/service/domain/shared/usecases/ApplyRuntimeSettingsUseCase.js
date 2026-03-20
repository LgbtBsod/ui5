sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/Result",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/TimeConfigService",
"PRODUCTION_CONTROL_CHECKLIST/service/framework/DebugLogger",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/AttachmentUploadPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/constants/FrontendConfigConstants",
    "PRODUCTION_CONTROL_CHECKLIST/model/schema/FrontendVariablesSchema"
], function (Result, TimeConfigService, DebugLogger, AttachmentUploadPolicy, FrontendConfigConstants, FrontendVariablesSchema) {
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

    function parseJsonArray(vRaw) {
        var aParsed = parseJsonObject(vRaw, []);
        return Array.isArray(aParsed) ? aParsed : [];
    }

    return {
        execute: function (input, ctx) {
            var oConfig = input && input.frontendConfig ? input.frontendConfig : {};
            var oStateModel = ctx.stateModel;
            var oEnvModel = ctx.envModel;
            var oMasterDataModel = ctx.masterDataModel;
            var oRuntimePayload = oConfig.runtimeSettingsPayload || {};

            var mTimers = TimeConfigService.normalize(oRuntimePayload, oStateModel.getProperty("/timers") || {});
            var aRequiredFields = parseJsonArray(oRuntimePayload.RequiredFieldsJson).map(function (sValue) {
                return String(sValue || "").trim();
            }).filter(Boolean);
            var mFrontendVariables = FrontendVariablesSchema.sanitize(
                parseJsonObject(oRuntimePayload.FrontendVariablesJson, FrontendConfigConstants.FALLBACKS.FRONTEND_VARIABLES)
            );
            var oUploadPolicy = AttachmentUploadPolicy.normalizeUploadPolicy(
                parseJsonObject(oRuntimePayload.UploadPolicyJson, {})
            );
            oStateModel.setProperty("/timers", mTimers);
            oStateModel.setProperty("/requiredFields", aRequiredFields);
            oStateModel.setProperty("/frontendVariables", mFrontendVariables);
            oEnvModel.setProperty("/source", oConfig.source || FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL);
            oEnvModel.setProperty("/loadedAt", new Date().toISOString());
            oEnvModel.setProperty("/timers", mTimers);
            oEnvModel.setProperty("/variables", mFrontendVariables);
            if (oMasterDataModel && oMasterDataModel.setProperty) {
                oMasterDataModel.setProperty("/runtime/timers", mTimers);
                oMasterDataModel.setProperty("/runtime/requiredFields", aRequiredFields);
                oMasterDataModel.setProperty("/runtime/frontendVariables", mFrontendVariables);
                oMasterDataModel.setProperty("/runtime/uploadPolicy", oUploadPolicy);
            }

            if (DebugLogger.isEnabled() && !_runtimeTimersLogDone && oConfig.source === FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL) {
                _runtimeTimersLogDone = true;
                DebugLogger.info("RUNTIME_TIMERS_APPLIED", "[RUNTIME_TIMERS_APPLIED]", { source: FrontendConfigConstants.SOURCES.RUNTIME_SETTINGS_GLOBAL, timers: mTimers, loadedAt: new Date().toISOString() });
            }

            return Promise.resolve(Result.ok({
                timers: mTimers,
                requiredFields: aRequiredFields,
                frontendVariables: mFrontendVariables,
                uploadPolicy: oUploadPolicy
            }, []));
        }
    };
});
