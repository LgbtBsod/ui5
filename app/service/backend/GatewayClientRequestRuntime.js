sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayErrorNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientSupport",
    "PRODUCTION_CONTROL_CHECKLIST/constants/RequestVerbConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/JsRuntimeStringConstants"
], function (GatewayErrorNormalizer, GatewayClientContracts, GatewayClientSupport, RequestVerbConstants, JsRuntimeStringConstants) {
    "use strict";

    var REQUEST = RequestVerbConstants.REQUEST;
    var TYPEOF = JsRuntimeStringConstants.TYPEOF;

    function toRequestHandle(fnExecutor) {
        var fnAbort = function () { return; };
        var pPromise = new Promise(function (resolve, reject) {
            try {
                fnAbort = fnExecutor(resolve, reject) || fnAbort;
            } catch (oError) {
                reject(oError);
            }
        });
        return {
            promise: pPromise,
            abort: typeof fnAbort === TYPEOF.FUNCTION ? fnAbort : function () { return; }
        };
    }

    function normalizeError(oError, sMethod, sCorrelationId) {
        var oNormalized = GatewayErrorNormalizer.normalizeError(oError);
        oNormalized.requestMethod = sMethod;
        if (!oNormalized.correlationId && sCorrelationId) {
            oNormalized.correlationId = sCorrelationId;
        }
        return oNormalized;
    }

    function withReadRequest(oModel, sPath, mParams, mHeaders) {
        return toRequestHandle(function (resolve, reject) {
            return oModel.read(GatewayClientSupport.assertCanonicalPath(GatewayClientSupport.normalizePath(sPath)), {
                urlParameters: mParams || {},
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function withDirectDeleteRequest(oModel, sPath, mHeaders) {
        var sNormalizedPath = GatewayClientSupport.assertCanonicalPath(GatewayClientSupport.normalizePath(sPath));
        return toRequestHandle(function (resolve, reject) {
            return oModel.remove(sNormalizedPath, {
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function withDirectFunctionImportRequest(oModel, sName, oPayload, mHeaders, mOptions) {
        var sFunctionName = GatewayClientSupport.assertAllowedFunctionName(sName);
        if (mOptions && mOptions.async === false) {
            throw new Error("Synchronous function imports are not supported");
        }
        if (GatewayClientSupport.allowlisted(sFunctionName, GatewayClientContracts.DIRECT_FUNCTION_QUERY_ALLOWLIST)) {
            return toRequestHandle(function (resolve, reject) {
                return oModel.callFunction("/" + sFunctionName, {
                    method: REQUEST.POST,
                    urlParameters: oPayload || {},
                    headers: mHeaders || {},
                    success: function (oData) { resolve(oData || {}); },
                    error: function (e) { reject(e); }
                });
            });
        }
        if (GatewayClientSupport.allowlisted(sFunctionName, GatewayClientContracts.DIRECT_FUNCTION_BODY_ALLOWLIST)) {
            return toRequestHandle(function (resolve, reject) {
                return oModel.create("/" + sFunctionName, oPayload || {}, {
                    headers: mHeaders || {},
                    success: function (oData) { resolve(oData || {}); },
                    error: function (e) { reject(e); }
                });
            });
        }
        throw new Error("Unsupported function import: " + sFunctionName);
    }

    function withDirectGetFunctionImportRequest(oModel, sName, mParams, mHeaders) {
        var sFunctionName = GatewayClientSupport.assertAllowedFunctionName(sName);
        if (!GatewayClientSupport.allowlisted(sFunctionName, GatewayClientContracts.DIRECT_GET_FUNCTION_ALLOWLIST)) {
            throw new Error("Unsupported GET function import: " + sFunctionName);
        }
        return toRequestHandle(function (resolve, reject) {
            return oModel.callFunction("/" + sFunctionName, {
                method: REQUEST.GET,
                urlParameters: mParams || {},
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    return {
        normalizeError: normalizeError,
        withDirectDeleteRequest: withDirectDeleteRequest,
        withDirectFunctionImportRequest: withDirectFunctionImportRequest,
        withDirectGetFunctionImportRequest: withDirectGetFunctionImportRequest,
        withReadRequest: withReadRequest
    };
});
