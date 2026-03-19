sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayErrorNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientSupport"
], function (GatewayErrorNormalizer, GatewayClientContracts, GatewayClientSupport) {
    "use strict";

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
            abort: typeof fnAbort === "function" ? fnAbort : function () { return; }
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

    function withDirectPostRequest(oModel, sPath, oPayload, mHeaders) {
        var sNormalizedPath = GatewayClientSupport.assertCanonicalPath(GatewayClientSupport.normalizePath(sPath));
        return toRequestHandle(function (resolve, reject) {
            return oModel.create(sNormalizedPath, oPayload || {}, {
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
        var bAsync = !mOptions || typeof mOptions.async !== "boolean" ? true : mOptions.async;
        if (GatewayClientSupport.allowlisted(sFunctionName, GatewayClientContracts.DIRECT_FUNCTION_QUERY_ALLOWLIST)) {
            return toRequestHandle(function (resolve, reject) {
                return oModel.callFunction("/" + sFunctionName, {
                    method: "POST",
                    async: bAsync,
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
                    async: bAsync,
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
                method: "GET",
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
        withDirectPostRequest: withDirectPostRequest,
        withReadRequest: withReadRequest
    };
});
