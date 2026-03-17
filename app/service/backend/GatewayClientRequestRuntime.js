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
        return toRequestHandle(function (resolve, reject) {
            return oModel.create(sPath, oPayload || {}, {
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function withDirectDeleteRequest(oModel, sPath, mHeaders) {
        return toRequestHandle(function (resolve, reject) {
            return oModel.remove(sPath, {
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function withDirectFunctionImportRequest(oModel, sName, oPayload, mHeaders) {
        var sFunctionName = GatewayClientSupport.assertAllowedFunctionName(sName);
        if (GatewayClientSupport.allowlisted(sFunctionName, GatewayClientContracts.DIRECT_FUNCTION_QUERY_ALLOWLIST)) {
            return toRequestHandle(function (resolve, reject) {
                return oModel.callFunction("/" + sFunctionName, {
                    method: "POST",
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
                method: "GET",
                urlParameters: mParams || {},
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function refreshSecurityToken(oModel) {
        return new Promise(function (resolve, reject) {
            if (!oModel || typeof oModel.refreshSecurityToken !== "function") {
                reject(new Error("security_token_refresh_unavailable"));
                return;
            }
            oModel.refreshSecurityToken(function () {
                resolve(true);
            }, function (oError) {
                reject(oError);
            }, true);
        });
    }

    return {
        normalizeError: normalizeError,
        refreshSecurityToken: refreshSecurityToken,
        withDirectDeleteRequest: withDirectDeleteRequest,
        withDirectFunctionImportRequest: withDirectFunctionImportRequest,
        withDirectGetFunctionImportRequest: withDirectGetFunctionImportRequest,
        withDirectPostRequest: withDirectPostRequest,
        withReadRequest: withReadRequest
    };
});
