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
        var sServiceUrl = String((oModel && oModel.sServiceUrl) || "").replace(/\/+$/, "");
        var sSecurityToken = String((oModel && oModel.getSecurityToken && oModel.getSecurityToken()) || "").trim();
        var bUseFetch = typeof window !== "undefined" && typeof window.fetch === "function" && !!sServiceUrl;

        if (bUseFetch) {
            return toRequestHandle(function (resolve, reject) {
                var oAbortController = typeof AbortController === "function" ? new AbortController() : null;
                var mRequestHeaders = Object.assign({
                    "Accept": "application/json",
                    "Content-Type": "application/json"
                }, mHeaders || {});
                if (sSecurityToken) {
                    mRequestHeaders["X-CSRF-Token"] = sSecurityToken;
                }
                window.fetch(sServiceUrl + sNormalizedPath, {
                    method: "POST",
                    headers: mRequestHeaders,
                    body: JSON.stringify(oPayload || {}),
                    credentials: "same-origin",
                    signal: oAbortController ? oAbortController.signal : undefined
                }).then(function (oResponse) {
                    var sContentType = String(oResponse.headers.get("content-type") || "").toLowerCase();
                    if (!oResponse.ok) {
                        return oResponse.text().then(function (sBody) {
                            reject({
                                statusCode: oResponse.status,
                                message: sBody || oResponse.statusText || "Request failed",
                                responseText: sBody || "",
                                responseHeaders: {
                                    "x-csrf-token": oResponse.headers.get("x-csrf-token") || ""
                                }
                            });
                        });
                    }
                    if (sContentType.indexOf("application/json") >= 0) {
                        return oResponse.json().then(function (oData) {
                            resolve((oData && oData.d) || oData || {});
                        });
                    }
                    return oResponse.text().then(function (sBody) {
                        resolve({ value: sBody || "" });
                    });
                }).catch(function (oError) {
                    reject(oError);
                });
                return function () {
                    if (oAbortController) {
                        oAbortController.abort();
                    }
                };
            });
        }
        return toRequestHandle(function (resolve, reject) {
            return oModel.create(sNormalizedPath, oPayload || {}, {
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

    return {
        normalizeError: normalizeError,
        withDirectDeleteRequest: withDirectDeleteRequest,
        withDirectFunctionImportRequest: withDirectFunctionImportRequest,
        withDirectGetFunctionImportRequest: withDirectGetFunctionImportRequest,
        withDirectPostRequest: withDirectPostRequest,
        withReadRequest: withReadRequest
    };
});
