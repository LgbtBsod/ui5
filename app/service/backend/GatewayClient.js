sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayErrorNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/RequestCoordinator",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientContracts",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientSupport",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClientRequestRuntime"
], function (GatewayErrorNormalizer, RequestCoordinator, GatewayClientContracts, GatewayClientSupport, GatewayClientRequestRuntime) {
    "use strict";

    var _oModel = null;
    var _sServiceUrl = "";

    function ensureModel() {
        if (!_oModel) {
            throw new Error("GatewayClient model is not initialized");
        }
        return _oModel;
    }

    function toPromise(fnExecutor) {
        return new Promise(function (resolve, reject) {
            fnExecutor(resolve, reject);
        });
    }

    function normalizeError(oError, sMethod, sCorrelationId) {
        return GatewayClientRequestRuntime.normalizeError(oError, sMethod, sCorrelationId);
    }

    function serviceUrl() {
        var oModel = ensureModel();
        if (_sServiceUrl) {
            return _sServiceUrl;
        }
        return String((oModel && oModel.sServiceUrl) || "").replace(/\/+$/, "");
    }

    function withReadRequest(sPath, mParams, mHeaders) {
        return GatewayClientRequestRuntime.withReadRequest(ensureModel(), sPath, mParams, mHeaders);
    }

    function withDirectPostRequest(sPath, oPayload, mHeaders) {
        return GatewayClientRequestRuntime.withDirectPostRequest(ensureModel(), sPath, oPayload, mHeaders);
    }

    function withDirectDeleteRequest(sPath, mHeaders) {
        return GatewayClientRequestRuntime.withDirectDeleteRequest(ensureModel(), sPath, mHeaders);
    }

    function withDirectFunctionImportRequest(sName, oPayload, mHeaders) {
        return GatewayClientRequestRuntime.withDirectFunctionImportRequest(ensureModel(), sName, oPayload, mHeaders);
    }

    function withDirectGetFunctionImportRequest(sName, mParams, mHeaders) {
        return GatewayClientRequestRuntime.withDirectGetFunctionImportRequest(ensureModel(), sName, mParams, mHeaders);
    }

    function executeRequest(mRequest) {
        var oRequest = mRequest || {};
        return RequestCoordinator.execute(oRequest).catch(function (oError) {
            throw normalizeError(oError, oRequest.method, oRequest.correlationId);
        });
    }

    return {
        setModel: function (oModel, mOptions) {
            _oModel = oModel || null;
            _sServiceUrl = String((mOptions && mOptions.serviceUrl) || (oModel && oModel.sServiceUrl) || "").replace(/\/+$/, "");
        },
        reset: function () {
            _oModel = null;
            _sServiceUrl = "";
        },
        setHeader: function (sName, sValue) {
            var oModel = ensureModel();
            var mHeaders = Object.assign({}, oModel.getHeaders ? oModel.getHeaders() : {});
            if (!sName) {
                return mHeaders;
            }
            if (sValue === undefined || sValue === null || sValue === "") {
                delete mHeaders[sName];
            } else {
                mHeaders[sName] = String(sValue);
            }
            oModel.setHeaders(mHeaders);
            return mHeaders;
        },
        readEntity: function (entitySet, key, mParams, mOptions) {
            return this.rawRead("/" + entitySet + "(" + key + ")", mParams || {}, mOptions || {});
        },
        rawRead: function (path, mParams, mOptions) {
            var sPath = GatewayClientSupport.assertCanonicalPath(GatewayClientSupport.normalizePath(path));
            var oOptions = mOptions || {};
            return executeRequest({
                method: "GET",
                dedupeKey: "GET|" + sPath + "|" + GatewayClientSupport.encodeUrlParameters(mParams || {}),
                responseGuardKey: oOptions.responseGuardKey,
                timeoutMs: oOptions.timeoutMs,
                retryCount: oOptions.retryCount,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withReadRequest(sPath, mParams || {}, GatewayClientSupport.buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        readSet: function (entitySet, mParams, mOptions) {
            return this.rawRead("/" + entitySet, mParams || {}, mOptions || {}).then(function (oData) {
                return (oData && oData.results) || [];
            });
        },
        serviceUrl: serviceUrl,
        callFunctionImport: function (name, oPayload, mOptions) {
            var oOptions = mOptions || {};
            return executeRequest({
                method: "POST_FUNCTION",
                timeoutMs: oOptions.timeoutMs,
                retryCount: 0,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectFunctionImportRequest(name, oPayload || {}, GatewayClientSupport.buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        callGetFunctionImport: function (name, mParams, mOptions) {
            var oOptions = mOptions || {};
            return executeRequest({
                method: "GET_FUNCTION",
                dedupeKey: "GET_FUNCTION|" + String(name || "") + "|" + GatewayClientSupport.encodeUrlParameters(mParams || {}),
                responseGuardKey: oOptions.responseGuardKey,
                timeoutMs: oOptions.timeoutMs,
                retryCount: oOptions.retryCount,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectGetFunctionImportRequest(name, mParams || {}, GatewayClientSupport.buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        postToPath: function (path, oPayload, mOptions) {
            var sPath = GatewayClientSupport.assertAllowedPath(
                GatewayClientSupport.assertCanonicalPath(GatewayClientSupport.normalizePath(path)),
                GatewayClientContracts.DIRECT_POST_ALLOWLIST,
                "POST"
            );
            var oOptions = mOptions || {};
            return executeRequest({
                method: "POST_ENTITY",
                timeoutMs: oOptions.timeoutMs,
                retryCount: 0,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectPostRequest(sPath, oPayload || {}, GatewayClientSupport.buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        deletePath: function (path, mOptions) {
            var sPath = GatewayClientSupport.assertAllowedPath(
                GatewayClientSupport.assertCanonicalPath(GatewayClientSupport.normalizePath(path)),
                GatewayClientContracts.DIRECT_DELETE_ALLOWLIST,
                "DELETE"
            );
            var oOptions = mOptions || {};
            return executeRequest({
                method: "DELETE",
                timeoutMs: oOptions.timeoutMs,
                retryCount: 0,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectDeleteRequest(sPath, GatewayClientSupport.buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        batch: function (groupId) {
            return toPromise(function (resolve, reject) {
                ensureModel().submitChanges({
                    groupId: groupId || undefined,
                    success: function (oData) {
                        resolve((oData && (oData.__batchResponses || oData.__changeResponses)) || []);
                    },
                    error: function (e) {
                        reject(GatewayErrorNormalizer.normalizeError(e));
                    }
                });
            });
        },
        fetchCsrfToken: function () {
            return toPromise(function (resolve, reject) {
                ensureModel().refreshSecurityToken(function () {
                    resolve(true);
                }, function (e) {
                    reject(GatewayErrorNormalizer.normalizeError(e));
                }, true);
            });
        },
        refreshSecurityToken: function () {
            return this.fetchCsrfToken();
        },
        normalizeError: GatewayErrorNormalizer.normalizeError,
        normalizeODataError: GatewayErrorNormalizer.normalizeODataError
    };
});
