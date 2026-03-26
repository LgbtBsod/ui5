sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayErrorNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayErrorNormalizer, GatewayContractConstants) {
    "use strict";

    var _oModel = null;
    var _sServiceUrl = "";
    var mResponseGuardTokens = {};
    var REQUEST = GatewayContractConstants.REQUEST;

    function createModelError() {
        var oError = new Error("GatewayClient model is not initialized");
        oError.code = "GATEWAY_MODEL_NOT_INITIALIZED";
        return oError;
    }

    function ensureModel() {
        if (!_oModel) {
            throw createModelError();
        }
        return _oModel;
    }

    function nextCorrelationId() {
        return [
            "req",
            Date.now().toString(36),
            Math.random().toString(36).slice(2, 10)
        ].join("-");
    }

    function escapeRegExp(sValue) {
        return String(sValue || "").replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    }

    function exactPattern(sValue) {
        return new RegExp("^" + escapeRegExp(sValue) + "$", "i");
    }

    function disallowedPathPattern(sTail) {
        return new RegExp("^\\/+" + sTail + "(?:$|[/?(])", "i");
    }

    var DIRECT_FUNCTION_BODY_ALLOWLIST = [
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.SAVE_CHANGES),
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.AUTO_SAVE),
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.CREATE_CHECKLIST),
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.REPORT_EXPORT),
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_ACQUIRE),
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_HEARTBEAT),
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.LOCK_RELEASE)
    ];

    var DIRECT_FUNCTION_QUERY_ALLOWLIST = [
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.COPY_CHECKLIST),
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.ANALYTICS_REFRESH_TRIGGER)
    ];

    var DIRECT_GET_FUNCTION_ALLOWLIST = [
        exactPattern(GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY)
    ];

    var FORBIDDEN_PATH_PATTERNS = [
        /^\/actions\//i,
        /^\/lock\//i,
        /^\/config\/frontend(?:$|[/?])/i,
    ].concat((GatewayContractConstants.DISALLOWED_PATHS || []).map(disallowedPathPattern));

    function normalizePath(sPath) {
        var sNormalized = String(sPath || "");
        return sNormalized.charAt(0) === "/" ? sNormalized : ("/" + sNormalized);
    }

    function allowlisted(sValue, aAllowed) {
        return (aAllowed || []).some(function (oPattern) {
            return oPattern.test(sValue);
        });
    }

    function assertCanonicalPath(sPath) {
        FORBIDDEN_PATH_PATTERNS.forEach(function (oPattern) {
            if (oPattern.test(sPath)) {
                throw new Error("Forbidden non-canonical OData path: " + sPath);
            }
        });
        return sPath;
    }

    function assertAllowedFunctionName(sName) {
        var sResolved = String(sName || "").trim();
        if (!sResolved) {
            throw new Error("Function import name is required");
        }
        if (FORBIDDEN_PATH_PATTERNS.some(function (oPattern) { return oPattern.test("/" + sResolved); })) {
            throw new Error("Forbidden non-canonical function import: " + sResolved);
        }
        return sResolved;
    }

    function buildHeaders(mHeaders, sCorrelationId) {
        var mResolved = Object.assign({}, mHeaders || {});
        if (sCorrelationId) {
            mResolved["X-Correlation-ID"] = sCorrelationId;
            mResolved["X-Request-ID"] = sCorrelationId;
        }
        return mResolved;
    }

    function normalizeError(oError, sMethod, sCorrelationId) {
        var oNormalized = GatewayErrorNormalizer.normalizeError(Object.assign({}, oError || {}, {
            requestMethod: sMethod,
            correlationId: (oError && oError.correlationId) || sCorrelationId || ""
        }));
        if (!oNormalized.correlationId && sCorrelationId) {
            oNormalized.correlationId = sCorrelationId;
        }
        return oNormalized;
    }

    function markResponseGuard(sGuardKey) {
        var sKey = String(sGuardKey || "").trim();
        if (!sKey) {
            return 0;
        }
        mResponseGuardTokens[sKey] = Number(mResponseGuardTokens[sKey] || 0) + 1;
        return mResponseGuardTokens[sKey];
    }

    function isCurrentGuard(sGuardKey, iToken) {
        var sKey = String(sGuardKey || "").trim();
        if (!sKey || !iToken) {
            return true;
        }
        return Number(mResponseGuardTokens[sKey] || 0) === Number(iToken || 0);
    }

    function createOutdatedError(sCorrelationId, sGuardKey) {
        return {
            code: "OUTDATED_RESPONSE",
            message: "Outdated response ignored",
            statusCode: 0,
            correlationId: sCorrelationId,
            responseGuardKey: String(sGuardKey || ""),
            ignored: true,
            silent: true
        };
    }

    function serviceUrl() {
        var oModel = ensureModel();
        if (_sServiceUrl) {
            return _sServiceUrl;
        }
        return String((oModel && oModel.sServiceUrl) || "").replace(/\/+$/, "");
    }

    function executeReadRequest(sMethod, mOptions, fnRequest) {
        var oOptions = mOptions || {};
        var sCorrelationId = String(oOptions.correlationId || nextCorrelationId()).trim();
        var sGuardKey = String(oOptions.responseGuardKey || "").trim();
        var iGuardToken = markResponseGuard(sGuardKey);

        return new Promise(function (resolve, reject) {
            fnRequest(resolve, reject, buildHeaders(oOptions.headers, sCorrelationId));
        }).then(function (oData) {
            if (sGuardKey && !isCurrentGuard(sGuardKey, iGuardToken)) {
                throw createOutdatedError(sCorrelationId, sGuardKey);
            }
            return oData || {};
        }).catch(function (oError) {
            if (sGuardKey && !isCurrentGuard(sGuardKey, iGuardToken)) {
                throw createOutdatedError(sCorrelationId, sGuardKey);
            }
            throw normalizeError(oError, sMethod, sCorrelationId);
        });
    }

    function executeMutatingRequest(sMethod, fnRequest, mOptions) {
        var oOptions = mOptions || {};
        var sCorrelationId = String(oOptions.correlationId || nextCorrelationId()).trim();

        return new Promise(function (resolve, reject) {
            fnRequest(resolve, reject, buildHeaders(oOptions.headers, sCorrelationId));
        }).catch(function (oError) {
            throw normalizeError(oError, sMethod, sCorrelationId);
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
            mResponseGuardTokens = {};
        },
        hasModel: function () {
            return !!_oModel;
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
            var sPath = assertCanonicalPath(normalizePath(path));
            return executeReadRequest(REQUEST.GET, mOptions, function (resolve, reject, mHeaders) {
                ensureModel().read(sPath, {
                    urlParameters: mParams || {},
                    headers: mHeaders,
                    success: function (oData) { resolve(oData || {}); },
                    error: function (oError) { reject(oError); }
                });
            });
        },
        readSet: function (entitySet, mParams, mOptions) {
            return this.rawRead("/" + entitySet, mParams || {}, mOptions || {}).then(function (oData) {
                return (oData && oData.results) || [];
            });
        },
        create: function (path, oPayload, mOptions) {
            var sPath = assertCanonicalPath(normalizePath(path));
            return executeMutatingRequest(REQUEST.POST, function (resolve, reject, mHeaders) {
                ensureModel().create(sPath, oPayload || {}, {
                    headers: mHeaders,
                    success: function (oData) { resolve(oData || {}); },
                    error: function (oError) { reject(oError); }
                });
            }, mOptions || {});
        },
        uploadMedia: function (path, vBody, mOptions) {
            var sPath = assertCanonicalPath(normalizePath(path));
            var oOptions = mOptions || {};
            return executeMutatingRequest(REQUEST.POST, function (resolve, reject, mHeaders) {
                var oModel = ensureModel();
                var sResolvedUrl = serviceUrl() + sPath;
                var sCsrfToken = String((oModel && oModel.getSecurityToken && oModel.getSecurityToken()) || "").trim();
                var mFetchHeaders = Object.assign({}, mHeaders || {}, oOptions.headers || {});

                if (!sResolvedUrl || typeof window === "undefined" || typeof window.fetch !== "function") {
                    reject({
                        code: "MEDIA_UPLOAD_UNSUPPORTED",
                        message: "Media upload requires fetch-capable browser runtime",
                        path: sPath
                    });
                    return;
                }

                if (sCsrfToken) {
                    mFetchHeaders["X-CSRF-Token"] = sCsrfToken;
                }
                if (oOptions.contentType) {
                    mFetchHeaders["Content-Type"] = String(oOptions.contentType);
                }

                window.fetch(sResolvedUrl, {
                    method: REQUEST.POST,
                    credentials: "same-origin",
                    headers: mFetchHeaders,
                    body: vBody || null
                }).then(function (oResponse) {
                    return oResponse.text().then(function (sText) {
                        var oPayload;
                        if (!oResponse.ok) {
                            reject({
                                statusCode: oResponse.status,
                                statusText: oResponse.statusText,
                                responseText: sText,
                                message: sText || oResponse.statusText || "Media upload failed",
                                path: sPath
                            });
                            return;
                        }
                        if (!sText) {
                            resolve({});
                            return;
                        }
                        try {
                            oPayload = JSON.parse(sText);
                        } catch (_parseError) {
                            resolve({ rawText: sText });
                            return;
                        }
                        resolve(oPayload || {});
                    });
                }).catch(function (oError) {
                    reject(oError);
                });
            }, oOptions);
        },
        serviceUrl: serviceUrl,
        getModel: function () {
            return ensureModel();
        },
        callFunctionImport: function (name, oPayload, mOptions) {
            var sFunctionName = assertAllowedFunctionName(name);
            var oOptions = mOptions || {};
            if (oOptions.async === false) {
                return Promise.reject(new Error("Synchronous function imports are not supported"));
            }
            return executeMutatingRequest(REQUEST.POST_FUNCTION, function (resolve, reject, mHeaders) {
                if (allowlisted(sFunctionName, DIRECT_FUNCTION_QUERY_ALLOWLIST)) {
                    ensureModel().callFunction("/" + sFunctionName, {
                        method: REQUEST.POST,
                        urlParameters: oPayload || {},
                        headers: mHeaders,
                        success: function (oData) { resolve(oData || {}); },
                        error: function (oError) { reject(oError); }
                    });
                    return;
                }
                if (allowlisted(sFunctionName, DIRECT_FUNCTION_BODY_ALLOWLIST)) {
                    ensureModel().create("/" + sFunctionName, oPayload || {}, {
                        headers: mHeaders,
                        success: function (oData) { resolve(oData || {}); },
                        error: function (oError) { reject(oError); }
                    });
                    return;
                }
                reject(new Error("Unsupported function import: " + sFunctionName));
            }, oOptions);
        },
        callGetFunctionImport: function (name, mParams, mOptions) {
            var sFunctionName = assertAllowedFunctionName(name);
            if (!allowlisted(sFunctionName, DIRECT_GET_FUNCTION_ALLOWLIST)) {
                return Promise.reject(new Error("Unsupported GET function import: " + sFunctionName));
            }
            return executeReadRequest(REQUEST.GET_FUNCTION, mOptions, function (resolve, reject, mHeaders) {
                ensureModel().callFunction("/" + sFunctionName, {
                    method: REQUEST.GET,
                    urlParameters: mParams || {},
                    headers: mHeaders,
                    success: function (oData) { resolve(oData || {}); },
                    error: function (oError) { reject(oError); }
                });
            });
        },
        deletePath: function (path, mOptions) {
            var sPath = assertCanonicalPath(normalizePath(path));
            return Promise.reject(normalizeError({
                code: "DIRECT_DELETE_UNSUPPORTED",
                message: "Direct DELETE is not supported in target Gateway contract",
                path: sPath
            }, REQUEST.DELETE, String((mOptions && mOptions.correlationId) || "").trim()));
        },
        batch: function (groupId) {
            return executeMutatingRequest(REQUEST.BATCH, function (resolve, reject) {
                ensureModel().submitChanges({
                    groupId: groupId || undefined,
                    success: function (oData) {
                        resolve((oData && (oData.__batchResponses || oData.__changeResponses)) || []);
                    },
                    error: function (oError) {
                        reject(oError);
                    }
                });
            }, {});
        },
        fetchCsrfToken: function () {
            return new Promise(function (resolve, reject) {
                var oModel = ensureModel();
                if (!oModel || typeof oModel.refreshSecurityToken !== "function") {
                    reject(new Error("security_token_refresh_unavailable"));
                    return;
                }
                oModel.refreshSecurityToken(function () {
                    resolve(true);
                }, function (oError) {
                    reject(normalizeError(oError, REQUEST.POST_FUNCTION, ""));
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
