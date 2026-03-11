sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayErrorNormalizer",
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/RequestCoordinator"
], function (GatewayErrorNormalizer, RequestCoordinator) {
    "use strict";

    var _oModel = null;
    var _sServiceUrl = "";
    var FORBIDDEN_PATH_PATTERNS = [
        /^\/actions\//i,
        /^\/lock\//i,
        /^\/config\/frontend(?:$|[/?])/i,
        /^\/FrontendRuntimeSettings(?:$|[/?(])/i,
        /^\/capabilities(?:$|[/?])/i,
        /^\/ChecklistRoots(?:$|[/?])/i,
        /^\/SearchRows(?:$|[/?])/i,
        /^\/ChecklistChecksSet(?:$|[/?])/i,
        /^\/ChecklistBarriersSet(?:$|[/?])/i
    ];
    var DIRECT_POST_ALLOWLIST = [
        /^\/CreateChecklist(?:$|[?(])/i,
        /^\/AutoSave(?:$|[?(])/i,
        /^\/SaveChanges(?:$|[?(])/i,
        /^\/ReportExport(?:$|[?(])/i
    ];
    var DIRECT_FUNCTION_QUERY_ALLOWLIST = [
        /^LockAcquire$/i,
        /^LockHeartbeat$/i,
        /^LockRelease$/i,
        /^CopyChecklist$/i,
        /^AnalyticsRefreshTrigger$/i,
        /^SetChecklistStatus$/i
    ];
    var DIRECT_FUNCTION_BODY_ALLOWLIST = [];
    var DIRECT_GET_FUNCTION_ALLOWLIST = [
        /^GetHierarchy$/i
    ];
    var DIRECT_PUT_ALLOWLIST = [
        /^\/AttachmentSet\(AttachmentKey='[^']+'\)\/\$value$/i
    ];
    var DIRECT_CREATE_ALLOWLIST = [
        /^\/AttachmentSet(?:$|[?(])/i
    ];
    var DIRECT_DELETE_ALLOWLIST = [
        /^\/ChecklistRootSet\('[^']+'\)$/i,
        /^\/AttachmentSet\(AttachmentKey='[^']+'\)$/i
    ];

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

    function normalizePath(sPath) {
        var sNormalized = String(sPath || "");
        return sNormalized.charAt(0) === "/" ? sNormalized : ("/" + sNormalized);
    }

    function assertCanonicalPath(sPath) {
        FORBIDDEN_PATH_PATTERNS.forEach(function (oPattern) {
            if (oPattern.test(sPath)) {
                throw new Error("Forbidden non-canonical OData path: " + sPath);
            }
        });
        return sPath;
    }

    function assertAllowedPath(sPath, aAllowed, sOperation) {
        var bAllowed = (aAllowed || []).some(function (oPattern) {
            return oPattern.test(sPath);
        });
        if (!bAllowed) {
            throw new Error("Unsupported " + sOperation + " OData path: " + sPath);
        }
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

    function allowlisted(sValue, aAllowed) {
        return (aAllowed || []).some(function (oPattern) {
            return oPattern.test(sValue);
        });
    }

    function encodeUrlParameters(mParameters) {
        return Object.keys(mParameters || {}).reduce(function (aPairs, sKey) {
            var vValue = mParameters[sKey];
            if (vValue === undefined || vValue === null || vValue === "") {
                return aPairs;
            }
            aPairs.push(encodeURIComponent(sKey) + "=" + encodeURIComponent(String(vValue)));
            return aPairs;
        }, []).join("&");
    }

    function serializePayload(vPayload) {
        try {
            return JSON.stringify(vPayload || {});
        } catch (_e) {
            return "";
        }
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
        var oNormalized = GatewayErrorNormalizer.normalizeError(oError);
        oNormalized.requestMethod = sMethod;
        if (!oNormalized.correlationId && sCorrelationId) {
            oNormalized.correlationId = sCorrelationId;
        }
        return oNormalized;
    }

    function serviceUrl() {
        var oModel = ensureModel();
        if (_sServiceUrl) {
            return _sServiceUrl;
        }
        return String((oModel && oModel.sServiceUrl) || "").replace(/\/+$/, "");
    }

    function withReadRequest(sPath, mParams, mHeaders) {
        return toRequestHandle(function (resolve, reject) {
            return ensureModel().read(assertCanonicalPath(normalizePath(sPath)), {
                urlParameters: mParams || {},
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function withDirectPostRequest(sPath, oPayload, mHeaders) {
        return toRequestHandle(function (resolve, reject) {
            return ensureModel().create(sPath, oPayload || {}, {
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function withDirectDeleteRequest(sPath, mHeaders) {
        return toRequestHandle(function (resolve, reject) {
            return ensureModel().remove(sPath, {
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function withDirectCreateEntityRequest(sPath, oPayload, mParameters, mHeaders) {
        return toRequestHandle(function (resolve, reject) {
            var oOptions = Object.assign({}, mParameters || {}, {
                headers: Object.assign({}, (mParameters && mParameters.headers) || {}, mHeaders || {}),
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
            return ensureModel().create(sPath, oPayload || {}, oOptions);
        });
    }

    function withDirectFunctionImportRequest(sName, oPayload, mHeaders) {
        var sFunctionName = assertAllowedFunctionName(sName);
        if (allowlisted(sFunctionName, DIRECT_FUNCTION_QUERY_ALLOWLIST)) {
            return toRequestHandle(function (resolve, reject) {
                return ensureModel().callFunction("/" + sFunctionName, {
                    method: "POST",
                    urlParameters: oPayload || {},
                    headers: mHeaders || {},
                    success: function (oData) { resolve(oData || {}); },
                    error: function (e) { reject(e); }
                });
            });
        }
        if (allowlisted(sFunctionName, DIRECT_FUNCTION_BODY_ALLOWLIST)) {
            return toRequestHandle(function (resolve, reject) {
                return ensureModel().create("/" + sFunctionName, oPayload || {}, {
                    headers: mHeaders || {},
                    success: function (oData) { resolve(oData || {}); },
                    error: function (e) { reject(e); }
                });
            });
        }
        throw new Error("Unsupported function import: " + sFunctionName);
    }

    function withDirectGetFunctionImportRequest(sName, mParams, mHeaders) {
        var sFunctionName = assertAllowedFunctionName(sName);
        if (!allowlisted(sFunctionName, DIRECT_GET_FUNCTION_ALLOWLIST)) {
            throw new Error("Unsupported GET function import: " + sFunctionName);
        }
        return toRequestHandle(function (resolve, reject) {
            return ensureModel().callFunction("/" + sFunctionName, {
                method: "GET",
                urlParameters: mParams || {},
                headers: mHeaders || {},
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(e); }
            });
        });
    }

    function withDirectPutRequest(sPath, vPayload, mOptions, mHeaders) {
        var oOptions = mOptions || {};
        var oModel = ensureModel();
        var oXhr = null;
        var bAborted = false;
        var sCsrfCheck = oModel.getSecurityToken ? String(oModel.getSecurityToken() || "").trim() : "";
        var pToken = sCsrfCheck ? Promise.resolve(sCsrfCheck) : new Promise(function (resolve) {
            oModel.refreshSecurityToken(function () {
                resolve(oModel.getSecurityToken ? String(oModel.getSecurityToken() || "").trim() : "");
            }, function () { resolve(""); }, true);
        });
        var pPromise = pToken.then(function (sCsrfToken) {
            var sBase = serviceUrl();
            var sFullUrl = sBase + sPath;
            var mModelHeaders = Object.assign({}, oModel.getHeaders ? oModel.getHeaders() : {});
            var mResolvedHeaders;

            delete mModelHeaders["content-type"];
            delete mModelHeaders["Content-Type"];
            mResolvedHeaders = Object.assign({
                "Accept": "application/json",
                "DataServiceVersion": "2.0",
                "MaxDataServiceVersion": "2.0",
                "Content-Type": oOptions.contentType || "application/octet-stream"
            }, mModelHeaders, mHeaders || {});
            if (sCsrfToken) {
                mResolvedHeaders["X-CSRF-Token"] = sCsrfToken;
            }

            return new Promise(function (resolve, reject) {
                if (bAborted) {
                    reject({ statusCode: 0, message: "Request aborted" });
                    return;
                }
                oXhr = new XMLHttpRequest();
                oXhr.open("PUT", sFullUrl, true);
                Object.keys(mResolvedHeaders).forEach(function (sKey) {
                    oXhr.setRequestHeader(sKey, mResolvedHeaders[sKey]);
                });
                oXhr.onreadystatechange = function () {
                    if (oXhr.readyState !== 4) {
                        return;
                    }
                    if (oXhr.status >= 200 && oXhr.status < 300) {
                        resolve({});
                        return;
                    }
                    reject({
                        statusCode: oXhr.status,
                        responseText: oXhr.responseText,
                        responseHeaders: oXhr.getAllResponseHeaders()
                    });
                };
                oXhr.onerror = function () {
                    reject({ statusCode: 0, message: "Network error during binary PUT" });
                };
                oXhr.send(vPayload || null);
            });
        });

        return {
            promise: pPromise,
            abort: function () {
                bAborted = true;
                if (oXhr) {
                    try {
                        oXhr.abort();
                    } catch (_e) {
                        return;
                    }
                }
            }
        };
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
            var oOptions = mOptions || {};
            return executeRequest({
                method: "GET",
                dedupeKey: "GET|" + sPath + "|" + encodeUrlParameters(mParams || {}),
                responseGuardKey: oOptions.responseGuardKey,
                timeoutMs: oOptions.timeoutMs,
                retryCount: oOptions.retryCount,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withReadRequest(sPath, mParams || {}, buildHeaders(oOptions.headers, sCorrelationId));
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
                    return withDirectFunctionImportRequest(name, oPayload || {}, buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        callGetFunctionImport: function (name, mParams, mOptions) {
            var oOptions = mOptions || {};
            return executeRequest({
                method: "GET_FUNCTION",
                dedupeKey: "GET_FUNCTION|" + String(name || "") + "|" + encodeUrlParameters(mParams || {}),
                responseGuardKey: oOptions.responseGuardKey,
                timeoutMs: oOptions.timeoutMs,
                retryCount: oOptions.retryCount,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectGetFunctionImportRequest(name, mParams || {}, buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        postToPath: function (path, oPayload, mOptions) {
            var sPath = assertAllowedPath(assertCanonicalPath(normalizePath(path)), DIRECT_POST_ALLOWLIST, "POST");
            var oOptions = mOptions || {};
            return executeRequest({
                method: "POST_ENTITY",
                timeoutMs: oOptions.timeoutMs,
                retryCount: 0,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectPostRequest(sPath, oPayload || {}, buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        createEntity: function (path, oPayload, mParameters) {
            var sPath = assertAllowedPath(assertCanonicalPath(normalizePath(path)), DIRECT_CREATE_ALLOWLIST, "CREATE");
            var oOptions = mParameters || {};
            return executeRequest({
                method: "CREATE",
                timeoutMs: oOptions.timeoutMs,
                retryCount: 0,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectCreateEntityRequest(sPath, oPayload || {}, oOptions || {}, buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        deletePath: function (path, mOptions) {
            var sPath = assertAllowedPath(assertCanonicalPath(normalizePath(path)), DIRECT_DELETE_ALLOWLIST, "DELETE");
            var oOptions = mOptions || {};
            return executeRequest({
                method: "DELETE",
                timeoutMs: oOptions.timeoutMs,
                retryCount: 0,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectDeleteRequest(sPath, buildHeaders(oOptions.headers, sCorrelationId));
                }
            });
        },
        putPath: function (path, vPayload, mOptions) {
            var sPath = assertAllowedPath(assertCanonicalPath(normalizePath(path)), DIRECT_PUT_ALLOWLIST, "PUT");
            var oOptions = mOptions || {};
            return executeRequest({
                method: "PUT",
                timeoutMs: oOptions.timeoutMs,
                retryCount: 0,
                correlationId: oOptions.correlationId,
                requestFactory: function (mRuntime) {
                    var sCorrelationId = mRuntime && mRuntime.correlationId;
                    return withDirectPutRequest(sPath, vPayload, oOptions || {}, buildHeaders(oOptions.headers, sCorrelationId));
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
