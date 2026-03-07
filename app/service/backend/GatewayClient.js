sap.ui.define([
    "checklist/app/service/backend/GatewayErrorNormalizer"
], function (GatewayErrorNormalizer) {
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
        /^CopyChecklist$/i
    ];
    var DIRECT_FUNCTION_BODY_ALLOWLIST = [
        /^SetChecklistStatus$/i
    ];
    var DIRECT_CREATE_ALLOWLIST = [
        /^\/AttachmentSet(?:$|[?(])/i
    ];
    var DIRECT_PUT_ALLOWLIST = [
        /^\/AttachmentSet\(Key='[^']+'\)\/\$value$/i
    ];
    var DIRECT_DELETE_ALLOWLIST = [
        /^\/ChecklistRootSet\('[^']+'\)$/i,
        /^\/AttachmentSet\(Key='[^']+'\)$/i
    ];
    function ensureModel() { if (!_oModel) { throw new Error("GatewayClient model is not initialized"); } return _oModel; }
    function toPromise(fnExecutor) { return new Promise(function (resolve, reject) { fnExecutor(resolve, reject); }); }
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

    function withRead(sPath, params) {
        return toPromise(function (resolve, reject) {
            ensureModel().read(assertCanonicalPath(normalizePath(sPath)), { urlParameters: params || {}, success: function (oData) { resolve(oData || {}); }, error: function (e) { reject(GatewayErrorNormalizer.normalizeError(e)); } });
        });
    }

    function serviceUrl() {
        var oModel = ensureModel();
        if (_sServiceUrl) {
            return _sServiceUrl;
        }
        return String((oModel && oModel.sServiceUrl) || "").replace(/\/+$/, "");
    }

    function withDirectPost(sPath, oPayload) {
        return toPromise(function (resolve, reject) {
            ensureModel().create(sPath, oPayload || {}, {
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(GatewayErrorNormalizer.normalizeError(e)); }
            });
        });
    }
    function withDirectDelete(sPath) {
        return toPromise(function (resolve, reject) {
            ensureModel().remove(sPath, {
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(GatewayErrorNormalizer.normalizeError(e)); }
            });
        });
    }
    function withDirectCreateEntity(sPath, oPayload, mParameters) {
        return toPromise(function (resolve, reject) {
            var oOptions = Object.assign({
                success: function (oData) { resolve(oData || {}); },
                error: function (e) { reject(GatewayErrorNormalizer.normalizeError(e)); }
            }, mParameters || {});
            ensureModel().create(sPath, oPayload || {}, oOptions);
        });
    }
    function withDirectFunctionImport(sName, oPayload) {
        var sFunctionName = assertAllowedFunctionName(sName);
        if (allowlisted(sFunctionName, DIRECT_FUNCTION_QUERY_ALLOWLIST)) {
            return toPromise(function (resolve, reject) {
                ensureModel().callFunction("/" + sFunctionName, {
                    method: "POST",
                    urlParameters: oPayload || {},
                    success: function (oData) { resolve(oData || {}); },
                    error: function (e) { reject(GatewayErrorNormalizer.normalizeError(e)); }
                });
            });
        }
        if (allowlisted(sFunctionName, DIRECT_FUNCTION_BODY_ALLOWLIST)) {
            return toPromise(function (resolve, reject) {
                ensureModel().create("/" + sFunctionName, oPayload || {}, {
                    success: function (oData) { resolve(oData || {}); },
                    error: function (e) { reject(GatewayErrorNormalizer.normalizeError(e)); }
                });
            });
        }
        throw new Error("Unsupported function import: " + sFunctionName);
    }

    function withDirectPut(sPath, vPayload, mOptions) {
        var oOptions = mOptions || {};
        return toPromise(function (resolve, reject) {
            var oModel = ensureModel();
            var xhr = new XMLHttpRequest();
            var sBaseUrl = serviceUrl();
            var sResolvedUrl = sBaseUrl + sPath;
            var mModelHeaders = Object.assign({}, oModel.getHeaders ? oModel.getHeaders() : {});
            var sCsrfToken = oModel.getSecurityToken ? String(oModel.getSecurityToken() || "").trim() : "";
            var mHeaders;

            delete mModelHeaders["content-type"];
            delete mModelHeaders["Content-Type"];
            mHeaders = Object.assign({
                "Accept": "application/json",
                "DataServiceVersion": "2.0",
                "MaxDataServiceVersion": "2.0",
                "Content-Type": oOptions.contentType || "application/octet-stream"
            }, mModelHeaders, oOptions.headers || {});
            if (sCsrfToken) {
                mHeaders["X-CSRF-Token"] = sCsrfToken;
            }

            if (typeof window !== "undefined" && /^\/(?!\/)/.test(sResolvedUrl)) {
                sResolvedUrl = window.location.origin + sResolvedUrl;
            }

            xhr.open("PUT", sResolvedUrl, true);
            xhr.withCredentials = true;
            Object.keys(mHeaders).forEach(function (sName) {
                var vValue = mHeaders[sName];
                if (vValue === undefined || vValue === null || vValue === "") {
                    return;
                }
                xhr.setRequestHeader(sName, String(vValue));
            });
            xhr.onload = function () {
                if (xhr.status >= 200 && xhr.status < 300) {
                    resolve({});
                    return;
                }
                reject(GatewayErrorNormalizer.normalizeError({
                    status: xhr.status,
                    statusCode: xhr.status,
                    message: xhr.statusText || "Gateway stream upload failed",
                    responseText: xhr.responseText || "",
                    responseHeaders: xhr.getAllResponseHeaders ? xhr.getAllResponseHeaders() : ""
                }));
            };
            xhr.onerror = function () {
                reject(GatewayErrorNormalizer.normalizeError({
                    status: xhr.status || 0,
                    statusCode: xhr.status || 0,
                    message: xhr.statusText || "Gateway stream upload failed",
                    responseText: xhr.responseText || "",
                    responseHeaders: xhr.getAllResponseHeaders ? xhr.getAllResponseHeaders() : ""
                }));
            };
            xhr.send(vPayload || null);
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
        readEntity: function (entitySet, key, params) { return withRead("/" + entitySet + "(" + key + ")", params); },
        rawRead: function (path, params) { return withRead(normalizePath(path), params); },
        readSet: function (entitySet, params) { return withRead("/" + entitySet, params).then(function (oData) { return (oData && oData.results) || []; }); },
        serviceUrl: serviceUrl,
        callFunctionImport: function (name, payload) {
            return withDirectFunctionImport(name, payload || {});
        },
        postToPath: function (path, payload) {
            var sNormalized = assertAllowedPath(assertCanonicalPath(normalizePath(path)), DIRECT_POST_ALLOWLIST, "POST");
            return withDirectPost(sNormalized, payload || {});
        },
        createEntity: function (path, payload, mParameters) {
            var sNormalized = assertAllowedPath(assertCanonicalPath(normalizePath(path)), DIRECT_CREATE_ALLOWLIST, "CREATE");
            return withDirectCreateEntity(sNormalized, payload || {}, mParameters || {});
        },
        deletePath: function (path) {
            var sNormalized = assertAllowedPath(assertCanonicalPath(normalizePath(path)), DIRECT_DELETE_ALLOWLIST, "DELETE");
            return withDirectDelete(sNormalized);
        },
        putPath: function (path, payload, mOptions) {
            var sNormalized = assertAllowedPath(assertCanonicalPath(normalizePath(path)), DIRECT_PUT_ALLOWLIST, "PUT");
            return withDirectPut(sNormalized, payload, mOptions || {});
        },
        batch: function (groupId) {
            return toPromise(function (resolve, reject) {
                ensureModel().submitChanges({ groupId: groupId || undefined, success: function (oData) { resolve((oData && (oData.__batchResponses || oData.__changeResponses)) || []); }, error: function (e) { reject(GatewayErrorNormalizer.normalizeError(e)); } });
            });
        },
        fetchCsrfToken: function () {
            return toPromise(function (resolve, reject) { ensureModel().refreshSecurityToken(function () { resolve(true); }, function (e) { reject(GatewayErrorNormalizer.normalizeError(e)); }, true); });
        },
        refreshSecurityToken: function () { return this.fetchCsrfToken(); },
        normalizeError: GatewayErrorNormalizer.normalizeError,
        normalizeODataError: GatewayErrorNormalizer.normalizeODataError
    };
});
