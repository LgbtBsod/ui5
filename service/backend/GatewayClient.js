sap.ui.define([], function () {
    "use strict";

    var _oModel = null;

    function _ensureModel() {
        if (!_oModel) {
            throw new Error("GatewayClient model is not initialized");
        }
        return _oModel;
    }

    function _toPromise(fnExecutor) {
        return new Promise(function (resolve, reject) {
            fnExecutor(resolve, reject);
        });
    }

    function _normalizeODataError(oError) {
        var oResponse = oError && (oError.response || oError);
        var sBody = oResponse && (oResponse.responseText || oResponse.body || "");
        var oPayload = null;
        try { oPayload = sBody ? JSON.parse(sBody) : null; } catch (e) { oPayload = null; }
        return {
            code: (oPayload && oPayload.error && oPayload.error.code) || "SYSTEM_ERROR",
            message: (oPayload && oPayload.error && oPayload.error.message && oPayload.error.message.value) || (oError && oError.message) || "OData request failed",
            details: (oPayload && oPayload.error && oPayload.error.innererror && oPayload.error.innererror.details) || []
        };
    }

    return {
        setModel: function (oModel) { _oModel = oModel || null; },

        readEntity: function (entitySet, key, params) {
            var oModel = _ensureModel();
            var sPath = "/" + entitySet + "(" + key + ")";
            return _toPromise(function (resolve, reject) {
                oModel.read(sPath, {
                    urlParameters: params || {},
                    success: function (oData) { resolve(oData || {}); },
                    error: function (e) { reject(_normalizeODataError(e)); }
                });
            });
        },

        readSet: function (entitySet, params) {
            var oModel = _ensureModel();
            return _toPromise(function (resolve, reject) {
                oModel.read("/" + entitySet, {
                    urlParameters: params || {},
                    success: function (oData) { resolve((oData && oData.results) || []); },
                    error: function (e) { reject(_normalizeODataError(e)); }
                });
            });
        },

        callFunctionImport: function (name, payload) {
            var oModel = _ensureModel();
            return _toPromise(function (resolve, reject) {
                oModel.callFunction("/" + name, {
                    method: "POST",
                    urlParameters: payload || {},
                    success: function (oData) { resolve(oData || {}); },
                    error: function (e) { reject(_normalizeODataError(e)); }
                });
            });
        },

        batch: function () { return Promise.resolve([]); },
        fetchCsrfToken: function () { return Promise.resolve(true); },
        normalizeODataError: _normalizeODataError
    };
});
