sap.ui.define([], function () {
    "use strict";

    var _oModel = null;

    function _ensureModel() {
        if (!_oModel) {
            throw new Error("OData model is not initialized");
        }
        return _oModel;
    }

    function _toPromise(fnExecutor) {
        return new Promise(function (resolve, reject) {
            fnExecutor(resolve, reject);
        });
    }

    function _normalizePath(sPath) {
        if (!sPath) {
            return "/";
        }
        return sPath.charAt(0) === "/" ? sPath : "/" + sPath;
    }

    function _parseFiResult(oData, sName) {
        if (!oData || !oData.d) {
            return oData || {};
        }
        if (Object.prototype.hasOwnProperty.call(oData.d, sName)) {
            var vPayload = oData.d[sName];
            if (typeof vPayload === "string") {
                try {
                    return JSON.parse(vPayload.replace(/'/g, '"'));
                } catch (e) {
                    return { value: vPayload };
                }
            }
            return vPayload;
        }
        return oData.d;
    }

    function _extractODataError(oError) {
        var oResponse = oError && (oError.response || oError);
        var sBody = oResponse && (oResponse.responseText || oResponse.body || "");
        var iStatus = Number(oResponse && (oResponse.statusCode || oResponse.status)) || 500;
        var oPayload = null;
        try {
            oPayload = sBody ? JSON.parse(sBody) : null;
        } catch (e) {
            oPayload = null;
        }

        var sCode = oPayload && oPayload.error && oPayload.error.code;
        var sMessage = (oPayload && oPayload.error && oPayload.error.message && oPayload.error.message.value)
            || (oError && oError.message)
            || "OData request failed";

        var oNormalized = new Error(sMessage);
        oNormalized.code = sCode || "SYSTEM_ERROR";
        oNormalized.httpStatus = iStatus;
        oNormalized.details = (oPayload && oPayload.error && oPayload.error.innererror && oPayload.error.innererror.details) || [];
        oNormalized.original = oError;
        return oNormalized;
    }

    return {
        setModel: function (oModel) {
            _oModel = oModel || null;
        },

        callFI: function (sName, sMethod, mUrlParameters, oPayload, sGroupId) {
            var oModel = _ensureModel();
            return _toPromise(function (resolve, reject) {
                var mParams = {
                    method: (sMethod || "POST").toUpperCase(),
                    urlParameters: Object.assign({}, mUrlParameters || {}),
                    groupId: sGroupId || "$direct",
                    success: function (oData) {
                        resolve(_parseFiResult(oData, sName));
                    },
                    error: function (oError) {
                        reject(_extractODataError(oError));
                    }
                };
                if (oPayload && typeof oPayload === "object") {
                    Object.keys(oPayload).forEach(function (sKey) {
                        if (!Object.prototype.hasOwnProperty.call(mParams.urlParameters, sKey)) {
                            mParams.urlParameters[sKey] = JSON.stringify(oPayload[sKey]);
                        }
                    });
                }
                oModel.callFunction("/" + sName, mParams);
            });
        },

        readSet: function (sPath, mUrlParameters, sGroupId) {
            var oModel = _ensureModel();
            return _toPromise(function (resolve, reject) {
                oModel.read(_normalizePath(sPath), {
                    urlParameters: mUrlParameters || {},
                    groupId: sGroupId || "$direct",
                    success: function (oData) {
                        resolve((oData && oData.results) || []);
                    },
                    error: function (oError) {
                        reject(_extractODataError(oError));
                    }
                });
            });
        },

        readEntity: function (sPath, mUrlParameters, sGroupId) {
            var oModel = _ensureModel();
            return _toPromise(function (resolve, reject) {
                oModel.read(_normalizePath(sPath), {
                    urlParameters: mUrlParameters || {},
                    groupId: sGroupId || "$direct",
                    success: function (oData, oResponse) {
                        resolve({ data: oData || {}, response: oResponse || {} });
                    },
                    error: function (oError) {
                        reject(_extractODataError(oError));
                    }
                });
            });
        }
    };
});
