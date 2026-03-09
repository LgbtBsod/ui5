sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayClient) {
    "use strict";

    function _asPath(sPath) {
        var s = String(sPath || "");
        return s.charAt(0) === "/" ? s : "/" + s;
    }

    function _toPromiseRead(sPath, mParams) {
        return GatewayClient.rawRead(_asPath(sPath), mParams || {});
    }

    return {
        request: function (mOptions) {
            var m = mOptions || {};
            var sMethod = String(m.method || "GET").toUpperCase();
            var sFunctionName = _asPath(m.path).replace(/^\//, "").split("?")[0];
            var mDispatch = {
                GET: function () {
                    return _toPromiseRead(m.path, m.params || {});
                },
                GET_FUNCTION: function () {
                    return GatewayClient.callGetFunctionImport(sFunctionName, m.params || {});
                },
                POST_ENTITY: function () {
                    return GatewayClient.postToPath(_asPath(m.path), m.body || {});
                },
                POST_FUNCTION: function () {
                    return GatewayClient.callFunctionImport(sFunctionName, m.body || {});
                }
            };
            var fn = mDispatch[sMethod] || mDispatch.POST_FUNCTION;
            return fn();
        },
        fetchCsrfToken: function () {
            return GatewayClient.fetchCsrfToken();
        }
    };
});
