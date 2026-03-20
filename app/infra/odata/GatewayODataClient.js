sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayClient) {
    "use strict";

    function asPath(sPath) {
        var s = String(sPath || "");
        return s.charAt(0) === "/" ? s : "/" + s;
    }

    function asFunctionName(sPath) {
        return asPath(sPath).replace(/^\//, "").split("?")[0];
    }

    function get(sPath, mParams, mOptions) {
        return GatewayClient.rawRead(asPath(sPath), mParams || {}, mOptions || {});
    }

    function getFunction(sPath, mParams, mOptions) {
        return GatewayClient.callGetFunctionImport(asFunctionName(sPath), mParams || {}, mOptions || {});
    }

    function postFunction(sPath, oBody, mOptions) {
        return GatewayClient.callFunctionImport(asFunctionName(sPath), oBody || {}, mOptions || {});
    }

    function remove(sPath, mOptions) {
        return GatewayClient.deletePath(asPath(sPath), mOptions || {});
    }

    return {
        get: get,
        getFunction: getFunction,
        postFunction: postFunction,
        deletePath: remove,
        fetchCsrfToken: function () {
            return GatewayClient.fetchCsrfToken();
        }
    };
});
