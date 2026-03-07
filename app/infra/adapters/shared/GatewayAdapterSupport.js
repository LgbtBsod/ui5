sap.ui.define([
    "sap_ui5/infra/odata/GatewayODataClient",
    "sap_ui5/infra/adapters/shared/ODataAdapterUtils"
], function (GatewayODataClient, ODataAdapterUtils) {
    "use strict";

    function request(mRequest) {
        return GatewayODataClient.request(mRequest);
    }

    function get(sPath, mParams) {
        return request({
            method: "GET",
            path: sPath,
            params: mParams || {}
        });
    }

    function postFunction(sPath, oBody) {
        return request({
            method: "POST_FUNCTION",
            path: sPath,
            body: oBody || {}
        });
    }

    function asArray(vData) {
        return ODataAdapterUtils.asArray(vData);
    }

    function unwrap(vData) {
        return ODataAdapterUtils.unwrap(vData);
    }

    return {
        asArray: asArray,
        get: get,
        postFunction: postFunction,
        request: request,
        unwrap: unwrap
    };
});
