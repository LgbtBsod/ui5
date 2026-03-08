sap.ui.define([
    "checklist/app/infra/odata/GatewayODataClient",
    "checklist/app/infra/adapters/shared/ODataAdapterUtils"
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

    function getFunction(sPath, mParams) {
        return request({
            method: "GET_FUNCTION",
            path: sPath,
            params: mParams || {}
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
        getFunction: getFunction,
        postFunction: postFunction,
        request: request,
        unwrap: unwrap
    };
});
