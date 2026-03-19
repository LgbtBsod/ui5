sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayClient) {
    "use strict";

    return {
        setModel: function (oModel, mOptions) {
            GatewayClient.setModel(oModel, mOptions || {});
        },
        reset: function () {
            GatewayClient.reset();
        },
        serviceUrl: function () {
            return GatewayClient.serviceUrl();
        },
        setHeader: function (sName, sValue) {
            return GatewayClient.setHeader(sName, sValue);
        },
        readEntity: function (entitySet, key, params) {
            return GatewayClient.readEntity(entitySet, key, params || {});
        },
        rawRead: function (path, params) {
            return GatewayClient.rawRead(path, params || {});
        },
        readSet: function (entitySet, params) {
            return GatewayClient.readSet(entitySet, params || {});
        },
        callFunctionImport: function (name, payload, mOptions) {
            return GatewayClient.callFunctionImport(name, payload || {}, mOptions || {});
        },
        postToPath: function (path, payload) {
            return GatewayClient.postToPath(path, payload || {});
        },
        deletePath: function (path) {
            return GatewayClient.deletePath(path);
        },
        fetchCsrfToken: function () {
            return GatewayClient.fetchCsrfToken();
        },
        refreshSecurityToken: function () {
            return GatewayClient.refreshSecurityToken();
        },
        normalizeError: GatewayClient.normalizeError,
        normalizeODataError: GatewayClient.normalizeODataError
    };
});
