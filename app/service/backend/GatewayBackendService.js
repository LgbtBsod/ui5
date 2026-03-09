sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient"
], function (GatewayClient) {
    "use strict";

    return {
        setModel: function (oModel, mOptions) {
            GatewayClient.setModel(oModel, mOptions || {});
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
        callFunctionImport: function (name, payload) {
            return GatewayClient.callFunctionImport(name, payload || {});
        },
        postToPath: function (path, payload) {
            return GatewayClient.postToPath(path, payload || {});
        },
        createEntity: function (path, payload, mParameters) {
            return GatewayClient.createEntity(path, payload || {}, mParameters || {});
        },
        deletePath: function (path) {
            return GatewayClient.deletePath(path);
        },
        putPath: function (path, payload, mOptions) {
            return GatewayClient.putPath(path, payload, mOptions || {});
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
