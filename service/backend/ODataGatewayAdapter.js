sap.ui.define([
    "sap_ui5/service/backend/GatewayClient"
], function (GatewayClient) {
    "use strict";

    function _pathToEntity(path) {
        var s = String(path || "").replace(/^\//, "");
        return s;
    }

    function _normalizeFiPayload(name, payload) {
        var oPayload = payload || {};
        if (oPayload && oPayload.d && Object.prototype.hasOwnProperty.call(oPayload.d, name)) {
            return oPayload.d[name];
        }
        return (oPayload && oPayload.d) || oPayload || {};
    }

    return {
        setModel: GatewayClient.setModel,

        callFI: function (sName, _sMethod, mUrlParameters, oPayload) {
            var mParams = Object.assign({}, mUrlParameters || {}, oPayload || {});
            return GatewayClient.callFunctionImport(sName, mParams).then(function (oData) {
                return _normalizeFiPayload(sName, oData);
            });
        },

        readSet: function (sPath, mUrlParameters) {
            return GatewayClient.readSet(_pathToEntity(sPath), mUrlParameters || {});
        },

        readEntity: function (sPath, mUrlParameters) {
            var sEntity = _pathToEntity(sPath);
            if (sEntity.indexOf("(") > -1) {
                var iStart = sEntity.indexOf("(");
                var iEnd = sEntity.lastIndexOf(")");
                return GatewayClient.readEntity(sEntity.slice(0, iStart), sEntity.slice(iStart + 1, iEnd), mUrlParameters || {}).then(function (oData) {
                    return { data: oData || {}, response: {} };
                });
            }
            return GatewayClient.readEntity(sEntity, "", mUrlParameters || {}).then(function (oData) {
                return { data: oData || {}, response: {} };
            });
        }
    };
});
