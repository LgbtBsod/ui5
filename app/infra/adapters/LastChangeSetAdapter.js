sap.ui.define([
    "sap_ui5/infra/odata/GatewayODataClient"
], function (GatewayODataClient) {
    "use strict";

    function parseMs(v) {
        var s = String(v || "");
        if (/^\/Date\(/.test(s)) {
            return Number(s.slice(6).split(")/")[0].split("+")[0].split("-")[0]) || 0;
        }
        var n = Date.parse(s);
        return Number.isFinite(n) ? n : 0;
    }

    function escapeKey(sValue) {
        return String(sValue || "").replace(/'/g, "''");
    }

    function firstResult(oRes) {
        if (!oRes || typeof oRes !== "object") {
            return null;
        }
        if (oRes.AggChangedOn) {
            return oRes;
        }
        if (Array.isArray(oRes.results) && oRes.results.length) {
            return oRes.results[0];
        }
        if (oRes.d && Array.isArray(oRes.d.results) && oRes.d.results.length) {
            return oRes.d.results[0];
        }
        return null;
    }

    function readAggChangedOn(vRes) {
        var oEntry = firstResult(vRes);
        return parseMs(oEntry && oEntry.AggChangedOn);
    }

    function create() {
        return {
            readAggChangedOn: function (sRootId) {
                var sRootKey = escapeKey(sRootId);
                return GatewayODataClient.request({
                    method: "GET",
                    path: "LastChangeSet('" + sRootKey + "')"
                }).then(function (oRes) {
                    return readAggChangedOn(oRes);
                }).catch(function () {
                    return GatewayODataClient.request({
                        method: "GET",
                        path: "LastChangeSet",
                        params: {
                            "$filter": "RootKey eq '" + sRootKey + "'",
                            "$top": 1
                        }
                    }).then(function (oRes) {
                        return readAggChangedOn(oRes);
                    }).catch(function () {
                        return 0;
                    });
                });
            }
        };
    }

    return { create: create };
});
