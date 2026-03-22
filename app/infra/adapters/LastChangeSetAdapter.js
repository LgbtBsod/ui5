sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, ODataAdapterUtils, ODataKeyContracts, GatewayContractConstants) {
    "use strict";

    function parseMs(v) {
        var s = String(v || "");
        if (/^\/Date\(/.test(s)) {
            return Number(s.slice(6).split(")/")[0].split("+")[0].split("-")[0]) || 0;
        }
        var n = Date.parse(s);
        return Number.isFinite(n) ? n : 0;
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

    function readAggChangedOnAdapter(sRootId) {
        return GatewayODataClient.get(ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.LAST_CHANGE_SET, sRootId, {
            name: "RootKey",
            type: ODataKeyContracts.TYPES.ROOT_KEY
        })).then(function (oRes) {
            return readAggChangedOn(oRes);
        }).catch(function () {
            return GatewayODataClient.get(GatewayContractConstants.ENTITY_SETS.LAST_CHANGE_SET, {
                "$filter": ODataAdapterUtils.buildEqFilter("RootKey", sRootId, ODataKeyContracts.TYPES.ROOT_KEY),
                "$top": 1
            }).then(function (oRes) {
                return readAggChangedOn(oRes);
            }).catch(function () {
                return 0;
            });
        });
    }

    return { readAggChangedOn: readAggChangedOnAdapter };
});
