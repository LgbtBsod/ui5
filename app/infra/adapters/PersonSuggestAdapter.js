sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, ODataAdapterUtils, GatewayContractConstants) {
    "use strict";

    function joinNameParts(aParts) {
        return (aParts || []).map(function (sPart) {
            return String(sPart || "").trim();
        }).filter(Boolean).join(" ");
    }

    function normalizePerson(oItem) {
        var sFullName = joinNameParts([
            oItem && (oItem.LastName || oItem.lastName),
            oItem && (oItem.FirstName || oItem.firstName),
            oItem && (oItem.MiddleName || oItem.middleName)
        ]);
        return {
            perner: (oItem && (oItem.Pernr || oItem.perner || oItem.PERNR)) || "",
            fullName: sFullName || ((oItem && (oItem.FullName || oItem.fullName || oItem.Name || "")) || ""),
            position: (oItem && (oItem.Position || oItem.position || "")) || "",
            orgUnit: (oItem && (oItem.OrgUnit || oItem.orgUnit || "")) || ""
        };
    }

    function suggest(mArgs) {
        var sQuery = String((mArgs && mArgs.query) || "").toLowerCase();
        var iLimit = Number((mArgs && mArgs.limit) || 10);
        var sDateCheck = String((mArgs && mArgs.dateCheck) || "").trim();
        var mParams = {
            "$top": Math.max(iLimit * 4, 40),
            Search: sQuery
        };

        if (sDateCheck) {
            mParams.DateCheck = sDateCheck;
        }

        return GatewayODataClient.get(GatewayContractConstants.ENTITY_SETS.PERSON_VALUE_HELP, mParams).then(function (oData) {
            var aItems = ODataAdapterUtils.asArray(oData).map(normalizePerson).filter(function (oPerson) {
                if (!sQuery) { return true; }
                return String(oPerson.fullName || "").toLowerCase().indexOf(sQuery) >= 0
                    || String(oPerson.perner || "").toLowerCase().indexOf(sQuery) >= 0;
            });
            return { items: aItems.slice(0, iLimit) };
        });
    }

    return { suggest: suggest };
});
