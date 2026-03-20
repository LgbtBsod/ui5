sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, ODataAdapterUtils, GatewayContractConstants) {
    "use strict";

    function normalizeNode(oItem) {
        var sNodeId = oItem.NodeID || oItem.NodeId || oItem.node_id || oItem.id || "";
        var sDescription = oItem.Description || oItem.Text || oItem.location_name || oItem.LocationName || "";
        return {
            location_id: sNodeId,
            parent_id: oItem.ParentNodeID || oItem.ParentId || oItem.parent_id || oItem.parentId || "",
            location_name: sDescription,
            location_code: oItem.LocationCode || oItem.location_code || sNodeId,
            drill_state: oItem.DrillState || oItem.drill_state || "leaf",
            children: Array.isArray(oItem.children) ? oItem.children.map(normalizeNode) : []
        };
    }

    function buildTree(aNodes) {
        var mById = {};
        var aRoots = [];
        (aNodes || []).forEach(function (oNode) {
            var oNorm = normalizeNode(oNode);
            oNorm.children = [];
            mById[oNorm.location_id] = oNorm;
        });
        Object.keys(mById).forEach(function (sId) {
            var oNode = mById[sId];
            var oParent = oNode.parent_id ? mById[oNode.parent_id] : null;
            if (oParent) {
                oParent.children.push(oNode);
            } else {
                aRoots.push(oNode);
            }
        });
        return aRoots;
    }

    function flattenTree(aNodes, aOut) {
        (aNodes || []).forEach(function (oNode) {
            var oNorm = normalizeNode(oNode);
            aOut.push(oNorm);
            flattenTree(oNorm.children, aOut);
        });
        return aOut;
    }

    function search(mArgs) {
        var sQuery = String((mArgs && mArgs.query) || "").toLowerCase();
        var iLimit = Number((mArgs && mArgs.limit) || 20);
        var sDateCheck = String((mArgs && mArgs.dateCheck) || "").trim();
        if (!sDateCheck) {
            sDateCheck = new Date().toISOString().slice(0, 10);
        }

        return GatewayODataClient.getFunction(GatewayContractConstants.FUNCTION_IMPORTS.GET_HIERARCHY, { Method: "location_tree", DateCheck: sDateCheck }).then(function (oData) {
            var oPayload = ODataAdapterUtils.unwrap(oData);
            var aNodes = Array.isArray(oPayload)
                ? oPayload
                : ((oPayload && Array.isArray(oPayload.results)) ? oPayload.results : []);
            var aTree = buildTree(aNodes);
            if (!sQuery) {
                return { items: aTree.slice(0, iLimit) };
            }
            var aFlat = flattenTree(aTree, []);
            var aFiltered = aFlat.filter(function (oItem) {
                if (!sQuery) { return true; }
                return String(oItem.location_name || "").toLowerCase().indexOf(sQuery) >= 0
                    || String(oItem.location_code || "").toLowerCase().indexOf(sQuery) >= 0
                    || String(oItem.location_id || "").toLowerCase().indexOf(sQuery) >= 0;
            }).map(function (oItem) {
                return Object.assign({}, oItem, { children: [] });
            });
            return { items: aFiltered.slice(0, iLimit) };
        });
    }

    return { search: search };
});
