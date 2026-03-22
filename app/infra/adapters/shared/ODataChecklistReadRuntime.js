sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistSnapshotRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, ODataChecklistSnapshotRuntime, ODataAdapterUtils, ODataEntityContracts, ChecklistSnapshotMapper, GatewayContractConstants) {
    "use strict";

    /*
     * AB-01 FIX: Standardized filter helpers.
     *
     * buildStringEqFilter  → Edm.String  (for text fields: Id, RootId lookup by human key)
     * buildBinaryEqFilter  → Edm.Binary  (for binary UUID fields: RootKey)
     *
     * Both delegate to ODataAdapterUtils.buildEqFilter which calls
     * sap.ui.model.odata.ODataUtils.formatValue internally.
     * Verify accepted format via st05 trace on target BASIS before changing type.
     */
    function buildStringEqFilter(sProperty, sRootId) {
        return ODataAdapterUtils.buildEqFilter(sProperty, sRootId);
    }

    function buildDetailFilter(oFilterContract, sRootId) {
        return ODataAdapterUtils.buildEqFilter(oFilterContract.property, sRootId, oFilterContract.type);
    }

    function resolveRootId(mArgs, mDeps) {
        var sRequestedId = String(mDeps.rootId(mArgs) || "").trim();
        if (!sRequestedId) {
            return Promise.resolve("");
        }
        return GatewayODataClient.get(GatewayContractConstants.ENTITY_SETS.CHECKLIST_SEARCH, {
            "$filter": buildStringEqFilter("Id", sRequestedId),
            "$top": 1
        }).then(function (oResponse) {
            var aRows = ODataAdapterUtils.asArray(oResponse);
            var oFirst = aRows[0] || {};
            return String(oFirst.Key || oFirst.RootKey || oFirst.Id || sRequestedId).trim();
        }).catch(function () {
            return sRequestedId;
        });
    }

    function fetchDetailSnapshot(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var oBasicFilter = ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BASIC_INFO;
        var oCheckFilter = ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_CHECK;
        var oBarrierFilter = ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BARRIER;
        var pRoot = GatewayODataClient.get(ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.CHECKLIST_ROOT, sRootId, {
            type: ODataEntityContracts.TYPES.ROOT_KEY
        }).replace(/^\//, ""));
        // ChecklistBasicInfoSet is a separate CDS-backed read model.
        // Keep it independent from ChecklistRootSet and read it via its own entity set.
        var pBasic = GatewayODataClient.get(oBasicFilter.entitySet, {
            "$filter": buildDetailFilter(oBasicFilter, sRootId),
            "$select": ODataEntityContracts.SELECTS.CHECKLIST_BASIC_INFO
        });
        var pChecks = GatewayODataClient.get(oCheckFilter.entitySet, {
            "$filter": buildDetailFilter(oCheckFilter, sRootId),
            "$select": ODataEntityContracts.SELECTS.CHECKLIST_CHECK
        });
        var pBarriers = GatewayODataClient.get(oBarrierFilter.entitySet, {
            "$filter": buildDetailFilter(oBarrierFilter, sRootId),
            "$select": ODataEntityContracts.SELECTS.CHECKLIST_BARRIER
        });
        return Promise.all([pRoot, pBasic, pChecks, pBarriers]).then(function (aResult) {
            var oSnapshot = ODataChecklistSnapshotRuntime.mapResult(aResult[0], aResult[1], aResult[2], aResult[3]);
            oSnapshot.attachments = [];
            return oSnapshot;
        });
    }

    function enrichServerSnapshot(oServerPayload, sFallbackRootId, mDeps) {
        var sResolvedRootId = mDeps.resolveServerRootId(oServerPayload, sFallbackRootId);
        if (!sResolvedRootId || mDeps.isCreateId(sResolvedRootId)) {
            return Promise.resolve(oServerPayload || {});
        }
        return fetchDetailSnapshot({ rootId: sResolvedRootId }, mDeps).then(function (oSnapshot) {
            var oResolvedSnapshot = oSnapshot || {};
            var oMeta = Object.assign({}, oResolvedSnapshot.meta || {});
            var oRoot = Object.assign({}, oResolvedSnapshot.root || {});
            var sAggChangedOn = (oServerPayload && (oServerPayload.AggChangedOn || oServerPayload.ChangedOn || oServerPayload.changed_on)) || oMeta.aggChangedOn || oRoot.server_changed_on || "";
            var iVersionNumber = Number((oServerPayload && (oServerPayload.version_number || oServerPayload.VersionNumber)) || oRoot.version_number || oMeta.versionNumber || 0) || 0;
            if (sAggChangedOn) {
                oMeta.aggChangedOn = sAggChangedOn;
                oRoot.server_changed_on = oRoot.server_changed_on || sAggChangedOn;
            }
            if (iVersionNumber) {
                oMeta.versionNumber = iVersionNumber;
                oRoot.version_number = iVersionNumber;
                oRoot.VersionNumber = iVersionNumber;
            }
            return Object.assign({}, oResolvedSnapshot, {
                root: Object.assign({}, oRoot, {
                    id: String(oRoot.id || sResolvedRootId).trim()
                }),
                meta: oMeta
            });
        }).catch(function () {
            return oServerPayload || {};
        });
    }

    return {
        enrichServerSnapshot: enrichServerSnapshot,
        buildDetailFilter: buildDetailFilter,
        loadDetailSnapshot: fetchDetailSnapshot,
        resolveRootId: resolveRootId
    };
});
