sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/service/backend/GatewayClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistSnapshotRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/ODataKeyNormalizer"
], function (GatewayClient, ODataChecklistSnapshotRuntime, ODataAdapterUtils, ODataEntityContracts, ChecklistSnapshotMapper, GatewayContractConstants, ODataKeyNormalizer) {
    "use strict";

    /* Chunked detail loading keeps initial open light while checks and barriers load in bounded pages. */
    var DETAIL_ROW_CHUNK_SIZE = 20;

    /* Filter helpers stay centralized on ODataAdapterUtils so string and binary predicates use one typed formatting seam. */
    /* String lookup is limited to human-readable checklist identifiers before canonical DB_KEY resolution. */
    function buildStringEqFilter(sProperty, sValue) {
        return ODataAdapterUtils.buildEqFilter(sProperty, sValue);
    }

    /* Canonical detail reads always filter by typed DB_KEY/PARENT_KEY boundary fields. */
    function buildDetailFilter(oFilterContract, sDbKey) {
        return ODataAdapterUtils.buildEqFilter(oFilterContract.property, ODataKeyNormalizer.normalizeBinaryKey(sDbKey), oFilterContract.type);
    }

    function isCanonicalBinaryKey(sValue) {
        return /^[A-F0-9]{32}$/.test(String(sValue || "").trim().toUpperCase());
    }

    /* Route input is normalized once into the canonical backend DB_KEY before any detail read happens. */
    function resolveChecklistDbKey(mArgs, mDeps) {
        var sRequestedDbKey = String(mDeps.dbKey(mArgs) || "").trim();
        if (!sRequestedDbKey) {
            return Promise.resolve("");
        }
        if (isCanonicalBinaryKey(sRequestedDbKey)) {
            return Promise.resolve(ODataKeyNormalizer.normalizeBinaryKey(sRequestedDbKey));
        }
        return GatewayClient.rawRead("/" + GatewayContractConstants.ENTITY_SETS.CHECKLIST_SEARCH, {
            "$filter": buildStringEqFilter("Id", sRequestedDbKey),
            "$top": 1
        }).then(function (oResponse) {
            var aRows = ODataAdapterUtils.asArray(oResponse);
            var oFirst = aRows[0] || {};
            return ODataKeyNormalizer.normalizeBinaryKey(oFirst.DB_KEY || oFirst.Id || sRequestedDbKey);
        }).catch(function () {
            return ODataKeyNormalizer.normalizeBinaryKey(sRequestedDbKey);
        });
    }

    /* Phase-one snapshot keeps the open path focused on checklist header/basic data and defers heavy child collections. */
    function fetchDetailSnapshot(mArgs, mDeps) {
        var sDbKey = ODataKeyNormalizer.normalizeBinaryKey(mDeps.dbKey(mArgs));
        var bIncludeChildren = !mArgs || mArgs.includeChildren !== false;
        var oBasicFilter = ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BASIC_INFO;
        var pRoot = GatewayClient.rawRead(ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.CHECKLIST_ROOT, sDbKey, {
            name: "DB_KEY",
            type: ODataEntityContracts.TYPES.DB_KEY
        }), {
            "$select": ODataEntityContracts.SELECTS.CHECKLIST_ROOT
        });
        // ChecklistBasicInfoSet is a separate CDS-backed read model.
        // Keep it independent from ChecklistRootSet and read it via its own entity set.
        var pBasic = GatewayClient.rawRead("/" + oBasicFilter.entitySet, {
            "$filter": buildDetailFilter(oBasicFilter, sDbKey),
            "$select": ODataEntityContracts.SELECTS.CHECKLIST_BASIC_INFO
        });
        var pRows = bIncludeChildren ? loadDetailRows({
            dbKey: sDbKey,
            includeChecks: true,
            includeBarriers: true
        }, mDeps) : Promise.resolve({ checks: [], barriers: [] });
        return Promise.all([pRoot, pBasic, pRows]).then(function (aResult) {
            var oRows = aResult[2] || {};
            var oSnapshot = ODataChecklistSnapshotRuntime.mapResult(aResult[0], aResult[1], oRows.checks || [], oRows.barriers || []);
            oSnapshot.attachments = [];
            return oSnapshot;
        });
    }

    /* Child collections are paged to avoid single heavy reads on large checklists. */
    function loadChunkedCollection(oFilterContract, sDbKey, sSelect, fnMapRow) {
        var aRows = [];

        function loadPage(iSkip) {
            return GatewayClient.rawRead("/" + oFilterContract.entitySet, {
                "$filter": buildDetailFilter(oFilterContract, sDbKey),
                "$select": sSelect,
                "$top": DETAIL_ROW_CHUNK_SIZE,
                "$skip": iSkip
            }).then(function (oResponse) {
                var aChunk = ODataAdapterUtils.asArray(oResponse).map(fnMapRow);
                aRows = aRows.concat(aChunk);
                if (aChunk.length < DETAIL_ROW_CHUNK_SIZE) {
                    return aRows;
                }
                return loadPage(iSkip + DETAIL_ROW_CHUNK_SIZE);
            });
        }

        return loadPage(0);
    }

    /* Checks and barriers stay behind one normalized deferred-read owner. */
    function loadDetailRows(mArgs, mDeps) {
        var sDbKey = ODataKeyNormalizer.normalizeBinaryKey(mDeps.dbKey(mArgs));
        var bChecks = !mArgs || mArgs.includeChecks !== false;
        var bBarriers = !mArgs || mArgs.includeBarriers !== false;
        var pChecks = bChecks
            ? loadChunkedCollection(
                ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_CHECK,
                sDbKey,
                ODataEntityContracts.SELECTS.CHECKLIST_CHECK,
                ChecklistSnapshotMapper.mapCheckRow
            )
            : Promise.resolve([]);
        var pBarriers = bBarriers
            ? loadChunkedCollection(
                ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BARRIER,
                sDbKey,
                ODataEntityContracts.SELECTS.CHECKLIST_BARRIER,
                ChecklistSnapshotMapper.mapBarrierRow
            )
            : Promise.resolve([]);

        return Promise.all([pChecks, pBarriers]).then(function (aRows) {
            return {
                checks: aRows[0] || [],
                barriers: aRows[1] || []
            };
        });
    }

    function enrichServerSnapshot(oServerPayload, sFallbackDbKey, mDeps) {
        var sResolvedDbKey = mDeps.resolveServerDbKey(oServerPayload, sFallbackDbKey);
        if (!sResolvedDbKey || mDeps.isCreateId(sResolvedDbKey)) {
            return Promise.resolve(oServerPayload || {});
        }
        return fetchDetailSnapshot({ dbKey: sResolvedDbKey, includeChildren: true }, mDeps).then(function (oSnapshot) {
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
                    id: String(oRoot.id || sResolvedDbKey).trim()
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
        loadDetailRows: loadDetailRows,
        loadDetailSnapshot: fetchDetailSnapshot,
        resolveDbKey: resolveChecklistDbKey
    };
});
