sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/odata/GatewayODataClient",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistSnapshotRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataEntityContracts",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/constants/GatewayContractConstants"
], function (GatewayODataClient, ODataChecklistSnapshotRuntime, ODataAdapterUtils, ODataEntityContracts, ChecklistSnapshotMapper, GatewayContractConstants) {
    "use strict";

    /* Этот блок задает размер чанка для догрузки строк detail-разделов.
     * Результат: проверки и барьеры приходят порциями, а initial open остается легким. */
    var DETAIL_ROW_CHUNK_SIZE = 20;

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
    /* Этот блок собирает строковый eq-filter для lookup по человекочитаемому идентификатору.
     * Результат: запрос к search/read-модели формируется в одном стандартизованном виде. */
    function buildStringEqFilter(sProperty, sRootId) {
        return ODataAdapterUtils.buildEqFilter(sProperty, sRootId);
    }

    /* Этот блок собирает канонический фильтр для detail entity set.
     * Результат: все children/read запросы используют один и тот же boundary-контракт. */
    function buildDetailFilter(oFilterContract, sRootId) {
        return ODataAdapterUtils.buildEqFilter(oFilterContract.property, sRootId, oFilterContract.type);
    }

    /* Этот блок разрешает входной route/id в реальный backend root key.
     * Результат: downstream detail flow работает с техническим ключом, а не с display-id. */
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

    /* Этот блок загружает phase-1 snapshot карточки.
     * Результат: на initial open приходят root + basic, а heavy rows остаются отложенными. */
    function fetchDetailSnapshot(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var bIncludeChildren = !mArgs || mArgs.includeChildren !== false;
        var oBasicFilter = ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BASIC_INFO;
        var pRoot = GatewayODataClient.get(ODataAdapterUtils.buildEntityPath(GatewayContractConstants.ENTITY_SETS.CHECKLIST_ROOT, sRootId, {
            type: ODataEntityContracts.TYPES.ROOT_KEY
        }).replace(/^\//, ""));
        // ChecklistBasicInfoSet is a separate CDS-backed read model.
        // Keep it independent from ChecklistRootSet and read it via its own entity set.
        var pBasic = GatewayODataClient.get(oBasicFilter.entitySet, {
            "$filter": buildDetailFilter(oBasicFilter, sRootId),
            "$select": ODataEntityContracts.SELECTS.CHECKLIST_BASIC_INFO
        });
        var pRows = bIncludeChildren ? loadDetailRows({
            rootId: sRootId,
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

    /* Этот блок постранично читает коллекцию дочерних строк из OData.
     * Результат: большие наборы данных не грузятся одним тяжелым запросом. */
    function loadChunkedCollection(oFilterContract, sRootId, sSelect, fnMapRow) {
        var aRows = [];

        function loadPage(iSkip) {
            return GatewayODataClient.get(oFilterContract.entitySet, {
                "$filter": buildDetailFilter(oFilterContract, sRootId),
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

    /* Этот блок объединяет чтение checks и barriers в один отложенный owner.
     * Результат: контроллер и use case получают уже нормализованный набор строк. */
    function loadDetailRows(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var bChecks = !mArgs || mArgs.includeChecks !== false;
        var bBarriers = !mArgs || mArgs.includeBarriers !== false;
        var pChecks = bChecks
            ? loadChunkedCollection(
                ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_CHECK,
                sRootId,
                ODataEntityContracts.SELECTS.CHECKLIST_CHECK,
                ChecklistSnapshotMapper.mapCheckRow
            )
            : Promise.resolve([]);
        var pBarriers = bBarriers
            ? loadChunkedCollection(
                ODataEntityContracts.DETAIL_ENTITY_FILTERS.CHECKLIST_BARRIER,
                sRootId,
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

    /* Этот блок после save/read достраивает полный snapshot из backend-ответа.
     * Результат: UI получает согласованный root/meta state даже после переходных payload-ов. */
    function enrichServerSnapshot(oServerPayload, sFallbackRootId, mDeps) {
        var sResolvedRootId = mDeps.resolveServerRootId(oServerPayload, sFallbackRootId);
        if (!sResolvedRootId || mDeps.isCreateId(sResolvedRootId)) {
            return Promise.resolve(oServerPayload || {});
        }
        return fetchDetailSnapshot({ rootId: sResolvedRootId, includeChildren: true }, mDeps).then(function (oSnapshot) {
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
        loadDetailRows: loadDetailRows,
        loadDetailSnapshot: fetchDetailSnapshot,
        resolveRootId: resolveRootId
    };
});
