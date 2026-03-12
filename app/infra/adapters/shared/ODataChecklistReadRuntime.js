sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayAdapterSupport",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistSnapshotRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataAdapterUtils",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataKeyContracts"
], function (GatewayAdapterSupport, ODataChecklistSnapshotRuntime, ODataAdapterUtils, ODataKeyContracts) {
    "use strict";

    function loadDetailSnapshot(mArgs, mDeps) {
        var sRootId = mDeps.rootId(mArgs);
        var pRoot = GatewayAdapterSupport.get(ODataAdapterUtils.buildEntityPath("ChecklistRootSet", sRootId, {
            type: ODataKeyContracts.TYPES.ROOT_KEY
        }).replace(/^\//, ""));
        var pBasic = GatewayAdapterSupport.get("ChecklistBasicInfoSet", {
            "$filter": ODataAdapterUtils.buildEqFilter("RootKey", sRootId, ODataKeyContracts.TYPES.ROOT_KEY)
        });
        var pChecks = GatewayAdapterSupport.get("ChecklistCheckSet", {
            "$filter": ODataAdapterUtils.buildEqFilter("RootKey", sRootId, ODataKeyContracts.TYPES.ROOT_KEY)
        });
        var pBarriers = GatewayAdapterSupport.get("ChecklistBarrierSet", {
            "$filter": ODataAdapterUtils.buildEqFilter("RootKey", sRootId, ODataKeyContracts.TYPES.ROOT_KEY)
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
        return loadDetailSnapshot({ rootId: sResolvedRootId }, mDeps).then(function (oSnapshot) {
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
        loadDetailSnapshot: loadDetailSnapshot
    };
});
