sap.ui.define([
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/GatewayRequestRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ChecklistSnapshotMapper",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/AttachmentRepoRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPayloadMapper",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistSnapshotRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistReadRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistStatusRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistPermissionRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistMutationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/infra/adapters/shared/ODataChecklistExportRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/service/domain/shared/DetailRuntimePayload",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CreateSentinel"
], function (GatewayRequestRuntime, ChecklistSnapshotMapper, AttachmentRepoRuntime, ODataChecklistPayloadMapper, ODataChecklistSnapshotRuntime, ODataChecklistReadRuntime, ODataChecklistStatusRuntime, ODataChecklistPermissionRuntime, ODataChecklistMutationRuntime, ODataChecklistExportRuntime, DetailRuntimePayload, CreateSentinel) {
    "use strict";
    function rootId(mArgs) { return DetailRuntimePayload.rootId(mArgs); }
    function normalizeRootKey(sRootId) { return ODataChecklistPayloadMapper.normalizeRootKey(sRootId); }

    function mutationDeps() {
        return {
            enrichServerSnapshot: function (oServerPayload, sFallbackRootId) {
                return ODataChecklistReadRuntime.enrichServerSnapshot(oServerPayload, sFallbackRootId, readDeps());
            },
            normalizeRootKey: normalizeRootKey,
            rootId: rootId
        };
    }

    function readDeps() {
        return {
            isCreateId: CreateSentinel.isCreateId,
            resolveServerRootId: ODataChecklistPayloadMapper.resolveServerRootId,
            rootId: rootId
        };
    }

    function create(mDeps) {
        return {
            loadDetailSnapshot: function (mArgs) {
                return ODataChecklistReadRuntime.loadDetailSnapshot(mArgs, readDeps());
            },
            saveChecklist: function (mArgs) {
                return ODataChecklistMutationRuntime.saveChecklist(mArgs, mutationDeps());
            },
            createChecklist: function (mArgs) {
                return ODataChecklistMutationRuntime.createChecklist(mArgs, mutationDeps());
            },
            copyChecklist: function (mArgs) {
                return ODataChecklistMutationRuntime.copyChecklist(mArgs, mutationDeps());
            },
            autosaveChecklist: function (mArgs) {
                return ODataChecklistMutationRuntime.autosaveChecklist(mArgs, mutationDeps());
            },
            setChecklistStatus: function (mArgs) {
                return ODataChecklistStatusRuntime.setChecklistStatus(mArgs, mutationDeps());
            },
            deleteChecklist: function (mArgs) {
                return ODataChecklistStatusRuntime.deleteChecklist(mArgs, mutationDeps());
            },
            exportSearchResults: function (mArgs) {
                return ODataChecklistExportRuntime.exportSearchResults(mArgs);
            },
            checkChecklistPermission: function (mArgs) {
                return ODataChecklistPermissionRuntime.checkChecklistPermission(mArgs, {
                    firstRow: ODataChecklistSnapshotRuntime.firstRow,
                    normalizeRootKey: normalizeRootKey,
                    rootId: rootId
                });
            },
            loadAttachments: AttachmentRepoRuntime.loadAttachments,
            deleteAttachment: AttachmentRepoRuntime.deleteAttachment
        };
    }
    return { create: create };
});
