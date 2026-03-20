sap.ui.define([
    "sap/ui/model/json/JSONModel",
    "PRODUCTION_CONTROL_CHECKLIST/model/StateSchema",
    "PRODUCTION_CONTROL_CHECKLIST/service/features/detail/contracts/AttachmentUploadPolicy",
    "PRODUCTION_CONTROL_CHECKLIST/service/shared/CloneUtil",
    "PRODUCTION_CONTROL_CHECKLIST/service/framework/LayoutPersonalizationRuntime",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowConstants",
    "PRODUCTION_CONTROL_CHECKLIST/constants/WorkflowRuntimeConstants"
], function (JSONModel, StateSchema, AttachmentUploadPolicy, CloneUtil, LayoutPersonalizationRuntime, WorkflowContracts, WorkflowRuntimeConstants) {
    "use strict";

    function clone(v) {
        return CloneUtil.clone(v, {});
    }

    function createModel(vData) {
        return new JSONModel(vData);
    }

    function createTimers() {
        return clone(StateSchema.createTimers());
    }

    function createMasterDataDefaults() {
        return {
            dict: {},
            runtime: { timers: {}, requiredFields: [], uploadPolicy: AttachmentUploadPolicy.DEFAULT_UPLOAD_POLICY },
            persons: [],
            lpc: [],
            professions: [],
            checksNumbers: [],
            barriersNumbers: [],
            statuses: ["SUCCESS", "WARNING", "CRITICAL"],
            resultTypes: ["PASS", "FAIL"],
            types: [],
            timezones: [],
            attachmentTypes: []
        };
    }

    function createLayoutDefaults() {
        var oPersonalization = LayoutPersonalizationRuntime.readAll();
        return {
            smartFilter: { useCustomSmartFilters: true, variant: "default", fields: [] },
            smartTable: { useCustomSmartTable: true, variant: "default", columns: [], selectionMode: "MultiSelect" },
            personalization: {
                compactRows: !!oPersonalization.compactRows,
                showHints: oPersonalization.showHints !== false,
                infoCardLayout: Array.isArray(oPersonalization.infoCardLayout) ? oPersonalization.infoCardLayout : []
            }
        };
    }

    return {
        createDataModel: function () {
            return createModel({ checkLists: [], visibleCheckLists: [], selectedChecklist: null });
        },

        createUiStateModel: function () {
            return createModel({
                mode: WorkflowContracts.EDIT_MODES.READ,
                busy: false,
                currentRootKey: "",
                sessionGuid: "",
                lock: { ok: false, reason: "FREE", isKilled: false },
                timers: createTimers(),
                activity: { lastActiveAt: "", idleUntil: "" }
            });
        },

        createViewModel: function () {
            return createModel({
                root: {},
                basicInfo: {},
                checks: { items: [] },
                barriers: { items: [] },
                attachments: { items: [] },
                meta: { aggChangedOn: "" },
                sessionAttachments: [],
                showSessionAttachments: true,
                attachmentsExpanded: false,
                attachmentsLoaded: false,
                attachmentBusy: false,
                attachmentActionsEnabled: false,
                attachmentMetaEditable: false,
                attachmentDesktopColumnsVisible: true,
                attachmentActionsColumnWidth: "14rem",
                attachmentCategoryKey: ""
            });
        },

        createSnapshotModel: function () {
            return createModel({});
        },

        createCacheModel: function () {
            return createModel({ byRootKey: {}, pristineSnapshot: null, keyMapping: {}, lastServerState: null });
        },

        createMasterDataModel: function () {
            return createModel(createMasterDataDefaults());
        },

        createHierarchyModel: function () {
            return createModel({ byDate: {} });
        },

        createStateModel: function () {
            return createModel(StateSchema.createStateDefaults());
        },

        createLayoutModel: function () {
            return createModel(createLayoutDefaults());
        },

        createMplModel: function () {
            return createModel({ locations: [] });
        },

        createEnvModel: function () {
            return createModel({ source: WorkflowRuntimeConstants.SOURCES.GATEWAY, loadedAt: "", variables: {}, timers: createTimers() });
        }
    };
});
